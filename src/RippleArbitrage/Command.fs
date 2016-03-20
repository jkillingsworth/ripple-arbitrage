module RippleTool.Command

open Chiron
open WebSocket4Net
open RippleTool.Types

//-------------------------------------------------------------------------------------------------

module Request =

    module Json =

        let private required name toJson value elements =
            (name, toJson value) :: elements

        let private optional name toJson = function
            | Some value -> value |> required name toJson
            | None -> id

        let private toObject = Object << Map
        let private ofString = String

        let private ofIssuedCurrency currency =
            []
            |> required "currency" ofString currency.Code
            |> required "issuer" ofString currency.Issuer
            |> toObject

        let private ofNativeCurrency currency =
            []
            |> required "currency" ofString "XRP"
            |> toObject

        let private ofCurrency = function
            | IssuedCurrency currency -> ofIssuedCurrency currency
            | NativeCurrency currency -> ofNativeCurrency currency

        let private ofLedger = function
            | Validated -> ofString "validated"
            | Closed    -> ofString "closed"
            | Current   -> ofString "current"

        let ofBookOffersRequest (command : BookOffersRequest) =
            []
            |> required "command" ofString "book_offers"
            |> optional "ledger_index" ofLedger command.Ledger
            |> required "taker_gets" ofCurrency command.TakerGets
            |> required "taker_pays" ofCurrency command.TakerPays
            |> toObject

//-------------------------------------------------------------------------------------------------

module Response =

    module Json =

        let private error () = failwith "Json error."

        let private required ofJson name = function
            | Object x
                -> if x.ContainsKey(name) then x.[name] |> ofJson else error ()
            | _ -> error ()

        let private optional ofJson name = function
            | Object x
                -> if x.ContainsKey(name) then x.[name] |> ofJson |> Some else None
            | _ -> error ()

        let private toString = function
            | String x -> x
            | _ -> error ()

        let private toArray = function
            | Array x -> x
            | _ -> error ()

        let private toAmount = function
            | String x
                -> { Value = (decimal x) / 1000000M; Currency = NativeCurrency Xrp }
            | Object x
                ->
                let value    = x |> Object |> required toString "value"
                let currency = x |> Object |> required toString "currency"
                let issuer   = x |> Object |> required toString "issuer"

                { Value = decimal value
                  Currency = IssuedCurrency { Code = currency; Issuer = issuer } }
            | _
                -> error ()

        let private toOffer json =

            { TakerGets = json |> required toAmount "TakerGets"
              TakerPays = json |> required toAmount "TakerPays" }

        let toBookOffersResponse json =

            json
            |> required id "result"
            |> required id "offers"
            |> toArray
            |> List.map toOffer

//-------------------------------------------------------------------------------------------------

let execute serverUri request =

    let computation = async {

        use ws = new WebSocket(serverUri)

        ws.Open()
        let! ea = Async.AwaitEvent(ws.Opened)

        ws.Send(request : string)
        let! ea = Async.AwaitEvent(ws.MessageReceived)
        let response = ea.Message

        ws.Close()
        let! ea = Async.AwaitEvent(ws.Closed)

        return response
    }

    Async.RunSynchronously(computation, 5000)
