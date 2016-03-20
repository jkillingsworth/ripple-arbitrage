module RippleTool.Types

open System

//-------------------------------------------------------------------------------------------------

type IssuedCurrency = { Code : string; Issuer : string }
type NativeCurrency = Xrp

type Currency =
    | IssuedCurrency of IssuedCurrency
    | NativeCurrency of NativeCurrency

type Amount = { Value : decimal; Currency : Currency }

type Ledger =
    | Validated
    | Closed
    | Current

//-------------------------------------------------------------------------------------------------

type BookOffersRequest =
    { Ledger    : Ledger option
      TakerGets : Currency
      TakerPays : Currency }

type BookOffer =
    { TakerGets : Amount
      TakerPays : Amount }

type BookOffersResponse = BookOffer list
