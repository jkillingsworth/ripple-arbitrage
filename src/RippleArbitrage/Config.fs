module RippleArbitrage.Config

open System.Configuration

//-------------------------------------------------------------------------------------------------

let private reader = AppSettingsReader()

let serverUri = reader.GetValue("serverUri", typeof<string>) :?> string
let accountId = reader.GetValue("accountId", typeof<string>) :?> string
let secretKey = reader.GetValue("secretKey", typeof<string>) :?> string
