module FSharp.TestsMain

open Expecto
open FSharp.Tests.Parser

[<EntryPoint>]
let main argv =
    runTestsWithArgs
        { defaultConfig with
            verbosity = Expecto.Logging.LogLevel.Info
            printer = Expecto.Impl.TestPrinters.summaryPrinter defaultConfig.printer }
        [||]
        parsersTests