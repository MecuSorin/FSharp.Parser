module Build.App

open Argu
open Fake.Core

type TargetToken =
    | Clean
    | AssemblyInfo
    | Restore
    | CheckFormatCode
    | Build
    | RunTests
    | NuGet
    | PublishNuGet
    | FormatCode
type AppArgument =
    | [<AltCommandLine("-t")>][<Unique>]Target of TargetToken
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Target _ -> "select a target"

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<AppArgument>(programName = "build.exe")
        let parsedArgs = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        match parsedArgs.GetResult (<@ Target @>, RunTests) with
            | Clean -> "Clean"
            | AssemblyInfo -> "AssemblyInfo"
            | Restore -> "Restore"
            | CheckFormatCode -> "CheckFormatCode"
            | Build -> "Build"
            | RunTests -> "RunTests"
            | NuGet -> "NuGet"
            | PublishNuGet -> "PublishNuGet"
            | FormatCode -> "FormatCode"
        |> fun targetName ->
            let targetResult =
                argv
                |> Array.toList
                |> Target.runSimple targetName
            match targetResult.WasSkipped, targetResult.Error with
            | true, _  ->
                System.Console.WriteLine("Target skipped")
                1
            | false, None -> 0
            | false, Some(error) ->
                System.Console.Error.WriteLine error
                1
    with e ->
        printfn "%s" e.Message
        1