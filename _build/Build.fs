module Build.App

open Argu
open Fake.Core
open Build.Specific

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
            argv
                |> Array.toList
                |> Context.FakeExecutionContext.Create false "build.fsx"
                |> Context.RuntimeContext.Fake
                |> Context.setExecutionContext
            initTargets ()
            Target.runOrDefaultWithArguments targetName
            0

    with e ->
        printfn "%s" e.Message
        1