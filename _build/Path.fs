namespace Build

module Result =
    let toOption = function
        | Ok v -> Some v
        | Error _ -> None
    let get = function
        | Ok v -> v
        | Error e -> failwithf "get: %A" e

module Path =
    open System.IO

    let combine a b =
        try
            Path.Combine(a, b) |> Ok
        with e -> Error e
    let combineRev a b = combine b a
    let makeFolderIfNotExists a =
        try
            if Directory.Exists a
                then (DirectoryInfo a).FullName |> Ok
                else (Directory.CreateDirectory a).FullName |> Ok
        with e -> Error e

    let stitch toPath pathFragments =
        let rec loop fragments  = function
            | Ok current ->
                match fragments with
                | [] -> Ok current
                | fragment :: restOfFragments ->
                    loop restOfFragments (combine current fragment)
            | Error r -> Error r
        loop pathFragments (Ok toPath)
    let findCandidateUpperInTree folder fileName =
        [0 .. 10]
        |> List.choose(fun x ->
            List.replicate x ".."
            |> stitch folder
            |> Result.bind (combineRev fileName)
            |> Result.toOption)
        |> List.tryFind File.Exists

    let rootDir =
        findCandidateUpperInTree "." "paket.dependencies"
        |> Option.map (fun packetDependenciesPath -> (FileInfo packetDependenciesPath).Directory.FullName)
        |> Option.defaultValue "."


