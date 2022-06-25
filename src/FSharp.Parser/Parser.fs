namespace FSharp

open System
open System.Globalization
open System.Security.Cryptography


// module Core =
//     type 'msg Dispatcher = 'msg -> unit
//     type IDateTime =
//         abstract member Now: unit -> System.DateTime


//     let safeDo f = try f () with _ -> ()
//     let inline between a b x = a <=x && x <= b
//     let inline isPercent x = between 1 100 x
//     [<AutoOpen>]
//     module Option =
//         let toResult e = function
//             | Some v -> Ok v
//             | None -> Error e
//         let fromResult = function
//             | Ok v -> Some v
//             | _ -> None
//         let fromValidation validator value = if validator value then Some value else None
//         let NoError = None
//         let WithError e = Some e
//         type OptionBuilder() =
//             member __.Return(x) = Some x
//             member __.ReturnFrom(m: _ option) = m
//             member __.Bind(m, f) = Option.bind f m
//             member __.Bind(m: Result<_,_>, f) = Option.bind f (fromResult m)
//             // member __.Bind(m, f: _ -> Result<_,_>) = Option.bind (f >> fromResult) m

//             member __.Zero() = None
//             member __.Combine(m, f) = Option.bind f m
//             member __.Delay(f: unit -> _) = f
//             member __.Run(f) = f()
//         let maybe = OptionBuilder()

[<AutoOpen>]
module Result =
    //         let fromBool failure b = if b then Ok b else Error failure
//         let fromOption = toResult
//         let fromValidation errorMessage isValidCheck value = if isValidCheck value then Ok value else Error errorMessage
//         let notNull errorMessage value = if isNull value then Error errorMessage else Ok value
    let defaultValue defaultValue =
        function
        | Ok v -> v
        | Error e -> defaultValue

    let get =
        function
        | Ok v -> v
        | Error m -> failwithf "Cannot extract Ok value from the result Error %A" m
//         let toErrorOption = function
//             | Ok _ -> None
//             | Error e -> Some e
//         let mapIgnore result r = Result.map (fun _ -> result) r
//         let fallback toErrorProjection = function
//             | Ok v -> v
//             | Error e -> toErrorProjection e

//         type ResultBuilder() =
//             member __.Return(x) = Ok x
//             member __.ReturnFrom(m: Result<_, _>) = m
//             member __.Bind(m, f) = Result.bind f m
//             member __.Bind((m, error): (Option<'T> * 'E), f) = m |> fromOption error |> Result.bind f


//             member __.Zero() = None
//             member __.Combine(m, f) = Result.bind f m
//             member __.Delay(f: unit -> _) = f
//             member __.Run(f) = f()
//             member __.TryWith(m, h) =
//                 try __.ReturnFrom(m)
//                 with e -> h e
//             member __.TryFinally(m, compensation) =
//                 try __.ReturnFrom(m)
//                 finally compensation()
//             member __.Using(res:#IDisposable, body) =
//                 __.TryFinally(body res, fun () -> res.Dispose())
//             member __.While(guard, f) =
//                 if not (guard()) then Ok () else
//                 do f() |> ignore
//                 __.While(guard, f)
//             member __.For(sequence:seq<_>, body) =
//                 __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

//         let result = ResultBuilder()

[<AutoOpen>]
module String =
    open System.Text

    let private appendTo (sbuilder: System.Text.StringBuilder) (s: #seq<char>) =
        s |> Seq.iter (sbuilder.Append >> ignore)

    let build (s: #seq<char>) =
        let builder = System.Text.StringBuilder()
        appendTo builder s
        builder.ToString()

    let builds (s: #seq<#seq<char>>) =
        let builder = System.Text.StringBuilder()
        s |> Seq.iter (appendTo builder)
        builder.ToString()

    let showAscii (s: string) =
        s.ToCharArray()
        |> Seq.map (int >> string)
        |> String.concat " "
//         let maxLength maxChars str =
//             if String.IsNullOrEmpty(str)
//             then String.Empty
//             else str.Substring(0, Math.Min(str.Length, maxChars))

//         let inline eprint (e: exn) =
//             let stackTrace =
//                 e.StackTrace.Split("\n", StringSplitOptions.RemoveEmptyEntries)
//                 |> Array.filter(fun x -> x.Contains("SmartController", StringComparison.InvariantCultureIgnoreCase))
//             if stackTrace.Length > 0
//                 then
//                     e.Message :: (stackTrace |> Array.toList)
//                     |> String.concat "\n\t"
//             else e.Message
//         let toBase64 (s: string) = Convert.ToBase64String(Encoding.UTF8.GetBytes(s))
//         let fromBase64 (s: string) = Encoding.UTF8.GetString(Convert.FromBase64String(s))
//         let toResult onNullOrEmpty (s: string) =
//             if String.IsNullOrEmpty(s)
//                 then Error onNullOrEmpty
//                 else Ok s

//     module Path =
//         open System.IO
//         let combine a b =
//             try
//                 Path.Combine(a, b) |> Ok
//             with e -> Error e
//         let (</>) = combine
//         let combineRev a b = combine b a
//         let makeFolderIfNotExists a =
//             try
//                 if Directory.Exists a
//                     then (DirectoryInfo a).FullName |> Ok
//                     else (Directory.CreateDirectory a).FullName |> Ok
//             with e -> Error e
//         let makeCertsFolder rootFolder =
//             let result = result {
//                 let! folder = rootFolder </> ".certs"
//                 let! fullPath = makeFolderIfNotExists folder
//                 return DirectoryInfo fullPath }
//             match result with
//             | Ok path -> path
//             | Error e -> failwithf "Failed to create the certification folder structure: %s" e.Message

//         let readFileContent file =
//             if File.Exists(file)
//                 then File.ReadAllText(file) |> Ok
//                 else Error (sprintf "Path %s doesn't exists in the current folder %s" file (Directory.GetCurrentDirectory()))


//     module List =
//         let rec all = function
//             | [] -> true
//             | head::tail ->
//                 if head then all tail
//                         else false
//         let rec allResults = function
//             | [] -> true
//             | head::tail ->
//                 match head with
//                 | Ok true -> allResults tail
//                 | _ -> false
//         let chooseFirstSuccess resultsMap results =
//             let rec tryNext errors = function
//                 | [] -> Error errors
//                 | h :: tail ->
//                     match resultsMap h with
//                     | Ok v -> Ok v
//                     | Error e -> tryNext (e::errors) tail
//             tryNext [] results

//     module Array =
//         let toOption (a: _ []) = if 0 = a.Length then None else Some a
//         let safeTake (howMany: int) (fromArray: _[]) = Array.take (Math.Min(howMany, fromArray.Length)) fromArray

//     [<AutoOpen>]
//     module Async =
//         let fireAndForget a = Async.Start a
//         let Return v = async.Return v


module Parser =

    type IndexInString = StrIndex of int
    type 'T ParserFunc = string -> IndexInString -> IndexInString * Result<'T, string>
    type 'T Parser = Parser of ('T ParserFunc)


    let getSample padding (s: string) (StrIndex fromPos) (StrIndex toPosition) =
        let startingPos = Math.Max(0, fromPos - padding)
        let takeChars = toPosition + padding - startingPos + 1

        let actualCharsAvailable =
            Math.Min(s.Length - startingPos, takeChars)

        s.Substring(startingPos, actualCharsAvailable), String(' ', fromPos - startingPos) + "^"

    let getNext (StrIndex indx, s: string) = Math.Min(indx + 5, s.Length)

    let prun (Parser p) s = p s (StrIndex 0)
    let parse customParser s = prun customParser s |> snd

    let preturn (v: 'T) : 'T Parser = Parser <| fun s p -> p, Ok v
    let pfail msg : 'T Parser = Parser <| fun s p -> p, Error msg

    let pmap (f: 'T -> 'U) (Parser pft: 'T Parser) : 'U Parser =
        fun str pos ->
            match pft str pos with
            | newPos, Error msg -> newPos, Error msg
            | tPos, Ok tVal -> tPos, Ok(f tVal)
        |> Parser

    let pmapResult f (Parser p) =
        fun str pos ->
            match p str pos with
            | newPos, Error msg -> newPos, Error msg
            | tPos, Ok tVal -> tPos, f tVal
        |> Parser

    let pbind (ft: 'T -> 'U Parser) (Parser pft: 'T Parser) : 'U Parser =
        fun str pos ->
            match pft str pos with
            | newPos, Error msg -> newPos, Error msg
            | tPos, Ok tVal ->
                let (Parser uParser) = ft tVal
                uParser str tPos
        |> Parser

    let pmaybe (Parser p) =
        fun str pos ->
            match p str pos with
            | _, Error _ -> pos, Ok None
            | newPos, Ok tVal -> newPos, Ok(Some tVal)
        |> Parser
    /// Is checking ahead if the parser succeeds without advancing on success
    let peek (Parser p) =
        fun str pos ->
            match p str pos with
            | newPos, Error e -> newPos, Error e
            | newPos, Ok tVal -> pos, Ok tVal
        |> Parser
    /// succeeds on the EOF
    let pendOfInput =
        fun (str: string) (StrIndex pos) ->
            if pos >= str.Length then
                StrIndex pos, Ok()
            else
                StrIndex pos,
                Error
                <| sprintf "Not at the end of the input %i/%i" pos str.Length
        |> Parser

    let pLabelError label (Parser p: 'T Parser) : 'T Parser =
        let getDetails (s: string) (StrIndex i) =
            if i < s.Length then
                s.[i]
                |> int
                |> sprintf "[Index%i:%c:%i]" i (s.[i])
            else
                "[EndOfString]"

        fun str pos ->
            match p str pos with
            | newPos, Error msg ->
                newPos,
                Error
                <| sprintf "%s%s -> %s" (getDetails str newPos) label msg
            | position, Ok v -> position, Ok v
        |> Parser

    let pLogInputOnFail (Parser p) =
        fun str pos ->
            match p str pos with
            | newPos, Error msg ->
                //newPos, Error <| sprintf "%s >> InputText[%s] %A -> %A" msg (String.showAscii str) pos newPos
                let (sampleRow1, sampleRow2) = getSample 5 str pos newPos

                newPos,
                Error
                <| sprintf
                    "%s >> InputText:\n[%s]\n[%s]\n[%s] %A -> %A"
                    msg
                    sampleRow1
                    sampleRow2
                    (String.showAscii sampleRow1)
                    pos
                    newPos

            | tPos, Ok tVal -> tPos, Ok tVal
        |> Parser

    let pcombine (pu: 'U Parser) (pt: 'T Parser) : 'U Parser = pt |> pbind (fun _ -> pu)
    /// Think about it as a parsers generator: Given a list of items, from them we build parsers and apply those parsers in succession
    let ptraverse (f: 'A -> 'B Parser) (s: 'A list) : Parser<'B list> =
        fun source position ->
            let rec applyNext pos remainingElements accumulated =
                match remainingElements with
                | [] -> pos, Ok(List.rev accumulated)
                | h :: tail ->
                    let (Parser p) = f h
                    let resultVal = p source pos

                    match resultVal with
                    | epos, Error msg -> epos, Error msg
                    | newPos, Ok bVal -> applyNext newPos tail (bVal :: accumulated)

            applyNext position s []
        |> Parser

    let palternative (Parser a: 'T Parser) (Parser b: 'T Parser) : 'T Parser =
        fun source pos ->
            match a source pos with
            | pa, Ok ra -> pa, Ok ra
            | pa, Error msgA ->
                match b source pos with
                | pb, Ok rb -> pb, Ok rb
                | _, Error msgB -> pa, Error(sprintf "%s | %s" msgA msgB) // here i can show info about the second parser that fails too, but will be hard to read when too many alternatives
        |> Parser

    type ParserBuilder() =
        // return
        member __.Return v = preturn v
        // return!
        member __.ReturnFrom(p: 'T Parser) = p
        // let!, use!, do!
        member __.Bind(t, ft) = pbind ft t
        // expr1; expr2
        member __.Combine(pt, pu) = pcombine pu pt
        // if ... then
        member __.Zero() = preturn ()

    let parser = ParserBuilder()
    let (<?>) p label = pLabelError label p
    let (>>=) p f = pbind f p
    let (|>>) p f = pmap f p
    let (|>>>) p f = pmapResult f p
    let (>>%) (p: _ Parser) v : _ Parser = pbind (fun _ -> preturn v) p
    let (<|>) a b = palternative a b //<?> "altenative"
    /// Ignores the result from the first parser
    let (>>.) pu pt =
        parser {
            let! _ = pu
            let! t = pt
            return t
        }
    /// Ignores the result from the second parser
    let (.>>) (pu: 'U Parser) (pt: 'T Parser) : 'U Parser =
        parser {
            let! u = pu
            let! _ = pt
            return u
        }
    /// Applies both parsers and tuples the results
    let (.>>.) (pu: 'U Parser) (pt: 'T Parser) : Parser<'U * 'T> =
        parser {
            let! u = pu
            let! t = pt
            return (u, t)
        }

    let (>>?) p p2 = p .>> pmaybe p2
    let (?>>) p2 p = (pmaybe p2) >>. p
    let ptuple2 p1 p2 = p1 .>>. p2

    let ptuple3 p1 p2 p3 =
        parser {
            let! v1 = p1
            let! v2 = p2
            let! v3 = p3
            return (v1, v2, v3)
        }

    let ptuple4 p1 p2 p3 p4 =
        parser {
            let! v1 = p1
            let! v2 = p2
            let! v3 = p3
            let! v4 = p4
            return (v1, v2, v3, v4)
        }

    let ptuple5 p1 p2 p3 p4 p5 =
        parser {
            let! v1 = p1
            let! v2 = p2
            let! v3 = p3
            let! v4 = p4
            let! v5 = p5
            return (v1, v2, v3, v4, v5)
        }

    let pmany (Parser pt: 'T Parser) : 'T list Parser =
        fun source position ->
            let rec applyAgain pos accumulatedParserOutputs =
                match pt source pos with
                | _, Error _msg -> pos, Ok(List.rev accumulatedParserOutputs)
                | tPos, Ok t -> applyAgain tPos (t :: accumulatedParserOutputs)

            applyAgain position []
        |> Parser
        <?> "pmany"

    let pmany1 p =
        parser {
            let! head = p
            let! tail = pmany p
            return head :: tail
        }

    let pmanyTill p pend =
        parser {
            let! manyValues = pmany p
            let! _endValue = pend
            return manyValues
        }

    let pmany1Till p pend =
        parser {
            let! many1 = pmany1 p
            let! _endValue = pend
            return many1
        }
    /// Will take p until a pend can succeed, whitout consuming pend
    let pfewerTill (Parser p: 'T Parser) (Parser pend: 'E Parser) : Parser<'T list> =
        fun source position ->
            let rec applyAgain pos accumulatedParserOutputs =
                match pend source pos with
                | _, Ok _ -> pos, Ok(List.rev accumulatedParserOutputs)
                | ePos, Error msg ->
                    match p source pos with
                    | pPos, Ok pVal -> applyAgain pPos (pVal :: accumulatedParserOutputs)
                    | _ -> ePos, Error msg

            applyAgain position []
        |> Parser

    // Optimized version of pfewerTill panyChar pEnd |> pmap ignore
    // [<ObsoleteAttribute("Don't use this to apply the same parser after it like pskipWhileNot p >>. p. That is stupid")>]
    // let pskipWhileNot (Parser pEnd: 'T Parser) : unit Parser =
    //     fun source position ->
    //         let rec skip (StrIndex pos) =
    //             match pEnd source (StrIndex pos) with
    //             | _, Ok _ -> StrIndex pos, Ok ()
    //             | (StrIndex e, Error msg) when e >= source.Length -> StrIndex e, Error msg
    //             | _ -> pos + 1 |> StrIndex |> skip
    //         skip position
    //     |> Parser

    let pmanyChars p = pmany p |>> String.build
    let pmany1CharsTill p pend = pmany1Till p pend |>> String.build

    let pappend pc pl =
        parser {
            let! c = pc
            let! l = pl
            return c :: l
        }

    let pbetween pStart pEnd pContent =
        (pStart >>. pContent
         .>> (pEnd <?> "Greedy content grabbing the end"))
        <?> "Between"

    let psepBy p pSeparator =
        let many1 =
            parser {
                let! first = p <?> "First from sequence"

                let! rest =
                    pmany (pSeparator >>. p)
                    <?> "Remaining of sequence"

                return first :: rest
            }

        (many1 <|> preturn []) <?> "SepBy"

    let panyChar: Char Parser =
        fun (source: string) (StrIndex p: IndexInString) ->
            if p >= source.Length then
                StrIndex p,
                Error
                <| sprintf "No more characters in the input, while I am looking for a char at position %i" p
            else
                StrIndex <| p + 1, Ok(source.[p])
        |> Parser

    let psatisfy satisfy (msg: string) (Parser p: 'A Parser) : 'A Parser =
        fun s i ->
            match p s i with
            | ie, Error msg -> ie, Error msg // ie by intent to help understand the parser
            | newIndex, Ok v ->
                if satisfy v then
                    newIndex, Ok v
                else
                    i, Error msg
        |> Parser

    let psatisfyWithInfo satisfy (msgInvalid: 'A -> string) (Parser p: 'A Parser) : 'A Parser =
        fun s i ->
            match p s i with
            | ie, Error msg -> ie, Error msg // ie by intent to help understand the parser
            | newIndex, Ok v ->
                if satisfy v then
                    newIndex, Ok v
                else
                    i, Error(msgInvalid v)
        |> Parser

    let pOptionGetValue msgWhenNone (Parser p: 'A option Parser) =
        fun s i ->
            match p s i with
            | ie, Error msg -> ie, Error msg // ie by intent to help understand the parser
            | newIndex, Ok None -> newIndex, Error msgWhenNone
            | newIndex, Ok (Some v) -> newIndex, Ok v
        |> Parser

    let pdigit =
        psatisfy Char.IsDigit "Looking for a digit" panyChar

    let pletter =
        psatisfy Char.IsLetter "Looking for a letter" panyChar

    let plower =
        psatisfy Char.IsLower "Looking for a lower letter" panyChar

    let pupper =
        psatisfy Char.IsUpper "Looking for an upper letter" panyChar

    let pletterOrDigit =
        psatisfy Char.IsLetterOrDigit "Looking for a letter or digit" panyChar

    let pspace =
        psatisfy Char.IsWhiteSpace "Looking for a space character" panyChar

    let pspaces = pmany pspace

    let pchar (c: Char) : Char Parser =
        psatisfyWithInfo (fun v -> v = c) (sprintf "Looking for %c <> %c" c) panyChar

    let pcharCI (c: Char) : Char Parser =
        let lowerInvariantSample = Char.ToLowerInvariant(c)

        psatisfy
            (fun v -> Char.ToLowerInvariant(v) = lowerInvariantSample)
            (sprintf "Looking for case insensitive %c" c)
            panyChar

    let phex =
        pdigit
        <|> pcharCI 'a'
        <|> pcharCI 'b'
        <|> pcharCI 'c'
        <|> pcharCI 'd'
        <|> pcharCI 'e'
        <|> pcharCI 'f'

    let pstring (s: string) : string Parser =
        fun (str: string) (StrIndex pos) ->
            if isNull s then
                StrIndex pos, Error "Not supporting null strings"
            else if s.Length + pos > str.Length then
                StrIndex pos,
                Error
                <| sprintf "The substring is not matching the input (insuficient input). Looking for %s" s
            else
                let extracted = str.Substring(pos, s.Length)

                if s = extracted then
                    StrIndex(pos + s.Length), Ok extracted
                else
                    StrIndex pos,
                    Error
                    <| sprintf "The substring is not matching the input. Looking for %s but got %s" s extracted
        |> Parser

    let pstringCI (s: string) : string Parser =
        fun (str: string) (StrIndex pos) ->
            if isNull s then
                StrIndex pos, Error "Not supporting null strings"
            else if s.Length + pos > str.Length then
                StrIndex pos,
                Error
                <| sprintf "The substring is not matching the input (insuficient input). Looking for %s" s
            else
                let extracted = str.Substring(pos, s.Length)

                if 0 = String.Compare(s, extracted, StringComparison.OrdinalIgnoreCase) then
                    StrIndex(pos + s.Length), Ok extracted
                else
                    StrIndex pos,
                    Error
                    <| sprintf "The substring is not matching the input. Looking for %s but got %s" s extracted
        |> Parser

    let pnewLineLinux = pstring "\n"
    let pnewLineWin = pstring "\r\n"
    let pnewLine = pnewLineWin <|> pnewLineLinux

    let pendOfRow =
        pnewLine |>> ignore <|> pendOfInput <?> "Row End"

    let pstringTill p = pfewerTill panyChar p |>> String.build

    let pnoneOf (s: string) =
        psatisfy (fun c -> not <| s.Contains(c.ToString())) (sprintf "Looking for any char but [%s]" s) panyChar

    let psign = pchar '+' <|> pchar '-' <|> preturn '+'

    let pint16 =
        parser {
            let! sign = psign
            let! digits = pmany1 pdigit

            let! nr =
                match Int16.TryParse(sign :: digits |> String.build) with
                | true, v -> preturn v
                | _ -> pfail "The number was out of range of Int16"

            return nr
        }

    let puint16 =
        parser {
            let! digits = pmany1 pdigit

            let! nr =
                match UInt16.TryParse(digits |> String.build) with
                | true, v -> preturn v
                | _ -> pfail "The number was out of range of Int16"

            return nr
        }

    let pbyte =
        pmany1 pdigit |>> String.build |>> Byte.TryParse
        >>= function
            | true, v -> preturn v
            | _ -> pfail "The number was out of range of Byte"
    /// System.Int32
    let pint =
        parser {
            let! sign = psign
            let! digits = pmany1 pdigit

            let! nr =
                match Int32.TryParse(sign :: digits |> String.build) with
                | true, v -> preturn v
                | _ -> pfail "The number was out of range of Int32"

            return nr
        }

    let pfloat =
        let decimalSeparator = '.'
        let pdecimalSeparator = pchar decimalSeparator
        let pifNoDigitsThen0 = (pmany1 pdigit <|> preturn [ '0' ])

        let form1 =
            parser {
                let! integral = pappend psign (pmany1 pdigit)

                let! decimal =
                    (pappend pdecimalSeparator pifNoDigitsThen0)
                    <|> preturn [ decimalSeparator; '0' ]

                return [ integral; decimal ] |> String.builds
            }

        let form2 =
            parser {
                let! integral = pappend psign pifNoDigitsThen0
                let! decimal = pappend pdecimalSeparator (pmany1 pdigit)
                return [ integral; decimal ] |> String.builds
            }

        parser {
            let! result = form1 <|> form2

            return!
                match Double.TryParse(result, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, v -> preturn v
                | _ -> pfail "The number was out of range of Decimal"
        }

    ///This is a way to have a recursive grammar. After you are creating it, you can use it in the initialization Action provided as the second tuple argument
    let inline createRecursiveParser () : 'T Parser * ('T Parser -> unit) =
        let dummy: 'T Parser =
            (fun s i -> failwith "You should initialize this recursive parser")
            |> Parser

        let r = ref dummy

        Parser
            (fun s i ->
                match r.Value with
                | Parser p -> p s i),
        fun p -> r.Value <- p

    let pEatWhileNotParserThenParser (Parser pEnd: 'T Parser) : 'T Parser =
        fun source position ->
            let rec skip (StrIndex pos) =
                match pEnd source (StrIndex pos) with
                | i, Ok v -> i, Ok v
                | (StrIndex e, Error msg) when e >= source.Length ->
                    StrIndex e,
                    Error(
                        sprintf
                            "Reached the EndOfString while eating chars trying to match a complex parser that fails with %s"
                            msg
                    )
                | _ -> pos + 1 |> StrIndex |> skip

            skip position
        |> Parser

    let (>@>) (pc: 't Parser) (pl: 't list Parser) : 't list Parser = pc .>>. pl |>> fun (c, l) -> l @ [ c ]
    /// Pay attention to the order of parsers. In the "1a" case use preturn [] <@< pchar 'a' <@< pdigit
    let (<@<) (pl: 't list Parser) (pc: 't Parser) : 't list Parser = pc .>>. pl |>> fun (c, l) -> c :: l

    let extractIp s =
        parse (pfewerTill panyChar (pchar ':') |>> String.build) s
        |> Result.defaultValue s

    let inline readDULabel defaultValue value =
        sprintf "%A " value
        |> parse (pfewerTill panyChar (pchar ' ') |>> String.build)
        |> Result.defaultValue defaultValue

    module Guid =
        let internal parse4times tailParser p = tailParser <@< p <@< p <@< p <@< p
        let internal hex4 = parse4times (preturn []) phex
        let internal hex8 = parse4times hex4 phex
        let internal hex12 = parse4times hex8 phex
        let internal separator = pchar '-'

        let parser =
            preturn []
            <@< hex12
            <@< (hex4 .>> separator)
            <@< (hex4 .>> separator)
            <@< (hex4 .>> separator)
            <@< (hex8 .>> separator)
            |>> List.map String.build
