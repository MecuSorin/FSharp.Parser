module FSharp.Tests.Parser

open Expecto
open FSharp
open FSharp.Parser

let inline simpleParseHelper parser input =
    let mbActual = parse parser input
    Expect.isOk mbActual "The parser should succeed"

let inline testHelper mbDescription parser expected input =
    let mbActual = parse parser input
    Expect.isOk mbActual (sprintf "Failed for input: %s%s" (Option.defaultValue "" mbDescription) input)
    Expect.equal (mbActual |> Result.get) expected (sprintf "Failed for input: %s" input)

let inline testParserFailHelper parser expectedFailurePos input =
    let (StrIndex pos, mbActual) = prun parser input
    Expect.isError mbActual "The parser should fail"
    Expect.equal pos expectedFailurePos "The failure position is not right"

let inline testParserSuccessHelper parser expectedSuccessPos expected input =
    let (StrIndex pos, mbActual) = prun parser input
    Expect.isOk mbActual "The parser should succeed"
    Expect.equal pos expectedSuccessPos "The success position is not right"
    Expect.equal (mbActual |> Result.get) expected (sprintf "Failed for input: %s" input)

let charTests =
    test "pChar" {
        let helper p c input = testHelper None p c input
        helper (pchar 'A' <?> "pchar") 'A' "ABC"

        helper
            (((pchar 'A' <?> "pcharA")
              >>. (pchar 'B' <?> "pCharB"))
             <?> "pcharAB")
            'B'
            "ABC"

        Expect.isError (parse (pchar 'a') "ABC") "Should not parse 'a' from \"ABC\""
    }

let floatTests =
    testCase "pfloat" (fun _ ->
        let testAgainst = testHelper None pfloat
        testAgainst 0. "0"
        testAgainst 0. "+0"
        testAgainst 0. "-0"
        testAgainst 3. "+3"
        testAgainst 3. "3,"
        testAgainst -100.0 "-100"
        testAgainst -0.12 "-.12"
        testAgainst 123.456 "123.456")

let spaceTests =
    test "pspace" {
        let helper spaceChar description =
            testHelper (Some description) pspace spaceChar (spaceChar.ToString())

        helper ' ' "pause"
        helper '\t' "tab"
        helper '\n' "newLine"
    }

let manyTests =
    test "pmany" {
        let helper p input expected =
            testHelper None (pmany p) expected input

        helper (pspace >>. pdigit) " 1" [ '1' ]
        helper (pspace >>. pdigit) " 1 " [ '1' ]
        helper (pspace >>. pdigit) " 1 2" [ '1'; '2' ]
        helper (pspace >>. pdigit) "1" []
        helper (pdigit .>> pspace) "1 " [ '1' ]
        helper (pdigit .>> pspace) "1 2 " [ '1'; '2' ]
        helper (pdigit .>> pspace) "1 2" [ '1' ]
    }

let maybeTests =
    test "pmaybe" {
        let helper p input pos expected =
            testParserSuccessHelper (pmaybe p) pos expected input

        helper pint "123a" 3 (Some 123)
        helper pint "a123a" 0 (None)
        helper pint "" 0 (None)
        testParserSuccessHelper (pint >>? pchar 'a') 3 123 "123"
        testParserSuccessHelper (pint >>? pchar 'a') 3 123 "123b"
        testParserSuccessHelper (pchar 'a') 1 'a' "abb"
        testParserSuccessHelper (pint .>> pchar 'a') 4 123 "123a"
        testParserSuccessHelper (pint >>? pchar 'a') 4 123 "123a"
        testParserSuccessHelper (pchar 'x' ?>> pchar 'a' |> pLogInputOnFail) 1 'a' "acc"
        testParserSuccessHelper (pint ?>> pchar 'a' |> pLogInputOnFail) 1 'a' "abc"
    }

let pstringTests =
    test "pstring and pstringCI" {
        let helper parserBody input expected =
            testHelper None (pstring parserBody) expected input

        let helperCI parserBody input expected =
            testHelper None (pstringCI parserBody) expected input

        helper "CARTH" "CARTH" "CARTH"
        helper "CARTH" "CARTHesia" "CARTH"
        helperCI "caRTH" "CaRTH" "CaRTH"
        helperCI "cARTH" "cARTHesia" "cARTH"
    }

let manyCharTests =
    test "pmanyChar" {
        let helper input expected =
            testHelper None (pmanyChars pdigit) expected input

        helper "123a" "123"
        helper "1 23" "1"
        helper "a234" ""
    }

let compositionTests =
    test "composition operators" {
        let inline helper input parser expected = testHelper None parser expected input
        helper " -123" (pspace >>. pint) -123
        helper " 234" (pspace >>. pmanyChars pdigit) "234"
        helper "1 2" (ptuple3 pdigit pspace pdigit) ('1', ' ', '2')

        helper
            "gold"
            ((pstring "silver" >>% "Ag")
             <|> (pstring "gold" >>% "Au"))
            "Au"

        helper
            "silver"
            ((pstring "silver" >>% "Ag")
             <|> (pstring "gold" >>% "Au"))
            "Ag"

        helper "123" (pint >>% "nope") "nope"
        helper "12" (pdigit .>> pdigit) '1'
        helper "[1]" (pbetween (pchar '[') (pchar ']') pdigit) '1'
        helper "1,2,3" (psepBy pdigit (pchar ',')) [ '1'; '2'; '3' ]
        helper "1" (psepBy pdigit (pchar ',')) [ '1' ]
        helper "a" (psepBy pdigit (pchar ',')) []
    }

type TreeNode =
    | Leaf of int
    | Node of int * TreeNode * TreeNode

let recursiveParsersTests =
    test "Recursive parser" {
        let (treeParser, parserInitializer) = createRecursiveParser ()
        let leafParser = (pchar 'L' >>. pint |>> Leaf) <?> "Leaf"

        let nodeParser =
            (pchar 'P' >>. ptuple3 pint treeParser treeParser
             |>> Node)
            <?> "Node"

        leafParser <|> nodeParser |> parserInitializer

        let helper input expected =
            testHelper None treeParser expected input

        helper "L1" <| Leaf 1
        helper "P2L1L3" <| Node(2, Leaf 1, Leaf 3)

        helper "P4P2L1L3L5"
        <| Node(4, Node(2, Leaf 1, Leaf 3), Leaf 5)
    }

let lookAheadTests =
    test "pfewerTill" {
        let skipper = pfewerTill (pchar '+' <|> pchar '-' <|> pdigit) (pstring "+++")
        // testHelper None (skipper >>. pskipWhileNot pdigit >>. pint) 5 "+1-2++3+-++4++++5+++6"
        testParserFailHelper (skipper >>. panyChar) ("+1-2++3+".Length) "+1-2++3+"
    }

let endOfInputTests =
    test "pendOfInput" {
        testHelper None (pint .>> pendOfInput) 2 "+2"
        testParserFailHelper (pint .>> pendOfInput) 2 "+2a"
        testHelper None pint 2 "+2a"
    }

let getSampleTests =
    test "Parsers getSample tests" {
        let input = "01234567890"

        let testSample a b output =
            Expect.equal (getSample 2 input (StrIndex a) (StrIndex b)) output "Should be equal"

        testSample 0 10 (input, "^")
        testSample 2 3 ("012345", "  ^")
        testSample 10 10 ("890", "  ^")
        testSample 0 0 ("012", "^")
        testSample 4 4 ("23456", "  ^")
        testSample 1 5 ("01234567", " ^")
    }

let guidTest =
    test "Guid" {
        let guid = System.Guid.NewGuid().ToString()

        let actual =
            parse Guid.parser guid
            |> Result.map (String.concat "-")

        Expect.equal (Ok guid) actual "Should be equal"
    }

let parsersTests =
    testList
        "Parsers"
        [ pstringTests
          getSampleTests
          charTests
          floatTests
          spaceTests
          manyCharTests
          maybeTests
          compositionTests
          manyTests
          recursiveParsersTests
          lookAheadTests
          guidTest
          endOfInputTests ]