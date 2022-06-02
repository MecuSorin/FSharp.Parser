# FSharp.Parser

A simple parser that is using a similar syntax with the big brother FParsec.

### Reason to be

I had issues with using FParsec(installing) on a computer with Windows in Deutch, so instead of spending time on fixing that particular issue, I invested 3h to write this.

### Roadmap

- Plan in the future to add proper documentation.
- To add support for "replace" in text.

## Samples

```fsharp
#r "nuget: FSharp.Parser"
open FSharp
open FSharp.Parser

let name =
    pfewerTill panyChar (pchar ':')                     // take all chars until ':'
    .>> pchar ':' .>> pspace                            // skip ':' and whitespace
    >>. pfewerTill (pletter <|> pspace) (pchar '.')     // drops the previous chars and take all letters or spaces until '.' without consumming '.'
    .>> pchar '.'                                       // skip '.' (can be omitted)
    .>> pendOfInput                                     // check if we reached the end of the input (can be omitted)
    |>> String.build                                    // compose the string from the sequence of chars
parse name "Hello, My name is: Inigo Montoya."
```

------------------

```fsharp
#r "nuget: FSharp.Parser"

open FSharp
open FSharp.Parser

type FileSpec = {
    id: FileId
    key: FileKey
    fileType: FileType
    fileName: FileName
}
and FileId = FileId of int
and FileKey = FileKey of string
and FileType = ImagePng | Unknown of string
and FileName = string


let sample = """

:id: 33
:key: 11345/33
:filetype: image/png
:filename: test.png"""
module Parsing =
    let pFileSpecFileIdRow = pspaces >>. pstringCI ":id:" .>> pspaces >>. pint <?> "FileSpec id" |>> FileId
    let pFileSpecKeyRow = pspaces >>. pstringCI ":key:" .>> pspaces >>. (pmany1 (pletter <|> pdigit <|> pchar '/')) <?> "FileSpec key" |>> (String.build >> FileKey)
    let pFileTypeValue = 
        pstringCI "image/png" >>% ImagePng
        <|> (pfewerTill (pletter <|> pchar '/') pspaces |>> (String.build >> FileType.Unknown))
    let pFileTypeRow = pspaces >>. pstringCI ":filetype:" .>> pspaces >>. pFileTypeValue <?> "FileSpec filetype"
    let pFileNameRow = pspaces >>. pstringCI ":filename:" .>> pspaces >>. pfewerTill panyChar pendOfRow <?> "FileSpec filename" |>> String.build
    let pFileSpec = ptuple4 pFileSpecFileIdRow pFileSpecKeyRow pFileTypeRow pFileNameRow |>> fun (i,k,t,n) -> { id = i; key = k; fileType = t; fileName = n }
    let pIgnoreAllUntilMyRecord = pfewerTill panyChar pFileSpec
    let pConfig = pIgnoreAllUntilMyRecord >>. pFileSpec

sample
|> parse Parsing.pConfig
|> printfn "%A"
```
is printing
```
Ok { id = FileId 33
     key = FileKey "11345/33"
     fileType = ImagePng     
     fileName = "test.png" } 
```
