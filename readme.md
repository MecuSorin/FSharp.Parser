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
