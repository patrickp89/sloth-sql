namespace SlothSQL.Parser

module Util =
    open FSharp.Text.Lexing

    type Info =
    | FI of string * int * int
    | UNKNOWN

    let info (lexbuf: LexBuffer<_>) =
        let f = "" // TODO: use lexbuf.StartPos.FileName()!
        let l = lexbuf.StartPos.Line
        let c = lexbuf.StartPos.AbsoluteOffset // TODO: is this the offset in the line or from the very start?
        Info.FI(f, l, c)
