module Parser = 
    type Token = 
        | A
        | B
        | OpenParen
        | CloseParen
        | VerticalBar
        | Asterisk
        | Eof
    
    type Ast = 
        | RegA
        | RegB
        | RegOr of Ast * Ast
        | RegAnd of Ast * Ast
        | RegStar of Ast
    
    let tokenize characterStream = 
        let left = 
            characterStream
            |> Seq.filter (fun c -> not (System.Char.IsWhiteSpace c))
            |> Seq.map (function 
                   | 'a' -> A
                   | 'b' -> B
                   | '(' -> OpenParen
                   | ')' -> CloseParen
                   | '|' -> VerticalBar
                   | '*' -> Asterisk
                   | ch -> failwith (sprintf "Unknown character in the input stream [%c]" ch))
        Seq.append left [ Eof ]
    
    let parse tokenStream = 
        let mutable stream = tokenStream |> List.ofSeq
        
        let eat (token : Token) = 
            match stream with
            | h :: t -> 
                if h = token then stream <- t
                else failwith (sprintf "Unexpected token in the stream. Expected [%A] got [%A]" token h)
            | _ -> failwith "Reading empty stream"
        
        let lookahead() = 
            match stream with
            | h :: _ -> h
            | _ -> failwith "Looking at empty stream"
        
        let rec parseOuter() = 
            match lookahead() with
            | A -> 
                eat A
                RegA
            | B -> 
                eat B
                RegB
            | OpenParen -> 
                eat OpenParen
                let lhs = parseOuter()
                let res = parseNested lhs
                eat CloseParen
                res
            | tok -> failwith (sprintf "Parsing error. Unexpected token [%A]" tok)
        
        and parseNested lhs = 
            match lookahead() with
            | Asterisk -> 
                eat Asterisk
                RegStar lhs
            | VerticalBar -> 
                eat VerticalBar
                RegOr(lhs, parseOuter())
            | _ -> RegAnd(lhs, parseOuter())
        
        let result = parseOuter()
        eat Eof
        result

[<EntryPoint>]
let main args = 
    let expressions = [ "((ab)|(ba))"; "((a|b)*)"; "((a*)(b(a*)))" ]
    let transform = Parser.tokenize >> Parser.parse
    expressions
    |> List.map transform
    |> List.iter (printf "%A\n")
    0 // return an integer exit code
