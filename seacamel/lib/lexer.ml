type token = 
    | Ident of string
    | IntLit of int
    | StringLit of string
    | CharLit of char
    | Keyword of string
    | Op of string
    | LParen | RParen | LBrace | RBrace | LBracket | RBracket
    | Semicolon | Comma
    | EOF

let string_of_token = function
    | Ident s -> Printf.sprintf "Ident %s" s
    | IntLit i -> Printf.sprintf "IntLit %d" i
    | StringLit s -> Printf.sprintf "StringLit %s" s
    | CharLit c -> Printf.sprintf "CharLit %c" c
    | Keyword s -> Printf.sprintf "Keyword %s" s
    | Op s -> Printf.sprintf "Op %s" s
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | LBracket -> "LBracket"
    | RBracket -> "RBracket"
    | Semicolon -> "Semicolon"
    | Comma -> "Comma"
    | EOF -> "EOF"

type lexer_state = {
    src: string;
    mutable pos: int;
    len : int;
}

let rec skip_whitespace_and_comments (state : lexer_state) = 
    if state.pos < state.len then
    match state.src.[state.pos] with
    | ' ' | '\t' | '\n' | '\r' ->
        state.pos <- state.pos + 1;
        skip_whitespace_and_comments state
    | '/' ->
        if state.pos + 1 < state.len then
            (* Single line comment *)
            if state.src.[state.pos + 1] = '/' then (
                state.pos <- state.pos + 2;
                while state.pos < state.len && state.src.[state.pos] <> '\n' do
                    state.pos <- state.pos + 1
                done;
                skip_whitespace_and_comments state
            )

            (* Block comment *)
            else if state.src.[state.pos + 1] = '*' then (
                state.pos <- state.pos + 2;
                let rec consume_block () = 
                    if state.pos + 1 >= state.len then
                        failwith "Unterminated block comment";
                    if state.src.[state.pos] = '*' && state.src.[state.pos + 1] = '/' then
                        state.pos <- state.pos + 2
                    else (state.pos <- state.pos + 1; consume_block ())
                in
                consume_block ();
                skip_whitespace_and_comments state
            )
            else ()
    | _ -> ()

let keyword_list = [
  "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do";
  "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if"; "inline"; "int"; "long";
  "register"; "restrict";   "return"; "short"; "signed";
  "sizeof"; "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void"; "volatile"; "while";
  "_Alignas"; "_Alignof"; "_Atomic"; "_Bool"; "_Complex"; "_Generic"; "_Imaginary"; "_Noreturn";
  "_Static_assert"; "_Thread_local"; "bool";  "true"; "false"; "typeof";
]

let lex_identifier_or_keyword state = 
    let start = state.pos in
    state.pos <- state.pos + 1;
    while state.pos < state.len &&
            (Utilities.is_alnum state.src.[state.pos] || state.src.[state.pos] = '_') do
        state.pos <- state.pos + 1
    done;
    let text = String.sub state.src start (state.pos - start) in
    if List.mem text keyword_list then Keyword text else Ident text

let lex_number state = 
    let start = state.pos in
    state.pos <- state.pos + 1;
    while state.pos < state.len && Utilities.is_digit state.src.[state.pos] do
        state.pos <- state.pos + 1
    done;
    IntLit (int_of_string (String.sub state.src start (state.pos - start)))

let lex_string state = 
    state.pos <- state.pos + 1;
    let buf = Buffer.create 32 in
    while state.pos < state.len && state.src.[state.pos] <> '"' do
        let c = state.src.[state.pos] in
        if c = '\\' then (
            state.pos <- state.pos + 1;
            if state.pos >= state.len then failwith "Unterminated escape";
            match state.src.[state.pos] with
            | 'n' -> Buffer.add_char buf '\n'
            | 't' -> Buffer.add_char buf '\t'
            | '\\' -> Buffer.add_char buf '\\'
            | '"' -> Buffer.add_char buf '"'
            | other -> Buffer.add_char buf other
        ) else
            Buffer.add_char buf c;
        state.pos <- state.pos + 1
    done;
    if state.pos >= state.len then failwith "Unterminated string literal";
    state.pos <- state.pos + 1;
    StringLit (Buffer.contents buf)

let multi_ops = [
  ">>="; "<<="; "->"; "++"; "--"; "<="; ">="; "==" ; "!=";
  "<<"; ">>"; "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^=";
]

let lex_operator (state : lexer_state) = 
    let remaining = state.len - state.pos in
    let try_op op = 
        let len = String.length op in
        remaining >= len && String.sub state.src state.pos len = op
    in
    match List.find_opt try_op multi_ops with
    | Some op -> 
        state.pos <- state.pos + String.length op;
        Op op
    | None ->
        let c = state.src.[state.pos] in
        state.pos <- state.pos + 1;
        Op (String.make 1 c)

let lex_char_literal (state: lexer_state) = 
    state.pos <- state.pos + 1;
    let character : char = state.src.[state.pos] in
    state.pos <- state.pos + 1;
    if state.src.[state.pos] <> '\'' then failwith "Improper character literal"
    else CharLit character

let next_token (state : lexer_state) : token = 
    skip_whitespace_and_comments state;
    if state.pos >= state.len then EOF
    else
        let c = state.src.[state.pos] in
        if Utilities.is_alpha c || c = '_' then lex_identifier_or_keyword state
        else if Utilities.is_digit c then lex_number state
        else match c with
            | '"' -> lex_string state
            | '\'' -> lex_char_literal state
            | '(' -> state.pos <- state.pos + 1; LParen
            | ')' -> state.pos <- state.pos + 1; RParen
            | '{' -> state.pos <- state.pos + 1; LBrace
            | '}' -> state.pos <- state.pos + 1; RBrace
            | '+' | '-' | '<' | '>' | '=' | '!' | '&' | '|' ->
                lex_operator state
            | ';' -> state.pos <- state.pos + 1; Semicolon
            | _ -> failwith ("Unknown character: " ^ Char.escaped c)

let tokenize (program : string) : token list = 
    let st = { src = program; pos = 0; len = String.length program } in
    let rec loop acc = 
        if st.pos >= st.len then List.rev acc
        else
            let tok = next_token st in
            loop (tok :: acc)
    in 
    loop []

