type expression
    = Null
    | Boolean of bool
    | Number of float
    | String of string
    | Array of expression list
    | Object of (string * expression) list

let json_number_of_number (n: float) : string =
    let s = string_of_float n in
    let idx = String.length s - 1 in
        if s.[idx] = '.' then
            (String.sub s 0 idx)
        else
            s

let json_string_of_string (s: string) : string =
    "\"" ^ (String.escaped s) ^ "\""

let rec string_of_expression (e: expression) : string =
    match e with
        | Null -> "null"
        | Boolean(v) -> if v then "true" else "false"
        | Number(v) -> json_number_of_number v
        | String(v) -> json_string_of_string v
        | Array(v) -> (
            "["
            ^
            String.concat "," (List.map string_of_expression v)
            ^
            "]"
        )
        | Object(v) -> (
            let string_of_keyvalue (key, value: string * expression) : string =
                (json_string_of_string key) ^ ":" ^ (string_of_expression value)
            in
                "{"
                ^
                String.concat "," (List.map string_of_keyvalue v)
                ^
                "}"
        )

let () = print_string (string_of_expression (
    Object([
        ("type", String("cart"));
        ("price", Number(45.));
        ("items", Array([
            String("Apple");
            String("Banana");
        ]));
    ])
))

let () = print_newline ()

let is_digit c = (c >= '0') && (c <= '9')

let json_filter_string : Tokenizer.filter_delegate =
    fun () ->
    let escape = ref 1 in
    let continue = function
        | any when !escape > 0 -> escape := !escape - 1; true
        | '"' -> false
        | '\\' -> escape := 1; true
        | any -> true
    in
    continue

let json_filter_number : Tokenizer.filter_delegate =
    fun () ->
    let continue = function
        | c when is_digit c -> true
        | '.' -> true
        | _ -> false
    in
    continue

let json_token_filter = function
    | ' ' | '\t' | '\n' | '\r' -> Tokenizer.Simple (true, false)
    | '{' | '}' | '[' | ']' | ':' | ',' -> Tokenizer.Simple(true, true)
    | '"' -> Tokenizer.Delegated (1, json_filter_string)
    | c when is_digit c -> Tokenizer.Delegated (0, json_filter_number)
    | _ -> Tokenizer.Simple (false, false)

let test_string = "{\"prop1\": 123.45, \"prop 2\":\"hello \\\"world\\\"\"}"

let tokens = Tokenizer.tokenize json_token_filter test_string

let () = List.iter print_endline tokens
