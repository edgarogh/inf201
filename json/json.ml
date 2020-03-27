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
