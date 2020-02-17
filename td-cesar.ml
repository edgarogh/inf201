let decode = "-d" = Array.get Sys.argv 1;;
let offset = 3;;

let iA = int_of_char 'A';;
let iZ = int_of_char 'Z';;

let createCesarCipher (offset: int) =
    let offsetCar (offset: int) (c: char) = (
        let index = int_of_char c in
            match c with
                | ' ' -> ' '
                |_ when (index >= iA && index <= iZ) -> (
                    char_of_int (((index - iA + offset + 26) mod 26) + iA)
                )
                |_ -> failwith "Invalid character"
    )
    
    in (offsetCar (offset), offsetCar (-offset))
;;


let (codeCar, decodeCar) = createCesarCipher offset;;

assert ('D' = codeCar 'A');;
assert (' ' = codeCar ' ');;
assert ('C' = codeCar 'Z');;

assert ('X' = decodeCar (codeCar 'X'));;

let codeString = String.map codeCar;;
let decodeString = String.map decodeCar;;

let transform =
    if decode then decodeString else codeString
;;

print_string (transform (read_line()));;
