(*
    Script de build pour compiler / lancer le projet

    `ocaml build.ml [point d'entrée]`: compiler les dépedendances et lancer le
    fichier

    Si aucun point d'entrée n'est spécifié, lance "main.ml"
*)

let dep_name_of_line line =
    let len = String.length line in
    if len < 6
    then None
    else if String.sub line 0 5 <> "open "
    then None
    else Some (String.lowercase_ascii @@ String.sub line 5 (len - 5))

let deps_of_file path =
    let file = open_in path in
    let rec next_deps () =
        let line = try input_line file with End_of_file -> "" in
        match dep_name_of_line line with
        | None -> []
        | Some dep -> dep :: next_deps ()
    in
    next_deps ()

let file_name ext dep = String.lowercase_ascii dep ^ "." ^ ext

let rec dep_list name =
    [name] @ let deps = deps_of_file @@ file_name "ml" name in
    List.flatten @@ List.map dep_list deps

let filter_duplicates list =
    List.fold_left (fun acc -> fun el -> if List.mem el acc then acc else acc @ [el]) [] list

let () = assert ([1; 2; 3] = filter_duplicates [1; 2; 1; 2; 2; 3; 1; 3])

let run cmd =
    print_endline ("$ " ^ cmd);
    ignore @@ Sys.command cmd

let rec list_without_tail = function
    | [] -> []
    | last :: [] -> []
    | el :: rest -> el :: list_without_tail rest

(* Compilation *)
let entry_point = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "main"
let dep_list = filter_duplicates @@ List.rev @@ dep_list entry_point
let deps ext dep_list = String.concat " " (List.map (file_name ext) dep_list)

let deps_ml = deps "ml" dep_list
let deps_cmo_wo_ep = deps "cmo" (list_without_tail dep_list)

let () = run ("ocamlc -c " ^ (deps "ml" dep_list))
let () = run ("ocaml " ^ deps_cmo_wo_ep ^ " " ^ file_name "ml" entry_point)
