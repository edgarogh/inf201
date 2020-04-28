(*
    Script de build pour compiler / lancer le projet

    `ocaml build.ml`: compiler et lancer le programme
    `ocaml build.ml build`: compiler seulement
    `ocaml build.ml start`: lancer seulement (changements pas pris en compte)
*)

(** Build / Start *)
type task_type = B | S

(* Raccourci vers `ocamlc` *)
let c file = "ocamlc -c " ^ file ^ ".ml"

let tasks = [
    B, c "multiensemble";
    B, c "tuiles";
    S, "ocaml multiensemble.cmo tuiles.cmo main.ml";
]

(* Exécuteur de tâches *)

let array_get_opt arr idx =
    try Some (Array.get arr idx)
    with Invalid_argument _ -> None

let run_command cmd =
    print_endline ("$ " ^ cmd);
    0 = Sys.command cmd

let task_types = match array_get_opt Sys.argv 1 with
    | None | Some "" -> [B; S]
    | Some "build" -> [B]
    | Some "start" -> [S]
    | _ -> failwith "Argument invalide. ``, `build` ou `start` attendu"

(* Point d'entrée *)
let () =
    let tasks = List.filter (fun (t, cmd) -> List.mem t task_types) tasks in
    let commands = List.map snd tasks in
    if List.for_all run_command commands
    then ()
    else print_endline "* Echec"
