open Core
open Seacamel

let input_file = ref ""

let spec = [
    ("-i", Arg.Set_string input_file, " Set input file path");
]

let usage_msg = "Usage: myprogram [options] <anonymous_args>"

let print_list l = 
    List.iter l ~f:(fun x -> Printf.printf "%s " (Lexer.string_of_token x));
    Printf.printf "\n"
let () =
    Arg.parse spec (fun anon_arg -> Printf.printf "Anonymous argument: %s\n" anon_arg) usage_msg;
    Printf.printf "Input file: %s\n" !input_file;
    let tokens = (Seacamel.Lexer.tokenize (In_channel.read_all !input_file)) in
    print_list tokens

