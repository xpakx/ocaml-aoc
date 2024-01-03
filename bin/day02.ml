let read_file (path: string): string = 
        In_channel.with_open_bin path In_channel.input_all

let lines: string -> string list = 
        String.split_on_char '\n'

let to_instruction (input: string): string * int = 
        let result = String.split_on_char ' ' input in
        match result with 
        | a :: b :: _ -> (a, int_of_string b)
        | _ -> failwith "List is wrong"

let rec to_instructions (input: string list): (string * int) list = match input with 
        | [] -> []
        | a :: tail -> (to_instruction a) :: (to_instructions tail)

let rec drop_last input = match input with 
        | [] -> []
        | [_] -> []
        | a :: tail -> a :: (drop_last tail)

let parse input = lines input 
        |> drop_last
        |> to_instructions

let rec calc_first input = match input with
        | [] -> (0, 0) 
        | ("down", b) :: list -> let (x, y) = calc_first list in (b+x, y)
        | ("up", b) :: list -> let (x, y) = calc_first list in (x-b, y)
        | ("forward", b) :: list -> let (x, y) = calc_first list in (x, y+b)
        | _ :: tail -> let (x, y) = calc_first tail in (x, y)

let solve input = 
        let (depth, dist) = calc_first input in
        depth * dist

let () = print_endline "Advent of Code, day 2"
let () = read_file "inputs/input02.txt"
        |> parse
        |> solve
        |> Printf.printf "*  %d\n"
