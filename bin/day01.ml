let read_file (path: string): string = 
        In_channel.with_open_bin path In_channel.input_all

let lines: string -> string list = 
        String.split_on_char '\n'

let rec to_nums (input: string list): int list = match input with 
        | [] -> [] 
        | a :: tail -> (int_of_string a) :: (to_nums tail)

let rec drop_last input = match input with 
        | [] -> []
        | [_] -> []
        | a :: tail -> a :: (drop_last tail)

let parse input = lines input 
        |> drop_last
        |> to_nums

let rec solve input = match input with
        | [] -> 0 
        | a :: ((b :: _) as list) when a < b -> 1 + solve list
        | _ :: tail -> solve tail

let rec solve_second input = match input with
        | [] -> 0 
        | a :: ((b :: c :: d :: _) as list) when a+b+c < b+c+d -> 1 + solve_second list
        | _ :: tail -> solve_second tail

let () = print_endline "Advent of Code, day 1"
let () = read_file "inputs/input01.txt"
        |> parse
        |> solve
        |> Printf.printf "*  %d\n"

let () = read_file "inputs/input01.txt"
        |> parse
        |> solve_second
        |> Printf.printf "** %d"
