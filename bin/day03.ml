let read_file (path: string): string = 
        In_channel.with_open_bin path In_channel.input_all

let lines: string -> string list = 
        String.split_on_char '\n'

let string_to_char_list (s : string) : char list =
        s |> String.to_seq |> List.of_seq

let rec transform_to_int (input: char list): int list =
        match input with 
        | '1' :: b  -> 1 :: transform_to_int b
        | '0' :: b  -> 0 :: transform_to_int b
        | _ :: _  -> failwith("Wrong bit")
        | [] -> []

let to_instruction (input: string): int list = 
        let result = string_to_char_list input in
        transform_to_int result

let rec to_lists (input: string list): int list list = match input with 
        | [] -> []
        | a :: tail -> (to_instruction a) :: (to_lists tail)

let rec drop_last input = match input with 
        | [] -> []
        | [_] -> []
        | a :: tail -> a :: (drop_last tail)

let parse input = lines input 
        |> drop_last
        |> to_lists

let rec transpose = function
        | [] -> []
        | []::_ -> []
        | matrix -> List.map List.hd matrix :: transpose (List.map List.tl matrix)

let most_common_number lst =
        let count_zeroes, count_ones =
        List.fold_left (fun (zeros, ones) x -> if x = 0 then (zeros + 1, ones) else (zeros, ones + 1)) (0, 0) lst
        in
        if count_zeroes > count_ones then 0 else 1

let rec calc_bits input = match input with
        | [] -> [] 
        | x :: tail -> most_common_number x :: calc_bits tail

let rec invert input = match input with
        | [] -> [] 
        | 1 :: tail -> 0 :: invert tail
        | 0 :: tail -> 1 :: invert tail
        | _ :: _  -> failwith("Wrong bit")

let binary_to_int lst =
        List.fold_left (fun acc bit -> (acc lsl 1) lor bit) 0 lst

let solve input = 
        let lst = calc_bits (transpose input) in
        let gamma = binary_to_int lst in
        let epsilon = binary_to_int (invert lst) in
        gamma * epsilon

let () = print_endline "Advent of Code, day 3"
let () = read_file "inputs/input03.txt"
        |> parse
        |> solve
        |> Printf.printf "*  %d\n"
