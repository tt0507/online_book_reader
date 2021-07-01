(* Import all the necessary libraries *)
open Core.In_channel
open Sys
open Str

(* used to suppress warning *)
let out_of_bound = "index out of bounds"

let read_file file = read_lines file

let get_number_of_file (text_lst : string list) number_line =
  let total_line = List.length text_lst in
  (total_line / number_line) + 1

(* 2D array with [[|"line of text - 1st file"|], [|"line of text - 2nd
   file"|]] *)
let get_line_lst lst (npages : int) nlines =
  let lst_arr = Array.of_list lst in
  let arr = Array.init npages (fun _ -> Array.make nlines "") in
  for i = 0 to Array.length arr - 1 do
    for j = 0 to Array.length arr.(i) - 1 do
      match lst_arr.((i * nlines) + j) with
      | _ -> arr.(i).(j) <- lst_arr.((i * nlines) + j)
      | exception Invalid_argument out_of_bound -> arr.(i).(j) <- "\n"
    done
  done;
  arr

let break_txt_smaller folder file file_name number_line =
  let text_lst = read_file file in
  let number_of_file = get_number_of_file text_lst number_line in
  let line_arr = get_line_lst text_lst number_of_file number_line in
  for num = 0 to Array.length line_arr - 1 do
    let text = Array.to_list line_arr.(num) in
    let combine_text = String.concat "\n" text in
    (* use regex to change double quotation to “. Not too sure how this
       will work with other files *)
    let reg_temp = regexp "\"" in
    let replace_dq = global_replace reg_temp "“" combine_text in
    let string_num = string_of_int num in
    let file = folder ^ "/" ^ file_name ^ "_" ^ string_num ^ ".txt" in
    let dq_string = String.make 1 '"' in
    ignore
      (command
         ("echo " ^ dq_string ^ replace_dq ^ dq_string ^ " > " ^ file))
  done
  [@@coverage off]
