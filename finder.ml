open Sys
open Reader
open Features

let book_link num =
  "https://www.gutenberg.org/files/" ^ string_of_int num ^ "/"
  ^ string_of_int num ^ "-0.txt"

let is_book_downloaded num books =
  let rec process_books = function
    | [] -> false
    | h :: t -> if get_pg_id h = (Some num) then true else process_books t
  in
  process_books books

let num_dirs filename dir =
  let b_arr = readdir dir in
  let rec process_dirs acc h =
    let fn_length = String.length filename in
    if String.length h >= fn_length then
      let len_file = String.length h in
      let len_diff = len_file - fn_length in
      let frst_sub = String.sub h 0 fn_length in
      let snd_sub = if len_diff = 0 then "0"
        else String.sub h fn_length len_diff
      in
      let same_name =
        String.uppercase_ascii snd_sub = snd_sub && frst_sub = filename
      in
      let new_acc = if same_name then acc + 1 else acc in
      new_acc
    else acc
  in
  Array.fold_left process_dirs 0 b_arr

let download_book title file_name num books =
  let url = book_link num in
  let num_dl = num_dirs file_name "books" in
  let new_name =
    if num_dl <> 0 then file_name ^ "_" ^ string_of_int num_dl
    else file_name
  in
  if is_book_downloaded num books then begin
    print_endline "This books is already downloaded!";
    books
  end
  else begin
    let file_with_ext = new_name ^ ".txt" in
    ignore (command ("mkdir books/" ^ new_name));
    ignore (command ("curl -silent -o " ^ file_with_ext ^ " " ^ url));
    break_txt_smaller ("books/" ^ new_name) file_with_ext new_name 50;
    ignore (command ("rm " ^ file_with_ext));
    let new_book = make_book title new_name (Some num) in
    new_book :: books
  end
  [@@coverage off]
