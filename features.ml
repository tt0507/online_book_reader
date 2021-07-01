open Yojson.Basic.Util
open Reader
open Printf
open Str
open Cohttp
open Lwt
open Cohttp_lwt_unix

type book = {
  title : string;
  directory : string;
  pg_id : int option;
  mutable page : int;
  mutable bookmark : int;
  mutable error : string;
}

type info = { books : book list }

type key = { key : string }

let make_book t dir pg_id =
  {
    title = t;
    directory = dir;
    pg_id;
    page = 0;
    bookmark = 0;
    error = "";
  }

let book_from_json j =
  {
    title = member "title" j |> to_string;
    page = member "page" j |> to_int;
    pg_id = member "pg_id" j |> to_int_option;
    directory = member "directory" j |> to_string;
    bookmark = member "bookmark" j |> to_int;
    error = member "error" j |> to_string;
  }

let info_from_json j =
  { books = member "books" j |> to_list |> List.map book_from_json }

let book_to_json b : Yojson.Basic.t =
  let pg_id =
    match b.pg_id with
    | Some x -> `Int x
    | None -> `Null
  in
  `Assoc
    [
      ("title", `String b.title);
      ("directory", `String b.directory);
      ("page", `Int b.page);
      ("pg_id", pg_id);
      ("bookmark", `Int b.bookmark);
      ("error", `String b.error);
    ]

let write_to_json binfo ofile =
  let blist = `List (List.map book_to_json binfo) in
  let new_info = `Assoc [ ("books", blist) ] in
  Yojson.Basic.to_file ofile new_info

let get_books info = info.books

let get_title book = book.title

let get_directory book = book.directory

let get_page book = book.page

let get_pg_id book = book.pg_id

let get_bookmark book = book.bookmark

let get_error book = book.error

let print text = Printf.printf "%s" text

let file page f =
  "books/" ^ f ^ "/" ^ f ^ "_" ^ string_of_int page ^ ".txt"

let rec page_lst str_lst =
  let reg = Str.regexp "[^0-9]+" in
  match str_lst with
  | [] -> []
  | h :: t ->
      (Str.global_replace reg "" h |> int_of_string) :: page_lst t

(********************** Page Feature *********************)

let get_page_lst (lst : string array) : int list =
  let pages = page_lst (Array.to_list lst) in
  pages

let reverse a b = if b > a then 1 else if a < b then -1 else 0

let get_last_page (book : book) : int =
  let file_list = Sys.readdir ("books/" ^ book.directory) in
  let page_lst = get_page_lst file_list in
  let page_sort = List.sort reverse page_lst in
  List.nth page_sort 0

let update_page_entry book counter =
  let change_page = book.page + counter in
  let last_page = get_last_page book in
  if change_page < 0 then (
    book.page <- last_page;
    book.error <- "";
    book )
  else if change_page > last_page then (
    book.page <- 0;
    book.error <- "";
    book )
  else if last_page = 0 && change_page > 0 then (
    book.page <- 0;
    book.error <- "";
    book )
  else (
    book.page <- book.page + counter;
    book.error <- "";
    book )

let update_bookmark book =
  book.bookmark <- book.page;
  print_endline ("Bookmark updated at page " ^ string_of_int book.page);
  book

let jump_bookmark book =
  book.page <- book.bookmark;
  book

let move_page book page_num =
  if page_num < 0 || page_num > get_last_page book then (
    book.error <- "Page does not exist";
    book )
  else (
    book.page <- page_num;
    book.error <- "";
    book )

(********************** Note Feature *********************)

let find_note book =
  let file_name =
    "notes/" ^ book.directory ^ "/" ^ book.directory ^ "_notes.txt"
  in
  if Sys.file_exists file_name then file_name
  else (
    ignore (Sys.command ("mkdir " ^ "notes/" ^ book.directory));
    ignore (Sys.command ("touch " ^ file_name));
    file_name )

(* test by running book reader *)
let add_to_notes note =
  ignore (Sys.command ("vi " ^ note))
  [@@coverage off]

(********************** Sticky note Feature *********************)

(* create directory in /stick_note when download book *)
let create_sticky_note book =
  let file =
    "sticky_notes/" ^ book.directory ^ "/" ^ book.directory ^ "_"
    ^ string_of_int book.page ^ ".txt"
  in
  if Sys.file_exists file then ()
  else ignore (Sys.command ("touch " ^ file));
  ignore (Sys.command ("vi " ^ file))
  [@@coverage off]

(********************** Dictionary Feature *********************)

let api_json (key : string) = `Assoc [ ("key", `String key) ]

let api_file_exists file = if Sys.file_exists file then true else false

let create_api api_file key =
  ignore (Sys.command "touch api.json");
  Yojson.Basic.to_file "api.json" (api_json key)
  [@@coverage off]

(* add to json file *)
let key_from_json j = { key = member "key" j |> to_string }

let get_json word (j : key) =
  let url = "https://www.dictionaryapi.com/api/v3/references/collegiate/json/"
    ^ word ^ "?key=" ^ j.key
  in
  Client.get (Uri.of_string url)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  match code with
  | 200 ->
      body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string
  | _ -> failwith "unimplemented"
  [@@coverage off]

let search_word_dictionary word key =
  get_json word key >>= fun def ->
  let definition = index 0 def |> member "shortdef" |> to_list in
  List.map (fun d -> d |> to_string) definition |> Lwt.return
  [@@coverage off]

(********************** Flashcard Feature *********************)
let rec create_json_string_list lst acc =
  match lst with
  | [] -> acc
  | h :: t -> create_json_string_list t (acc @ [ `String h ])

let get_json_entry entry json =
  json |> member entry |> to_list |> filter_string

let flashcard_to_json (word : string list) (def : string list) num =
  `Assoc
    [
      ("word", `List (create_json_string_list word []));
      ("def", `List (create_json_string_list def []));
      ("num", `Int num);
    ]

let checkfile file =
  if Sys.file_exists file then file
  else (
    ignore (Sys.command ("touch " ^ file));
    let json_input = flashcard_to_json [] [] 0 in
    Yojson.Basic.to_file file json_input;
    file )

let get_flashcard book =
  Yojson.Basic.from_file ("flashcard/" ^ book.directory ^ ".json")

let add_flashcard book word def file =
  let flashcard_json = Yojson.Basic.from_file file in
  let word_lst = get_json_entry "word" flashcard_json in
  let def_lst = get_json_entry "def" flashcard_json in
  let new_word_lst = word_lst @ [ word ] in
  let new_def_lst = def_lst @ [ def ] in
  let json_input = flashcard_to_json new_word_lst new_def_lst 0 in
  Yojson.Basic.to_file file json_input

let show_flashcard_entry book count =
  let json = get_flashcard book in
  let word = get_json_entry "word" json in
  let def = get_json_entry "def" json in
  let display_word = List.nth word count in
  let display_def = List.nth def count in
  (display_word, display_def)

let find_length book =
  let json = get_flashcard book in
  let word = get_json_entry "word" json in
  List.length word

let get_flashcard_lst book =
  let json = get_flashcard book in
  let word = get_json_entry "word" json in
  let def = get_json_entry "def" json in
  (word, def)

let update_flashcard_json_entry book num =
  let json = get_flashcard book in
  let word = get_json_entry "word" json in
  let def = get_json_entry "def" json in
  let new_json = flashcard_to_json word def num in
  let file = "flashcard/" ^ book.directory ^ ".json" in
  Yojson.Basic.to_file file new_json

let word_def_assoc book =
  let json = get_flashcard book in
  let word = get_json_entry "word" json in
  let def = get_json_entry "def" json in
  List.combine word def

let search_assoc assoc_lst input =
  match List.assoc input assoc_lst with
  | exception Not_found -> (input ^ " not found in flashcard", "")
  | num -> (input, num)

let delete_flashcard_entry book assoc_lst input =
  let new_lst = List.remove_assoc input assoc_lst in
  let w, d = List.split new_lst in
  let num = get_flashcard book |> member "num" |> to_int in
  let new_json = flashcard_to_json w d num in
  Yojson.Basic.to_file
    ("flashcard/" ^ book.directory ^ ".json")
    new_json

(********************** Catalog Feature *********************)

let get_key json_key file =
  let json = Yojson.Basic.from_file file in
  let lst = json |> member json_key |> to_list in
  List.map (fun x -> x |> to_string) lst

let compare_assoc (k, v) (k', v') =
  let compare_fst = compare k k' in
  if compare_fst <> 0 then compare_fst else compare v v'

let sort_assoc input1 input2 =
  let assoc = List.combine input1 input2 in
  List.sort compare_assoc assoc

let show_catalog input1 (input2 : string list) key =
  if key = "id" || key = "download_count" then
    let new_input1 = List.map (fun x -> int_of_string x) input1 in
    let int_str_lst = sort_assoc new_input1 input2 in
    List.map (fun (x, y) -> (string_of_int x, y)) int_str_lst
  else sort_assoc input1 input2
