open Sys
open Core
open Stdlib
open Yojson.Basic.Util
open Features
open Reader
open Finder
open Publisher
open Lwt

(* prints bookshelf *)
let rec print_books books acc =
  match books with
  | [] -> print_endline "\n"
  | h :: t ->
      let bookmark_message =
        let bookmark = get_bookmark h in
        if bookmark <> 0 then
          " (bookmark at page " ^ string_of_int bookmark ^ ")"
        else ""
      in
      print_endline
        ( "\n[" ^ string_of_int acc ^ "] " ^ get_title h
        ^ bookmark_message );
      print_books t (acc + 1)
  [@@coverage off]

(* prints original works *)
let rec print_origs orig_ws acc =
  match orig_ws with
  | [] -> print_endline "\n"
  | h :: t ->
      print_endline ("\n[" ^ string_of_int acc ^ "] " ^ get_w_title h);
      print_origs t (acc + 1)
  [@@coverage off]

(* a type to describe whether a number or file name is being requested
   at download. *)
type dl =
  | Num
  | Name

(* a type to describe whether an original book is being published or
   edited *)
type orig_act =
  | Edit
  | Pub

let pg_id_string =
  "Please enter the Project Gutenberg ID of the book to download"

(* Converts an integer to the corresponding book. *)
let valid_book num info = num |> List.nth_opt info [@@coverage off]

(* [read f] starts reads the book [f] starting from page 0 *)
let read f =
  let line = In_channel.read_all f in
  print line;
  Out_channel.flush stdout;
  ()
  [@@coverage off]

let rec read_def def =
  match def with
  | h :: t ->
      print h;
      print "\n";
      print "\n";
      read_def t
  | [] -> ()
  [@@coverage off]

let rec move_second_input info book =
  print_string "Please enter the page number you want to move to: ";
  let input = String.trim (read_line ()) in
  match int_of_string input with
  | exception e ->
      print_string "Must be a number, please try again.\n";
      move_second_input info book
  | page -> ( match input with _ -> move_page book page )
  [@@coverage off]

let display_flashcard (entry : string * string) =
  match entry with
  | word, def ->
      print word;
      print "\n\n";
      print def;
      print "\n\n"
  [@@coverage off]

let display_flashcard_lst tup =
  match tup with
  | word, def ->
      print "Word    -     Definition \n\n\n";
      let def_lst = List.map2 (fun w d -> w ^ ": " ^ d) word def in
      read_def def_lst
  [@@coverage off]

let display_message (k, v) = print (k ^ "      " ^ v ^ "\n\n")

let rec display_catalog lst key1 key2 =
  match lst with
  | [] -> print "\n"
  | (k, v) :: t ->
      print (k ^ "         " ^ v ^ "\n");
      print "\n";
      display_catalog t key1 key2
  [@@coverage off]

(** [contains s1 s2] is the position of the first character of substring
    [s2] in string [s1]. If [s1] does not contain [s2], return -1. *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try Str.search_forward re s1 0 with Not_found -> -1
  [@@coverage off]

(* helper for search_book *)
let rec get_search_results query book_dir last_page acc =
  match last_page with
  | 0 -> acc
  | page_num -> (
      let file_name = file page_num book_dir in
      let page_contents = In_channel.read_all file_name in
      let content_length = String.length page_contents in
      match contains page_contents query with
      | -1 -> get_search_results query book_dir (last_page - 1) acc
      | pos ->
          let substr_start = max 0 (pos - 20) in
          let substr_end = min (pos + 20) (content_length - 1) in
          let substr =
            String.sub page_contents substr_start
              (substr_end - substr_start + 1)
          in
          get_search_results query book_dir (last_page - 1)
            ((page_num, "..." ^ substr ^ "...") :: acc) )
  [@@coverage off]

(* returns list of exact matches with format (page_num, substring) *)
let search_book book query =
  let last_page = get_last_page book in
  let book_dir = get_directory book in
  get_search_results query book_dir last_page []
  [@@coverage off]

(* prints the list of results returned by search_book *)
let rec print_search_results results =
  match results with
  | [] -> ()
  | (page_num, substr) :: t ->
      print_endline
        ("[" ^ string_of_int page_num ^ "] " ^ "\"" ^ substr ^ "\"");
      print_search_results t
  [@@coverage off]

(* prints search results interface *)
let print_search_interface has_results =
  if has_results then
    ANSITerminal.(
      print_string [ yellow ] "\nEnter page number to go to result.\n")
  else ();
  ANSITerminal.(
    print_string [ yellow ] "\n[s] Search Again  [q] Quit\n")
  [@@coverage off]

let print_red s = ANSITerminal.(print_string [ red ] s)

let print_cyan s = ANSITerminal.(print_string [ cyan ] s)

let feature_string =
  "\n\
   [n] Next  [b] Back  [move] Move to page\n\
   [update] Update bookmark  [jump] Move to bookmark\n\
   [flashcard] Search through flashcard \n\
   [catalog] See catalog \n\
   [notes] Edit note  [sticky] Edit sticky note\n\
   [define] Search work in dictionary  [search] Search this book\n\
   [h] Home  [q] Quit\n\n"
  [@@coverage off]

(** String to print at the home screen *)
let home_string =
  "\n\
   Type the number of a book to start reading.\n\n\
   [d] Download book from Project Gutenberg\n\
   [w] Original book menu\n\
   [q] Quit\n\n\n\
   Your books:"
  [@@coverage off]

let orig_book_string =
  "\n\
   [n] New book \n\
   [e] Edit book \n\
   [p] Publish book \n\
   [a] Change author name \n\
   [h] Home  [q] Quit\n\n\n\
  \  "
  [@@coverage off]

let edit_orig_string =
  "\n\
   Type the number of a book to start editing.\n\n\
   [w] Original book menu \n\
   [h] Home  [q] Quit\n\n\
   Your books:\n\n\
  \  "
  [@@coverage off]

let publish_orig_string =
  "\n\
   Type the number of a book to publish.\n\n\
   [w] Original book menu \n\
   [h] Home  [q] Quit\n\n\
   Your books:\n\n\
  \  "
  [@@coverage off]

let note_string =
  "\n\n\n\n\
   This is the notes section. Please enter [edit] to add/delete notes.\n\
   Notes are written with the Vi editor.\n\n\
   Press [q] to go back to book reader.\n"
  [@@coverage off]

let sticky_string =
  "\n\n\n\n\
   This is the sticky notes section.\n\n\
   Please type [edit] to add/delete to the sticky note.\n\n\
   Press [q] to go back to book reader.\n"
  [@@coverage off]

let dictionary_string =
  "Please enter api-key from Merriam-Webster Development Center.\n\n\
   If you don't have a Merriam-Webster account, please create one\n\n\
   at https://dictionaryapi.com/.\n"
  [@@coverage off]

let flashcard_string =
  "Please enter the word to add on the first input and\n\
   add any text on the second input.\n\n\
   [start] Iterate through the flashcards\n\
   [search] Search for entry in flashcard \n\
   [delete] Delete entry in flashcard \n\
   [list] See all the flashcards created\n\n\
   [q] Exit flashcards\n\n"
  [@@coverage off]

let flashcard_next_back_string =
  "[n] Next flashcard\n\
   [b] Previous flashcard\n\
   [q] Back to flashcards\n\
   [qq] Back to book reader\n\n"
  [@@coverage off]

let catalog_screen =
  "Welcome to the catalog.\n\n\
   [author] Sort by author\n\
   [download_count] Sort by download count\n\
   [id] Sort by id\n\
   [title] Sort by title\n\
   [q] Back to book reader\n"
  [@@coverage off]

let flashcard_list_string = "Press [q] to exit\n\n" [@@coverage off]

let print_page book =
  "\nPage: "
  ^ string_of_int (get_page book)
  ^ "/"
  ^ string_of_int (get_last_page book)
  ^ "\n"
  [@@coverage off]

let print_book_page book =
  "\n" ^ get_title book ^ ", page "
  ^ string_of_int (get_page book)
  ^ "\n\n"
  [@@coverage off]

let count_page lst_length count counter =
  if count + counter > lst_length - 1 then 0
  else if count + counter < 0 then lst_length - 1
  else count + counter
  [@@coverage off]

(* behavior to complete upon quitting *)
let quit_behavior info =
  write_to_json info "info.json";
  ANSITerminal.erase Screen;
  exit 0

(* reads user input while in book *)
let rec behavior info book =
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | exception End_of_file -> ()
  | "h" -> home info
  | "q" -> quit_behavior info
  | "n" -> read_book info (update_page_entry book 1)
  | "b" -> read_book info (update_page_entry book ~-1)
  | "update" -> behavior info (update_bookmark book)
  | "jump" -> read_book info (jump_bookmark book)
  | "move" -> move_second_input info book |> read_book info
  | "notes" -> notes info book
  | "sticky" -> sticky_note info book
  | "define" -> dictionary_interface info book [ "" ]
  | "flashcard" -> flashcard info book ("", "")
  | "catalog" -> catalog info book "edited_catalog.json" "id" "title"
  | "search" -> in_book_search info book
  | _ -> behavior info book
  [@@coverage off]

(* [read_book info book] prints the contents of [book] at [book.page] to
   the screen. *)
and read_book info book =
  ANSITerminal.erase Screen;
  ANSITerminal.(print_string [ cyan ] (print_book_page book));
  let page = file (get_page book) (get_directory book) in
  read page;
  ( if get_error book <> "" then
    ANSITerminal.(print_string [ red ] ("\n" ^ get_error book ^ "\n"))
  );
  ANSITerminal.(print_string [ cyan ] (print_page book));
  ANSITerminal.(print_string [ yellow ] feature_string);
  behavior info book
  [@@coverage off]

and notes info book =
  ANSITerminal.erase Screen;
  let note = find_note book in
  read note;
  ANSITerminal.(print_string [ cyan ] note_string);
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "q" -> read_book info book
  | "edit" ->
      add_to_notes note;
      notes info book
  | _ -> notes info book
  [@@coverage off]

and sticky_note info book =
  ANSITerminal.erase Screen;
  ANSITerminal.(print_string [ cyan ] sticky_string);
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "q" -> read_book info book
  | "edit" ->
      create_sticky_note book;
      sticky_note info book
  | _ -> sticky_note info book
  [@@coverage off]

(* interface of the dictionary *)
and dictionary_interface info book word =
  let api_file = "api.json" in
  search_api info book api_file;
  ANSITerminal.erase Screen;
  read_def word;
  ANSITerminal.(
    print_string [ cyan ]
      "\n Type in the word you want to search \n Press q to quit \n");
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "q" -> read_book info book
  | _ as word -> show_definition info book word
  [@@coverage off]

and show_definition info book word =
  let api_key = Yojson.Basic.from_file "api.json" |> key_from_json in
  let definition = Lwt_main.run (search_word_dictionary word api_key) in
  read_def definition;
  dictionary_interface info book definition
  [@@coverage off]

and search_api info book file =
  if not (api_file_exists file) then enter_api file info book;
  ()
  [@@coverage off]

(* if user doesn't have api.json file then make user enter api-key and
   create api.json file *)
and enter_api api_file info book =
  ANSITerminal.erase Screen;
  ANSITerminal.(print_string [ cyan ] dictionary_string);
  print_string "> ";
  let input = String.trim (read_line ()) in
  create_api api_file input;
  dictionary_interface info book [ "" ]
  [@@coverage off]

and flashcard info book (message : string * string) =
  let flashcard_file =
    checkfile ("flashcard/" ^ get_directory book ^ ".json")
  in
  ANSITerminal.erase Screen;
  display_message message;
  ANSITerminal.(print_string [ cyan ] flashcard_string);
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "start" ->
      let count_num =
        Yojson.Basic.from_file flashcard_file |> member "num" |> to_int
      in
      start_flashcard info book count_num
  | "list" -> list_flashcard info book
  | "search" -> flashcard info book (search_flashcard info book)
  | "delete" -> delete_flashcard info book
  | "q" -> read_book info book
  | _ as word -> enter_flashcard info book word flashcard_file
  [@@coverage off]

and enter_flashcard info book word file =
  print_string "> ";
  let def = String.trim (read_line ()) in
  add_flashcard book word def file;
  flashcard info book ("", "")
  [@@coverage off]

and start_flashcard info book count =
  ANSITerminal.erase Screen;
  let word_def = show_flashcard_entry book count in
  display_flashcard word_def;
  ANSITerminal.(print_string [ cyan ] flashcard_next_back_string);
  print_string "> ";
  let list_length = find_length book in
  let input = String.trim (read_line ()) in
  match input with
  | "n" -> start_flashcard info book (count_page list_length count 1)
  | "b" -> start_flashcard info book (count_page list_length count ~-1)
  | "q" ->
      update_flashcard_json_entry book count;
      flashcard info book ("", "")
  | "qq" ->
      update_flashcard_json_entry book count;
      read_book info book
  | _ -> start_flashcard info book count
  [@@coverage off]

and list_flashcard info book =
  ANSITerminal.erase Screen;
  let flashcard_lst = get_flashcard_lst book in
  display_flashcard_lst flashcard_lst;
  ANSITerminal.(print_string [ cyan ] flashcard_list_string);
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "q" -> flashcard info book ("", "")
  | _ -> list_flashcard info book
  [@@coverage off]

and search_flashcard info book =
  let wd_to_assoc = word_def_assoc book in
  print_string "Type the word to search: ";
  let input = String.trim (read_line ()) in
  search_assoc wd_to_assoc input
  [@@coverage off]

and delete_flashcard info book =
  let wd_to_assoc = word_def_assoc book in
  print_string "Type word to delete from flashcard: ";
  let input = String.trim (read_line ()) in
  delete_flashcard_entry book wd_to_assoc input;
  flashcard info book (input ^ " was deleted from flashcard", "")
  [@@coverage off]

(* let wd_assoc = word_def_assoc book; print_string "> "; () *)
and catalog info book file key1 key2 =
  ANSITerminal.erase Screen;
  print (key1 ^ "         " ^ key2 ^ "\n\n");
  let strlist1 = get_key key1 file in
  let strlist2 = get_key key2 file in
  let catalog_lst = show_catalog strlist1 strlist2 key1 in
  display_catalog catalog_lst key1 key2;
  ANSITerminal.(print_string [ cyan ] catalog_screen);
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | "author" -> catalog info book "edited_catalog.json" "author" "title"
  | "download_count" ->
      catalog info book "edited_catalog.json" "download_count" "title"
  | "id" -> catalog info book "edited_catalog.json" "id" "title"
  | "title" -> catalog info book "edited_catalog.json" "title" "author"
  | "q" -> read_book info book
  | _ -> catalog info book "edited_catalog.json" key1 key2
  [@@coverage off]

(* interface for in-book search, read input and call search_book *)
and in_book_search info book =
  ANSITerminal.erase Screen;
  print_cyan "\nPlease enter your search term, or press q to quit\n";
  print_string "> ";
  match String.trim (read_line ()) with
  | "q" -> read_book info book
  | query ->
      let has_results =
        match search_book book query with
        | [] ->
            print_red "\nNo matches found.\n";
            false
        | results ->
            print_cyan "\nSearch results:\n\n";
            print_search_results results;
            true
      in
      print_search_interface has_results;
      search_results_interface info book
  [@@coverage off]

(* interface for navigating search results *)
and search_results_interface info book =
  print_string "\n> ";
  match String.trim (read_line ()) with
  | "q" -> read_book info book
  | "s" -> in_book_search info book
  | p -> (
      match int_of_string p with
      | exception e ->
          print_red "\nNot a valid input, please try again.\n";
          search_results_interface info book
      | page_num -> search_results_help page_num book info
  )
  [@@coverage off]

(* helper function for search_results_interface *)
and search_results_help page_num book info =
  let new_book = move_page book page_num in
  match get_error new_book with
  | "" -> read_book info book
  | _ ->
      print_red "\nPage does not exist.\n";
      search_results_interface info book
  [@@coverage off]

(* Print home screen and options. *)
and home info =
  ANSITerminal.erase Screen;
  ANSITerminal.(
    print_string [ red ] "\nWelcome to the Offline Book Reader\n\n");
  print_endline home_string;
  print_books info 0;
  home_behavior info
  [@@coverage off]

and home_behavior info =
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | exception End_of_file -> ()
  | "q" -> quit_behavior info
  | "d" -> download_process info Num None
  | "w" ->
      let obooks =
        Yojson.Basic.from_file "originals.json" |> originals_from_json
      in
      orig_menu info obooks
  | x -> book_from_list x info
  [@@coverage off]

and orig_menu info obooks =
  ANSITerminal.erase Screen;
  print_endline orig_book_string;
  orig_menu_behavior info obooks
  [@@coverage off]

and orig_menu_behavior info origs =
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | exception End_of_file -> ()
  | "q" ->
      write_originals_to_json origs "originals.json";
      quit_behavior info
  | "h" ->
      write_originals_to_json origs "originals.json";
      write_to_json info "info.json";
      home info
  | "n" -> new_orig_book info origs
  | "p" -> orig_ep_menu info origs Pub
  | "e" -> orig_ep_menu info origs Edit
  | "a" -> failwith ""
  | _ ->
      print_endline "Invalid input";
      orig_menu_behavior info origs
  [@@coverage off]

and new_orig_book info origs =
  print_endline "Enter the title of your new book: ";
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | exception End_of_file -> ()
  | x ->
      let file_name = process_title x in
      let title_list = get_orig_works origs |> List.map get_w_title in
      let num_t = List.fold_left (count_t file_name) 0 title_list in
      (* creates the relevant file or stops the process *)
      title_exists num_t file_name x origs info;
      orig_menu info origs
  [@@coverage off]

and title_exists num_t file title origs info =
  if num_t <> 0 then begin
    let num = num_dirs file "originals" in
    print_endline (duplicate_title_string num);
    match read_line () with
    | "1" -> new_orig_book info origs
    | "2" ->
        let fn = file ^ string_of_int (get_next_id origs) in
        let n_orig = create_work title fn origs in
        orig_menu info n_orig
    | "3" -> orig_menu info origs
    | _ ->
        print_endline "Invalid input";
        title_exists num_t file title origs info
  end
  else
    let n_orig = create_work title file origs in
    orig_menu info n_orig
  [@@coverage off]

and book_from_list n info =
  match int_of_string n with
  | exception e ->
      print_endline "Invalid input";
      home_behavior info
  | x -> (
      match valid_book x info with
      | None ->
          print_endline "Invalid input";
          home_behavior info
      | Some x -> read_book info x )
  [@@coverage off]

and orig_from_list n info origs act =
  match int_of_string n with
  | exception e ->
      print_endline "Invalid input";
      orig_ep_behavior info origs act
  | x -> begin
      match valid_book x (get_orig_works origs) with
      | None ->
          print_endline "Invalid input";
          orig_ep_behavior info origs act
      | Some w ->
          if act = Pub then
            let new_info = publish_work w origs info in
            orig_menu new_info origs
          else (
            edit_work w;
            orig_menu info origs )
    end
  [@@coverage off]

(* walks the user through downloading a book *)
and download_process info kind results =
  if kind = Num then begin
    print_endline pg_id_string;
    print_string "> ";
    match read_line () with
    | exception End_of_file -> exit 0
    | num -> (
        match String.trim num |> int_of_string with
        | exception e ->
            print_endline "Please enter a valid number.\n";
            download_process info kind None
        | n -> download_process info Name (Some n) )
  end
  else choose_filename_dl results info
  [@@coverage off]

and choose_filename_dl results info =
  print_endline "Please enter a filename.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> exit 0
  | name -> (
      match results with
      | None -> download_process info Num None
      | Some n -> home (Finder.download_book name name n info) )
  [@@coverage off]

and orig_ep_menu info origs act =
  ANSITerminal.erase Screen;
  let pstr () =
    if act = Edit then print_endline edit_orig_string
    else print_endline publish_orig_string
  in
  pstr ();
  print_origs (get_orig_works origs) 0;
  orig_ep_behavior info origs act

and orig_ep_behavior info origs act =
  print_string "> ";
  let input = String.trim (read_line ()) in
  match input with
  | exception End_of_file -> ()
  | "q" ->
      write_originals_to_json origs "originals.json";
      quit_behavior info
  | "h" ->
      write_originals_to_json origs "originals.json";
      write_to_json info "info.json";
      home info
  | "w" -> orig_menu info origs
  | x -> orig_from_list x info origs act

(* [main ()] fetches the relevant info from info.json and starts the UI. *)
let main () =
  let binfo =
    Yojson.Basic.from_file "info.json" |> info_from_json |> get_books
  in
  home binfo
  [@@coverage off]

(* Execute the engine. *)
let () = main ()
