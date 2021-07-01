open Finder
open Reader
open Features
open Yojson.Basic.Util

type work = {
  id : int;
  title : string;
  mutable published : bool;
  l_filename : string;
  mutable p_filename : string option;
}

type originals = {
  mutable next_id : int;
  author : string option;
  works : work list;
}

let make_orig_work id title l_fn =
  { id; title; published = false; l_filename = l_fn; p_filename = None }

let get_w_id w = w.id

let get_w_title w = w.title

let get_publish_status w = w.published

let get_local_filename w = w.l_filename

let get_published_filename w = w.p_filename

let get_next_id o = o.next_id

let get_author_orig o = o.author

let get_orig_works o = o.works

let work_from_json j =
  {
    id = member "id" j |> to_int;
    title = member "title" j |> to_string;
    published = member "published" j |> to_bool;
    l_filename = member "l_filename" j |> to_string;
    p_filename = member "p_filename" j |> to_string_option;
  }

let originals_from_json j =
  {
    next_id = member "next_id" j |> to_int;
    author = member "author" j |> to_string_option;
    works = member "works" j |> to_list |> List.map work_from_json;
  }

let work_to_json w : Yojson.Basic.t =
  let p_from_opt = function
  | Some x -> `String x
  | None -> `Null
  in
  `Assoc
    [
      ("id", `Int w.id);
      ("title", `String w.title);
      ("published", `Bool w.published);
      ("l_filename", `String w.l_filename);
      ("p_filename", p_from_opt w.p_filename);
    ]

let write_originals_to_json origs ofile =
  let wlist = `List (List.map work_to_json origs.works) in
  let author_from_opt = function
  | Some x -> `String x
  | None -> `Null
  in
  let new_info =
    `Assoc
      [
        ("next_id", `Int origs.next_id);
        ("author", author_from_opt origs.author);
        ("works", wlist);
      ]
  in
  Yojson.Basic.to_file ofile new_info

let process_title title =
  String.lowercase_ascii title
  |> String.split_on_char ' '
  |> String.concat "_"

let duplicate_title_string num =
  "\nYou've already written " ^ string_of_int num
  ^ " books by this title! What do you want to do?[1] Change title \n\
     [2] Continue with same title \n\
     [3] Stop process \n\n"
  [@@coverage off]

let count_t fn acc t =
  if process_title t = fn then acc + 1 else acc

let create_work title file_name oinfo =
  let file_with_ext = "originals/" ^ file_name ^ ".txt" in
  let w = make_orig_work (get_next_id oinfo) title file_name in
  let new_orig = { oinfo with works = w :: oinfo.works } in
  let nxt_id = get_next_id new_orig in
  new_orig.next_id <- nxt_id + 1;
  ignore (Sys.command ("touch " ^ file_with_ext));
  ignore (Sys.command ("vi " ^ file_with_ext));
  new_orig
  [@@coverage off]

let is_published id oinfo =
  let find_by_id w = get_w_id w = id in
  let result = get_orig_works oinfo |> List.find_opt find_by_id in
  match result with
  | Some w -> get_publish_status w
  | None -> false

let publish_help w id oinfo =
  let file_name = get_local_filename w in
  let num_dl = num_dirs file_name "originals" in
  let new_name =
    if is_published id oinfo then
      match get_published_filename w with
      | Some x -> x
      | None -> failwith "Impossible"
    else if num_dl <> 0 then file_name ^ "_" ^ string_of_int num_dl
    else file_name
  in
  let file_with_ext = "originals/" ^ new_name ^ ".txt" in
  ignore (Sys.command ("mkdir books/" ^ new_name));
  break_txt_smaller ("books/" ^ new_name) file_with_ext new_name 50;
  (* update publication status *)
  w.published <- true;
  w.p_filename <- Some new_name
  [@@coverage off]

let publish_work w oinfo binfo =
  let id = get_w_id w in
  publish_help w id oinfo;
  let p_fn =
    match get_published_filename w with
    | Some x -> x
    | None -> failwith "Impossible"
  in
  let new_book = make_book (get_w_title w) p_fn None in
  new_book :: binfo
  [@@coverage off]

let edit_work w =
  let file = get_local_filename w in
  let file_with_ext = "originals/" ^ file ^ ".txt" in
  ignore (Sys.command ("touch " ^ file_with_ext));
  ignore (Sys.command ("vi " ^ file_with_ext))
  [@@coverage off]
