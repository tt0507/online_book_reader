open Yojson.Basic
open Yojson.Basic.Util
open Features

(* This file is used to format catalog.json into appropriate format to
   be used for the book reader *)

let catalog_json =
  from_file "catalog.json" |> member "results" |> to_list

let title_lst json =
  List.map (fun x -> x |> member "title" |> to_string) json

let id_lst json = List.map (fun x -> x |> member "id" |> to_int) json

let download_count_lst json =
  List.map (fun x -> x |> member "download_count" |> to_int) json

let iterate lst =
  List.map (fun y -> y |> member "name" |> to_string) lst

let author_lst json =
  let lst = List.map (fun x -> x |> member "authors") json in
  List.map (fun x -> x |> to_list |> iterate) lst

let rec get_author_lst author acc : string list =
  match author with
  | [] -> acc
  | h :: t ->
      let split = String.split_on_char ',' h in
      if List.length split = 0 then get_author_lst t (acc @ [])
      else if List.length split = 1 then
        get_author_lst t (acc @ [ List.nth split 0 ])
      else
        let name = List.nth split 1 in
        let name_non_space =
          String.sub name 1 (String.length name - 1)
        in
        let fullname = name_non_space ^ " " ^ List.nth split 0 in
        get_author_lst t (acc @ [ fullname ])

let format_author json acc =
  let author = author_lst json in
  let author_lst = List.flatten author in
  get_author_lst author_lst acc

let catalog_to_json catalog_json =
  let title = title_lst catalog_json in
  let id = id_lst catalog_json in
  let download_count = download_count_lst catalog_json in
  let author = format_author catalog_json [] in
  let download_count_str =
    List.map (fun x -> string_of_int x) download_count
  in
  let id_str = List.map (fun x -> string_of_int x) id in
  let new_json =
    `Assoc
      [
        ("title", `List (create_json_string_list title []));
        ("id", `List (create_json_string_list id_str []));
        ( "download_count",
          `List (create_json_string_list download_count_str []) );
        ("author", `List (create_json_string_list author []));
      ]
  in
  to_file "edited_catalog.json" new_json
  [@@coverage off]

(* let main () = failwith "unimplemented" *)
