(** Formats catalog.json into appropriate format to be used for the book
    reader. *)

(** A list containing catalog entries. *)
val catalog_json : Yojson.Basic.t list

(** [title_lst json] gets all the elements of the key title from [json]
    and creates a string list. *)
val title_lst : Yojson.Basic.t list -> string list

(** [id_lst json] gets all the elements of the key id from [json] and
    creates a string list. *)
val id_lst : Yojson.Basic.t list -> int list

(** [download_count_lst json] gets all the elements of the key
    download_count from [json] and creates a string list. *)
val download_count_lst : Yojson.Basic.t list -> int list

(** [iterate lst] creates a list [lst] of names from the json file in
    which all values are converted to a string *)
val iterate : Yojson.Basic.t list -> string list

(** [author_lst json] gets all the elements of the key author from
    [json] and creates a string list. *)
val author_lst : Yojson.Basic.t list -> string list list

val get_author_lst : string list -> string list -> string list

(** [format_author json acc] is a string list with reformatted author
    names. *)
val format_author : Yojson.Basic.t list -> string list -> string list

(** [catalog_to_json json] creates a new json file that has the
    following keys: title, id, download_count, author. *)
val catalog_to_json : Yojson.Basic.t list -> unit
