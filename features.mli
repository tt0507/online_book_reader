(** Provides functions to access book information and various features
    of the offline book reader. *)

(** [book] defines the properties stored in a book. *)
type book

(** [info] defines the books accessible to the offline reader, as well
    as any bookmarks. *)
type info

(** [key] defines the api key that is used to send requests to the
    dictionary. *)
type key

(** [make_book t dir id] makes a book with title [t], directory [d],
    pg_id [id] and 0 or "" for all other default values. *)
val make_book : string -> string -> int option -> book

(** [book_from_json j] converts a json [j] into a record of type [book]. *)
val book_from_json : Yojson.Basic.t -> book

(** [info_from_json j] converts a json [j] to a record containing all
    the information stored in info.json. *)
val info_from_json : Yojson.Basic.t -> info

(** [book_to_json b] converts [b] from the custom type [book] to the
    [Yojson.t] type. *)
val book_to_json : book -> Yojson.Basic.t

(** [write_to_json binfo] converts [binfo] from the custom type [info]
    into the [Yojson.t] type. Takes the this converted information and
    writes it to [info.json]. *)
val write_to_json : book list -> string -> unit

(** [get_books info] gets the list of books from the record [info]. *)
val get_books : info -> book list

(** [get_title book] gets the title of the book. *)
val get_title : book -> string

(** [get_directory book] gets the directory of the book. *)
val get_directory : book -> string

(** [get_page book] gets the page number of the book. *)
val get_page : book -> int

(** [get_pg_id book] returns the Project Gutenberg ID number of the
    book as an option. *)
val get_pg_id : book -> int option

(** [get_bookmark book] gets the bookmark of the book. *)
val get_bookmark : book -> int

(** [get_error book] gets the error value of the book. *)
val get_error : book -> string

(** [file page f] gets the [page] for book [f]. *)
val file : int -> string -> string

(** [print line] prints [line]. *)
val print : string -> unit

(** [page_lst lst] returns the list of integer proportions of the file
    names that are inside the directory the reader is reading. *)
val page_lst : string list -> int list

(** [get_page lst] returns an int list of all the existing pages of the
    book the reader is reading. *)
val get_page_lst : string array -> int list

(** [reverse a b] used for the comparion function of List.sort - Sorts
    list in reverse order *)
val reverse : 'a -> 'a -> int

(** [get_last_page book] returns the last page of the book. *)
val get_last_page : book -> int

(** [update_page_entry book counter] increments/decrements the page
    entry of the [book] json by [counter]. *)
val update_page_entry : book -> int -> book

(** [update_bookmark book] updates the bookmark entry of the book. *)
val update_bookmark : book -> book

(** [jump_bookmark book] updates the reader view to the page of the
    bookmark. *)
val jump_bookmark : book -> book

(** [move_page book page_num] moves to page [page_num] for [book]. *)
val move_page : book -> int -> book

(** [find_note book] creates a file
    notes/book.directory/book.directory_notes.txt inside
    notes/book.directory folder if the file does not exist. If the file
    does exist then the path of the file is returned. *)
val find_note : book -> string

(** [add_to_notes note] opens vi editor to edit the file [note]. *)
val add_to_notes : string -> unit

(** [create_sticky_note book] creates a directory in /sticky_notes when
    [book] is downloaded. *)
val create_sticky_note : book -> unit

(** [api_json key] converts [key] to a json with entry key *)
val api_json :
  string -> [> `Assoc of (string * [> `String of string ]) list ]

(** [api_file_exists file] checks whether api.json exists where the API
    key is stored. *)
val api_file_exists : string -> bool

(** [create_api api_file key] creates the file api.json with the user's
    API key. *)
val create_api : string -> string -> unit

(** [key_from_json j] converts json [j] to a record of type [key]. *)
val key_from_json : Yojson.Basic.t -> key

(** [get_json word j] searches for [word] using the key entry of json
    [j] *)
val get_json : string -> key -> Yojson.Basic.t Lwt.t

(** [search_word_dictionary work key] searches the definition of [word]
    by sending a request using the API key [key]. *)
val search_word_dictionary : string -> key -> string list Lwt.t

(** [create_json_string_lst lst acc] is a tail recursive algorithm that
    converts all entry inside [lst] as `String. Returns [acc]. *)
val create_json_string_list :
  'a list -> ([> `String of 'a ] as 'b) list -> 'b list

(** [get_json_entry entry json] converts [entry] key of json to a string
    list *)
val get_json_entry : string -> Yojson.Basic.t -> string list

(** [flashcard_to_json word def num] creates a json with [word] [def]
    [num] as key*)
val flashcard_to_json :
  string list ->
  string list ->
  'a ->
  [> `Assoc of
     (string * [> `Int of 'a | `List of [> `String of string ] list ])
     list
  ]

(** [checkfile file] checks if [file] exists inside the folder. *)
val checkfile : string -> string

(** [get_flashcard book] gets converts flashcard/book.directory.json
    into a json *)
val get_flashcard : book -> Yojson.Basic.t

(** [add_flashcard book word def file] adds a flashcard entry to [book]
    with word [word] and definition [def]. *)
val add_flashcard : book -> string -> string -> string -> unit

(** [show_flashcard_entry book count] displays the word and definition
    of the flashcard stored in position [count]. *)
val show_flashcard_entry : book -> int -> string * string

(** [find_length book] finds the length of [book]'s flashcard. Entries
    are stored as array and the length of the word and definition are
    the same. *)
val find_length : book -> int

(** [get_flashcard_lst book] gets the word and definition stored as a
    tuple of string list and string list. *)
val get_flashcard_lst : book -> string list * string list

(** [update_flashcard_json_entry book num] updates the num key of the
    json file associated with [book] to [num]. *)
val update_flashcard_json_entry : book -> int -> unit

(** [word_def_assoc book] combines the word and definition list for the
    flashcard associated with [book] to one list *)
val word_def_assoc : book -> (string * string) list

(** [search_assoc assoc_lst input] finds the first entry of input in
    assoc_lst. Returns a tuple of (word, def) *)
val search_assoc : (string * string) list -> string -> string * string

(** [delete_flashcard_entry book assoc_lst input] returns a new json
    with [input] entry removed from the word and def list *)
val delete_flashcard_entry :
  book -> (string * string) list -> string -> unit

(** [get_key json_key file] gets [json_key] from [file] json and turns
    the element into a string list. *)
val get_key : string -> string -> string list

(** [compare_assoc (k, v) (k', v')] is used to compare the ordering of
    tup1 and tup2 for the List.sort function *)
val compare_assoc : 'a * 'b -> 'a * 'b -> int

(** [sort_assoc input1 input2] sorts the association list according to
    input 1 *)
val sort_assoc : 'a list -> 'b list -> ('a * 'b) list

(** [show_catalog input1 input2 key] combines [input1] and [input2] into
    an association list and sorts them by [input1]. *)
val show_catalog :
  string list -> string list -> string -> (string * string) list
