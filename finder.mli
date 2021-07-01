(** Finds and downloads books from Project Gutenberg. *)

(** [book_link num] outputs the link from Project Gutenberg from which a
    book with the given number [num] can be downloaded. *)
val book_link : int -> string

(** [is_book_downloaded num books] determines if a book with pg_id [num]
    has been downloaded previously. *)
val is_book_downloaded : int -> Features.book list -> bool

(** [num_dirs filename dir] returns the number of books downloaded
    with the filename [filename] in directory [dir], not counting any symbols
    or numbers at the end of the filename. *)
val num_dirs : string -> string -> int

(** [download_book title file_name num books] imports a file with Project
    Gutenberg id [num] under [file_name] with title [title]. Returns a new list
    of books with the downloaded book included. *)
val download_book : string -> string -> int -> Features.book list ->
    Features.book list
