(** Reads and processes downloaded book txt file. *)

(** [read_file file] reads in each line of the file and returns a string
    list. *)
val read_file : string -> string list

(** [get_number_of_file text_lst number_line] gets the number of files
    necessary when there are [number_line] lines per page. *)
val get_number_of_file : string list -> int -> int

(** [get_line_lst lst npage nline] breaks the string list [lst] into a
    2D array with length [npage] and the length of each element being
    [nline]. If empty elements exist, fill in with "\n".

    Example: get_line_lst ["a"; "b"; "c"; "d"; "e"; "f"] 3 2 =>
    [\[|"a"; "b"|\]; \[|"c"; "d"|\]; \[|"e"; "f"|\]] *)
val get_line_lst : string list -> int -> int -> string array array

(** [break_txt_smaller folder file file_name number_line] reads [file]
    and creates multiple files in which each file has [number_line]
    lines. [folder] is the folder path, [file] is the file to read, and
    [file_name] is the name of the file to add to [folder]. *)
val break_txt_smaller : string -> string -> string -> int -> unit
