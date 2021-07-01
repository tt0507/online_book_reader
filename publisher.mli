(** Enables users to publish their own book. *)

(** [work] defines the properties of a user's original work. *)
type work

(** [originals] defines the information about all the user's orignal
    works. *)
type originals

(** [make_orig_work id t fn] makes a new work with id [id], title [t],
    l_filename [fn], and default values for the rest. *)
val make_orig_work : int -> string -> string -> work

(** [get_w_id w] returns the internal id of [w] *)
val get_w_id : work -> int

(** [get_w_title w] returns the title of [w] *)
val get_w_title : work -> string

(** [get_publish_status w] returns the publication status of [w] *)
val get_publish_status : work -> bool

(** [get_local_filename w] returns the filename under which [w] has been
    stored. *)
val get_local_filename : work -> string

(** [get_published_filename w] returns the filename option under which
    [w] has been published. If [w] has not been published, returns
    [None] *)
val get_published_filename : work -> string option

(** [get_next_id o] returns the next id which will be used to categorize
    an original work in [o]. *)
val get_next_id : originals -> int

(** [get_author_orig o] returns the name option the user has chosen to
    write their original books under. If the user has not set an author
    name, returns [None] *)
val get_author_orig : originals -> string option

(** [get_orig_works o] returns a list of [works] the user has started
    writing or published. *)
val get_orig_works : originals -> work list

(** [work_from_json j] converts [j] from the [Yojson] type to the custom
    type [work]. *)
val work_from_json : Yojson.Basic.t -> work

(** [originals_from_json j] converts [j] from the [Yojson] type to
    custom type [originals]. *)
val originals_from_json : Yojson.Basic.t -> originals

(** [work_to_json w] coverts [w] from custom type [work] to the [Yojson]
    type. *)
val work_to_json : work -> Yojson.Basic.t

(** [write_originals_to_json origs ofile] converts [origs] from custom
    type [originals] into the [Yojson] type, then writes this to
    [ofile]. *)
val write_originals_to_json : originals -> string -> unit

(** [process_title title] processes [title] by converting all characters
    to lowercase and replacing spaces with '_'. *)
val process_title : string -> string

(** [duplicate_string n] prints a message describing that the user has
    already titled [n] books with a particular title, then prompts the
    user to take action. *)
val duplicate_title_string : int -> string

(** [count_t fn acc t] is a small helper function where if [process_title t] is
    the same as fn, then it returns [acc + 1] *)
val count_t : string -> int -> string -> int

(** [create_work title fn oinfo] creates a new work under filename [fn], title 
    [title], opens it for editing, then returns an object of type [originals]
    with the new work added. *)
val create_work : string -> string -> originals -> originals

(** [is_published id oinfo] returns whether the work in [oinfo] with id
    [id] is published. If a work with [id] is not in [oinfo], returns
    false. *)
val is_published : int -> originals -> bool

(** [publish_work w oinfo] converts work [w] in [oinfo] into a form available
    on the bookshelf.  *)
val publish_work : work -> originals -> Features.book list -> Features.book list

(** [edit_work w] opens work [w] and allows the user to edit it. *)
val edit_work : work -> unit
