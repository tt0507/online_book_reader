(* Test plan:

   We manually tested main.ml, which executes the engine and starts the
   book reader interface. The modules Reader, Feature, Finder, Catalog,
   and Publisher were included both automatically tested by OUnit and
   using some manual testing. We manually tested parts that were able to
   be verified through using the app, such as fetching from an API and
   writing the output to a json file. Most of our tests were created
   through glass box testing.

   Testing manually ensures that the interface exhibits the expected
   behavior, while testing with OUnit helps verify that individual
   feature functions behave according to their specifications.

   Note: due to some error that we have been unable to resolve, in order
   to run the tests, you must run [#load "test.cmo";;] from utop if make
   test does not work. *)

open OUnit2
open Finder
open Reader
open Features
open Catalog
open Publisher
open Yojson.Basic.Util
open Yojson.Basic

let get_line_lst_test
    name
    (expected_output : string array array)
    lst
    npages
    nlines =
  name >:: fun _ ->
  assert_equal expected_output
    (get_line_lst lst npages nlines : string array array)

let get_number_of_file_test name expected_output text_lst number_line =
  name >:: fun _ ->
  assert_equal expected_output
    (get_number_of_file text_lst number_line)
    ~printer:string_of_int

let reader_test =
  [
    get_line_lst_test "Test get_line_lst_test method: 2-2 split"
      [| [| "a"; "b" |]; [| "c"; "d" |] |]
      [ "a"; "b"; "c"; "d" ] 2 2;
    get_line_lst_test "Test get_line_lst_test method: 3-2 split"
      [| [| "a"; "b" |]; [| "c"; "d" |]; [| "e"; "f" |] |]
      [ "a"; "b"; "c"; "d"; "e"; "f" ]
      3 2;
    get_line_lst_test "Test get_line_lst_test method: spillover"
      [|
        [| "a"; "b" |]; [| "c"; "d" |]; [| "e"; "f" |]; [| "g"; "\n" |];
      |]
      [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]
      4 2;
    get_number_of_file_test
      "Test get_number_of_file method for tom_sawyer book" 185
      (read_file "tom_sawyer.txt")
      50;
  ]

let tom_sawyer =
  let book = from_file "info.json" |> info_from_json |> get_books in
  List.nth book 1

(* may have to change the index later *)
let demo_book =
  let book = from_file "info.json" |> info_from_json |> get_books in
  List.nth book (List.length book - 1)

let page_lst_demo =
  [
    "tom_sawyer_87.txt";
    "tom_sawyer_93.txt";
    "tom_sawyer_108.txt";
    "tom_sawyer_120.txt";
    "tom_sawyer_134.txt";
  ]

let page_array =
  [|
    "tom_sawyer_87.txt";
    "tom_sawyer_93.txt";
    "tom_sawyer_108.txt";
    "tom_sawyer_120.txt";
    "tom_sawyer_134.txt";
  |]

let tom_sawyer_update_page_less_0 =
  let book = from_file "info.json" |> info_from_json |> get_books in
  let tom_sawyer_book = List.nth book 1 in
  update_page_entry tom_sawyer_book ~-1

let tom_sawyer_update_last_page =
  let book = from_file "info.json" |> info_from_json |> get_books in
  let tom_sawyer_book = List.nth book 1 in
  update_page_entry (move_page tom_sawyer_book 184) 1

let tom_sawyer_update_increment_page =
  let book = from_file "info.json" |> info_from_json |> get_books in
  let tom_sawyer_book = List.nth book 1 in
  update_page_entry tom_sawyer_book 1

let tom_sawyer_move_page =
  let book = from_file "info.json" |> info_from_json |> get_books in
  List.nth book 1

let book_test name expected_output book func =
  name >:: fun _ -> assert_equal expected_output (func book)

let print_test name expected_output text =
  name >:: fun _ -> assert_equal expected_output (print text)

let file_test name expected_output page f =
  name >:: fun _ -> assert_equal expected_output (file page f)

let page_lst_test name expected_output lst =
  name >:: fun _ -> assert_equal expected_output (page_lst lst)

let get_page_lst_test name expected_output lst =
  name >:: fun _ -> assert_equal expected_output (get_page_lst lst)

let get_last_page_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output (get_last_page book)
    ~printer:string_of_int

let jump_bookmark_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output
    (jump_bookmark book |> get_page)
    ~printer:string_of_int

let update_page_entry_test name expected_output book counter =
  name >:: fun _ ->
  assert_equal expected_output
    (update_page_entry book counter |> get_page)
    ~printer:string_of_int

let update_bookmark_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output
    (update_bookmark book |> get_bookmark)
    ~printer:string_of_int

let move_page_test name expected_output book page_num =
  name >:: fun _ ->
  assert_equal expected_output (move_page book page_num |> get_page)

let move_page_error_test name expected_output book page_num =
  name >:: fun _ ->
  assert_equal expected_output (move_page book page_num |> get_error)

let find_note_test name expected_output book =
  name >:: fun _ -> assert_equal expected_output (find_note book)

let find_note_exist_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output
    ( ignore
        (Sys.command
           ( "rm " ^ "books/notes" ^ get_directory demo_book
           ^ "_notes.txt" ));
      find_note book )

let api_file_exists_test name expected_output file =
  name >:: fun _ ->
  assert_equal expected_output (api_file_exists file)
    ~printer:string_of_bool

let read_write_read_compare ifile ofile =
  let pre_write = from_file ifile |> info_from_json in
  write_to_json (get_books pre_write) ofile;
  let post_write = from_file ofile |> info_from_json in
  pre_write = post_write

let json_write_test name file =
  name >:: fun _ ->
  assert_equal true
    (read_write_read_compare file "testinfo.json")
    ~printer:string_of_bool

let find_length_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output (find_length book) ~printer:string_of_int

let show_flashcard_entry_test name expected_output book count =
  name >:: fun _ ->
  assert_equal expected_output (show_flashcard_entry book count)

let get_flashcard_lst_test name expected_output book =
  name >:: fun _ ->
  assert_equal expected_output (get_flashcard_lst book)

let search_assoc_test name expected_output assoc_lst input =
  name >:: fun _ ->
  assert_equal expected_output (search_assoc assoc_lst input)

let sort_assoc_test name expected_output input1 input2 =
  name >:: fun _ ->
  assert_equal expected_output (sort_assoc input1 input2)

let feature_test =
  [
    book_test "Get title of Tom Sawyer book" "Tom Sawyer" tom_sawyer
      get_title;
    book_test "Get directory of Tom Sawyer book" "tom_sawyer" tom_sawyer
      get_directory;
    book_test "Get page of Tom Sawyer book" 0 tom_sawyer get_page;
    book_test "Get bookmark of Tom Sawyer book" 0 tom_sawyer
      get_bookmark;
    book_test "Get error message of Tom Sawyer book" "" tom_sawyer
      get_error;
    print_test "Test for print method" () "CS3110";
    file_test "file method" "books/tom_sawyer/tom_sawyer_0.txt" 0
      "tom_sawyer";
    page_lst_test "Test for page_lst method"
      [ 87; 93; 108; 120; 134 ]
      page_lst_demo;
    get_page_lst_test "Test for get_page_lst method"
      [ 87; 93; 108; 120; 134 ]
      page_array;
    get_last_page_test "Test for get_last_page method" 184 tom_sawyer;
    update_page_entry_test
      "Test for update_page_entry when page is less than 0" 184
      tom_sawyer_update_page_less_0 ~-1;
    update_page_entry_test
      "Test for update_page_entry when on last page" 1
      tom_sawyer_update_last_page 1;
    update_page_entry_test
      "Test for update_page_entry -> go to next page" 1
      tom_sawyer_update_increment_page 1;
    update_bookmark_test "Test for update_bookmark" 0
      tom_sawyer_update_increment_page;
    jump_bookmark_test "Test jump_bookmark method" 0 tom_sawyer;
    move_page_error_test
      "Test move_page error messages - greater than last page"
      "Page does not exist" tom_sawyer_move_page 200;
    move_page_error_test
      "Test move_page error messages - less than first page"
      "Page does not exist" tom_sawyer_move_page ~-1;
    move_page_test "Test move_page page - greater than last page" 0
      tom_sawyer_move_page 200;
    move_page_test "Test move_page page - less than first page" 0
      tom_sawyer_move_page ~-1;
    move_page_test "Test move_page within book pages" 100
      tom_sawyer_move_page 100;
    move_page_error_test
      "Test move_page error messages when move within pages" ""
      tom_sawyer_move_page 100;
    find_note_test "find_note test - file already exists"
      "notes/tom_sawyer/tom_sawyer_notes.txt" tom_sawyer;
    find_note_exist_test
      "find_note test - when demo_book_notes.txt exists"
      "notes/demo_book/demo_book_notes.txt" demo_book;
    api_file_exists_test "api_file_exists test" true "api.json";
    find_length_test "find_length test" 2 demo_book;
    get_flashcard_lst_test "get_flashcard_lst test"
      ([ "demo"; "demo2" ], [ "demo1"; "demo3" ])
      demo_book;
    show_flashcard_entry_test "show_flashcard_entry test"
      ("demo", "demo1") demo_book 0;
    json_write_test "test json writing function works properly"
      "info.json";
    search_assoc_test "search_assoc_test - word in list" ("a", "b")
      [ ("a", "b"); ("c", "d") ]
      "a";
    search_assoc_test "search_assoc_test - word in list"
      ("e not found in flashcard", "")
      [ ("a", "b"); ("c", "d") ]
      "e";
    sort_assoc_test
      "sort_assoc test - sort int and string with duplicate key"
      [ (1, "aa"); (1, "ab") ]
      [ 1; 1 ] [ "ab"; "aa" ];
    sort_assoc_test
      "sort_assoc test - sort int and string with non-duplicate key"
      [ (1, "aa"); (2, "ab") ]
      [ 2; 1 ] [ "ab"; "aa" ];
    sort_assoc_test
      "sort_assoc test - sort string and string with duplicate key"
      [ ("aa", "aa"); ("aa", "ab") ]
      [ "aa"; "aa" ] [ "ab"; "aa" ];
    sort_assoc_test
      "sort_assoc test - sort string and string with non-duplicate key"
      [ ("a", "ab"); ("b", "aa") ]
      [ "a"; "b" ] [ "ab"; "aa" ];
  ]

let book_link_test name expected_value num =
  name >:: fun _ ->
  assert_equal expected_value (book_link num) ~printer:(fun x -> x)

let book_download_test name ex num =
  let binfo = from_file "info.json" |> info_from_json |> get_books in
  name >:: fun _ ->
  assert_equal ex (is_book_downloaded num binfo) ~printer:string_of_bool

let test_num_dirs name ex filename =
  name >:: fun _ ->
  assert_equal ex (num_dirs filename "books") ~printer:string_of_int

let finder_test =
  [
    book_link_test "Test for book_link method"
      "https://www.gutenberg.org/files/0/0-0.txt" 0;
    book_download_test "Test ensure downloaded book downloaded" true 74;
    book_download_test "Test undownloaded book not downloaded" false
      30000;
    test_num_dirs
      "Test undownloaded file has 0 directories named after it" 0
      "why_dinos_are_awesome";
    test_num_dirs "Test downloaded book has 1 directory named after it"
      1 "tom_sawyer";
  ]

let test_catalog_json =
  from_file "test_json/test_catalog.json" |> member "results" |> to_list

let title_lst_test name expected_output json =
  name >:: fun _ -> assert_equal expected_output (title_lst json)

let id_lst_test name expected_output json =
  name >:: fun _ -> assert_equal expected_output (id_lst json)

let download_count_lst_test name expected_output json =
  name >:: fun _ ->
  assert_equal expected_output (download_count_lst json)

let format_author_test name expected_output json acc =
  name >:: fun _ ->
  assert_equal expected_output (format_author json acc)

let get_author_lst_test name expected_output author acc =
  name >:: fun _ ->
  assert_equal expected_output (get_author_lst author acc)

let catalog_test =
  [
    title_lst_test "title_lst test"
      [
        "Alice's Adventures in Wonderland";
        "Peter Pan";
        "Narrative of the Life of Frederick Douglass, an American Slave";
        "The King James Version of the Bible";
        "Through the Looking-Glass";
      ]
      test_catalog_json;
    id_lst_test "id_lst test" [ 11; 16; 23; 10; 12 ] test_catalog_json;
    download_count_lst_test "download_count_lst test"
      [ 27708; 11459; 10757; 4133; 3954 ]
      test_catalog_json;
    format_author_test "format_author test"
      [
        "Lewis Carroll";
        "J. M. (James Matthew) Barrie";
        "Frederick Douglass";
        "Lewis Carroll";
      ]
      test_catalog_json [];
    get_author_lst_test "get_author_lst_test - one author"
      [ "Lewis Carroll" ] [ "Carroll, Lewis" ] [];
    get_author_lst_test
      "get_author_lst_test - one author and no last name" [ "Lewis" ]
      [ "Lewis" ] [];
    get_author_lst_test "get_author_lst_test - no author" [ "" ] [ "" ]
      [];
    get_author_lst_test "get_author_lst_test - multiple author"
      [ "Lewis Carroll"; "Frederick Douglass" ]
      [ "Carroll, Lewis"; "Douglass, Frederick" ]
      [];
    get_author_lst_test
      "get_author_lst_test - multiple author and no last name"
      [ "Lewis Carroll"; "Frederick" ]
      [ "Carroll, Lewis"; "Frederick" ]
      [];
  ]

let origs = from_file "originals.json" |> originals_from_json

let testwork = List.nth (get_orig_works origs) 1

let test_getwid name w ex =
  name >:: fun _ -> assert_equal ex (get_w_id w) ~printer:string_of_int

let test_getwtitle name w ex =
  name >:: fun _ ->
  assert_equal ex (get_w_title w) ~printer:(fun x -> x)

let test_get_publish_status name w ex =
  name >:: fun _ ->
  assert_equal ex (get_publish_status w) ~printer:string_of_bool

let test_get_local_filename name w ex =
  name >:: fun _ ->
  assert_equal ex (get_local_filename w) ~printer:(fun x -> x)

let string_of_str_opt = function
  | Some s -> "Some " ^ s
  | None -> "None"

let test_get_published_filename name w ex =
  name >:: fun _ ->
  assert_equal ex (get_published_filename w) ~printer:string_of_str_opt

let test_get_next_id name oinfo =
  name >:: fun _ ->
  assert_equal true (get_next_id oinfo > 0) ~printer:string_of_bool

let test_get_author_orig name oinfo ex =
  name >:: fun _ ->
  assert_equal ex (get_author_orig oinfo) ~printer:string_of_str_opt

let test_process_title name ex t =
  name >:: fun _ ->
  assert_equal ex (process_title t) ~printer:(fun x -> x)

let test_is_published name oinfo id ex =
  name >:: fun _ ->
  assert_equal ex (is_published id oinfo) ~printer:string_of_bool

let publisher_test =
  [
    test_getwid "test get_w_id for demo work" testwork 0;
    test_getwtitle "test get_w_title for demo work" testwork "Demo Work";
    test_get_publish_status "test for published for demo work" testwork
      false;
    test_get_local_filename "test for l_filename, demo work" testwork
      "demo_work";
    test_get_published_filename "test for p_filename, demo work"
      testwork None;
    test_get_next_id "test get_next_id" origs;
    test_get_author_orig "test get_author_orig" origs None;
    test_process_title "test process_title" "demo_book_this"
      "Demo Book ThiS";
    test_is_published "test is_published, demo_book" origs 0 false;
  ]

let suite =
  "test suite for project"
  >::: List.flatten
         [
           reader_test;
           feature_test;
           finder_test;
           catalog_test;
           publisher_test;
         ]

(* move pages for book back to 0 to avoid test failing when re-running
   test *)
let move_page_back_less_0 = move_page tom_sawyer_update_page_less_0 0

let move_page_back_greater_184 = move_page tom_sawyer_update_last_page 0

let move_page_back_increment =
  let book = move_page tom_sawyer_update_increment_page 0 in
  update_bookmark book

let move_page_back_movepage = move_page tom_sawyer_move_page 0

let _ = run_test_tt_main suite
