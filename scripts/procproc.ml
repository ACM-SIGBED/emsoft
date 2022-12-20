#!/usr/bin/env -S ocaml unix.cma

(*
  2022-12-18 T. Bourke
 *)

type name = {
  last : string;
  first : string;
}

type article = {
  title      : string;
  authors    : name list;
  doi        : string;
  url        : string option;
  conference : string;
  session    : string;
}

type conference = {
  title     : string;
  chairs    : name list;
  where     : string;
  published : string;
  articles  : article list;
}

(* Poor man's removal of unicode accents *)
let unimap = [
  ("Á", 'a');
  ("À", 'a');
  ("Â", 'a');
  ("Ä", 'a');
  ("Ă", 'a');
  ("É", 'e');
  ("È", 'e');
  ("Ê", 'e');
  ("Ë", 'e');
  ("Í", 'i');
  ("Ì", 'i');
  ("Î", 'i');
  ("Ï", 'i');
  ("Ó", 'o');
  ("Ò", 'o');
  ("Ô", 'o');
  ("Ö", 'o');
  ("Ï", 'o');
  ("Ú", 'u');
  ("Ù", 'u');
  ("Û", 'u');
  ("Ü", 'u');
  ("Ç", 'c');
  ("Č", 'c');
  ("Ș", 's');
  ("Ț", 't');
  ("ẞ", 's');
  ]

let convert_first s =
  let rec f = function
    | [] -> Char.lowercase_ascii s.[0]
    | (m, c) :: mcs when String.starts_with ~prefix:m s -> c
    | _ :: mcs -> f mcs
  in
  f unimap

(* Reverse lookups on name *)

type name_info = Info of {
  articles : article list;
}

let empty_name_info = Info { articles = [] }

let name_hash = ((Hashtbl.create 1000) : (name, name_info) Hashtbl.t)

let add_article article name =
  match Hashtbl.find_opt name_hash name with
  | None ->
      Hashtbl.add name_hash name
        (Info { articles = [article] })
  | Some (Info { articles }) ->
      Hashtbl.replace name_hash name
        (Info { articles = article :: articles })

let name_compare { last = l1; first = f1 } { last = l2; first = f2 } =
  match String.compare l1 l2 with 0 -> String.compare f1 f2 | n -> n

let all_names () =
  List.sort name_compare
    (Hashtbl.fold (fun name _ names -> name :: names) name_hash [])

let get_name_info = Hashtbl.find name_hash

(* Printing *)

let string_of_name { last; first } = last ^ ", " ^ first

let output_name out name = output_string out (string_of_name name)

let output_name_endline out name =
  output_name out name;
  output_char out '\n'

let rec output_names out names =
  match names with
  | []    -> ()
  | [x]   -> output_name out x; output_char out '\n'
  | x::xs ->
      output_name out x;
      output_string out " | ";
      output_names out xs

let output_heading out n s =
  for i = 1 to n do output_char out '#' done;
  output_char out ' ';
  output_string out s;
  output_char out '\n'

let output_field out n v =
  output_string out "* ";
  output_string out n;
  output_string out ": ";
  output_string out v;
  output_char out '\n'

let output_opt_field n out v =
  match v with None -> () | Some v -> output_field n out v

let check_article_session out current_session { session; _ } =
  if String.equal current_session session
  then current_session
  else (output_heading out 2 session; output_char out '\n'; session)

let check_article_conference out current_conference { conference; _ } =
  if String.equal current_conference conference
  then current_conference
  else (output_heading out 2 conference; output_char out '\n'; conference)

let output_article check_current out current
    ({ title; authors; doi; url; _ } as article) =
  let current = check_current out current article in
  output_heading out 3 title;
  output_names out authors;
  output_field out "DOI" doi;
  output_opt_field out "URL" url;
  output_char out '\n';
  current

let output_conference out { title; chairs; where; published; articles } =
  output_heading out 1 title;
  output_names out chairs;
  output_field out "At" where;
  output_field out "Published" published;
  output_char out '\n';
  ignore (List.fold_left (output_article check_article_session out) "" articles)

let output_summary name articles out =
  output_heading out 1 (string_of_name name);
  output_char out '\n';
  ignore (List.fold_left
            (output_article check_article_conference out) "" articles)

(* Parsing *)

let filename = ref "stdin"
let line = ref 0

let reset_filename s =
  filename := s;
  line := 0

exception Parse_error

type line =
  | Heading of int * string
  | Field of string * string
  | Text of string

let print_line = function
  | Heading (n, s) -> print_endline ("heading " ^ Int.to_string n ^ ": " ^ s)
  | Field (n, v)   -> print_endline ("field: " ^ n ^ "=" ^ v)
  | Text s         -> print_endline ("text: " ^ s)

let error fmt =
  let pr s = prerr_endline s; raise Parse_error in
  Format.(kasprintf (kasprintf pr "%s, line %d: %s" !filename !line) fmt)

let read_line fin =
  let s = input_line fin in
  incr line;
  s

let rec read_next_line fin =
  let s = String.trim (read_line fin) in
  if s = "" then read_next_line fin
  else s

let count_leading_chars c s =
  let rec f i = if s.[i] = c then f (i + 1) else i in
  f 0

let split_on_first_char c s =
  match String.index_from_opt s 0 c with
  | None -> error "no %c in '%s'" c (String.trim s)
  | Some i -> String.(sub s 0 i, sub s (i + 1) (length s - i - 1))

let parse_line fin =
  let s = read_next_line fin in
  if String.length s = 0 then Text s
  else if s.[0] = '#'
  then let h = count_leading_chars '#' s in
        Heading (h, String.(trim (sub s h (length s - h))))
  else if s.[0] = '*'
  then let s1, s2 = split_on_first_char ':' s in
       Field (String.(trim (sub s1 1 (length s1 - 1))), String.trim s2)
  else Text s

let rec make_seq fin =
  let rec f () =
    match parse_line fin with
    | Text "" -> f ()
    | v -> Seq.Cons (v, f)
    | exception End_of_file -> Seq.Nil
  in f

let parse_name s =
  let last, first = split_on_first_char ',' s in
  String.{ last = trim last; first = trim first }

let parse_names s = List.map parse_name String.(split_on_char '|' s)

let expect_names seq =
  match seq () with
  | Seq.Cons (Text s, seq') -> (parse_names s, seq')
  | _ -> error "expected a list of names (separated by '|'s"

let try_heading n seq =
  match seq () with
  | Seq.Cons (Heading (m, s), seq') when n = m -> Some s, seq'
  | v -> None, (fun () -> v)

let expect_heading n seq =
  match try_heading n seq with
  | None, _ -> error "expected heading level %d (leading '%s')" n (String.make n '#')
  | Some s, seq' -> s, seq'

let try_field n seq =
  match seq () with
  | Seq.Cons (Field (n', v), seq') when n = n' -> Some v, seq'
  | v -> None, (fun () -> v)

let expect_field n seq =
  match try_field n seq with
  | None, _ -> error "expected field '%s'" n
  | Some v, seq' -> v, seq'

let expect_end seq =
  match seq () with
  | Seq.Nil -> ()
  | _ -> error "expected end of file"

let try_session session seq =
  match try_heading 2 seq with
  | None, seq -> session, seq
  | Some session, seq -> session, seq

let try_article (conference, session) seq =
  let session, seq = try_session session seq in
  match try_heading 3 seq with
  | None, seq -> None, seq
  | Some title, seq ->
      let authors, seq = expect_names seq in
      let doi, seq = expect_field "DOI" seq in
      let url, seq = try_field "URL" seq in
      let article = { title; authors; doi; url; conference; session } in
      List.iter (add_article article) authors;
      Some (article, (conference, session)), seq

let rec try_to_list tryf =
  let rec f acc state seq =
    match tryf state seq with
    | None, seq' -> List.rev acc, seq'
    | Some (x, state), seq' -> f (x::acc) state seq'
  in
  f []

let expect_articles data seq =
  try_to_list try_article data seq

let read_conference fin =
  let seq = make_seq fin in
  let title, seq = expect_heading 1 seq in
  let chairs, seq = expect_names seq in
  let where, seq = expect_field "At" seq in
  let published, seq = expect_field "Published" seq in
  let session, seq = expect_heading 2 seq in
  let articles, seq = expect_articles (title, session) seq in
  expect_end seq;
  { title; chairs; where; published; articles }

(* Algorithms *)

let conferences = ref []

let load_file filename =
  let fin = open_in filename in
  reset_filename filename;
  conferences := read_conference fin :: !conferences;
  close_in fin

let output_conferences outc =
  List.(iter (output_conference outc) (rev !conferences))

let output_conferences_to_file filename =
  let fout = open_out filename in
  output_conferences fout;
  close_out fout

let to_output f filename =
  let fout = open_out filename in
  try f fout; close_out fout
  with e -> (close_out fout; raise e)

let print_authors outc =
  List.(iter (fun { articles; _ } ->
                iter (fun { authors; _ } ->
                  iter (output_name_endline outc) authors) articles)
        (rev !conferences))

let make_name_summaries path =
  let make_path ({ last; _ } as name) =
    let first_letter = String.make 1 (convert_first last) in
    let path = Filename.concat path first_letter in
    (try Unix.mkdir path 0o777 with Unix.(Unix_error (EEXIST, _, _)) -> ());
    Filename.concat path (string_of_name name ^ ".md")
  in
  let summarize name =
    let Info { articles } = get_name_info name in
    to_output (output_summary name (List.rev articles)) (make_path name)
  in
  List.iter summarize (all_names ())

let _ = Arg.parse [
    ("--print", Arg.Unit (fun () -> output_conferences stdout),
     "print conferences to stdout");
    ("--output", Arg.String (to_output output_conferences),
     "write conferences to file");
    ("--print-authors", Arg.Unit (fun () -> print_authors stdout),
     "print authors to stdout");
    ("--summarize-by-name", Arg.String make_name_summaries,
     "create summary pages indexed by last name")
  ]
  load_file
  "procproc: process article files"

