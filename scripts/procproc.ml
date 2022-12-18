#!/usr/bin/env ocaml

(*
  2022-12-18 T. Bourke
 *)

type name = {
  last : string;
  first : string;
}

type article = {
  title   : string;
  authors : name list;
  doi     : string;
  url     : string option;
}

type conference = {
  title     : string;
  chairs    : name list;
  where     : string;
  published : string;
  articles  : article list;
}

(* Printing *)

let output_name out { last; first } =
  output_string out last;
  output_string out ", ";
  output_string out first

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

let output_article out { title; authors; doi; url } =
  output_heading out 2 title;
  output_names out authors;
  output_field out "DOI" doi;
  output_opt_field out "URL" url;
  output_char out '\n'

let output_conference out { title; chairs; where; published; articles } =
  output_heading out 1 title;
  output_names out chairs;
  output_field out "At" where;
  output_field out "Published" published;
  output_char out '\n';
  List.iter (output_article out) articles

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

let try_article seq =
  match try_heading 2 seq with
  | None, seq -> None, seq
  | Some title, seq ->
      let authors, seq = expect_names seq in
      let doi, seq = expect_field "DOI" seq in
      let url, seq = try_field "URL" seq in
      Some { title; authors; doi; url }, seq

let rec try_to_list tryf =
  let rec f acc seq =
    match tryf seq with
    | None, seq' -> List.rev acc, seq'
    | Some x, seq' -> f (x::acc) seq'
  in
  f []

let expect_articles seq = try_to_list try_article seq

let read_conference fin =
  let seq = make_seq fin in
  let title, seq = expect_heading 1 seq in
  let chairs, seq = expect_names seq in
  let where, seq = expect_field "At" seq in
  let published, seq = expect_field "Published" seq in
  let articles, seq = expect_articles seq in
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

let _ = Arg.parse [
    ("--print", Arg.Unit (fun () -> output_conferences stdout),
     "print conferences to stdout");
    ("--output", Arg.String (to_output output_conferences),
     "write conferences to file");
    ("--print-authors", Arg.Unit (fun () -> print_authors stdout),
     "print authors to stdout");
  ]
  load_file
  "procproc: process article files"

