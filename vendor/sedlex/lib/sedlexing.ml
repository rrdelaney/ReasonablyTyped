(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed


let gen_of_channel chan =
  let f () =
    try Some (input_char chan)
    with End_of_file -> None
  in
  f

let (>>=) o f = match o with
  | Some x -> f x
  | None -> None


(* For legacy purposes. *)
let gen_of_stream stream =
  let f () =
    try Some (Stream.next stream)
    with Stream.Failure -> None
  in f

let eof = -1

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill: (int array -> int -> int -> int);
  mutable buf: int array;
  mutable len: int;    (* Number of meaningful char in buffer *)
  mutable offset: apos; (* Position of the first char in buffer
			    in the input stream *)
  mutable pos: int;
  mutable start: int; (* First char we need to keep visible *)

  mutable marked_pos: int;
  mutable marked_val: int;

  mutable finished: bool;
}

let chunk_size = 512

let empty_lexbuf = {
  refill = (fun _ _ _ -> assert false);
  buf = [| |];
  len = 0;
  offset = 0;
  pos = 0;
  start = 0;
  marked_pos = 0;
  marked_val = 0;
  finished = false;
}

let create f = {
  empty_lexbuf with
    refill = f;
    buf = Array.make chunk_size 0;
}


let fill_buf_from_gen f gen buf pos len =
  let rec aux i =
    if i >= len then len
    else match gen () with
      | Some c -> buf.(pos + i) <- f c ; aux (i+1)
      | None -> i
  in
  aux 0

let from_gen s =
  create (fill_buf_from_gen (fun id -> id) s)

let from_stream s = from_gen @@ gen_of_stream s

let from_int_array a =
  let len = Array.length a in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> a.(i));
      len = len;
      finished = true;
  }


let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf
  then begin
    let s = lexbuf.start in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen 0 in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.start <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if (n = 0)
  then begin
    lexbuf.buf.(lexbuf.len) <- eof;
    lexbuf.len <- lexbuf.len + 1;
  end
  else lexbuf.len <- lexbuf.len + n

let next lexbuf =
  let i =
    if lexbuf.pos = lexbuf.len then
      if lexbuf.finished then eof
      else (refill lexbuf; lexbuf.buf.(lexbuf.pos))
    else lexbuf.buf.(lexbuf.pos)
  in
  if i = eof then lexbuf.finished <- true else lexbuf.pos <- lexbuf.pos + 1;
  i

let start lexbuf =
  lexbuf.start <- lexbuf.pos;
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- (-1)

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- i

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start

let lexeme_start lexbuf = lexbuf.start + lexbuf.offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset

let loc lexbuf = (lexbuf.start + lexbuf.offset, lexbuf.pos + lexbuf.offset)

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start

let sub_lexeme lexbuf pos len =
  Array.sub lexbuf.buf (lexbuf.start + pos) len

let lexeme lexbuf =
  Array.sub lexbuf.buf (lexbuf.start) (lexbuf.pos - lexbuf.start)

let lexeme_char lexbuf pos =
  lexbuf.buf.(lexbuf.start + pos)


module Latin1 = struct
  let from_gen s =
    create (fill_buf_from_gen Char.code s)

  let from_stream s = from_gen @@ gen_of_stream s

  let from_string s =
    let len = String.length s in
    {
     empty_lexbuf with
     buf = Array.init len (fun i -> Char.code s.[i]);
     len = len;
     finished = true;
    }

  let from_channel ic =
    from_gen (gen_of_channel ic)

  let to_latin1 c =
    if (c >= 0) && (c < 256)
    then Char.chr c
    else raise (InvalidCodepoint c)

  let lexeme_char lexbuf pos =
    to_latin1 (lexeme_char lexbuf pos)

  let sub_lexeme lexbuf pos len =
    let s = Bytes.create len in
    for i = 0 to len - 1 do Bytes.set s i (to_latin1 lexbuf.buf.(lexbuf.start + pos + i)) done;
    Bytes.to_string s

  let lexeme lexbuf =
    sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start)
end


module Utf8 = struct
  module Helper = struct
    (* http://www.faqs.org/rfcs/rfc3629.html *)

    let width = Array.make 256 (-1)
    let () =
      for i = 0 to 127 do width.(i) <- 1 done;
      for i = 192 to 223 do width.(i) <- 2 done;
      for i = 224 to 239 do width.(i) <- 3 done;
      for i = 240 to 247 do width.(i) <- 4 done

    let next s i =
      match s.[i] with
      | '\000'..'\127' as c ->
          Char.code c
      | '\192'..'\223' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
          if (n2 lsr 6 != 0b10) then raise MalFormed;
          ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
      | '\224'..'\239' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise MalFormed;
	  let p =
            ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
	  in
	  if (p >= 0xd800) && (p <= 0xdf00) then raise MalFormed;
	  p
      | '\240'..'\247' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
	  let n4 = Char.code s.[i+3] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
	  then raise MalFormed;
          ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
          ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)
      | _ -> raise MalFormed


    let from_gen s =
      Gen.next s >>= function
      | '\000'..'\127' as c ->
          Some (Char.code c)
      | '\192'..'\223' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
          if (n2 lsr 6 != 0b10) then raise MalFormed;
          Some (((n1 land 0x1f) lsl 6) lor (n2 land 0x3f))
      | '\224'..'\239' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
	  Gen.next s >>= fun c3 ->
	  let n3 = Char.code c3 in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise MalFormed;
          Some (((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f))
      | '\240'..'\247' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
	  Gen.next s >>= fun c3 ->
	  let n3 = Char.code c3 in
	  Gen.next s >>= fun c4 ->
	  let n4 = Char.code c4 in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
	  then raise MalFormed;
          Some (((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
          ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f))
      | _ -> raise MalFormed



    let compute_len s pos bytes =
      let rec aux n i =
        if i >= pos + bytes then if i = pos + bytes then n else raise MalFormed
        else
          let w = width.(Char.code s.[i]) in
          if w > 0 then aux (succ n) (i + w)
          else raise MalFormed
      in
      aux 0 pos

    let rec blit_to_int s spos a apos n =
      if n > 0 then begin
        a.(apos) <- next s spos;
        blit_to_int s (spos + width.(Char.code s.[spos])) a (succ apos) (pred n)
      end

    let to_int_array s pos bytes =
      let n = compute_len s pos bytes in
      let a = Array.make n 0 in
      blit_to_int s pos a 0 n;
      a

(**************************)

    let store b p =
      if p <= 0x7f then
        Buffer.add_char b (Char.chr p)
      else if p <= 0x7ff then (
        Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0xffff then (
        if (p >= 0xd800 && p < 0xe000) then raise MalFormed;
        Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0x10ffff then (
        Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6)  land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else raise MalFormed

    let from_int_array a apos len =
      let b = Buffer.create (len * 4) in
      let rec aux apos len =
        if len > 0 then (store b a.(apos); aux (succ apos) (pred len))
        else Buffer.contents b in
      aux apos len

    let gen_from_char_gen s = (fun () -> from_gen s)
  end

  let from_channel ic =
    from_gen (Helper.gen_from_char_gen (gen_of_channel ic))

  let from_gen s =
    create (fill_buf_from_gen (fun id -> id)
        (Helper.gen_from_char_gen s))

  let from_stream s = from_gen @@ gen_of_stream s

  let from_string s =
    from_int_array (Helper.to_int_array s 0 (String.length s))

  let sub_lexeme lexbuf pos len =
    Helper.from_int_array lexbuf.buf (lexbuf.start + pos) len

  let lexeme lexbuf =
    sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start)
end


module Utf16 = struct
  type byte_order = Little_endian | Big_endian
  module Helper = struct
    (* http://www.ietf.org/rfc/rfc2781.txt *)

    let number_of_char_pair bo c1 c2 = match bo with
    | Little_endian -> ((Char.code c2) lsl 8) + (Char.code c1)
    | Big_endian -> ((Char.code c1) lsl 8) + (Char.code c2)

    let char_pair_of_number bo num = match bo with
    | Little_endian ->
        (Char.chr (num land 0xFF), Char.chr ((num lsr 8) land 0xFF ))
    | Big_endian ->
        (Char.chr ((num lsr 8) land 0xFF), Char.chr (num land 0xFF))

    let next_in_gen bo s =
      Gen.next s >>= fun c1 ->
      Gen.next s >>= fun c2 ->
      Some (number_of_char_pair bo c1 c2)

    let from_gen bo s w1 =
      if w1 = 0xfffe then raise (InvalidCodepoint w1);
      if w1 < 0xd800 || 0xdfff < w1 then Some w1
      else if w1 <= 0xdbff
      then
        next_in_gen bo s >>= fun w2 ->
        if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
        let upper10 = (w1 land 0x3ff) lsl 10
        and lower10 = w2 land 0x3ff in
        Some (0x10000 + upper10 + lower10)
      else raise MalFormed

    let gen_from_char_gen opt_bo s =
      let bo = ref opt_bo in
      fun () ->
        Gen.next s >>= fun c1 ->
        Gen.next s >>= fun c2 ->
        let o = match !bo with
          | Some o -> o
          | None ->
              let o = match (Char.code c1, Char.code c2) with
                | (0xff,0xfe) -> Little_endian
                | _ -> Big_endian in
              bo := Some o;
              o in
        from_gen o s (number_of_char_pair o c1 c2)


    let compute_len opt_bo str pos bytes =
      let s = gen_from_char_gen opt_bo
          (Gen.init ~limit:(bytes - pos) (fun i -> (str.[i + pos])))
      in
      let l = ref 0 in
      Gen.iter (fun _ -> incr l) s ;
      !l

    let blit_to_int opt_bo s spos a apos bytes =
      let s = gen_from_char_gen opt_bo
          (Gen.init ~limit:(bytes - spos) (fun i -> (s.[i + spos]))) in
      let p = ref apos in
      Gen.iter (fun x -> a.(!p) <- x ; incr p) s

    let to_int_array opt_bo s pos bytes =
      let len = compute_len opt_bo s pos bytes in
      let a = Array.make len 0 in
      blit_to_int opt_bo s pos a 0 bytes ;
      a

    let store bo buf code =
      if code < 0x10000
      then (
        let (c1,c2) = char_pair_of_number bo code in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2
       ) else (
        let u' = code - 0x10000  in
        let w1 = 0xd800 + (u' lsr 10)
        and w2 = 0xdc00 + (u' land 0x3ff) in
        let (c1,c2) = char_pair_of_number bo w1
        and (c3,c4) = char_pair_of_number bo w2 in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2;
        Buffer.add_char buf c3;
        Buffer.add_char buf c4
       )

    let from_int_array bo a apos len bom =
      let b = Buffer.create (len * 4) in
      if bom then store bo b 0xfeff ; (* first, store the BOM *)
      let rec aux apos len =
        if len > 0
        then (store bo b a.(apos); aux (succ apos) (pred len))
        else Buffer.contents b  in
      aux apos len
  end


  let from_gen s opt_bo =
    from_gen (Helper.gen_from_char_gen opt_bo s)

  let from_stream s = from_gen @@ gen_of_stream s

  let from_channel ic opt_bo =
    from_gen (gen_of_channel ic) opt_bo

  let from_string s opt_bo =
    let a = Helper.to_int_array opt_bo s 0 (String.length s) in
    from_int_array a

  let sub_lexeme lb pos len bo bom  =
    Helper.from_int_array bo lb.buf (lb.start + pos) len bom

  let lexeme lb bo bom =
    sub_lexeme lb 0 (lb.pos - lb.start) bo bom
end
