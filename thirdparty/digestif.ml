(*
The MIT License (MIT)

Copyright (c) 2014 oklm-wsh

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module Digestif_bi = struct
  open Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  let create n = Array1.create Char c_layout n
  let length = Array1.dim
  let sub = Array1.sub
  let empty = Array1.create Char c_layout 0
  let get = Array1.get

  let copy t =
    let r = create (length t) in
    Array1.blit t r;
    r

  let init l f =
    let v = Array1.create Char c_layout l in
    for i = 0 to l - 1 do
      Array1.set v i (f i)
    done;
    v

  external unsafe_get_32 : t -> int -> int32 = "%caml_bigstring_get32u"
  external unsafe_get_64 : t -> int -> int64 = "%caml_bigstring_get64u"

  let unsafe_get_nat : t -> int -> nativeint =
   fun s i ->
    if Sys.word_size = 32 then Nativeint.of_int32 @@ unsafe_get_32 s i
    else Int64.to_nativeint @@ unsafe_get_64 s i

  external unsafe_set_32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
  external unsafe_set_64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"

  let unsafe_set_nat : t -> int -> nativeint -> unit =
   fun s i v ->
    if Sys.word_size = 32 then unsafe_set_32 s i (Nativeint.to_int32 v)
    else unsafe_set_64 s i (Int64.of_nativeint v)

  let to_string v = String.init (length v) (Array1.get v)

  let blit_from_bytes src src_off dst dst_off len =
    for i = 0 to len - 1 do
      Array1.set dst (dst_off + i) (Bytes.get src (src_off + i))
    done

  external swap32 : int32 -> int32 = "%bswap_int32"
  external swap64 : int64 -> int64 = "%bswap_int64"
  external swapnat : nativeint -> nativeint = "%bswap_native"

  let cpu_to_be32 s i v =
    if Sys.big_endian then unsafe_set_32 s i v else unsafe_set_32 s i (swap32 v)

  let cpu_to_le32 s i v =
    if Sys.big_endian then unsafe_set_32 s i (swap32 v) else unsafe_set_32 s i v

  let cpu_to_be64 s i v =
    if Sys.big_endian then unsafe_set_64 s i v else unsafe_set_64 s i (swap64 v)

  let cpu_to_le64 s i v =
    if Sys.big_endian then unsafe_set_64 s i (swap64 v) else unsafe_set_64 s i v

  let be32_to_cpu s i =
    if Sys.big_endian then unsafe_get_32 s i else swap32 @@ unsafe_get_32 s i

  let le32_to_cpu s i =
    if Sys.big_endian then swap32 @@ unsafe_get_32 s i else unsafe_get_32 s i

  let be64_to_cpu s i =
    if Sys.big_endian then unsafe_get_64 s i else swap64 @@ unsafe_get_64 s i

  let le64_to_cpu s i =
    if Sys.big_endian then swap64 @@ unsafe_get_64 s i else unsafe_get_64 s i

  let benat_to_cpu s i =
    if Sys.big_endian then unsafe_get_nat s i else swapnat @@ unsafe_get_nat s i

  let cpu_to_benat s i v =
    if Sys.big_endian then unsafe_set_nat s i v
    else unsafe_set_nat s i (swapnat v)
end

module Digestif_by = struct
  include Bytes

  external unsafe_get_32 : t -> int -> int32 = "%caml_bytes_get32u"
  external unsafe_get_64 : t -> int -> int64 = "%caml_bytes_get64u"

  let unsafe_get_nat : t -> int -> nativeint =
   fun s i ->
    if Sys.word_size = 32 then Nativeint.of_int32 @@ unsafe_get_32 s i
    else Int64.to_nativeint @@ unsafe_get_64 s i

  external unsafe_set_32 : t -> int -> int32 -> unit = "%caml_bytes_set32u"
  external unsafe_set_64 : t -> int -> int64 -> unit = "%caml_bytes_set64u"

  let unsafe_set_nat : t -> int -> nativeint -> unit =
   fun s i v ->
    if Sys.word_size = 32 then unsafe_set_32 s i (Nativeint.to_int32 v)
    else unsafe_set_64 s i (Int64.of_nativeint v)

  let blit_from_bigstring src src_off dst dst_off len =
    for i = 0 to len - 1 do
      set dst (dst_off + i) src.{src_off + i}
    done

  let rpad a size x =
    let l = length a in
    let b = create size in
    blit a 0 b 0 l;
    fill b l (size - l) x;
    b

  external swap32 : int32 -> int32 = "%bswap_int32"
  external swap64 : int64 -> int64 = "%bswap_int64"
  external swapnat : nativeint -> nativeint = "%bswap_native"

  let cpu_to_be32 s i v =
    if Sys.big_endian then unsafe_set_32 s i v else unsafe_set_32 s i (swap32 v)

  let cpu_to_le32 s i v =
    if Sys.big_endian then unsafe_set_32 s i (swap32 v) else unsafe_set_32 s i v

  let cpu_to_be64 s i v =
    if Sys.big_endian then unsafe_set_64 s i v else unsafe_set_64 s i (swap64 v)

  let cpu_to_le64 s i v =
    if Sys.big_endian then unsafe_set_64 s i (swap64 v) else unsafe_set_64 s i v

  let be32_to_cpu s i =
    if Sys.big_endian then unsafe_get_32 s i else swap32 @@ unsafe_get_32 s i

  let le32_to_cpu s i =
    if Sys.big_endian then swap32 @@ unsafe_get_32 s i else unsafe_get_32 s i

  let be64_to_cpu s i =
    if Sys.big_endian then unsafe_get_64 s i else swap64 @@ unsafe_get_64 s i

  let le64_to_cpu s i =
    if Sys.big_endian then swap64 @@ unsafe_get_64 s i else unsafe_get_64 s i

  let benat_to_cpu s i =
    if Sys.big_endian then unsafe_get_nat s i else swapnat @@ unsafe_get_nat s i

  let cpu_to_benat s i v =
    if Sys.big_endian then unsafe_set_nat s i v
    else unsafe_set_nat s i (swapnat v)
end

module Digestif_conv = struct
  let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

  module Make (D : sig
    val digest_size : int
  end) =
  struct
    let to_hex hash =
      let res = Bytes.create (D.digest_size * 2) in
      let chr x =
        match x with
        | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> Char.chr (48 + x)
        | _ -> Char.chr (97 + (x - 10))
      in
      for i = 0 to D.digest_size - 1 do
        let v = Char.code hash.[i] in
        Bytes.unsafe_set res (i * 2) (chr (v lsr 4));
        Bytes.unsafe_set res ((i * 2) + 1) (chr (v land 0x0F))
      done;
      Bytes.unsafe_to_string res

    let code x =
      match x with
      | '0' .. '9' -> Char.code x - Char.code '0'
      | 'A' .. 'F' -> Char.code x - Char.code 'A' + 10
      | 'a' .. 'f' -> Char.code x - Char.code 'a' + 10
      | _ -> invalid_arg "of_hex: %02X" (Char.code x)

    let decode chr1 chr2 = Char.chr ((code chr1 lsl 4) lor code chr2)

    let of_hex hex =
      let offset = ref 0 in
      let rec go have_first idx =
        if !offset + idx >= String.length hex then '\x00'
        else
          match hex.[!offset + idx] with
          | ' ' | '\t' | '\r' | '\n' ->
              incr offset;
              go have_first idx
          | chr2 when have_first -> chr2
          | chr1 ->
              incr offset;
              let chr2 = go true idx in
              if chr2 <> '\x00' then decode chr1 chr2
              else invalid_arg "of_hex: odd number of hex characters"
      in
      String.init D.digest_size (go false)

    let of_hex_opt hex =
      match of_hex hex with
      | digest -> Some digest
      | exception Invalid_argument _ -> None

    let consistent_of_hex str =
      let offset = ref 0 in
      let rec go have_first idx =
        if !offset + idx >= String.length str then
          invalid_arg "Not enough hex value"
        else
          match str.[!offset + idx] with
          | ' ' | '\t' | '\r' | '\n' ->
              incr offset;
              go have_first idx
          | chr2 when have_first -> chr2
          | chr1 ->
              incr offset;
              let chr2 = go true idx in
              decode chr1 chr2
      in
      let res = String.init D.digest_size (go false) in
      let is_wsp = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
      while
        D.digest_size + !offset < String.length str
        && is_wsp str.[!offset + (D.digest_size * 2)]
      do
        incr offset
      done;
      if !offset + D.digest_size = String.length str then res
      else
        invalid_arg "Too much enough bytes (reach: %d, expect: %d)"
          (!offset + (D.digest_size * 2))
          (String.length str)

    let consistent_of_hex_opt hex =
      match consistent_of_hex hex with
      | digest -> Some digest
      | exception Invalid_argument _ -> None

    let pp ppf hash =
      for i = 0 to D.digest_size - 1 do
        Format.fprintf ppf "%02x" (Char.code hash.[i])
      done

    let of_raw_string x =
      if String.length x <> D.digest_size then invalid_arg "invalid hash size"
      else x

    let of_raw_string_opt x =
      match of_raw_string x with
      | digest -> Some digest
      | exception Invalid_argument _ -> None

    let to_raw_string x = x
  end
end

module Xor = struct
  module Nat = struct
    include Nativeint

    let ( lxor ) = Nativeint.logxor
  end

  module type BUFFER = sig
    type t

    val length : t -> int
    val sub : t -> int -> int -> t
    val copy : t -> t
    val benat_to_cpu : t -> int -> nativeint
    val cpu_to_benat : t -> int -> nativeint -> unit
  end

  let imin (a : int) (b : int) = if a < b then a else b

  module Make (B : BUFFER) = struct
    let size_of_long = Sys.word_size / 8

    (* XXX(dinosaure): I'm not sure about this code. May be we don't need the
       first loop and the _optimization_ is irrelevant. *)
    let xor_into src src_off dst dst_off n =
      let n = ref n in
      let i = ref 0 in
      while !n >= size_of_long do
        B.cpu_to_benat dst (dst_off + !i)
          Nat.(
            B.benat_to_cpu dst (dst_off + !i)
            lxor B.benat_to_cpu src (src_off + !i));
        n := !n - size_of_long;
        i := !i + size_of_long
      done;
      while !n > 0 do
        B.cpu_to_benat dst (dst_off + !i)
          Nat.(
            B.benat_to_cpu src (src_off + !i)
            lxor B.benat_to_cpu dst (dst_off + !i));
        incr i;
        decr n
      done

    let xor_into a b n =
      if n > imin (B.length a) (B.length b) then
        raise (Invalid_argument "Baijiu.Xor.xor_inrot: buffers to small")
      else xor_into a 0 b 0 n

    let xor a b =
      let l = imin (B.length a) (B.length b) in
      let r = B.copy (B.sub b 0 l) in
      xor_into a r l;
      r
  end

  module Bytes = Make (Digestif_by)
  module Bigstring = Make (Digestif_bi)
end

module Baijiu_sha1 = struct
  module By = Digestif_by
  module Bi = Digestif_bi

  module Int32 = struct
    include Int32

    let ( lsl ) = Int32.shift_left
    let ( lsr ) = Int32.shift_right_logical
    let ( asr ) = Int32.shift_right
    let ( lor ) = Int32.logor
    let ( lxor ) = Int32.logxor
    let ( land ) = Int32.logand
    let ( + ) = Int32.add
    let rol32 a n = (a lsl n) lor (a lsr (32 - n))
  end

  module Int64 = struct
    include Int64

    let ( land ) = Int64.logand
    let ( lsl ) = Int64.shift_left
  end

  module type S = sig
    type ctx
    type kind = [ `SHA1 ]

    val init : unit -> ctx
    val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
    val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val unsafe_get : ctx -> By.t
    val dup : ctx -> ctx
  end

  module Unsafe : S = struct
    type kind = [ `SHA1 ]
    type ctx = { mutable size : int64; b : Bytes.t; h : int32 array }

    let dup ctx = { size = ctx.size; b = By.copy ctx.b; h = Array.copy ctx.h }

    let init () =
      let b = By.make 64 '\x00' in
      {
        size = 0L;
        b;
        h =
          [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l; 0xc3d2e1f0l |];
      }

    let f1 x y z = Int32.(z lxor (x land (y lxor z)))
    let f2 x y z = Int32.(x lxor y lxor z)
    let f3 x y z = Int32.((x land y) + (z land (x lxor y)))
    let f4 = f2
    let k1 = 0x5a827999l
    let k2 = 0x6ed9eba1l
    let k3 = 0x8f1bbcdcl
    let k4 = 0xca62c1d6l

    let sha1_do_chunk :
        type a. be32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
     fun ~be32_to_cpu ctx buf off ->
      let a = ref ctx.h.(0) in
      let b = ref ctx.h.(1) in
      let c = ref ctx.h.(2) in
      let d = ref ctx.h.(3) in
      let e = ref ctx.h.(4) in
      let w = Array.make 16 0l in
      let m i =
        let ( && ) a b = a land b in
        let ( -- ) a b = a - b in
        let v =
          Int32.(
            rol32
              (w.(i && 0x0F)
              lxor w.((i -- 14) && 0x0F)
              lxor w.((i -- 8) && 0x0F)
              lxor w.((i -- 3) && 0x0F))
              1)
        in
        w.(i land 0x0F) <- v;
        w.(i land 0x0F)
      in
      let round a b c d e f k w =
        (e := Int32.(!e + rol32 !a 5 + f !b !c !d + k + w));
        b := Int32.(rol32 !b 30)
      in
      for i = 0 to 15 do
        w.(i) <- be32_to_cpu buf (off + (i * 4))
      done;
      round a b c d e f1 k1 w.(0);
      round e a b c d f1 k1 w.(1);
      round d e a b c f1 k1 w.(2);
      round c d e a b f1 k1 w.(3);
      round b c d e a f1 k1 w.(4);
      round a b c d e f1 k1 w.(5);
      round e a b c d f1 k1 w.(6);
      round d e a b c f1 k1 w.(7);
      round c d e a b f1 k1 w.(8);
      round b c d e a f1 k1 w.(9);
      round a b c d e f1 k1 w.(10);
      round e a b c d f1 k1 w.(11);
      round d e a b c f1 k1 w.(12);
      round c d e a b f1 k1 w.(13);
      round b c d e a f1 k1 w.(14);
      round a b c d e f1 k1 w.(15);
      round e a b c d f1 k1 (m 16);
      round d e a b c f1 k1 (m 17);
      round c d e a b f1 k1 (m 18);
      round b c d e a f1 k1 (m 19);
      round a b c d e f2 k2 (m 20);
      round e a b c d f2 k2 (m 21);
      round d e a b c f2 k2 (m 22);
      round c d e a b f2 k2 (m 23);
      round b c d e a f2 k2 (m 24);
      round a b c d e f2 k2 (m 25);
      round e a b c d f2 k2 (m 26);
      round d e a b c f2 k2 (m 27);
      round c d e a b f2 k2 (m 28);
      round b c d e a f2 k2 (m 29);
      round a b c d e f2 k2 (m 30);
      round e a b c d f2 k2 (m 31);
      round d e a b c f2 k2 (m 32);
      round c d e a b f2 k2 (m 33);
      round b c d e a f2 k2 (m 34);
      round a b c d e f2 k2 (m 35);
      round e a b c d f2 k2 (m 36);
      round d e a b c f2 k2 (m 37);
      round c d e a b f2 k2 (m 38);
      round b c d e a f2 k2 (m 39);
      round a b c d e f3 k3 (m 40);
      round e a b c d f3 k3 (m 41);
      round d e a b c f3 k3 (m 42);
      round c d e a b f3 k3 (m 43);
      round b c d e a f3 k3 (m 44);
      round a b c d e f3 k3 (m 45);
      round e a b c d f3 k3 (m 46);
      round d e a b c f3 k3 (m 47);
      round c d e a b f3 k3 (m 48);
      round b c d e a f3 k3 (m 49);
      round a b c d e f3 k3 (m 50);
      round e a b c d f3 k3 (m 51);
      round d e a b c f3 k3 (m 52);
      round c d e a b f3 k3 (m 53);
      round b c d e a f3 k3 (m 54);
      round a b c d e f3 k3 (m 55);
      round e a b c d f3 k3 (m 56);
      round d e a b c f3 k3 (m 57);
      round c d e a b f3 k3 (m 58);
      round b c d e a f3 k3 (m 59);
      round a b c d e f4 k4 (m 60);
      round e a b c d f4 k4 (m 61);
      round d e a b c f4 k4 (m 62);
      round c d e a b f4 k4 (m 63);
      round b c d e a f4 k4 (m 64);
      round a b c d e f4 k4 (m 65);
      round e a b c d f4 k4 (m 66);
      round d e a b c f4 k4 (m 67);
      round c d e a b f4 k4 (m 68);
      round b c d e a f4 k4 (m 69);
      round a b c d e f4 k4 (m 70);
      round e a b c d f4 k4 (m 71);
      round d e a b c f4 k4 (m 72);
      round c d e a b f4 k4 (m 73);
      round b c d e a f4 k4 (m 74);
      round a b c d e f4 k4 (m 75);
      round e a b c d f4 k4 (m 76);
      round d e a b c f4 k4 (m 77);
      round c d e a b f4 k4 (m 78);
      round b c d e a f4 k4 (m 79);
      ctx.h.(0) <- Int32.add ctx.h.(0) !a;
      ctx.h.(1) <- Int32.add ctx.h.(1) !b;
      ctx.h.(2) <- Int32.add ctx.h.(2) !c;
      ctx.h.(3) <- Int32.add ctx.h.(3) !d;
      ctx.h.(4) <- Int32.add ctx.h.(4) !e;
      ()

    let feed :
        type a.
        blit:(a -> int -> By.t -> int -> int -> unit) ->
        be32_to_cpu:(a -> int -> int32) ->
        ctx ->
        a ->
        int ->
        int ->
        unit =
     fun ~blit ~be32_to_cpu ctx buf off len ->
      let idx = ref Int64.(to_int (ctx.size land 0x3FL)) in
      let len = ref len in
      let off = ref off in
      let to_fill = 64 - !idx in
      ctx.size <- Int64.add ctx.size (Int64.of_int !len);
      if !idx <> 0 && !len >= to_fill then (
        blit buf !off ctx.b !idx to_fill;
        sha1_do_chunk ~be32_to_cpu:By.be32_to_cpu ctx ctx.b 0;
        len := !len - to_fill;
        off := !off + to_fill;
        idx := 0);
      while !len >= 64 do
        sha1_do_chunk ~be32_to_cpu ctx buf !off;
        len := !len - 64;
        off := !off + 64
      done;
      if !len <> 0 then blit buf !off ctx.b !idx !len;
      ()

    let unsafe_feed_bytes = feed ~blit:By.blit ~be32_to_cpu:By.be32_to_cpu

    let unsafe_feed_bigstring =
      feed ~blit:By.blit_from_bigstring ~be32_to_cpu:Bi.be32_to_cpu

    let unsafe_get ctx =
      let index = Int64.(to_int (ctx.size land 0x3FL)) in
      let padlen = if index < 56 then 56 - index else 64 + 56 - index in
      let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
      let bits = By.create 8 in
      By.cpu_to_be64 bits 0 Int64.(ctx.size lsl 3);
      unsafe_feed_bytes ctx padding 0 padlen;
      unsafe_feed_bytes ctx bits 0 8;
      let res = By.create (5 * 4) in
      for i = 0 to 4 do
        By.cpu_to_be32 res (i * 4) ctx.h.(i)
      done;
      res
  end
end

module Baijiu_sha256 = struct
  module By = Digestif_by
  module Bi = Digestif_bi

  module Int32 = struct
    include Int32

    let ( lsl ) = Int32.shift_left
    let ( lsr ) = Int32.shift_right_logical
    let ( asr ) = Int32.shift_right
    let ( lor ) = Int32.logor
    let ( lxor ) = Int32.logxor
    let ( land ) = Int32.logand
    let ( + ) = Int32.add
    let rol32 a n = (a lsl n) lor (a lsr (32 - n))
    let ror32 a n = (a lsr n) lor (a lsl (32 - n))
  end

  module Int64 = struct
    include Int64

    let ( land ) = Int64.logand
    let ( lsl ) = Int64.shift_left
  end

  module type S = sig
    type kind = [ `SHA256 ]
    type ctx = { mutable size : int64; b : Bytes.t; h : int32 array }

    val init : unit -> ctx
    val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
    val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val unsafe_get : ctx -> By.t
    val dup : ctx -> ctx
  end

  module Unsafe : S = struct
    type kind = [ `SHA256 ]
    type ctx = { mutable size : int64; b : Bytes.t; h : int32 array }

    let dup ctx = { size = ctx.size; b = By.copy ctx.b; h = Array.copy ctx.h }

    let init () =
      let b = By.make 128 '\x00' in
      {
        size = 0L;
        b;
        h =
          [|
            0x6a09e667l;
            0xbb67ae85l;
            0x3c6ef372l;
            0xa54ff53al;
            0x510e527fl;
            0x9b05688cl;
            0x1f83d9abl;
            0x5be0cd19l;
          |];
      }

    let k =
      [|
        0x428a2f98l;
        0x71374491l;
        0xb5c0fbcfl;
        0xe9b5dba5l;
        0x3956c25bl;
        0x59f111f1l;
        0x923f82a4l;
        0xab1c5ed5l;
        0xd807aa98l;
        0x12835b01l;
        0x243185bel;
        0x550c7dc3l;
        0x72be5d74l;
        0x80deb1fel;
        0x9bdc06a7l;
        0xc19bf174l;
        0xe49b69c1l;
        0xefbe4786l;
        0x0fc19dc6l;
        0x240ca1ccl;
        0x2de92c6fl;
        0x4a7484aal;
        0x5cb0a9dcl;
        0x76f988dal;
        0x983e5152l;
        0xa831c66dl;
        0xb00327c8l;
        0xbf597fc7l;
        0xc6e00bf3l;
        0xd5a79147l;
        0x06ca6351l;
        0x14292967l;
        0x27b70a85l;
        0x2e1b2138l;
        0x4d2c6dfcl;
        0x53380d13l;
        0x650a7354l;
        0x766a0abbl;
        0x81c2c92el;
        0x92722c85l;
        0xa2bfe8a1l;
        0xa81a664bl;
        0xc24b8b70l;
        0xc76c51a3l;
        0xd192e819l;
        0xd6990624l;
        0xf40e3585l;
        0x106aa070l;
        0x19a4c116l;
        0x1e376c08l;
        0x2748774cl;
        0x34b0bcb5l;
        0x391c0cb3l;
        0x4ed8aa4al;
        0x5b9cca4fl;
        0x682e6ff3l;
        0x748f82eel;
        0x78a5636fl;
        0x84c87814l;
        0x8cc70208l;
        0x90befffal;
        0xa4506cebl;
        0xbef9a3f7l;
        0xc67178f2l;
      |]

    let e0 x = Int32.(ror32 x 2 lxor ror32 x 13 lxor ror32 x 22)
    let e1 x = Int32.(ror32 x 6 lxor ror32 x 11 lxor ror32 x 25)
    let s0 x = Int32.(ror32 x 7 lxor ror32 x 18 lxor (x lsr 3))
    let s1 x = Int32.(ror32 x 17 lxor ror32 x 19 lxor (x lsr 10))

    let sha256_do_chunk :
        type a. be32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
     fun ~be32_to_cpu ctx buf off ->
      let a, b, c, d, e, f, g, h, t1, t2 =
        ( ref ctx.h.(0),
          ref ctx.h.(1),
          ref ctx.h.(2),
          ref ctx.h.(3),
          ref ctx.h.(4),
          ref ctx.h.(5),
          ref ctx.h.(6),
          ref ctx.h.(7),
          ref 0l,
          ref 0l )
      in
      let w = Array.make 64 0l in
      for i = 0 to 15 do
        w.(i) <- be32_to_cpu buf (off + (i * 4))
      done;
      let ( -- ) a b = a - b in
      for i = 16 to 63 do
        w.(i) <-
          Int32.(s1 w.(i -- 2) + w.(i -- 7) + s0 w.(i -- 15) + w.(i -- 16))
      done;
      let round a b c d e f g h k w =
        let open Int32 in
        t1 := !h + e1 !e + (!g lxor (!e land (!f lxor !g))) + k + w;
        t2 := e0 !a + (!a land !b lor (!c land (!a lor !b)));
        d := !d + !t1;
        h := !t1 + !t2
      in
      for i = 0 to 7 do
        round a b c d e f g h k.((i * 8) + 0) w.((i * 8) + 0);
        round h a b c d e f g k.((i * 8) + 1) w.((i * 8) + 1);
        round g h a b c d e f k.((i * 8) + 2) w.((i * 8) + 2);
        round f g h a b c d e k.((i * 8) + 3) w.((i * 8) + 3);
        round e f g h a b c d k.((i * 8) + 4) w.((i * 8) + 4);
        round d e f g h a b c k.((i * 8) + 5) w.((i * 8) + 5);
        round c d e f g h a b k.((i * 8) + 6) w.((i * 8) + 6);
        round b c d e f g h a k.((i * 8) + 7) w.((i * 8) + 7)
      done;
      let open Int32 in
      ctx.h.(0) <- ctx.h.(0) + !a;
      ctx.h.(1) <- ctx.h.(1) + !b;
      ctx.h.(2) <- ctx.h.(2) + !c;
      ctx.h.(3) <- ctx.h.(3) + !d;
      ctx.h.(4) <- ctx.h.(4) + !e;
      ctx.h.(5) <- ctx.h.(5) + !f;
      ctx.h.(6) <- ctx.h.(6) + !g;
      ctx.h.(7) <- ctx.h.(7) + !h;
      ()

    let feed :
        type a.
        blit:(a -> int -> By.t -> int -> int -> unit) ->
        be32_to_cpu:(a -> int -> int32) ->
        ctx ->
        a ->
        int ->
        int ->
        unit =
     fun ~blit ~be32_to_cpu ctx buf off len ->
      let idx = ref Int64.(to_int (ctx.size land 0x3FL)) in
      let len = ref len in
      let off = ref off in
      let to_fill = 64 - !idx in
      ctx.size <- Int64.add ctx.size (Int64.of_int !len);
      if !idx <> 0 && !len >= to_fill then (
        blit buf !off ctx.b !idx to_fill;
        sha256_do_chunk ~be32_to_cpu:By.be32_to_cpu ctx ctx.b 0;
        len := !len - to_fill;
        off := !off + to_fill;
        idx := 0);
      while !len >= 64 do
        sha256_do_chunk ~be32_to_cpu ctx buf !off;
        len := !len - 64;
        off := !off + 64
      done;
      if !len <> 0 then blit buf !off ctx.b !idx !len;
      ()

    let unsafe_feed_bytes = feed ~blit:By.blit ~be32_to_cpu:By.be32_to_cpu

    let unsafe_feed_bigstring =
      feed ~blit:By.blit_from_bigstring ~be32_to_cpu:Bi.be32_to_cpu

    let unsafe_get ctx =
      let index = Int64.(to_int (ctx.size land 0x3FL)) in
      let padlen = if index < 56 then 56 - index else 64 + 56 - index in
      let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
      let bits = By.create 8 in
      By.cpu_to_be64 bits 0 Int64.(ctx.size lsl 3);
      unsafe_feed_bytes ctx padding 0 padlen;
      unsafe_feed_bytes ctx bits 0 8;
      let res = By.create (8 * 4) in
      for i = 0 to 7 do
        By.cpu_to_be32 res (i * 4) ctx.h.(i)
      done;
      res
  end
end

module Baijiu_sha3 = struct
  module By = Digestif_by
  module Bi = Digestif_bi

  let nist_padding = 0x06L
  let keccak_padding = 0x01L

  module Int64 = struct
    include Int64

    let ( lsl ) = Int64.shift_left
    let ( lsr ) = Int64.shift_right_logical
    let ( asr ) = Int64.shift_right
    let ( lor ) = Int64.logor
    let ( land ) = Int64.logand
    let ( lxor ) = Int64.logxor
    let ( + ) = Int64.add
    let ror64 a n = (a lsr n) lor (a lsl (64 - n))
    let rol64 a n = (a lsl n) lor (a lsr (64 - n))
  end

  module Unsafe (P : sig
    val padding : int64
  end) =
  struct
    type ctx = {
      q : int64 array;
      rsize : int;
      (* block size *)
      mdlen : int;
      (* output size *)
      mutable pt : int;
    }

    let dup ctx =
      {
        q = Array.copy ctx.q;
        rsize = ctx.rsize;
        mdlen = ctx.mdlen;
        pt = ctx.pt;
      }

    let init mdlen =
      let rsize = 200 - (2 * mdlen) in
      { q = Array.make 25 0L; rsize; mdlen; pt = 0 }

    let keccakf_rounds = 24

    let keccaft_rndc : int64 array =
      [|
        0x0000000000000001L;
        0x0000000000008082L;
        0x800000000000808aL;
        0x8000000080008000L;
        0x000000000000808bL;
        0x0000000080000001L;
        0x8000000080008081L;
        0x8000000000008009L;
        0x000000000000008aL;
        0x0000000000000088L;
        0x0000000080008009L;
        0x000000008000000aL;
        0x000000008000808bL;
        0x800000000000008bL;
        0x8000000000008089L;
        0x8000000000008003L;
        0x8000000000008002L;
        0x8000000000000080L;
        0x000000000000800aL;
        0x800000008000000aL;
        0x8000000080008081L;
        0x8000000000008080L;
        0x0000000080000001L;
        0x8000000080008008L;
      |]

    let keccaft_rotc : int array =
      [|
        1;
        3;
        6;
        10;
        15;
        21;
        28;
        36;
        45;
        55;
        2;
        14;
        27;
        41;
        56;
        8;
        25;
        43;
        62;
        18;
        39;
        61;
        20;
        44;
      |]

    let keccakf_piln : int array =
      [|
        10;
        7;
        11;
        17;
        18;
        3;
        5;
        16;
        8;
        21;
        24;
        4;
        15;
        23;
        19;
        13;
        12;
        2;
        20;
        14;
        22;
        9;
        6;
        1;
      |]

    let sha3_keccakf (q : int64 array) =
      for r = 0 to keccakf_rounds - 1 do
        let ( lxor ) = Int64.( lxor ) in
        let lnot = Int64.lognot in
        let ( land ) = Int64.( land ) in
        (* Theta *)
        let bc =
          Array.init 5 (fun i ->
              q.(i)
              lxor q.(i + 5)
              lxor q.(i + 10)
              lxor q.(i + 15)
              lxor q.(i + 20))
        in
        for i = 0 to 4 do
          let t = bc.((i + 4) mod 5) lxor Int64.rol64 bc.((i + 1) mod 5) 1 in
          for k = 0 to 4 do
            let j = k * 5 in
            q.(j + i) <- q.(j + i) lxor t
          done
        done;

        (* Rho Pi *)
        let t = ref q.(1) in
        let _ =
          Array.iteri
            (fun i rotc ->
              let j = keccakf_piln.(i) in
              bc.(0) <- q.(j);
              q.(j) <- Int64.rol64 !t rotc;
              t := bc.(0))
            keccaft_rotc
        in

        (* Chi *)
        for k = 0 to 4 do
          let j = k * 5 in
          let bc = Array.init 5 (fun i -> q.(j + i)) in
          for i = 0 to 4 do
            q.(j + i) <-
              q.(j + i) lxor (lnot bc.((i + 1) mod 5) land bc.((i + 2) mod 5))
          done
        done;

        (* Iota *)
        q.(0) <- q.(0) lxor keccaft_rndc.(r)
      done

    let masks =
      [|
        0xffffffffffffff00L;
        0xffffffffffff00ffL;
        0xffffffffff00ffffL;
        0xffffffff00ffffffL;
        0xffffff00ffffffffL;
        0xffff00ffffffffffL;
        0xff00ffffffffffffL;
        0x00ffffffffffffffL;
      |]

    let feed :
        type a. get_uint8:(a -> int -> int) -> ctx -> a -> int -> int -> unit =
     fun ~get_uint8 ctx buf off len ->
      let ( && ) = ( land ) in

      let ( lxor ) = Int64.( lxor ) in
      let ( land ) = Int64.( land ) in
      let ( lor ) = Int64.( lor ) in
      let ( lsr ) = Int64.( lsr ) in
      let ( lsl ) = Int64.( lsl ) in

      let j = ref ctx.pt in

      for i = 0 to len - 1 do
        let v =
          (ctx.q.(!j / 8) land (0xffL lsl ((!j && 0x7) * 8)))
          lsr ((!j && 0x7) * 8)
        in
        let v = v lxor Int64.of_int (get_uint8 buf (off + i)) in
        ctx.q.(!j / 8) <-
          ctx.q.(!j / 8) land masks.(!j && 0x7) lor (v lsl ((!j && 0x7) * 8));
        incr j;
        if !j >= ctx.rsize then (
          sha3_keccakf ctx.q;
          j := 0)
      done;

      ctx.pt <- !j

    let unsafe_feed_bytes ctx buf off len =
      let get_uint8 buf off = Char.code (By.get buf off) in
      feed ~get_uint8 ctx buf off len

    let unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit =
     fun ctx buf off len ->
      let get_uint8 buf off = Char.code (Bi.get buf off) in
      feed ~get_uint8 ctx buf off len

    let unsafe_get ctx =
      let ( && ) = ( land ) in

      let ( lxor ) = Int64.( lxor ) in
      let ( lsl ) = Int64.( lsl ) in

      let v = ctx.q.(ctx.pt / 8) in
      let v = v lxor (P.padding lsl ((ctx.pt && 0x7) * 8)) in
      ctx.q.(ctx.pt / 8) <- v;

      let v = ctx.q.((ctx.rsize - 1) / 8) in
      let v = v lxor (0x80L lsl (((ctx.rsize - 1) && 0x7) * 8)) in
      ctx.q.((ctx.rsize - 1) / 8) <- v;

      sha3_keccakf ctx.q;

      (* Get hash *)
      (* if the hash size in bytes is not a multiple of 8 (meaning it is
         not composed of whole int64 words, like for sha3_224), we
         extract the whole last int64 word from the state [ctx.st] and
         cut the hash at the right size after conversion to bytes. *)
      let n =
        let r = ctx.mdlen mod 8 in
        ctx.mdlen + if r = 0 then 0 else 8 - r
      in

      let hash = By.create n in
      for i = 0 to (n / 8) - 1 do
        By.unsafe_set_64 hash (i * 8)
          (if Sys.big_endian then By.swap64 ctx.q.(i) else ctx.q.(i))
      done;

      By.sub hash 0 ctx.mdlen
  end
end

module Baijiu_sha512 = struct
  module By = Digestif_by
  module Bi = Digestif_bi

  module Int64 = struct
    include Int64

    let ( lsl ) = Int64.shift_left
    let ( lsr ) = Int64.shift_right_logical
    let ( asr ) = Int64.shift_right
    let ( lor ) = Int64.logor
    let ( land ) = Int64.logand
    let ( lxor ) = Int64.logxor
    let ( + ) = Int64.add
    let ror64 a n = (a lsr n) lor (a lsl (64 - n))
    let rol64 a n = (a lsl n) lor (a lsr (64 - n))
  end

  module type S = sig
    type kind = [ `SHA512 ]
    type ctx = { mutable size : int64 array; b : Bytes.t; h : int64 array }

    val init : unit -> ctx
    val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
    val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val unsafe_get : ctx -> By.t
    val dup : ctx -> ctx
  end

  module Unsafe : S = struct
    type kind = [ `SHA512 ]
    type ctx = { mutable size : int64 array; b : Bytes.t; h : int64 array }

    let dup ctx =
      { size = Array.copy ctx.size; b = By.copy ctx.b; h = Array.copy ctx.h }

    let init () =
      let b = By.make 128 '\x00' in
      {
        size = [| 0L; 0L |];
        b;
        h =
          [|
            0x6a09e667f3bcc908L;
            0xbb67ae8584caa73bL;
            0x3c6ef372fe94f82bL;
            0xa54ff53a5f1d36f1L;
            0x510e527fade682d1L;
            0x9b05688c2b3e6c1fL;
            0x1f83d9abfb41bd6bL;
            0x5be0cd19137e2179L;
          |];
      }

    let k =
      [|
        0x428a2f98d728ae22L;
        0x7137449123ef65cdL;
        0xb5c0fbcfec4d3b2fL;
        0xe9b5dba58189dbbcL;
        0x3956c25bf348b538L;
        0x59f111f1b605d019L;
        0x923f82a4af194f9bL;
        0xab1c5ed5da6d8118L;
        0xd807aa98a3030242L;
        0x12835b0145706fbeL;
        0x243185be4ee4b28cL;
        0x550c7dc3d5ffb4e2L;
        0x72be5d74f27b896fL;
        0x80deb1fe3b1696b1L;
        0x9bdc06a725c71235L;
        0xc19bf174cf692694L;
        0xe49b69c19ef14ad2L;
        0xefbe4786384f25e3L;
        0x0fc19dc68b8cd5b5L;
        0x240ca1cc77ac9c65L;
        0x2de92c6f592b0275L;
        0x4a7484aa6ea6e483L;
        0x5cb0a9dcbd41fbd4L;
        0x76f988da831153b5L;
        0x983e5152ee66dfabL;
        0xa831c66d2db43210L;
        0xb00327c898fb213fL;
        0xbf597fc7beef0ee4L;
        0xc6e00bf33da88fc2L;
        0xd5a79147930aa725L;
        0x06ca6351e003826fL;
        0x142929670a0e6e70L;
        0x27b70a8546d22ffcL;
        0x2e1b21385c26c926L;
        0x4d2c6dfc5ac42aedL;
        0x53380d139d95b3dfL;
        0x650a73548baf63deL;
        0x766a0abb3c77b2a8L;
        0x81c2c92e47edaee6L;
        0x92722c851482353bL;
        0xa2bfe8a14cf10364L;
        0xa81a664bbc423001L;
        0xc24b8b70d0f89791L;
        0xc76c51a30654be30L;
        0xd192e819d6ef5218L;
        0xd69906245565a910L;
        0xf40e35855771202aL;
        0x106aa07032bbd1b8L;
        0x19a4c116b8d2d0c8L;
        0x1e376c085141ab53L;
        0x2748774cdf8eeb99L;
        0x34b0bcb5e19b48a8L;
        0x391c0cb3c5c95a63L;
        0x4ed8aa4ae3418acbL;
        0x5b9cca4f7763e373L;
        0x682e6ff3d6b2b8a3L;
        0x748f82ee5defb2fcL;
        0x78a5636f43172f60L;
        0x84c87814a1f0ab72L;
        0x8cc702081a6439ecL;
        0x90befffa23631e28L;
        0xa4506cebde82bde9L;
        0xbef9a3f7b2c67915L;
        0xc67178f2e372532bL;
        0xca273eceea26619cL;
        0xd186b8c721c0c207L;
        0xeada7dd6cde0eb1eL;
        0xf57d4f7fee6ed178L;
        0x06f067aa72176fbaL;
        0x0a637dc5a2c898a6L;
        0x113f9804bef90daeL;
        0x1b710b35131c471bL;
        0x28db77f523047d84L;
        0x32caab7b40c72493L;
        0x3c9ebe0a15c9bebcL;
        0x431d67c49c100d4cL;
        0x4cc5d4becb3e42b6L;
        0x597f299cfc657e2aL;
        0x5fcb6fab3ad6faecL;
        0x6c44198c4a475817L;
      |]

    let e0 x = Int64.(ror64 x 28 lxor ror64 x 34 lxor ror64 x 39)
    let e1 x = Int64.(ror64 x 14 lxor ror64 x 18 lxor ror64 x 41)
    let s0 x = Int64.(ror64 x 1 lxor ror64 x 8 lxor (x lsr 7))
    let s1 x = Int64.(ror64 x 19 lxor ror64 x 61 lxor (x lsr 6))

    let sha512_do_chunk :
        type a. be64_to_cpu:(a -> int -> int64) -> ctx -> a -> int -> unit =
     fun ~be64_to_cpu ctx buf off ->
      let a, b, c, d, e, f, g, h, t1, t2 =
        ( ref ctx.h.(0),
          ref ctx.h.(1),
          ref ctx.h.(2),
          ref ctx.h.(3),
          ref ctx.h.(4),
          ref ctx.h.(5),
          ref ctx.h.(6),
          ref ctx.h.(7),
          ref 0L,
          ref 0L )
      in
      let w = Array.make 80 0L in
      for i = 0 to 15 do
        w.(i) <- be64_to_cpu buf (off + (i * 8))
      done;
      let ( -- ) a b = a - b in
      for i = 16 to 79 do
        w.(i) <-
          Int64.(s1 w.(i -- 2) + w.(i -- 7) + s0 w.(i -- 15) + w.(i -- 16))
      done;
      let round a b c d e f g h k w =
        let open Int64 in
        t1 := !h + e1 !e + (!g lxor (!e land (!f lxor !g))) + k + w;
        t2 := e0 !a + (!a land !b lor (!c land (!a lor !b)));
        d := !d + !t1;
        h := !t1 + !t2
      in
      for i = 0 to 9 do
        round a b c d e f g h k.((i * 8) + 0) w.((i * 8) + 0);
        round h a b c d e f g k.((i * 8) + 1) w.((i * 8) + 1);
        round g h a b c d e f k.((i * 8) + 2) w.((i * 8) + 2);
        round f g h a b c d e k.((i * 8) + 3) w.((i * 8) + 3);
        round e f g h a b c d k.((i * 8) + 4) w.((i * 8) + 4);
        round d e f g h a b c k.((i * 8) + 5) w.((i * 8) + 5);
        round c d e f g h a b k.((i * 8) + 6) w.((i * 8) + 6);
        round b c d e f g h a k.((i * 8) + 7) w.((i * 8) + 7)
      done;
      let open Int64 in
      ctx.h.(0) <- ctx.h.(0) + !a;
      ctx.h.(1) <- ctx.h.(1) + !b;
      ctx.h.(2) <- ctx.h.(2) + !c;
      ctx.h.(3) <- ctx.h.(3) + !d;
      ctx.h.(4) <- ctx.h.(4) + !e;
      ctx.h.(5) <- ctx.h.(5) + !f;
      ctx.h.(6) <- ctx.h.(6) + !g;
      ctx.h.(7) <- ctx.h.(7) + !h;
      ()

    let feed :
        type a.
        blit:(a -> int -> By.t -> int -> int -> unit) ->
        be64_to_cpu:(a -> int -> int64) ->
        ctx ->
        a ->
        int ->
        int ->
        unit =
     fun ~blit ~be64_to_cpu ctx buf off len ->
      let idx = ref Int64.(to_int (ctx.size.(0) land 0x7FL)) in
      let len = ref len in
      let off = ref off in
      let to_fill = 128 - !idx in
      ctx.size.(0) <- Int64.add ctx.size.(0) (Int64.of_int !len);
      if ctx.size.(0) < Int64.of_int !len then
        ctx.size.(1) <- Int64.succ ctx.size.(1);
      if !idx <> 0 && !len >= to_fill then (
        blit buf !off ctx.b !idx to_fill;
        sha512_do_chunk ~be64_to_cpu:By.be64_to_cpu ctx ctx.b 0;
        len := !len - to_fill;
        off := !off + to_fill;
        idx := 0);
      while !len >= 128 do
        sha512_do_chunk ~be64_to_cpu ctx buf !off;
        len := !len - 128;
        off := !off + 128
      done;
      if !len <> 0 then blit buf !off ctx.b !idx !len;
      ()

    let unsafe_feed_bytes = feed ~blit:By.blit ~be64_to_cpu:By.be64_to_cpu

    let unsafe_feed_bigstring =
      feed ~blit:By.blit_from_bigstring ~be64_to_cpu:Bi.be64_to_cpu

    let unsafe_get ctx =
      let index = Int64.(to_int (ctx.size.(0) land 0x7FL)) in
      let padlen = if index < 112 then 112 - index else 128 + 112 - index in
      let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
      let bits = By.create 16 in
      By.cpu_to_be64 bits 0
        Int64.((ctx.size.(1) lsl 3) lor (ctx.size.(0) lsr 61));
      By.cpu_to_be64 bits 8 Int64.(ctx.size.(0) lsl 3);
      unsafe_feed_bytes ctx padding 0 padlen;
      unsafe_feed_bytes ctx bits 0 16;
      let res = By.create (8 * 8) in
      for i = 0 to 7 do
        By.cpu_to_be64 res (i * 8) ctx.h.(i)
      done;
      res
  end
end

module Baijiu_sha3_512 = struct
  module By = Digestif_by
  module Bi = Digestif_bi

  module type S = sig
    type ctx
    type kind = [ `SHA3_512 ]

    val init : unit -> ctx
    val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
    val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val unsafe_get : ctx -> By.t
    val dup : ctx -> ctx
  end

  module Unsafe : S = struct
    type kind = [ `SHA3_512 ]

    module U = Baijiu_sha3.Unsafe (struct
      let padding = Baijiu_sha3.nist_padding
    end)

    open U

    type nonrec ctx = ctx

    let init () = U.init 64
    let unsafe_get = unsafe_get
    let dup = dup
    let unsafe_feed_bytes = unsafe_feed_bytes
    let unsafe_feed_bigstring = unsafe_feed_bigstring
  end
end

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a iter = ('a -> unit) -> unit
type 'a compare = 'a -> 'a -> int
type 'a equal = 'a -> 'a -> bool
type 'a pp = Format.formatter -> 'a -> unit

module By = Digestif_by
module Bi = Digestif_bi
module Conv = Digestif_conv

let failwith fmt = Format.ksprintf failwith fmt

module type S = sig
  val digest_size : int

  type ctx
  type t

  val empty : ctx
  val init : unit -> ctx
  val feed_bytes : ctx -> ?off:int -> ?len:int -> Bytes.t -> ctx
  val feed_string : ctx -> ?off:int -> ?len:int -> String.t -> ctx
  val feed_bigstring : ctx -> ?off:int -> ?len:int -> bigstring -> ctx
  val feedi_bytes : ctx -> Bytes.t iter -> ctx
  val feedi_string : ctx -> String.t iter -> ctx
  val feedi_bigstring : ctx -> bigstring iter -> ctx
  val get : ctx -> t
  val digest_bytes : ?off:int -> ?len:int -> Bytes.t -> t
  val digest_string : ?off:int -> ?len:int -> String.t -> t
  val digest_bigstring : ?off:int -> ?len:int -> bigstring -> t
  val digesti_bytes : Bytes.t iter -> t
  val digesti_string : String.t iter -> t
  val digesti_bigstring : bigstring iter -> t
  val digestv_bytes : Bytes.t list -> t
  val digestv_string : String.t list -> t
  val digestv_bigstring : bigstring list -> t
  val hmac_bytes : key:string -> ?off:int -> ?len:int -> Bytes.t -> t
  val hmac_string : key:string -> ?off:int -> ?len:int -> String.t -> t
  val hmac_bigstring : key:string -> ?off:int -> ?len:int -> bigstring -> t
  val hmaci_bytes : key:string -> Bytes.t iter -> t
  val hmaci_string : key:string -> String.t iter -> t
  val hmaci_bigstring : key:string -> bigstring iter -> t
  val hmacv_bytes : key:string -> Bytes.t list -> t
  val hmacv_string : key:string -> String.t list -> t
  val hmacv_bigstring : key:string -> bigstring list -> t
  val pp : t pp
  val of_hex : string -> t
  val of_hex_opt : string -> t option
  val consistent_of_hex : string -> t
  val consistent_of_hex_opt : string -> t option
  val to_hex : t -> string
  val of_raw_string : string -> t
  val of_raw_string_opt : string -> t option
  val to_raw_string : t -> string
  val get_into_bytes : ctx -> ?off:int -> bytes -> unit
end

module type MAC = sig
  type t

  val mac_bytes : key:string -> ?off:int -> ?len:int -> Bytes.t -> t
  val mac_string : key:string -> ?off:int -> ?len:int -> String.t -> t
  val mac_bigstring : key:string -> ?off:int -> ?len:int -> bigstring -> t
  val maci_bytes : key:string -> Bytes.t iter -> t
  val maci_string : key:string -> String.t iter -> t
  val maci_bigstring : key:string -> bigstring iter -> t
  val macv_bytes : key:string -> Bytes.t list -> t
  val macv_string : key:string -> String.t list -> t
  val macv_bigstring : key:string -> bigstring list -> t
end

module type Desc = sig
  val digest_size : int
  val block_size : int
end

module type Hash = sig
  type ctx

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe (Hash : Hash) (D : Desc) = struct
  open Hash

  let digest_size = D.digest_size
  let block_size = D.block_size
  let empty = init ()
  let init = init

  let unsafe_feed_bytes ctx ?off ?len buf =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | Some off, None -> (off, By.length buf - off)
      | None, Some len -> (0, len)
      | None, None -> (0, By.length buf)
    in
    if off < 0 || len < 0 || off > By.length buf - len then
      invalid_arg "offset out of bounds"
    else unsafe_feed_bytes ctx buf off len

  let unsafe_feed_string ctx ?off ?len buf =
    unsafe_feed_bytes ctx ?off ?len (By.unsafe_of_string buf)

  let unsafe_feed_bigstring ctx ?off ?len buf =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | Some off, None -> (off, Bi.length buf - off)
      | None, Some len -> (0, len)
      | None, None -> (0, Bi.length buf)
    in
    if off < 0 || len < 0 || off > Bi.length buf - len then
      invalid_arg "offset out of bounds"
    else unsafe_feed_bigstring ctx buf off len

  let unsafe_get = unsafe_get

  let get_into_bytes ctx ?(off = 0) buf =
    if off < 0 || off >= Bytes.length buf then
      invalid_arg "offset out of bounds";
    if Bytes.length buf - off < digest_size then
      invalid_arg "destination too small";
    let raw = unsafe_get (Hash.dup ctx) in
    Bytes.blit raw 0 buf off digest_size
end

module Core (Hash : Hash) (D : Desc) = struct
  type t = string
  type ctx = Hash.ctx

  include Unsafe (Hash) (D)
  include Conv.Make (D)

  let get t =
    let t = Hash.dup t in
    unsafe_get t |> By.unsafe_to_string

  let feed_bytes t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_bytes t ?off ?len buf;
    t

  let feed_string t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_string t ?off ?len buf;
    t

  let feed_bigstring t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_bigstring t ?off ?len buf;
    t

  let feedi_bytes t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_bytes t buf in
    iter feed;
    t

  let feedi_string t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_string t buf in
    iter feed;
    t

  let feedi_bigstring t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_bigstring t buf in
    iter feed;
    t

  let digest_bytes ?off ?len buf = feed_bytes empty ?off ?len buf |> get
  let digest_string ?off ?len buf = feed_string empty ?off ?len buf |> get
  let digest_bigstring ?off ?len buf = feed_bigstring empty ?off ?len buf |> get
  let digesti_bytes iter = feedi_bytes empty iter |> get
  let digesti_string iter = feedi_string empty iter |> get
  let digesti_bigstring iter = feedi_bigstring empty iter |> get
  let digestv_bytes lst = digesti_bytes (fun f -> List.iter f lst)
  let digestv_string lst = digesti_string (fun f -> List.iter f lst)
  let digestv_bigstring lst = digesti_bigstring (fun f -> List.iter f lst)
end

module Make (H : Hash) (D : Desc) = struct
  include Core (H) (D)

  let bytes_opad = By.init block_size (fun _ -> '\x5c')
  let bytes_ipad = By.init block_size (fun _ -> '\x36')

  let rec norm_bytes key =
    match Stdlib.compare (String.length key) block_size with
    | 1 -> norm_bytes (digest_string key)
    | -1 -> By.rpad (Bytes.unsafe_of_string key) block_size '\000'
    | _ -> By.of_string key

  let hmaci_bytes ~key iter =
    let key = norm_bytes key in
    let outer = Xor.Bytes.xor key bytes_opad in
    let inner = Xor.Bytes.xor key bytes_ipad in
    let ctx = feed_bytes empty inner in
    let res = feedi_bytes ctx iter |> get in
    let ctx = feed_bytes empty outer in
    feed_string ctx (res :> string) |> get

  let hmaci_string ~key iter =
    let key = norm_bytes key in
    (* XXX(dinosaure): safe, [rpad] and [digest] have a read-only access. *)
    let outer = Xor.Bytes.xor key bytes_opad in
    let inner = Xor.Bytes.xor key bytes_ipad in
    let ctx = feed_bytes empty inner in
    let res = feedi_string ctx iter |> get in
    let ctx = feed_bytes empty outer in
    feed_string ctx (res :> string) |> get

  let hmaci_bigstring ~key iter =
    let key = norm_bytes key in
    let outer = Xor.Bytes.xor key bytes_opad in
    let inner = Xor.Bytes.xor key bytes_ipad in
    let ctx = feed_bytes empty inner in
    let res = feedi_bigstring ctx iter |> get in
    let ctx = feed_bytes empty outer in
    feed_string ctx (res :> string) |> get

  let hmac_bytes ~key ?off ?len buf =
    let buf =
      match (off, len) with
      | Some off, Some len -> By.sub buf off len
      | Some off, None -> By.sub buf off (By.length buf - off)
      | None, Some len -> By.sub buf 0 len
      | None, None -> buf
    in
    hmaci_bytes ~key (fun f -> f buf)

  let hmac_string ~key ?off ?len buf =
    let buf =
      match (off, len) with
      | Some off, Some len -> String.sub buf off len
      | Some off, None -> String.sub buf off (String.length buf - off)
      | None, Some len -> String.sub buf 0 len
      | None, None -> buf
    in
    hmaci_string ~key (fun f -> f buf)

  let hmac_bigstring ~key ?off ?len buf =
    let buf =
      match (off, len) with
      | Some off, Some len -> Bi.sub buf off len
      | Some off, None -> Bi.sub buf off (Bi.length buf - off)
      | None, Some len -> Bi.sub buf 0 len
      | None, None -> buf
    in
    hmaci_bigstring ~key (fun f -> f buf)

  let hmacv_bytes ~key bufs = hmaci_bytes ~key (fun f -> List.iter f bufs)
  let hmacv_string ~key bufs = hmaci_string ~key (fun f -> List.iter f bufs)

  let hmacv_bigstring ~key bufs =
    hmaci_bigstring ~key (fun f -> List.iter f bufs)
end

module SHA1 : S =
  Make
    (Baijiu_sha1.Unsafe)
    (struct
      let digest_size, block_size = (20, 64)
    end)

module SHA256 : S =
  Make
    (Baijiu_sha256.Unsafe)
    (struct
      let digest_size, block_size = (32, 64)
    end)

module SHA512 : S =
  Make
    (Baijiu_sha512.Unsafe)
    (struct
      let digest_size, block_size = (64, 128)
    end)

module SHA3_512 : S =
  Make
    (Baijiu_sha3_512.Unsafe)
    (struct
      let digest_size, block_size = (64, 72)
    end)

type 'k hash =
  | SHA1 : SHA1.t hash
  | SHA256 : SHA256.t hash
  | SHA512 : SHA512.t hash
  | SHA3_512 : SHA3_512.t hash

let sha1 = SHA1
let sha256 = SHA256
let sha512 = SHA512
let sha3_512 = SHA3_512

type hash' = [ `SHA1 | `SHA256 | `SHA512 | `SHA3_512 ]

let hash_to_hash' : type a. a hash -> hash' = function
  | SHA1 -> `SHA1
  | SHA256 -> `SHA256
  | SHA512 -> `SHA512
  | SHA3_512 -> `SHA3_512

let module_of_hash' : hash' -> (module S) = function
  | `SHA1 -> (module SHA1)
  | `SHA256 -> (module SHA256)
  | `SHA512 -> (module SHA512)
  | `SHA3_512 -> (module SHA3_512)

let module_of : type k. k hash -> (module S with type t = k) = function
  | SHA1 -> (module SHA1)
  | SHA256 -> (module SHA256)
  | SHA512 -> (module SHA512)
  | SHA3_512 -> (module SHA3_512)

type 'hash t = 'hash

let digest_bytes : type k. k hash -> Bytes.t -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_bytes buf

let digest_string : type k. k hash -> String.t -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_string buf

let digest_bigstring : type k. k hash -> bigstring -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_bigstring buf

let digesti_bytes : type k. k hash -> Bytes.t iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_bytes iter

let digesti_string : type k. k hash -> String.t iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_string iter

let digesti_bigstring : type k. k hash -> bigstring iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_bigstring iter

let hmaci_bytes : type k. k hash -> key:string -> Bytes.t iter -> k t =
 fun hash ~key iter ->
  let module H = (val module_of hash) in
  H.hmaci_bytes ~key iter

let hmaci_string : type k. k hash -> key:string -> String.t iter -> k t =
 fun hash ~key iter ->
  let module H = (val module_of hash) in
  H.hmaci_string ~key iter

let hmaci_bigstring : type k. k hash -> key:string -> bigstring iter -> k t =
 fun hash ~key iter ->
  let module H = (val module_of hash) in
  H.hmaci_bigstring ~key iter

(* XXX(dinosaure): unsafe part to avoid overhead. *)

let pp : type k. k hash -> k t pp =
 fun hash ppf t ->
  let module H = (val module_of hash) in
  H.pp ppf t

let of_hex : type k. k hash -> string -> k t =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.of_hex hex

let of_hex_opt : type k. k hash -> string -> k t option =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.of_hex_opt hex

let consistent_of_hex : type k. k hash -> string -> k t =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.consistent_of_hex hex

let consistent_of_hex_opt : type k. k hash -> string -> k t option =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.consistent_of_hex_opt hex

let to_hex : type k. k hash -> k t -> string =
 fun hash t ->
  let module H = (val module_of hash) in
  H.to_hex t

let of_raw_string : type k. k hash -> string -> k t =
 fun hash s ->
  let module H = (val module_of hash) in
  H.of_raw_string s

let of_raw_string_opt : type k. k hash -> string -> k t option =
 fun hash s ->
  let module H = (val module_of hash) in
  H.of_raw_string_opt s

let to_raw_string : type k. k hash -> k t -> string =
 fun hash t ->
  let module H = (val module_of hash) in
  H.to_raw_string t

let of_digest (type hash) (module H : S with type t = hash) (hash : H.t) :
    hash t =
  hash

let of_sha1 hash = hash
let of_sha256 hash = hash
let of_sha512 hash = hash
let of_sha3_512 hash = hash
