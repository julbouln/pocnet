(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)



open Cryptokit;;

(** Simple data to net conversion *)

let gzip_compress s=
  let cs=transform_string (Zlib.compress()) s in
(*    print_string cs ;*)cs;;

let gzip_uncompress s=
(*  print_string s; *)
  transform_string (Zlib.uncompress()) s;;

(** copy data to output chan *)
let data_to_chan d oc=
  output_value oc (gzip_compress d);; 
(*  output_value oc d *)
(*  output_string oc (gzip_compress d);; *)

(** get data from input chan *)
let chan_to_data ic=
  let s=input_value ic in
    gzip_uncompress(s);;

(*input_value ic*)
(*
  let bsize=512 in
  print_string "message";print_newline();
  let r=ref bsize in
  let wbuf=Buffer.create bsize in
    while !r do
      let buf=String.create (bsize) in
      let dlen=input ic buf (0) (bsize) in
	(*		r:=dlen; *)
	print_int dlen;print_newline();
	
	Buffer.add_string wbuf (String.sub buf 0 dlen);	   
    done;
    let rbuf=(Buffer.contents wbuf)  in
      *)

(*    gzip_uncompress(input_line ic);; *)
