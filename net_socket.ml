open Cryptokit;;

let gzip_compress s=
  let cs=transform_string (Zlib.compress()) s in
(*    print_string cs ;*)cs;;

let gzip_uncompress s=
(*  print_string s; *)
  transform_string (Zlib.uncompress()) s;;


let data_to_chan d oc=
  output_value oc (gzip_compress d);; 
(*  output_value oc d *)
(*  output_string oc (gzip_compress d);; *)

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
