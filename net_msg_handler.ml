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

open Value_xml;;
open Value_val;;

open Net_message;;

(** Generic message handlers *)

(** test message *)
class test_message_handler=
object(self)
  inherit message_handler
  method parse msg=
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String msg#get_type);
	res#set_values vl;
	print_string (msg#get_src^" say : "^(string_of_val (msg#get_values#get_val (`String "test"))));print_newline();
	res
  method check msg=true

end;;


(** ident message *)
class ident_message_handler get_port set_port get_ident set_ident=
object(self)
  inherit message_handler
  method parse msg=
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String msg#get_type);
	(match get_port with
	  | Some gp->
	      vl#set_val (`String "port") (`Int (gp()));
	  | None -> ());
	res#set_values vl;
	(match set_ident with
	   | Some si ->
	       let ident=(string_of_val (msg#get_values#get_val (`String "ident"))) in    
		 print_string ("POCNET: set_ident : "^ident);print_newline();
		 si ident;
	   | None -> ());

	  res
  method check msg=
    print_string "check";print_newline();
    (match set_port with
       | Some sp->
	   let port=(int_of_val (msg#get_values#get_val (`String "port"))) in    
	     print_string ("POCNET: set_port : "^string_of_int port);print_newline();
	     sp port
       | None -> ());
    true

end;;

(** xml file transfert message *)
class xml_file_message_handler=
object(self)
  inherit message_handler
  method parse msg=
    let file=(string_of_val (msg#get_values#get_val (`String "filename"))) in
      msg#get_data#to_file file;
      message_generic_response msg;

  method check msg=
    true
end;;

(** login message *)
(* TODO *)

(** chat message *)
class virtual chat=
object
  method virtual say : string -> string -> unit
  method virtual say_private : string -> string -> string -> unit
  method virtual action : string -> string -> unit
end;;

class chat_message_handler (ch:chat)=
object(self)
  inherit message_handler
  method parse msg=
    let nick=(string_of_val (msg#get_values#get_val (`String "nickname"))) in
    let t=(string_of_val (msg#get_values#get_val (`String "chat_type"))) in
    let message=(text_of_val (msg#get_values#get_val (`String "chat_message"))) in
      (match t with
	 | "global" ->
	     ch#say nick message
	 | "private" ->
	     let pv=(string_of_val (msg#get_values#get_val (`String "chat_dest"))) in
	       ch#say_private nick pv message
	 | "action" ->
	     ch#action nick message
	 | _ ->()
      );
      message_generic_response msg;

  method check msg=
    true
end;;


