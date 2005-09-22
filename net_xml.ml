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



open Value_common;;
open Value_object;;

open Value_xml;;
open Value_xmlparser;;
open Value_lua;;
open Value_val;;

open Net_message;;
open Net_client;;

(** global default graphics parser, can be overided *)
(*let xml_default_net_message_parser=
  Global.empty("xml_default_net_message_parser");;
*)

class lua_message_handler=
object(self)
  inherit message_handler
  inherit lua_object as lo
  inherit generic_object

  val mutable parse_fun=fun v->[OLuaVal.Nil]
  val mutable check_fun=fun v->[OLuaVal.Nil]


  method parse xmsg=
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String xmsg#get_type);
	let r=parse_fun [OLuaVal.Table xmsg#get_values#to_lua#to_table] in
	let lo=new lua_obj in
	let rt=(
	  match (List.nth r 0) with
	    | OLuaVal.Table tbl -> tbl
	) in
	  lo#from_table rt;
	  vl#from_lua lo;
	  res#set_values vl;
	res

  method check xmsg=
    let r=check_fun [OLuaVal.Table xmsg#get_values#to_lua#to_table] in
    true



  method lua_init()=
    lua#set_val (OLuaVal.String "parse") (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.table) (fun v->v));
    lua#set_val (OLuaVal.String "check") (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.unit) (fun v->()));

    lo#lua_init();

    parse_fun<-lua#get_fun (OLuaVal.String "parse");
    check_fun<-lua#get_fun (OLuaVal.String "check");

end;;

class xml_net_msg_handler_parser= 
object (self)
  inherit xml_parser

(** object unique id *)
  val mutable id=""
  method get_id=id
(** object type *)
  val mutable nm=""
  method get_type=nm

(** lua code for this object *)
  val mutable lua=""
(** object properties *)

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()

  method parse_child k v=
    match k with
      | "script" -> lua<-v#pcdata;
      | _ -> ()

(** object initial init *)
  method init_object o=
    o#set_lua_script lua;

  method get_val=
    let ofun()=
      let o=
	new lua_message_handler
      in
	self#init_object o;
	o
    in      
      (id,ofun)

end;;


class xml_net_msg_handlers_parser= 
object
  inherit [(unit->lua_message_handler)] xml_stringhash_parser "net_message_handler" (fun()->new xml_net_msg_handler_parser) as super

  method init (add_handler:string->message_handler->unit) (pr:lua_object)=
    Hashtbl.iter (
      fun k v->
	let vo=v() in
	  pr#lua_parent_of k (vo:>lua_object);	  
	  add_handler k (vo:>message_handler);
	  vo#lua_init();

    ) super#get_hash;
end

(*

  <net_message_handlers>
   <net_message_handler type="add_object_type"/>

   <net_message_handler type="login">
    <script>
     function self.parse(v)
     end
     function self.check(v)
     end
    </script>
   </net_message_handler>
  </net_message_handlers>

*)
