open Value_xml;;
open Value_val;;

class message=
object(self)
  val mutable name=""
  method set_type t=name<-t
  method get_type=name

  val mutable src=""
  method set_src s=src<-s
  method get_src=src

  val mutable dst=""
  method set_dst d=dst<-d
  method get_dst=dst
end;;


exception Xml_message_error

class xml_message=
object(self)
  inherit message

  val mutable parsed=false
  method is_parsed=parsed
  method set_parsed p=parsed<-p

  val mutable values=new val_generic_handler
  method get_values=values
  method set_values v=values<-v

  val mutable data=new xml_node
  method get_data=data
  method set_data d=data<-d


  method to_xml=
    let xml=new xml_node in
      xml#set_tag "message";
      xml#add_attrib ("type",name);
      xml#add_attrib ("src",src);
      xml#add_attrib ("dst",dst);

      let xvals=values#to_xml in
	xml#add_child xvals; 

(*	let xdata=new xml_node in
	  xdata#set_tag "data";
	  xdata#add_child data; 

	  xml#add_child xdata;
*)
	  xml

  method from_xml (xml:xml_node)=
    if xml#tag="message" then (
    List.iter (
      fun (ak,av) ->
	match ak with
	  | "type" -> name<-av
	  | "src" -> src<-av
	  | "dst" -> dst<-av
	  | _ -> ()
    ) xml#attribs;
    List.iter (
      fun c->
	match c#tag with
	  | "values" -> values#from_xml c
	  | "data" -> data<-(List.nth c#children 0)
	  | _ -> ()
    ) xml#children;
    )
    else raise Xml_message_error

end;;


let xml_message_of_string str=
  let xn=new xml_node in
    xn#of_string str;
    let xmsg=new xml_message in
      xmsg#from_xml xn;
      xmsg;;


class virtual message_handler=
object(self)
  method virtual parse : xml_message -> xml_message
  method virtual check : xml_message -> bool
end;;

class empty_message_handler=
object
  inherit message_handler
  method parse msg=new xml_message
  method check msg=false
end;;



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

exception Message_parser_not_found of string

(* message_parser handler *)
class message_parser_handler=
object(self)
  val mutable handlers=Hashtbl.create 2

  method get_handlers=handlers
  method set_handlers h=handlers<-h

  method handler_add (k:string) (h:message_handler)=Hashtbl.add handlers k h
  method handler_replace (k:string) (h:message_handler)=Hashtbl.replace handlers k h
  method handler_get (k:string)=
      try 
	Hashtbl.find handlers k 
      with Not_found -> (*new empty_message_handler*)  raise (Message_parser_not_found k) 


  

  method message_parse xmsg=
    let res=ref (new xml_message) in
    let h=xmsg#get_type in
    let hdlr=self#handler_get h in
      if xmsg#is_parsed=false then (
	res:=hdlr#parse xmsg;
	xmsg#set_parsed true; 
      );
      !res


  method message_check xmsg=
    let h=(string_of_val (xmsg#get_values#get_val (`String "type"))) in
      let hdlr=self#handler_get h in
	hdlr#check xmsg;
 
end;;

(*
let xmsg=new xml_message;;
let xtmsg=new xml_node;;
xtmsg#of_file "test.xml";;

xmsg#from_xml xtmsg;;
print_string (xmsg#to_xml#to_string);;
*)
