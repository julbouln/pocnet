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

	(try 
	   let t=data#tag in	     
	   let xdata=new xml_node in
	     xdata#set_tag "data";
	     xdata#add_child data; 
	     
	     xml#add_child xdata;
	 with
	     Xml_node_binding_not_found xet->()
	);

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

let message_generic_response msg=
  let res=new xml_message in
    res#set_type "response";
    res#set_dst msg#get_src;
    let vl=new val_generic_handler in
      vl#set_id "values";
      vl#set_val (`String "type") (`String msg#get_type);
      res#set_values vl;
      
      res;;




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

  method handler_foreach f=
    Hashtbl.iter f handlers 
  

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


