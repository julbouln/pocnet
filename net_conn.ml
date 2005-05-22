open Unix;;

open Value_val;;
open Value_xml;;

open Net_socket;;
open Net_message;;
open Net_msg_handler;;

let get_my_addr()=
inet_addr_any;;

exception Network_error
exception Network_not_connected

class virtual network_object=
object(self)
  method virtual message_send : xml_message -> bool
end;;



class network_connection on_disconnect (message_resend:xml_message->unit) (iport:int)=
object(self)
  inherit network_object
  val mutable port=iport
  method get_port=port
  method set_port p=port<-p

  val mutable mph=new message_parser_handler
  method get_mph=mph
  method set_mph m=mph<-m

  val mutable ident=""
  method set_ident i=ident<-i
  method get_ident=ident

  val mutable recv_sock=Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  val mutable send_sock=Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

  val mutable recv_chans=(None : (in_channel * out_channel) option )
  val mutable send_chans=(None : (in_channel * out_channel) option )

  method recv_chans=match recv_chans with 
    | Some c->c
    | None -> raise Network_not_connected

  method send_chans=match send_chans with 
    | Some c->c
    | None -> raise Network_not_connected


  method disconnect()=
    print_string "POCNET : disconnect";print_newline();
    on_disconnect ident;
    (try 
       Unix.shutdown recv_sock Unix.SHUTDOWN_ALL;
       Unix.shutdown send_sock Unix.SHUTDOWN_ALL;
     with _ -> ());
(*    Thread.exit(); *)

  method message_send (xmsg:xml_message)=
    (try 
       let (ic,oc)=self#send_chans in
	 (*      print_string "POCNET : send message";print_newline(); *)
	 if xmsg#get_src="" then
	   xmsg#set_src ident;
	 let msg=xmsg#to_xml#to_string in
	   data_to_chan (msg^"\n") oc;
	   flush oc;
	   
	   (*	print_string "POCNET : wait for response message";print_newline(); *)
	   let recv=chan_to_data ic in
	   let rxmsg=new xml_message in
	   let rxn=new xml_node in
	     rxn#of_string recv;
	     rxmsg#from_xml rxn;
	     mph#message_check rxmsg;
	     (*	  print_string "POCNET : response message received";print_newline();  *)
     with 
	 End_of_file -> 
	   print_string "POCNET : WARNING : End of file (send message)";print_newline(); 
	   self#disconnect();false
       | Sys_error e ->
	   print_string ("POCNET : WARNING : "^e^" (send message)");print_newline(); 
	   self#disconnect();false
    );
	     true
  method message_receive()=
    let (ic,oc)=self#recv_chans in
    let recv=chan_to_data ic in
(*      print_string "POCNET : receive message";print_newline(); *)
    let xmsg=new xml_message in
    let xn=new xml_node in
      xn#of_string recv;
      xmsg#from_xml xn;
      message_resend xmsg;
(*      print_string "POCNET : message received";print_newline(); *)

      if xmsg#get_type<>"response" then (
	let rxmsg=mph#message_parse xmsg in
(*	  print_string "POCNET : message parsed";print_newline(); *)
	  rxmsg#get_values#set_id "values";
	  if ident=xmsg#get_src then
	    rxmsg#set_src "server" 
	  else
	    rxmsg#set_src ident;

	  rxmsg#set_dst xmsg#get_src;
	  let rmsg=rxmsg#to_xml#to_string in
(*	    print_string "POCNET : send response message";print_newline(); *)
	    data_to_chan (rmsg^"\n") oc;
	    flush oc;
      )

  method run()=

    while true do 
      try  
(*      print_string "POCNET : wait for message";print_newline(); *)
	self#message_receive(); 
      with 
	  End_of_file -> 
	    print_string "POCNET : WARNING : End of file";print_newline(); 
	    self#disconnect(); 
	    Thread.exit(); 
	| Sys_error e ->
	    print_string ("POCNET : WARNING : "^e);print_newline(); 
	    self#disconnect();
	    Thread.exit();  
    done;


  method start()=
    let t=Thread.create (function()->self#run()) () in
      print_string ("Thread "^string_of_int (Thread.id t)^" launched (connection)");
      print_newline();

end;;

