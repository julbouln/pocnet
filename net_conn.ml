open Unix;;

open Value_val;;
open Value_xml;;

open Net_socket;;
open Net_message;;

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
    Thread.exit();

  method message_send (xmsg:xml_message)=
    let (ic,oc)=self#send_chans in
      print_string "POCNET : send message";print_newline(); 
      if xmsg#get_src="" then
	xmsg#set_src ident;
      let msg=xmsg#to_xml#to_string in
	data_to_chan (msg) oc;
	flush oc;

	print_string "POCNET : wait for response message";print_newline(); 
	let recv=chan_to_data ic in
	let rxmsg=new xml_message in
	let rxn=new xml_node in
	  rxn#of_string recv;
	  rxmsg#from_xml rxn;
	  mph#message_check rxmsg;
	  print_string "POCNET : response message received";print_newline(); 
	  true
  method message_receive()=
    let (ic,oc)=self#recv_chans in
    let recv=chan_to_data ic in
      print_string "POCNET : receive message";print_newline(); 
    let xmsg=new xml_message in
    let xn=new xml_node in
      xn#of_string recv;
      xmsg#from_xml xn;
      message_resend xmsg;
      print_string "POCNET : message received";print_newline(); 

      if xmsg#get_type<>"response" then (
	let rxmsg=mph#message_parse xmsg in
	  print_string "POCNET : message parsed";print_newline(); 
	  rxmsg#get_values#set_id "values";
	  rxmsg#set_src ident;
	  let rmsg=rxmsg#to_xml#to_string in
	    print_string "POCNET : send response message";print_newline(); 
	    data_to_chan (rmsg) oc;
	    flush oc;
      )

  method run()=

    while true do 
      try  
      print_string "POCNET : wait for message";print_newline(); 
	self#message_receive(); 
      with 
	  End_of_file -> 
	    print_string "POCNET : WARNING : End of file";print_newline(); 
	    self#disconnect(); 
	| Sys_error e ->
	    print_string ("POCNET : WARNING : "^e);print_newline(); 
	    self#disconnect(); 
    done;


  method start()=
    let t=Thread.create (function()->self#run()) () in
      print_string ("Thread "^string_of_int (Thread.id t)^" launched (connection)");
      print_newline();

end;;

class network_client cp=
object(self)
  inherit network_connection (fun s->()) (fun m->()) cp

  initializer 
    mph#handler_add "ident" (new ident_message_handler (Some (fun()->self#get_port)) None None (Some self#set_ident));

  method connect addr p=
    let host=(Unix.gethostbyname addr) in 
      print_string ("POCNET_CLIENT: Connecting to "^host.Unix.h_name);print_newline();

      let clientaddr=(host.Unix.h_addr_list.(0)) in
	Unix.connect recv_sock (Unix.ADDR_INET(clientaddr,p)); 
	recv_chans<-Some (Unix.in_channel_of_descr recv_sock,Unix.out_channel_of_descr recv_sock); 

	let my_address=get_my_addr() in
	let rec recbind()=
	  (try 
	     Unix.bind send_sock (Unix.ADDR_INET(my_address,port)) ;
	   with Unix.Unix_error(e,f,v) -> self#set_port (port+1);recbind()) in
	  recbind();
	self#message_receive();
      

	  Unix.listen send_sock 1;

	  print_string ("POCNET_CLIENT: wait for server connection");
	  print_newline();
	  let (sd,sa)=Unix.accept send_sock in
	    send_chans<-Some (Unix.in_channel_of_descr sd,Unix.out_channel_of_descr sd);
	    
	    print_string "POCNET_CLIENT: server connected";print_newline();

	    self#start(); 



end;;


class network_server_connection check_ident on_disconnect message_resend (sd,sa) cp=
object(self) 
  inherit network_connection on_disconnect message_resend cp

  initializer 
    mph#handler_add "ident" (new ident_message_handler None (Some self#set_port) (Some (fun()->self#get_ident)) None);
    mph#handler_add "test" (new test_message_handler);


  method connect()=
    send_chans<-Some (Unix.in_channel_of_descr sd,Unix.out_channel_of_descr sd);

    (match sa with
       | ADDR_INET (ia,p)->
	   self#set_ident (check_ident (string_of_inet_addr ia)); 
       | _ -> raise Network_error);
      
      self#message_send 
	(xml_message_of_string (
	   "<message type=\"ident\">
                        <values>
                         <val_string name=\"ident\" value=\""^self#get_ident^"\"/>
                        </values>
                       </message>
                      ")
	);

    let nsa=match sa with
      | ADDR_INET (ia,p)->
	  print_string ("POCNET_SERVER: try connect to "^string_of_inet_addr ia);
	  print_newline();
	  self#set_ident (check_ident (string_of_inet_addr ia)); 
	  ADDR_INET (ia,port)
      | _ -> raise Network_error in

      Unix.connect recv_sock nsa;
      recv_chans<-Some (Unix.in_channel_of_descr recv_sock,Unix.out_channel_of_descr recv_sock); 

end;;
