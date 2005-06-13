open Net_conn;;
open Net_message;;
open Net_msg_handler;;

class network_client cp=
object(self)
  inherit network_connection (fun s->()) (fun m->()) cp as super

  initializer 
    mph#handler_add "ident" (new ident_message_handler (Some (fun()->self#get_port)) None None (Some self#set_ident));

  method message_send (xmsg:xml_message)=
    Thread.create (fun()->ignore(super#message_send(xmsg))) ();
    true
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

