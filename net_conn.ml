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
exception Network_recv_thread_not_launched

class virtual network_object=
object(self)
  method virtual message_send : xml_message -> bool
end;;



class network_connection on_disconnect (message_resend:xml_message->unit) (iport:int)=
object(self)
  inherit network_object

  val mutable recv_thread=None
  method get_recv_thread=
    match recv_thread with
      | Some t->t
      | None -> raise Network_recv_thread_not_launched

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


  method force_disconnect sock=
    let r=ref false in
    let cn=ref 0 in
      while !r=false do
	(try 
	   Unix.shutdown sock Unix.SHUTDOWN_ALL;
	   cn:= !cn+1;
	   if !cn=10 then r:=true;
	 with
	     Unix.Unix_error(Unix.ENOTCONN,_,_) -> r:=true);
      done

  method disconnect_send()=
    let r=ref false in
      while !r=false do
	(try 
	   Unix.shutdown send_sock Unix.SHUTDOWN_SEND
	 with
	     Unix.Unix_error(Unix.ENOTCONN,_,_) -> r:=true);
      done



  method disconnect()=
    print_string "POCNET : disconnect";print_newline();
    on_disconnect ident;
    (try 
    self#force_disconnect send_sock; 
(*    Unix.close send_sock; *)
(*    self#disconnect_send(); *)
(*       Unix.shutdown send_sock Unix.SHUTDOWN_ALL; *)
       print_string "POCNET : send sock closed";print_newline();
    send_sock<-Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0; 
    self#force_disconnect recv_sock; 
(*       Unix.close recv_sock; *)
(*       Unix.shutdown recv_sock Unix.SHUTDOWN_ALL; *)
       print_string "POCNET : receive sock closed";print_newline(); 
    recv_sock<-Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0; 
(*    self#message_send (new xml_message); *)



    send_chans<-None;
    recv_chans<-None;

    
(*
       Unix.shutdown recv_sock Unix.SHUTDOWN_ALL;
       Unix.shutdown send_sock Unix.SHUTDOWN_ALL;
*)
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
(*	   Thread.kill self#get_recv_thread; *)
(*	   Unix.close recv_sock; *)
	   self#disconnect();false
       | Sys_error e ->
	   print_string ("POCNET : WARNING : "^e^" (send message)");print_newline(); 
(*	   Thread.kill self#get_recv_thread; *)

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
    recv_thread<-Some (Thread.create (function()->self#run()) ());
    print_string ("Thread "^string_of_int (Thread.id self#get_recv_thread)^" launched (connection)");
    print_newline();

end;;

