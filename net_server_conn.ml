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

open Net_conn;;
open Net_message;;
open Net_msg_handler;;

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
	   "<message type=\"ident\" src=\"server\" dst=\""^self#get_ident^"\">
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
