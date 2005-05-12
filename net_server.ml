open Unix;;
open Net_message;;
open Net_conn;;

exception Client_not_found of string;;
exception Group_not_found of string;;

(** Network transport *)
class network_transport=
object(self)
  val mutable m=Mutex.create();
  val mutable clients=Hashtbl.create 2 

  (** add client to transport *)
  method client_add (c:string) (conn:network_server_connection)=
    Mutex.lock m;
    Hashtbl.add clients c conn;			
    Mutex.unlock m;
  (** get transported client *)
  method client_get c=
    try
      let cli=Mutex.lock m;Hashtbl.find clients c in 
	Mutex.unlock m;cli					
    with Not_found -> raise (Client_not_found c) 
	
  (** del transported client *)
  method client_del cl=
    try
      Mutex.lock m;
      Hashtbl.remove clients cl;
      Mutex.unlock m;
    with Not_found -> raise (Client_not_found cl) 

  (** rename tranported client *)
  method client_rename o n=
    let c=self#client_get o in
      self#client_del o; 
      self#client_add n c;
      c#set_ident n;

  method client_is i=
    Hashtbl.mem clients i;

  (** foreach transported client *)
  method foreach_client f=
    Mutex.lock m;
    Hashtbl.iter f clients;
    Mutex.unlock m;

  val mutable dest_check=(fun d->true)

  (** set the destination checker *)
  method set_dest_check f=dest_check<-f


  val mutable gm=Mutex.create();

  (** groups *)
  val mutable groups=Hashtbl.create 2

  (** add destinations group *)
  method group_add (k:string) (n:string list)=
    Mutex.lock gm;
    Hashtbl.add groups k n;
    Mutex.unlock gm;
      
  (** get destinations group *)
  method group_get (k:string)=
    try
      let g=Mutex.lock gm;Hashtbl.find groups k in Mutex.unlock gm;g
    with Not_found-> raise (Group_not_found k)

  (** add destination to group *)
  method group_add_dest k d=
    let cg=self#group_get k in      
      Mutex.lock gm;
      let ng=List.append cg [d] in
	Hashtbl.replace groups k ng;
	Mutex.unlock gm;
  (** del destination from group *)
  method group_del_dest k d=
    let cg=self#group_get k in
      Mutex.lock gm;
      let ng=List.filter (fun v->
			    v<>d ) cg in
	Hashtbl.replace groups k ng;
      Mutex.unlock gm;

	
  (** get clients in list *)
  method private get_clients()=
    let a=DynArray.create() in
      self#foreach_client 
	(fun k v->
		      DynArray.add a k
		   );
      DynArray.to_list a
	

  (** get clients without src *) 
  method private get_clients_without l src=
    
    let a=DynArray.create() in
      List.iter (fun v->
		   if v<> src then
		     DynArray.add a v;
	       ) l;
      DynArray.to_list a

  (** parse the dest arg to list *)
  method message_get_dest (msg:message)=
    match msg#get_dst with
      | "+" ->	 
	  self#get_clients_without (self#get_clients()) msg#get_src
      | "*" -> 
	  self#get_clients()
      | v when Str.first_chars v 1="#" ->
	  let r=(Str.string_after v 1) in
	    if (Str.last_chars r 1)="+" then
	      self#get_clients_without (self#group_get (Str.string_before r (String.length r - 1))) msg#get_src
	    else
	      self#group_get r
      | v -> 
	  try 
	    ExtString.String.nsplit v ",";
	  with Invalid_argument a -> [v]

  (** foreach dest *)	
  method message_foreach_dest msg f=
    let l=self#message_get_dest msg in
      List.iter f l;
      
  (** destination exist in message ? *)
  method private is_dest (dl:string list) (dest:string)=
    let r=ref false in
      List.iter (fun k->
		   if k=dest then r:=true
		) dl;
      !r  
	
  (** message must be send to dest *)
  method message_is_dest dest (msg) =
    self#is_dest (self#message_get_dest msg) dest


  (** send message to his dest *)
  method message_send_to_dest (msg:xml_message)=
    let l=self#message_get_dest (msg:>message) in
      List.iter (fun c->
		   if c<>"server" && dest_check c then (
		     let conn=self#client_get c in		     
		       conn#message_send msg;()
		   )
		) l;
(*
  method messages_list_send_to_dest (msg:xml_messages_list)=
    let l=self#message_get_dest (msg:>message) in
      List.iter (fun c->
		   if c<>"server" && dest_check c then (
		     let conn=self#client_get c in		     
		       conn#messages_list_send msg
		   )
		) l;

*)
 
end;;

exception Bad_client_ident;;

(** Network server class *)
class network_server p=
object(self)
  val mutable m=Mutex.create();
  val mutable mph=new message_parser_handler

  (** for compatibility *)
  method get_mm=mph
    
  (** get the parser handler *)
  method get_mph=mph

  val mutable trans=new network_transport
  method get_trans=trans
 
  (** parse xml message *)
  method message_parse xmsg=
    mph#message_parse xmsg;

(*  method messages_list_parse xmsgl=
    mph#messages_list_parse xmsgl;
*)

  method message_resend xmsg=
    trans#message_send_to_dest xmsg; 

  val port=p
  val mutable sock=Unix.socket PF_INET Unix.SOCK_STREAM 0 
  val mutable m=Mutex.create();
  
  val mutable connect=(fun (cl:string)->())
  method set_connect c=connect<-c

  val mutable on_update=(fun()->())
  method set_update u=on_update<-u

  val mutable on_update_sleep=(fun()->())
  method set_update_sleep u=on_update_sleep<-u

  initializer
  let my_address=get_my_addr() in
    Unix.bind sock (Unix.ADDR_INET(my_address,port)) ;
    Unix.listen sock 3;


  val mutable disconnect=(fun (s:string)->())
  method set_disconnect d=disconnect<-d
 
  method on_disconnect cl=
    disconnect cl;
    print_string ("POCNET_SERVER: "^cl^" disconnected.");print_newline();

    trans#client_del cl;
    
  method update()=
    while true do
      on_update();
      on_update_sleep(); 

    done;
(*
  method xmessages_list_send src xmsg=
    xmsg#set_src src;
    trans#messages_list_send_to_dest xmsg; 
*)

  method message_send src xmsg=
      xmsg#set_src src;
      trans#message_send_to_dest xmsg; 
(*      self#message_resend xmsg; *)


  method check_client_ident cn=
    let rec check n=
      if trans#client_is n=false then
	n
      else
	check (n^"'") in
      check cn


  method run()=
    print_string "POCNET_SERVER: started";print_newline(); 

    Thread.create(function()->self#update()) (); 

    while true do      
      let (sd,sa)=Unix.accept sock in
      let conn=new network_server_connection self#check_client_ident self#on_disconnect self#message_resend (sd,sa) 25001 in
(*	conn#set_mph mph; *)
	let i=ref 0 in
	  while !i<5 do
	    Thread.delay (2.);
	    (try 

	       conn#connect();
	       i:=10;
	       (* GET IDENT *)
(*      	       conn#message_receive();  *)
	       if(conn#get_ident<>"none") then (
		 trans#client_add conn#get_ident conn;      
		 
		 print_string ("POCNET_SERVER: "^conn#get_ident^" connected.");print_newline(); 
		 
		 conn#start();	

		 connect conn#get_ident;	  

	    )
	     with Unix.Unix_error (_,_,_)->
	       if !i<4 then (
		 print_string "POCNET_SERVER: connection error, retry ...";
		 print_newline();
		 i:= !i+1
	       ) else
		 (
		   print_string "POCNET_SERVER: connection error, abort ...";
		   print_newline();
		   i:= !i+1
		 )
	    );
	  done;	  

	


    done;

end;;
