open Net_server;;
open Net_message;;


at_exit (fun()->());
let serv=new network_server 25000 in
serv#set_update (fun() -> Thread.delay 1.0);
serv#run();
