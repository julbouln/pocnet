open Value_xml;;
open Net_conn;;
open Net_message;;


let cli=new network_client 25001 in
cli#get_mph#handler_add "test" (new test_message_handler);

cli#connect "localhost" 25000;
while true do
Thread.delay 1.0;
let msg="
<message type=\"test\" dst=\"+\">
 <values>
  <val_string name=\"test\" value=\"blabla\"/>
 </values>
</message>
" in
let xn=new xml_node in
  xn#of_string msg;
let xmsg=new xml_message in
  xmsg#from_xml xn;
cli#message_send xmsg;
done
