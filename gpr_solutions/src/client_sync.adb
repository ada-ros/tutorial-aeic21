with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Rclada.Std_Srvs.Services.Trigger;

use RCL;
use ROSIDL.Static.Rclada;

procedure Client_Sync is

   Node : Nodes.Node := Nodes.Init;

   Request : Std_Srvs.Services.Trigger.Handling.Request_Message;
   --  No need to initialize this message fields, as it is used just to trigger
   --  the service response.

   function Caller is new Nodes.Typed_Client_Call_Func
     (Std_Srvs.Services.Trigger.Handling);

begin
   --  Delaying with the node up may be necessary when the client calls are non-blocking.
   --  delay 5.0;

   Logging.Info ("Ada calling...");

   declare
      Response : constant Std_Srvs.Services.Trigger.Handling.Resp_Handling.Shared_Message :=
                   Caller (Node, "/ada_service",
                           Request,
                           Connect_Timeout => 5.0,
                           Timeout         => 10.0);
   begin
      Logging.Info ("Gossip is that... " & Types.Get_String (Response.Message));
   end;

end Client_Sync;
