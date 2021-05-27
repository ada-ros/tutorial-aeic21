with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Rclada.Std_Srvs.Services.Trigger;

use RCL;
use ROSIDL.Static.Rclada;

procedure Client_Async is

   Node : Nodes.Node := Nodes.Init;

   -----------------------
   -- Response_Listener --
   -----------------------

   procedure Response_Listener
     (Node : in out Nodes.Node'Class;
      Msg  :  Std_Srvs.Services.Trigger.Handling.Resp_Handling.Raw_Message)
   is
      pragma Unreferenced (Node);
   begin
      Logging.Info ("Ada gossip is: " & Types.Get_String (Msg.Message));
   end Response_Listener;

   procedure Caller is
     new Nodes.Typed_Client_Call_Proc
       (Handling => Std_Srvs.Services.Trigger.Handling,
        Callback => Response_Listener);

   Request : Std_Srvs.Services.Trigger.Handling.Request_Message;
   --  No need to initialize this message fields, as it is used just to trigger
   --  the service response.

begin
   --  Delaying with the node up may be necessary when the client calls are non-blocking.
   --  delay 5.0;

   Logging.Info ("Ada calling...");
   Caller (Node, "/ada_service",
           Request,
           Connect_Timeout => 0.0,  -- refers to service availability
           Timeout         => 0.0); -- refers to waiting for an answer
   Logging.Info ("Call sent.");

   Node.Spin (During => 5.0);
end Client_Async;
