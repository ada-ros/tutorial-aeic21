pragma Warnings (Off); -- TODO: remove this pragma

--  The objective is to call a service (the one created in previous exercises,
--  for example) with a known message type, with the function profile that
--  returns the message without needing a callback

--  This requires: a Node, an instantiation of the function.

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

   ------------
   -- Caller --
   ------------

   --  function Caller is
   --    new Nodes.Typed_Client_Call_Func (...);
   --  TODO: Complete the argument needed to instantiate the generic

begin
   Logging.Info ("Ada calling...");

   declare
      --  TODO: Uncomment the call using the instantiated caller
      --  Response : constant Std_Srvs.Services.Trigger.Handling.Resp_Handling.Shared_Message :=
      --               Caller (Node, "/ada_service",
      --                       Request,
      --                       Connect_Timeout => 5.0,
      --                       Timeout         => 10.0);
   begin
      --  TODO: extract the string from the message and print it
      --  Logging.Info ("Gossip is that... "
      --                & ...);
      null;
   end;


end Client_Sync;
