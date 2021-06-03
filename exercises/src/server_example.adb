pragma Warnings (Off); -- TODO: remove this pragma

with RCL.Logging;
with RCL.Nodes;

--  TODO: import the Trigger service of Std_Msgs, already generated in Rclada:
--  with ROSIDL.Static.Rclada. ...;

use RCL;

with ROSIDL.Static.Rclada;
use ROSIDL.Static.Rclada;

procedure Server_Example is

   Node : Nodes.Node'Class := Nodes.Init;

   -----------------------
   -- Answering_Machine --
   -----------------------
   --  TODO : Uncomment and complete once the "with"s have been fixed
   --  procedure Answering_Machine
   --    (Node     : in out Nodes.Node'Class;
   --     Request  :        Std_Srvs.Services.Trigger.Handling.Request_Raw_Message;
   --     Response : in out Std_Srvs.Services.Trigger.Handling.Response_Raw_Message)
   --  is
   --     pragma Unreferenced (Node, Request);
   --  begin
   --     Logging.Info ("Incoming call!");
   --
   --     --  TODO: set Response.Success and Response.Message to a meaningful value
   --  end Answering_Machine;

   --------------------------------
   -- Answering_Machine_Instance --
   --------------------------------

   --  TODO: uncomment once the Answering_Machine procedure is complete
   --  package Answering_Machine_Instance is new Nodes.Typed_Serve
   --     (Handling => Std_Srvs.Services.Trigger.Handling,
   --      Node     => Node,
   --      Name     => "/ada_service",
   --      Callback => Answering_Machine);
   --  pragma Unreferenced (Answering_Machine_Instance);

begin
   Logging.Info ("Ready to answer");
   Node.Spin (During => Forever);
end Server_Example;
