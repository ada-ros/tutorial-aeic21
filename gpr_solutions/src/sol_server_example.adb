with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Rclada.Std_Srvs.Services.Trigger;

use RCL;
use ROSIDL.Static.Rclada;

procedure Sol_Server_Example is

   Node : Nodes.Node'Class := Nodes.Init;

   -----------------------
   -- Answering_Machine --
   -----------------------

   procedure Answering_Machine
     (Node     : in out Nodes.Node'Class;
      Request  :        Std_Srvs.Services.Trigger.Handling.Request_Raw_Message;
      Response : in out Std_Srvs.Services.Trigger.Handling.Response_Raw_Message)
   is
      pragma Unreferenced (Node, Request);
   begin
      Logging.Info ("Incoming call!");
      Response.Success := Types.Bool (True);
      Types.Set_String (Response.Message, "Ada rocks!");
   end Answering_Machine;

   package Answering_Machine_Instance is new Nodes.Typed_Serve
      (Handling => Std_Srvs.Services.Trigger.Handling,
       Node     => Node,
       Name     => "/ada_service",
       Callback => Answering_Machine);

   pragma Unreferenced (Answering_Machine_Instance);

begin
   Logging.Info ("Ready to answer");
   Node.Spin (During => Forever);
end Sol_Server_Example;
