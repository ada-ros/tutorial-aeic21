with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Rclada.Std_Srvs.Services.Trigger;
with ROSIDL.Typesupport;

use RCL;
use ROSIDL.Static.Rclada;

procedure Server_Example is

   Node : Nodes.Node'Class := Nodes.Init;

   -----------------------
   -- Answering_Machine --
   -----------------------

   procedure Answering_Machine
     (Node     : in out Nodes.Node'Class;
      Request  : Std_Srvs.Services.Trigger.Handling.Request_Raw_Message;
      Response : in out Std_Srvs.Services.Trigger.Handling.Response_Raw_Message)
   is
   begin
      null;
   end Answering_Machine;

   package Answering_Machine_Instance is new Nodes.Typed_Serve
      (Handling => Std_Srvs.Services.Trigger.Handling,
       Node     => Node,
       Name     => "/ada_service",
       Callback => Answering_Machine);

begin
   Node.Spin (During => Forever);
end Server_Example;
