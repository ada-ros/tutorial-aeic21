with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;

with ROSIDL.Static.Rclada.Std_Msgs.Messages.String;
with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;
use ROSIDL.Static.Rclada;

procedure Subscriber_Static is

   Node : Nodes.Node'Class := Nodes.Init;

   --------------
   -- Callback --
   --------------

   procedure Callback (Node : in out Nodes.Node'Class;
                       Msg  :        Std_Msgs.Messages.String.Message;
                       Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
   begin
      Logging.Info ("Radio chatter: " & ROSIDL.Types.Get_String (Msg.Data));
   end Callback;

   package Subscriber is new Subscriptions.Typed
     (Handling => Std_Msgs.Messages.String.Handling,
      Callback => Callback);

begin
   Subscriber.Subscribe (Node, "/chatter");

   Node.Spin (During => Forever);
end Subscriber_Static;
