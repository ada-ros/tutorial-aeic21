with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Dynamic;
with ROSIDL.Static.Rclada.Std_Msgs.Messages.String;
with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;
use ROSIDL.Static.Rclada;

procedure Sol_Publisher_Static is

   Node : Nodes.Node'Class := Nodes.Init;

   Msg : Std_Msgs.Messages.String.Handling.Message;
   --  Note how the message is already typed

   package Pub is new Nodes.Typed_Publish
      (Handling => Std_Msgs.Messages.String.Handling,
       Node     => Node,
       Topic => "/chatter");
   --  This generic automatically creates the subscription

begin
   for Count in Positive'Range loop
      --  To use a typed message as if it were untyped, we can do:
      --  Msg.Dynamic.Field ("data").Set_String ("XWZ");

      --  This is the typed way of doing it:
      ROSIDL.Types.Set_String (Msg.Data.Data,
                               "Hello from the Ada side" & Count'Image);

      Pub.Publish (Msg);
      Logging.Info ("I just said: "
                    & ROSIDL.Types.Get_String (Msg.Data.Data));

      --  We don't actually need to spin on a node just to publish:
      --  Node.Spin (During => 1.0);

      delay 1.0;
   end loop;
end Sol_Publisher_Static;
