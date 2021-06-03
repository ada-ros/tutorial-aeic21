pragma Warnings (Off); -- TODO: remove this pragma

--  This exercise is a node that publishes once per second a string, using
--  the std_msgs/String predefined message. This message has a single field,
--  "data", of type String. We need to create a node, a publisher, and
--  a message of the std_msgs/String type. The publisher must publish in
--  the /chatter topic for it to be interoperable with other ROS2 standard
--  examples, but it can be anything else for your experiments.

with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Rclada;
--  TODO: import the spec for the std_msgs/String type. This message is already
--  available as part of the Rclada imported interfaces:
--  with ROSIDL.Static.Rclada. ...

use RCL;
use ROSIDL.Static.Rclada;

procedure Publisher_Static is

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: declare a message of the appropriate type (an Ada RAII type)
   --  Msg : Std_Msgs.Messages.String.Handling. ...;

   --  TODO: complete the instantiation with the Handling package of the
   --  message being published.
   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => ...,
   --      Node     => Node,
   --      Topic => "/chatter");
   --  This generic automatically creates the subscription, so it is
   --  immediately usable.

begin
   for Count in Positive'Range loop

      --  TODO: fill the "data" field of the message with a string.
      --  Types.Set_String (Msg. ...,
      --                    "Hello from the Ada side" & Count'Image);

      --  TODO: uncomment once the message declaration is fixed
      --  Pub.Publish (Msg);

      --  TODO: uncomment once the message declaration is fixed
      --  Logging.Info ("I just said: " & Types.Get_String (Msg.Data.Data));

      delay 1.0;
   end loop;
end Publisher_Static;
