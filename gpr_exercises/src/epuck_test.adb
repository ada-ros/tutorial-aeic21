with RCL.Nodes;
with ROSIDL.Static.Tutorial_Aeic21.Geometry_Msgs.Messages.Twist;

use RCL;
use ROSIDL.Static.Tutorial_Aeic21;

procedure Epuck_Test is
   Node : Nodes.Node'Class := Nodes.Init ("epuck_test");
   Msg  : Geometry_Msgs.Messages.Twist.Handling.Message;
   package Pub is new Nodes.Typed_Publish
     (Geometry_Msgs.Messages.Twist.Handling,
      Node,
      "/cmd_vel");
begin
   Msg.Data.Linear.X  := 0.1;
   Msg.Data.Angular.Z := 0.5;

   Pub.Publish (Msg);

   Node.Spin (During => 5.0);

   Msg.Data.Linear.X  := 0.0;
   Msg.Data.Angular.Z := 0.0;

   Pub.Publish (Msg);

   Node.Spin (During => 1.0);
end Epuck_Test;
