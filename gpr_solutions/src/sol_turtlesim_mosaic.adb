with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics;

with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Tutorial_Aeic21.Geometry_Msgs.Messages.Twist;
with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;
use ROSIDL.Static.Tutorial_Aeic21;

procedure Sol_Turtlesim_Mosaic is

   use type ROSIDL.Types.Float64;

   Node : Nodes.Node'Class := Nodes.Init;

   package Pub is new Nodes.Typed_Publish
      (Handling => Geometry_Msgs.Messages.Twist.Handling,
       Node     => Node,
       Topic    => "/turtle1/cmd_vel");

   Msg_Move, Msg_Turn : Geometry_Msgs.Messages.Twist.Handling.Message;

begin
   if Argument_Count /= 1 then
      Logging.Warn ("Please give the angle in degrees to turn at each vertex");
      return;
   end if;

   --  Msgs are 0-filled by ROS2

   Msg_Move.Data.Linear.X := 2.0;

   Msg_Turn.Data.Angular.Z :=
     ROSIDL.Types.Float64'Value (Argument (1)) / 180.0 * Ada.Numerics.Pi;

   loop
      --  Let's alternate 1" of drawing and 1" of turning counterclockwise

      Pub.Publish (Msg_Move);
      Logging.Info ("Commanding movement...");
      delay 1.0;

      Pub.Publish (Msg_Turn);
      Logging.Info ("Commanding rotation in rad/s ="
                    & Duration (Msg_Turn.Data.Angular.Z)'Image);
      --  Convert to Duration just to get fixed point display
      delay 1.0;
   end loop;
end Sol_Turtlesim_Mosaic;
