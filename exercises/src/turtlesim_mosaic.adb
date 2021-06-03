pragma Warnings (Off); -- TODO: remove this pragma

--  The objective of this node is to move the turtle with a fixed sequence. In
--  this solution, an alternating straight line and rotation is applied, taking
--  the degrees to turn from the first argument in the command line.

--  This example requires importing the interfaces from the package
--  "geometry_msgs", as one of its messages is used to give velocity commands.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics;

with RCL.Logging;
with RCL.Nodes;

--  TODO: complete with te appropriate message used to publish to /cmd_vel
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages. ...;
--  use ROSIDL.Static.Tutorial_Exercises;

use RCL;

procedure Turtlesim_Mosaic is

   use type Types.Float64;

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: complete the instantiation of the publisher
   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => ...,
   --      Node     => Node,
   --      Topic    => "/turtle1/cmd_vel");

   --  Instead of reusing a single message, we have pre-filled messages for the
   --  straight and turning movements
   --  TODO: uncomment once the message spec imports are complete
   --  Msg_Move,
   --  Msg_Turn : Geometry_Msgs.Messages. ... .Handling.Message;

begin
   if Argument_Count /= 1 then
      Logging.Warn ("Please give the angle in degrees to turn at each vertex");
      return;
   end if;

   --  TODO: uncomment once the message declarations are available
   --  --  Msgs are 0-filled by ROS2
   --  Msg_Move.Data.Linear.X := 2.0;
   --  Msg_Turn.Data.Angular.Z :=
   --    Types.Float64'Value (Argument (1)) / 180.0 * Ada.Numerics.Pi;

   loop
      --  Let's alternate 1" of drawing and 1" of turning counterclockwise

      --  TODO: uncomment once the message declaration is available
      --  Pub.Publish (Msg_Move);
      Logging.Info ("Commanding movement...");
      delay 1.0;

      --  TODO: uncomment and comploteonce the message declaration is available
      --  Pub.Publish ( ... );
      --  Logging.Info ("Commanding rotation in rad/s ="
      --                & Duration (Msg_Turn.Data.Angular.Z)'Image);
      --  --  Convert to Duration just to get fixed point display

      delay 1.0;
   end loop;
end Turtlesim_Mosaic;
