--  with Ada.Calendar; use Ada.Calendar;
--  with Ada.Numerics;
--  with Ada.Text_IO; use Ada.Text_IO;
--
--  with ANSI;
--
--  with Common;

with RCL.Logging;
with RCL.Nodes;

--  with ROSIDL.Static.Tutorial_Aeic21.Geometry_Msgs.Messages.Twist;
with ROSIDL.Static.Tutorial_Aeic21.Sensor_Msgs.Messages.Laserscan;

use RCL;
use ROSIDL.Static.Tutorial_Aeic21;

procedure Turtlebot_Wander is

   --  use type Types.Float64;
   --
   --  subtype Real is Types.Float64;

   Node : Nodes.Node'Class := Nodes.Init;

   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => Geometry_Msgs.Messages.Twist.Handling,
   --      Node     => Node,
   --      Topic    => "/turtle1/cmd_vel");

   ----------------
   -- Read_Laser --
   ----------------

   procedure Read_Laser (Node : in out Nodes.Node'Class;
                         Msg  : Sensor_Msgs.Messages.Laserscan.Message;
                         Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Msg, Info);
   begin
      Logging.Info ("Got lazer");
   end Read_Laser;

   procedure Subscribe_To_Laser is
     new Nodes.Typed_Subscribe (Handling => Sensor_Msgs.Messages.Laserscan.Handling,
                                Callback => Read_Laser);

   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

begin
   Subscribe_To_Laser (Node, "/scan");
   Logging.Info ("Ready to see the world.");

   loop
      Node.Spin (During => Forever);
      --  Pub.Publish (Msg_Move);
   end loop;
end Turtlebot_Wander;
