with Ada.Calendar; use Ada.Calendar;
--  with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
--  with Ada.Text_IO; use Ada.Text_IO;
--
--  with ANSI;
--
with Common;

with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;
with RCL.QoS;

with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
with ROSIDL.Static.Tutorial_Exercises.Sensor_Msgs.Messages.Laserscan;

use RCL;
use ROSIDL.Static.Tutorial_Exercises;

procedure Turtlebot_Wander is

   Rnd : Ada.Numerics.Float_Random.Generator;

   Node : Nodes.Node'Class := Nodes.Init;

   package Pub is new Nodes.Typed_Publish
     (Handling => Geometry_Msgs.Messages.Twist.Handling,
      Node     => Node,
      Topic    => "/cmd_vel");

   Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   Turn_Start : Time    := Clock;
   Turning    : Boolean := False;
   Turn_Span  : Duration;
   Turn_Side  : Types.Float64 := 1.0;

   use type Types.Float32;
   use type Types.Float64;

   Near_Distance : constant Types.Float32 := 0.2;

   ----------------
   -- Start_Turn --
   ----------------

   procedure Start_Turn is
   begin
      Turning := True;
      Turn_Start := Clock;

      --  Turn between 0.5 and 2.5 seconds
      Turn_Span := Duration (Ada.Numerics.Float_Random.Random (Rnd)
                             * 2.0 + 0.5);

      Turn_Side := (if Ada.Numerics.Float_Random.Random (Rnd) > 0.5
                    then 1.0
                    else -1.0);
   end Start_Turn;

   ----------------
   -- Read_Laser --
   ----------------

   procedure Read_Laser (Node : in out Nodes.Node'Class;
                         Msg  : Sensor_Msgs.Messages.Laserscan.Message;
                         Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);

      function In_Range (Dist : Types.Float32) return Boolean
      is (Dist > Msg.Range_Min and then Dist < Msg.Range_Max);

      Closest : Types.Float32 := Types.Float32'Last;
      Angle   : Types.Float32 := Msg.Angle_Min;

      Angle_Window : constant := 3.14159 / 6.0;
      --  We only consider readings in -Window .. Window angle

   begin
      for I in 1 .. Msg.Ranges.Size loop
         if Angle >= -Angle_Window and then Angle <= Angle_Window and then
           Msg.Ranges.Data (I)'Valid and then In_Range (Msg.Ranges.Data (I))
         then
            Closest := Types.Float32'Min (Closest, Msg.Ranges.Data (I));
         end if;

         Angle := @ + Msg.Angle_Increment;
      end loop;

      Logging.Info ("Got range:" & Common.Fixed (Closest)'Image);

      --  If nearing an obstacle, rotate for a while, unless we are already
      --  rotating.

      if Closest < Near_Distance and then not Turning then
         Logging.Warn ("BRACE!");
         Start_Turn;
      end if;
   end Read_Laser;

   procedure Subscribe_To_Laser is
     new Nodes.Typed_Subscribe (Handling => Sensor_Msgs.Messages.Laserscan.Handling,
                                Callback => Read_Laser);

begin
   Subscribe_To_Laser (Node, "/scan",
                     Subscriptions.Defaults.Using (QoS.Profiles.Sensor_Data));
   Logging.Info ("Ready to see the world.");

   loop
      Node.Spin (During => 0.1);

      --  Update at 10Hz, for example
      if Turning then
         Turning := Clock - Turn_Start < Turn_Span;
         Msg_Move.Data.Linear.X  := 0.0;
         Msg_Move.Data.Angular.Z := 1.57 * Turn_Side; -- rad/s
      else
         Msg_Move.Data.Linear.X  := 2.0;
         Msg_Move.Data.Angular.Z := 0.0;

         --  Don't go straight for too long
         if Clock - Turn_Start > 10.0 then
            Start_Turn;
         end if;
      end if;

      Pub.Publish (Msg_Move);
   end loop;
end Turtlebot_Wander;
