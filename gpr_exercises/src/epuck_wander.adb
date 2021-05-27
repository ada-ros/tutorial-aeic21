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

with ROSIDL.Static.Tutorial_Aeic21.Geometry_Msgs.Messages.Twist;
with ROSIDL.Static.Tutorial_Aeic21.Sensor_Msgs.Messages.Range_Data;

use RCL;
use ROSIDL.Static.Tutorial_Aeic21;

procedure Epuck_Wander is

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

   Near_Distance : constant Types.Float32 := 0.15;

   ----------------
   -- Start_Turn --
   ----------------

   procedure Start_Turn is
   begin
      Turning := True;
      Turn_Start := Clock;

      --  Turn between 0.5 and 2.5 seconds
      Turn_Span := Duration (Ada.Numerics.Float_Random.Random (Rnd)
                             * 1.5 + 0.5);

      Turn_Side := (if Ada.Numerics.Float_Random.Random (Rnd) > 0.5
                    then 1.0
                    else -1.0);
   end Start_Turn;

   --------------
   -- Read_ToF --
   --------------

   procedure Read_ToF (Node : in out Nodes.Node'Class;
                       Msg  : Sensor_Msgs.Messages.Range_Data.Message;
                       Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
   begin
      Logging.Info ("Got range:" & Common.Fixed (Msg.Range_Data)'Image);

      --  Process a valid reading. If near an obstacle, rotate for a while,
      --  unless we are already rotating.

      if Msg.Range_Data > Msg.Min_Range and then Msg.Range_Data < Msg.Max_Range
      then
         if Msg.Range_Data < Near_Distance and then not Turning then
            Logging.Warn ("HEADS UP!");
            Start_Turn;
         end if;
      end if;
   end Read_ToF;

   procedure Subscribe_To_ToF is
     new Nodes.Typed_Subscribe (Handling => Sensor_Msgs.Messages.Range_Data.Handling,
                                Callback => Read_ToF);

begin
   Subscribe_To_ToF (Node, "/tof",
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
         Msg_Move.Data.Linear.X  := 1.0;
         Msg_Move.Data.Angular.Z := 0.0;

         --  Don't go straight for too long
         if Clock - Turn_Start > 6.0 then
            Start_Turn;
         end if;
      end if;

      Pub.Publish (Msg_Move);
   end loop;
end Epuck_Wander;
