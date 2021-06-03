pragma Warnings (Off); -- TODO: remove this pragma

--  This exercise implements a teleoperation node using the keyboard. We
--  need a publisher to emit velocity commands, and to check the keyboard
--  periodically. To make the experience more responsive, we use Get_Immediate,
--  which does not block the program waiting for input.
--  Check the TODOs to complete the program.

--  This exercise is essentially the same as epuck_commander.adb.

with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with ANSI;

with Common;

--  with RCL.Logging;
with RCL.Nodes;

--  TODO: import the interfaces from geometry_msgs in the CMakeLists.txt file
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
--  use ROSIDL.Static.Tutorial_Exercises;

use RCL;

procedure Turtlesim_Commander is

   use type Types.Float64;

   subtype Real is Types.Float64;

   type Fixed is delta 0.01 digits 10;

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: complete instantiation arguments
   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => ... .Handling,
   --      Node     => Node,
   --      Topic    => "/turtle1/cmd_vel");

   --  TODO: uncomment this message, in which we store the movement command
   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   --  These constants are used to incrementally modify the Twist command
   Speed_Delta : constant Real := 0.1;
   Turn_Delta  : constant Real := Ada.Numerics.Pi / 4.0;

   --------------
   -- Keypress --
   --------------

   procedure Keypress (Char : Character) is
   begin
   --  TODO: once the message Msg_Move exists, uncomment
      case Char is
         --  when 'a' | 'A' =>
         --     Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z + Turn_Delta;
         --  when 'd' | 'D' =>
         --     Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z - Turn_Delta;
         --  when 'w' | 'W' =>
         --     Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X + Speed_Delta;
         --  when 's' | 'S' =>
         --     Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X - Speed_Delta;
         --  when ' ' =>
         --     Msg_Move.Data.Linear.X := 0.0;
         --     Msg_Move.Data.Angular.Z := 0.0;
         when others =>
            null;
      end case;
   end Keypress;

begin
   loop
      --  TODO: complete the argument to Publish
      --  Pub.Publish ( ... );

      declare
         Char      : Character;
         Available : Boolean := False;
      begin
         --  TODO: Complete the call to Get_Immediate
         --  Ada.Text_IO.Get_Immediate ( ..., ... );

         if Available then
            Keypress (Char); -- Process the keypress
         else
            delay 0.1;
         end if;

         --  TODO: uncomment to print the current velocity command
         --  Put_Line (ANSI.Clear_Line
         --            & ANSI.Horizontal
         --            & ANSI.Up
         --            & ANSI.Clear_Line
         --            & "vx:"
         --            & Common.Emph (Fixed (Msg_Move.Data.Linear.X)'Image)
         --            & " "
         --            & "vz:"
         --            & Common.Emph (Fixed (Msg_Move.Data.Angular.Z)'Image));
      end;
   end loop;
end Turtlesim_Commander;
