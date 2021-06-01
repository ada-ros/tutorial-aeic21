pragma Warnings (Off);

--  This exercise implements a teleoperation node using the keyboard. We
--  need a publisher to emit velocity commands, and to check the keyboard
--  periodically. To make the experience more responsive, we use Get_Immediate,
--  which does not block the program waiting for input.
--  Check the TODOs to complete the program.

with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with ANSI;

with Common;

--  with RCL.Logging;
with RCL.Nodes;

--  TODO: import the interfaces from geometry_msgs in the CMakeLists.txt file
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
--  use ROSIDL.Static.Tutorial_Exercises;

with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;

procedure Epuck_Commander is

   use type Types.Float64;

   subtype Real is Types.Float64;

   type Fixed is delta 0.01 digits 10;

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: complete the instantiation with the Handling package of the Twist message.
   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => ...,
   --      Node     => Node,
   --      Topic    => "/cmd_vel");

   --  TODO: uncomment this message, in which we store the movement command
   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   --  These constants are used to incrementally modify the Twist command
   Speed_Delta : constant Real := 0.1;
   Turn_Delta  : constant Real := Ada.Numerics.Pi / 4.0;

   --------------
   -- Keypress --
   --------------

   procedure Keypress (Char : Character) is null;
   --  TODO: once the message Msg_Move exists, uncomment
   --  begin
   --     case Char is
   --        when 'a' | 'A' =>
   --           Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z + Turn_Delta;
   --        when 'd' | 'D' =>
   --           Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z - Turn_Delta;
   --        when 'w' | 'W' =>
   --           Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X + Speed_Delta;
   --        when 's' | 'S' =>
   --           Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X - Speed_Delta;
   --        when ' ' =>
   --           TODO: implement a hard stop when space is pressed (set speeds to 0)
   --        when others =>
   --           null;
   --     end case;
   --  end Keypress;

begin
   loop

      --  TODO: use the instantiated Pub to send the velocity command (line 39)
      --  ... (Msg_Move);

      declare
         Char      : Character;
         Available : Boolean := False;
      begin
         Ada.Text_IO.Get_Immediate (Char, Available);

         if Available then
            Keypress (Char); -- Process the keypress
         else
            delay 0.1;
         end if;

         --  TODO: uncomment so the current vel command is printed
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
end Epuck_Commander;
