with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with ANSI;

with Common;

--  with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Static.Tutorial_Aeic21.Geometry_Msgs.Messages.Twist;
with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;
use ROSIDL.Static.Tutorial_Aeic21;

procedure Sol_Epuck_Commander is

   use type ROSIDL.Types.Float64;

   subtype Real is ROSIDL.Types.Float64;

   type Fixed is delta 0.01 digits 10;

   Node : Nodes.Node'Class := Nodes.Init;

   package Pub is new Nodes.Typed_Publish
      (Handling => Geometry_Msgs.Messages.Twist.Handling,
       Node     => Node,
       Topic    => "/cmd_vel");

   Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   Speed_Delta : constant Real := 0.1;
   Turn_Delta  : constant Real := Ada.Numerics.Pi / 4.0;

   --------------
   -- Keypress --
   --------------

   procedure Keypress (Char : Character) is
   begin
      case Char is
         when 'a' | 'A' =>
            Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z + Turn_Delta;
         when 'd' | 'D' =>
            Msg_Move.Data.Angular.Z := Msg_Move.Data.Angular.Z - Turn_Delta;
         when 'w' | 'W' =>
            Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X + Speed_Delta;
         when 's' | 'S' =>
            Msg_Move.Data.Linear.X := Msg_Move.Data.Linear.X - Speed_Delta;
         when ' ' =>
            Msg_Move.Data.Linear.X := 0.0;
            Msg_Move.Data.Angular.Z := 0.0;
         when others =>
            null;
      end case;
   end Keypress;

begin
   loop
      Pub.Publish (Msg_Move);

      declare
         Char      : Character;
         Available : Boolean := False;
      begin
         Ada.Text_IO.Get_Immediate (Char, Available);

         if Available then
            Keypress (Char);
         else
            delay 0.1;
         end if;

         Put_Line (ANSI.Clear_Line
                   & ANSI.Horizontal
                   & ANSI.Up
                   & ANSI.Clear_Line
                   & "vx:"
                   & Common.Emph (Fixed (Msg_Move.Data.Linear.X)'Image)
                   & " "
                   & "vz:"
                   & Common.Emph (Fixed (Msg_Move.Data.Angular.Z)'Image));
      end;
   end loop;
end Sol_Epuck_Commander;
