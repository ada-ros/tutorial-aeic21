with Ada.Calendar; use Ada.Calendar;

with RCL.Logging;
with RCL.Nodes;
with RCL.Utils;

with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
with ROSIDL.Types;

procedure Turtlesim_Hello is

   use RCL;
   use ROSIDL.Static.Tutorial_Exercises;
   use all type ROSIDL.Types.Float64;

   MsgDraw,
   MsgLin,
   MsgRot : Geometry_Msgs.Messages.Twist.Handling.Message;

   Node : Nodes.Node'Class := Nodes.Init   (Utils.Command_Name);

begin
   Logging.Info ("Turtlesim Hello starting...");

   declare
      package Pub is new Nodes.Typed_Publish
        (Handling => Geometry_Msgs.Messages.Twist.Handling,
         Node     => Node,
         Topic    => "/turtle1/cmd_vel");

      Next   : Time    := Clock;
      Rotate : Boolean := False;
      Count  : Natural := 0;

      ----------
      -- Draw --
      ----------

      procedure Draw is
         type Twist is record
            Vlin, Vang : ROSIDL.Types.Float64;
            Period     : Duration;
         end record;
         Twists : constant array (Positive range <>) of Twist :=
                    ((0.0, 0.0, 1.0),
                     (0.0, 0.0, 1.0), -- Warm-up
                     (0.0, 1.11, 1.0),
                     (2.24, 0.0, 1.0), -- Half A
                     (0.0, -2.22, 1.0),
                     (2.24, 0.0, 1.0), -- Half A
                     (0.0, 1.11, 1.0), (0.0, 1.57, 1.0),
                     (2.0, 0.0, 1.0),  -- Half D
                     (0.0, -1.57, 1.0),
                     (1.57, -1.57, 1.0), -- Half D
                     (1.57, -1.57, 1.0), -- Half D
                     (0.0, 3.1416, 1.0),
                     (1.0, 0.0, 1.0), -- Base
                     (0.0, 1.11, 1.0),
                     (2.24, 0.0, 1.0), -- Half A
                     (0.0, -2.22, 1.0),
                     (2.24, 0.0, 1.0), -- Half A
                     (0.0, -2.22, 1.0),
                     (6.66, 0.0, 1.0)); -- Bye
         Count : Natural := 0;
      begin
         for T of Twists loop
            Count := Count + 1;
            MsgDraw.Data.Linear.X  := T.Vlin;
            MsgDraw.Data.Angular.Z := T.Vang;
            Pub.Publish (MsgDraw);
            Logging.Info ("Sent default command" & Count'Img);
            delay T.Period;
         end loop;
      end Draw;

   begin
      Draw;

      --  Continue with the star

      MsgLin.Data.Linear.X  := 3.0;
      MsgRot.Data.Angular.Z := 2.0;

      loop
         if Next < Clock then
            Count := Count + 1;
            MsgRot.Data.Angular.X := ROSIDL.Types.Float64 (Count);
            --  This is not used for movement, but is informative when echoing
            --  the topic for inspection.

            if Rotate then
               Pub.Publish (MsgRot);
            else
               Pub.Publish (MsgLin);
            end if;

            Rotate := not Rotate;

            Next := Clock + 1.0;
            Logging.Info ("Sending..." & Count'Img);
         end if;
      end loop;
   end;
end Turtlesim_Hello;
