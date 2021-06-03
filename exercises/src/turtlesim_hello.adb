pragma Warnings (Off); -- TODO: remove this pragma

--  In this exercise, we use the turtle to draw some concrete thing. Check the
--  solution in sol_turtlesim_hello.adb for an example. Here, everything is
--  working except the sequence of commands to give, that is empty.

with Ada.Calendar; use Ada.Calendar;

with RCL.Logging;
with RCL.Nodes;
with RCL.Utils;

--  TODO: import geometry_msgs interfaces in
--  CMakeLists.txt, and uncomment after a colcon build:
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
--  use ROSIDL.Static.Tutorial_Exercises;

procedure Turtlesim_Hello is

   use RCL;
   use all type Types.Float64;

   --  TODO: uncomment once the "with" is uncommented
   --  MsgDraw,
   --  MsgLin,
   --  MsgRot : Geometry_Msgs.Messages.Twist.Handling.Message;

   Node : Nodes.Node'Class := Nodes.Init   (Utils.Command_Name);

begin
   Logging.Info ("Turtlesim Hello starting...");

   declare
      --  TODO: uncomment once the package Handling is available
      --  package Pub is new Nodes.Typed_Publish
      --    (Handling => Geometry_Msgs.Messages.Twist.Handling,
      --     Node     => Node,
      --     Topic    => "/turtle1/cmd_vel");

      Next   : Time    := Clock;
      Rotate : Boolean := False;
      Count  : Natural := 0;

      ----------
      -- Draw --
      ----------

      procedure Draw is
         type Twist is record
            Vlin, Vang : Types.Float64;
            Period     : Duration; -- NOTE: unused in the current implementation
         end record;
         Twists : constant array (Positive range <>) of Twist :=
                    ((0.0, 0.0, 1.0), -- Warm-up so topics are discovered
                     (0.0, 0.0, 1.0), -- Warm-up so topics are discovered
                     --  TODO: add elements to this sequence to do your drawing
                     (0.0, 0.0, 0.0)
                    );
         Count : Natural := 0;
      begin
         for T of Twists loop
            Count := Count + 1;
            --  TODO: uncoment once the message declarations are available
            --  MsgDraw.Data.Linear.X  := T.Vlin;
            --  MsgDraw.Data.Angular.Z := T.Vang;
            --  Pub.Publish (MsgDraw);
            Logging.Info ("Sent default command" & Count'Img);
            delay T.Period;
         end loop;
      end Draw;

   begin
      Draw;

      --  Continue drawing the star

      --  TODO: uncomment once message declarations are available
      --  MsgLin.Data.Linear.X  := 3.0;
      --  MsgRot.Data.Angular.Z := 2.0;

      loop
         if Next < Clock then
            Count := Count + 1;

            --  TODO: uncomment once msg declarations are available
            --  if Rotate then
            --     Pub.Publish (MsgRot);
            --  else
            --     Pub.Publish (MsgLin);
            --  end if;

            Rotate := not Rotate;

            Next := Clock + 1.0;
            Logging.Info ("Sending..." & Count'Img);
         end if;
      end loop;
   end;
end Turtlesim_Hello;
