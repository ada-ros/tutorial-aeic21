pragma Warnings (Off); -- TODO: remove this pragma

--  This exercise implements a wandering mode. The robot goes in a straight
--  line until some time elapses or a nearby obstacle is detected. We need a
--  publisher to emit velocity commands to /cmd_vel, and a subscriber to the
--  range finder in /scan. Identify the type of these topics with the "ros2 topic"

with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random;

with Common;

with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;
with RCL.QoS;

--  TODO: import in CMakeLists.txt the geometry_msgs if not yet done in other exercises
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;

--  TODO: find type for the /scan topic and import in CMakeLists.txt the appropriate interfaces
--  with ROSIDL.Static.Tutorial_Exercises. ...
--  use ROSIDL.Static.Tutorial_Exercises;

use RCL;

procedure Epuck_Wander is

   Rnd : Ada.Numerics.Float_Random.Generator;

   Node : Nodes.Node'Class := Nodes.Init;

   -- TODO: pass the appropriate Handling package to complete the instance
   --  package Pub is new Nodes.Typed_Publish
   --     (Handling => ...,
   --      Node     => Node,
   --      Topic    => "/cmd_vel");

   --  TODO: uncomment once the package has been generated
   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   --  We use Turn_Start to track how much time we are in a turn
   Turn_Start : Time    := Clock;
   Turning    : Boolean := False;
   Turn_Span  : Duration;

   --  We use Turn_Side to turn clock- or counterclockwise
   Turn_Side  : Types.Float64 := 1.0;

   use type Types.Float32;
   use type Types.Float64;

   --  This is the distance under which an obstacle triggers turning
   Near_Distance : constant Types.Float32 := 0.15;

   ----------------
   -- Start_Turn --
   ----------------
   --  This procedure sets up the variables that track the turning motion
   procedure Start_Turn is
   begin
      Turning    := True;
      Turn_Start := Clock;

      --  Turn between 0.5 and 2.5 seconds
      Turn_Span := Duration (Ada.Numerics.Float_Random.Random (Rnd)
                             * 1.5 + 0.5);

      --  TODO: improve so turns aren't always counterclockwise
      Turn_Side := 1.0;
   end Start_Turn;

   --------------
   -- Read_ToF --
   --------------
   --  TODO: uncomment and complete the "..." bits
   --  procedure Read_ToF (Node : in out Nodes.Node'Class;
   --                      Msg  : ... .Range_Data.Message; -- TODO: complete parent packages
   --                      Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --  begin
   --     Logging.Info ("Got range:" & Common.Fixed (Msg. ...)'Image);
   --     --  TODO: identify and fill the record member providing the range data
   --
   --     --  Process a valid reading. If near an obstacle, rotate for a while,
   --     --  unless we are already rotating. Take care of not reacting to invalid
   --     --  data.
   --
   --     if Msg.Range_Data > Msg.Min_Range and then Msg.Range_Data < Msg.Max_Range
   --     then
   --        if Msg.Range_Data < Near_Distance and then not Turning then
   --           Logging.Warn ("HEADS UP!");
   --           Start_Turn;
   --        end if;
   --     end if;
   --  end Read_ToF;

   ----------------------
   -- Subscribe_To_ToF --
   ----------------------
   --  TODO: uncomment once Read_ToF is completed
   --  procedure Subscribe_To_ToF is
   --    new Nodes.Typed_Subscribe (Handling => Sensor_Msgs.Messages.Range_Data.Handling,
   --                               Callback => Read_ToF);

begin
   --  TODO: use the Sensor_Data profile defined in package RCL.QoS to subscribe
   --  Subscribe_To_ToF (Node, "/tof",
   --                    Subscriptions.Defaults.Using (QoS. ... ));
   Logging.Info ("Ready to see the world.");

   loop
      --  TODO: think about the meaning of this Spin call with duration 0.1
      Node.Spin (During => 0.1);

      if Turning then
         Turning := Clock - Turn_Start < Turn_Span; -- Should we stop turning?
         --  TODO: uncomment once Msg_Move exists
         --  Msg_Move.Data.Linear.X  := 0.0;
         --  Msg_Move.Data.Angular.Z := 1.57 * Turn_Side; -- rad/s
      else
         --  TODO: uncomment once Msg_Move exists
         --  Msg_Move.Data.Linear.X  := 1.0;
         --  Msg_Move.Data.Angular.Z := 0.0;

         --  Don't go straight for too long
         if Clock - Turn_Start > 6.0 then
            Start_Turn;
         end if;
      end if;

      --  TODO: use the publisher (line 34) to emit the velocity command
      -- ... Publish (Msg_Move);
   end loop;
end Epuck_Wander;
