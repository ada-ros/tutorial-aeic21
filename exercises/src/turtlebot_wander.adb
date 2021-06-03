pragma Warnings (Off); -- TODO: remove this pragma

--  This exercise implements a wandering mode. The robot goes in a straight
--  line until some time elapses or a nearby obstacle is detected. We need a
--  publisher to emit velocity commands to /cmd_vel, and a subscriber to the
--  range finder in /scan. Identify the type of these topics with the "ros2 topic"

--  Although similar to the epuck_wander.adb exercise, the larger size of the
--  turtlebot and the richer information provider by the laser rangefinder make
--  for some important differences.

--  Also, given the number of previous exercises already available, code has
--  been removed more aggressively in this skeleton.

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

procedure Turtlebot_Wander is

   Rnd : Ada.Numerics.Float_Random.Generator;

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: identify the topic to command the turtlebot, complete instantiation
   --  package Pub is new Nodes.Typed_Publish
   --    (Handling => ...,
   --     Node     => Node,
   --     Topic    => ...);

   --  TODO: uncomment once the package has been generated
   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;

   Turn_Start : Time    := Clock;
   Turning    : Boolean := False;
   Turn_Span  : Duration;

   --  We use Turn_Side to turn clock- or counterclockwise
   Turn_Side  : Types.Float64 := 1.0;

   use type Types.Float32;
   use type Types.Float64;

   Near_Distance : constant Types.Float32 := 0.2;

   ----------------
   -- Start_Turn --
   ----------------
   --  This procedure sets up the variables that track the turning motion
   procedure Start_Turn is
   begin
      Turning := True;
      Turn_Start := Clock;

      --  Turn between 0.5 and 2.5 seconds
      Turn_Span := Duration (Ada.Numerics.Float_Random.Random (Rnd)
                             * 2.0 + 0.5);

      --  TODO: improve so turns aren't always counterclockwise
      Turn_Side := 1.0;
   end Start_Turn;

   ----------------
   -- Read_Laser --
   ----------------
   --  TODO: uncomment and complete. The simple approach proposed here is to
   --  find the closes reading in an angular window in from of the robot (see
   --  Angle_Window declaration). If this closest reading is too close (see
   --  Near_Distance), stop advancing and start turning.
   --  procedure Read_Laser (Node : in out Nodes.Node'Class;
   --                        Msg  : Sensor_Msgs.Messages.Laserscan.Message;
   --                        Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --
   --     --  TODO: use fields Msg.Range_Min and Msg.Range_Max to ensure a reading is valid
   --     --  function In_Range (Dist : Types.Float32) return Boolean
   --     --  is (...);
   --
   --     --  Used to store the closest reading in front of the robot
   --     Closest : Types.Float32 := Types.Float32'Last;
   --
   --     --  Since the Laserscan message type only provides raw readings in an
   --     --  array, we need to manually compute the angle of each reading. Note
   --     --  that a field in the Msg tells us the starting angle of the first
   --     --  reading. TODO: assign the appropriate initial value (the angle of
   --     --  the first reading).
   --     --  Angle   : Types.Float32 := Msg.Angle_ ... ;
   --
   --     Angle_Window : constant := 3.14159 / 6.0;
   --     --  We only consider readings in -Window .. Window angle
   --
   --  begin
   --     --  NOTE: turtlebot readings may be NaNs, that cause Ada to raise. Use
   --     --  'Valid to check and avoid those.
   --
   --     --  Loop over the laser readings
   --     for I in 1 .. Msg.Ranges.Size loop
   --        --  TODO: complete if condition (whether we should use this reading)
   --        if ...
   --          --  is the reading a valid value? is it in the proper range min .. max?
   --        then
   --           --  Store new closer reading
   --           Closest := Types.Float32'Min (Closest, Msg.Ranges.Data (I));
   --        end if;
   --
   --        --  TODO: Update the angle for the next reading. A field in
   --        --  the Laserscan message tells us this parameter which is
   --        --  hardware-dependent.
   --        Angle := @ + Msg.Angle_ ...;
   --     end loop;
   --
   --     Logging.Info ("Got range:" & Common.Fixed (Closest)'Image);
   --
   --     --  If nearing an obstacle, rotate for a while, unless we are already
   --     --  rotating.
   --
   --     if Closest < Near_Distance and then not Turning then
   --        Logging.Warn ("BRACE!");
   --        Start_Turn;
   --     end if;
   --  end Read_Laser;

   ------------------------
   -- Subscribe_To_Laser --
   ------------------------
   --  TODO: complete instantiation to have a subscriber for the Laserscan topic
   --  procedure Subscribe_To_Laser is
   --    new Nodes.Typed_Subscribe (Handling => ...,
   --                               Callback => Read_Laser);

begin

   --  TODO: complete arguments, REMEMBER to use Sensor_Data QoS profile (in RCL.QoS).
   --  Subscribe_To_Laser
   --    (Node    => Node,
   --     Topic   => ..., -- topic where laser is published
   --     Options => ...);

   Logging.Info ("Ready to see the world.");

   loop
      Node.Spin (During => 0.1);
      --  Update at 10Hz, for example

      if Turning then
         Turning := Clock - Turn_Start < Turn_Span;
         --  TODO: uncomment once Msg_Move exists
         --  Msg_Move.Data.Linear.X  := 0.0;
         --  Msg_Move.Data.Angular.Z := 1.57 * Turn_Side; -- rad/s
      else
         --  TODO: uncomment once Msg_Move exists
         --  Msg_Move.Data.Linear.X  := 2.0;
         --  Msg_Move.Data.Angular.Z := 0.0;

         --  Don't go straight for too long
         if Clock - Turn_Start > 10.0 then
            Start_Turn;
         end if;
      end if;

      --  TODO: Use the publisher to emit the velocity command stored in Msg_Move
      --  Pub. ... ( ... );
   end loop;
end Turtlebot_Wander;
