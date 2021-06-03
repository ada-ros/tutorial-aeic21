pragma Warnings (Off); -- TODO: remove this pragma

--  In this exercise we must loop doing the following:

--  1) Wait for a goal to be published in the appropriate topic.

--  2) Monitor the odometry topic, to know our current location.

--  3) Once we have a goal, start using the laser readings to steer to this
--  goal. It is simpler to always be reading the laser, and only use it when
--  we have a goal. NOTE that the laser topic requires Sensor_Data QoS profile.

--  4) Publish the stop or steering command to the velocity command topic

with Common;

with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;
with RCL.QoS;

with ROSIDL.Static.Tutorial_Common.Sensor_Msgs.Messages.Laserscan;
use ROSIDL.Static.Tutorial_Common;

--  TODO: import in CMakeLists.txt and here the necessary message types
--  use ROSIDL.Static.Tutorial_Exercises;

use RCL;

--  TODO: peruse the specification of the VFH package (in tutorial_common), in
--  particular the profile of the Steer function.
with VFH;

procedure Turtlebot_VFH is

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: create the publisher for the velocity commands
   --  package Pub is new Nodes.Typed_Publish
   --    (Handling => ...,
   --     Node     => Node,
   --     Topic    => ...);

   --  TODO: declare the messages to publish movement commands
   --  Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;
   --  Msg_Stop : Geometry_Msgs.Messages.Twist.Handling.Message;

   --  Goal, and flag to know we have one
   Have_Goal : Boolean := False;
   Goal      : VFH.Pose2D;

   --  Odometry pose, and flag to know it is already valid
   Have_Pose : Boolean := False;
   Odom_Pose : VFH.Pose2D;

   ---------------
   -- Read_Goal --
   ---------------
   --  TODO: complete the parameters and uncomment
   --  procedure Read_Goal (Node : in out Nodes.Node'Class;
   --                       Msg  : ... ;
   --                       Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --  begin
   --     Have_Goal := True;
   --     Goal.X     := Msg.Pose.Position.X;
   --     Goal.Y     := Msg.Pose.Position.Y;
   --     Goal.Angle := 0.0;
   --     Logging.Warn ("BY YOUR COMMAND: "
   --                   & Common.Fixed (Goal.X)'image & " "
   --                   & Common.Fixed (Goal.Y)'image & " "
   --                   & Common.Fixed (Goal.Angle)'Image);
   --  end Read_Goal;

   ----------------
   -- Read_Laser --
   ----------------
   --  TODO: uncomment and complete any extra TODOs in the procedure body
   --  procedure Read_Laser (Node : in out Nodes.Node'Class;
   --                        Msg  : Sensor_Msgs.Messages.Laserscan.Message;
   --                        Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --     use Common;
   --  begin
   --     if Have_Goal and then Have_Pose then
   --        declare
   --           --  Ask the VFH algorithm for an appropriate steering command
   --           --  towards the goal, while avoiding obstacles in the laser scan.
   --           --  TODO: complete the arguments.
   --           --  Cmd_Vel : constant VFH.Velocity2D := VFH.Steer
   --           --    (Odom   => ...,
   --           --     Goal   => ...,
   --           --     Scan   => ...);
   --        begin
   --           Logging.Info ("VFH command: "
   --                         & Fixed (Cmd_Vel.Linear)'Image & " "
   --                         & Fixed (Cmd_Vel.Angular)'Image);
   --
   --           --  TODO: copy the steering command (type VFH.Velocity2D) to the
   --           --  message (type Twist) where we will publish it.
   --           --  Msg_Move.Data.Linear.X  := ... ;
   --           --  Msg_Move.Data.Angular.Z := ... ;
   --
   --           Pub.Publish (Msg_Move);
   --        end;
   --
   --        Logging.Info
   --          ("Pose: "
   --           & Fixed (Odom_Pose.X)'Image & " "
   --           & Fixed (Odom_Pose.Y)'Image & " "
   --           & Fixed (Odom_Pose.Angle)'Image & " "
   --           & " Distance:" & Fixed (VFH.Distance (Odom_Pose, Goal))'Image);
   --     else
   --
   --        --  TODO : publish a message to stop the robot
   --        null;
   --
   --     end if;
   --  end Read_Laser;

   ---------------
   -- Read_Odom --
   ---------------
   --  TODO: complete arguments and body
   --  procedure Read_Odom (Node : in out Nodes.Node'Class;
   --                       Msg  : ... ;
   --                       Info : ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --  begin
   --     Have_Pose   := True;
   --
   --     --  TODO: store in Odom_Pose the pose received in Msg. Note that we
   --     --  receive a quaternion for rotation, that must converted to a yaw
   --     --  (e.g. with VFH.Quad_To_Euler).
   --
   --     Odom_Pose.X := ... ;
   --     Odom_Pose.Y := .. . ;
   --     Odom_Pose.Angle := VFH.Quat_To_Euler ( ... );
   --  end Read_Odom;

   -----------------------
   -- Subscribe_To_Goal --
   -----------------------
   --  TODO: complete and uncomment
   --  procedure Subscribe_To_Goal is
   --    new Nodes.Typed_Subscribe (Handling => ... ,
   --                               Callback => Read_Goal);

   ------------------------
   -- Subscribe_To_Laser --
   ------------------------
   --  TODO: complete and uncomment
   --  procedure Subscribe_To_Laser is
   --    new Nodes.Typed_Subscribe (Handling => ... ,
   --                               Callback => Read_Laser);

   -----------------------
   -- Subscribe_To_Odom --
   -----------------------
   --  TODO: complete and uncomment
   --  procedure Subscribe_To_Odom is
   --    new Nodes.Typed_Subscribe (Handling => ... ,
   --                               Callback => Read_Odom);

begin
   -- TODO: use the Subscribe_To_* procedures to subscribe to the /scan, /odom
   -- and /goal_pose topics. REMEMBER to use QoS.Profiles.Sensor_Data for the
   -- laser subscription options. (Find defaults in RCL.Subscriptions).
   -- Subscribe_To_Goal ... ;
   -- Subscribe_To_Laser ... ;
   -- Subscribe_To_Odom ... ;

   Logging.Info ("Ready to see the world.");

   loop
      Node.Spin (During => Forever);
   end loop;
end Turtlebot_VFH;
