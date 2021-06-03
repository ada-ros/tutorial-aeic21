pragma Warnings (Off); -- TODO: remove this pragma

--  In this exercise we must loop doing the following:

--  1) Wait for a goal to be published in the appropriate topic.

--  2) Monitor the odometry topic, to know our current location.

--  3) Once we have a goal, start using the laser readings to steer to this
--  goal. It is simpler to always be reading the laser, and only use it when
--  we have a goal. NOTE that the laser topic requires Sensor_Data QoS profile.

--  NOTE
--  3B) The novelty in regard to turtlebot_vfh.adb is that now we want to
--  transform the raw laser readings into a point array in the odometry pose of
--  the robot. This is done in the Convert_To_Cloud function inside the laser
--  subscriber. If you already did the turtlebot_vfh.adb exercise, this is the
--  point to concentrate on.
--  END NOTE

--  4) Publish the stop or steering command to the velocity command topic

with Common;

with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;
with RCL.QoS;
with RCL.TF2;

--  TODO: import appropriate messages
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Posestamped;
--  with ROSIDL.Static.Tutorial_Exercises.Geometry_Msgs.Messages.Twist;
--  with ROSIDL.Static.Tutorial_Exercises.Nav_Msgs.Messages.Odometry;
with ROSIDL.Static.Tutorial_Common.Sensor_Msgs.Messages.Laserscan;

use RCL;
use ROSIDL.Static.Tutorial_Common;

with VFH;

procedure Turtlebot_VFH_TF2 is

   Node : Nodes.Node'Class := Nodes.Init;

   --  TODO: complete the publisher instantiation for velocity commands
   --  package Pub is new Nodes.Typed_Publish
   --    (Handling => ... ,
   --     Node     => Node,
   --     Topic    => ... );

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
   --                        Scan : Sensor_Msgs.Messages.Laserscan.Message;
   --                        Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --     use Common;
   --
   --     ----------------------
   --     -- Convert_To_Cloud --
   --     ----------------------
   --     --  TODO: Uncomment and COMPLETE ANY EMBEDDED TODOs
   --     function Convert_To_Cloud return TF2.Point_Array is
   --        Cloud      : TF2.Point_Array (1 .. Natural (Scan.Ranges.Size));
   --        Cloud_Last : Natural := Cloud'First - 1;
   --
   --        subtype Real is TF2.Real;
   --        use type Real;
   --
   --        --  We use this variable to keep track of the current reading angle in
   --        --  the loop below.
   --        Scan_Angle : Real := Real (Scan.Angle_Min);
   --
   --        --  Reference frames to use: from the laser one to the odometry one.
   --        --  TODO: assign the appropriate value, found in the header of the Scan message, field Frame_Id.
   --        From       : constant String := Types.Get_String ( ... );
   --        Into       : constant String := "odom";
   --     begin
   --        for Scan_I in 1 .. Scan.Ranges.Size loop
   --           declare
   --              use type Types.Float32;
   --              Reading     : Types.Float32 renames Scan.Ranges.Data (Scan_I);
   --
   --              --  We use this point to convert the laser polar reading into
   --              --  a cartesian 3D point. Then, we will transform this point
   --              --  to the odometry reference frame.
   --              Point : TF2.Point3D;
   --           begin
   --              if Reading'Valid and Then
   --                Reading > Scan.Range_Min and then Reading < Scan.Range_Max
   --              Then
   --                 Cloud_Last := @ + 1; -- One more cloud point to store
   --
   --                 --  Get the cartesian coordinates in the laser ref. frame
   --                 Point :=
   --                   (X => TF2.To_Point ((Bearing => TF2.Radians (Scan_Angle),
   --                                        Distance => TF2.Real (Reading))).X,
   --                    Y => TF2.To_Point ((Bearing => TF2.Radians (Scan_Angle),
   --                                        Distance => TF2.Real (Reading))).Y,
   --                    Z => 0.0);
   --
   --                 --  Now transform the point to the odometry frame
   --                 --  TODO: complete the parameters in the call
   --                 Point := TF2.Transform (Point => ... ,
   --                                         Into  => ... ,
   --                                         From  => ... );
   --
   --                 --  And assign it to the cloud array:
   --                 Cloud (Cloud_Last) := Point;
   --              end if;
   --           end;
   --
   --           --  Update scan angle for the next sample
   --           Scan_Angle := @ + Real (Scan.Angle_Increment);
   --        end loop;
   --
   --        return Cloud;
   --     end Convert_To_Cloud;
   --
   --  begin
   --     if Have_Goal and then Have_Pose then
   --        declare
   --           Cmd_Vel : constant VFH.Velocity2D := VFH.Steer
   --             (Odom   => Odom_Pose,
   --              Goal   => Goal,
   --              Cloud  => Convert_To_Cloud);
   --        begin
   --           Logging.Info ("VFH command: "
   --                         & Fixed (Cmd_Vel.Linear)'Image & " "
   --                         & Fixed (Cmd_Vel.Angular)'Image);
   --           Msg_Move.Data.Linear.X  := Types.Float64 (Cmd_Vel.Linear);
   --           Msg_Move.Data.Angular.Z := Types.Float64 (Cmd_Vel.Angular);
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
   --        Pub.Publish (Msg_Stop);
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

   Node.Spin (During => Forever);
end Turtlebot_VFH_TF2;
