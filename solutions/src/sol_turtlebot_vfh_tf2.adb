--  with Ada.Text_IO; use Ada.Text_IO;
--
--  with ANSI;

with Common;

with RCL.Logging;
with RCL.Nodes;
with RCL.Subscriptions;
with RCL.QoS;
with RCL.TF2;

with ROSIDL.Static.Tutorial_Solutions.Geometry_Msgs.Messages.Posestamped;
with ROSIDL.Static.Tutorial_Solutions.Geometry_Msgs.Messages.Twist;
with ROSIDL.Static.Tutorial_Solutions.Nav_Msgs.Messages.Odometry;
with ROSIDL.Static.Tutorial_Common.Sensor_Msgs.Messages.Laserscan;

use RCL;
use ROSIDL.Static.Tutorial_Common;
use ROSIDL.Static.Tutorial_Solutions;

with VFH;

procedure Sol_Turtlebot_VFH_TF2 is

   Node : Nodes.Node'Class := Nodes.Init;

   package Pub is new Nodes.Typed_Publish
     (Handling => Geometry_Msgs.Messages.Twist.Handling,
      Node     => Node,
      Topic    => "/cmd_vel");

   Msg_Move : Geometry_Msgs.Messages.Twist.Handling.Message;
   Msg_Stop : Geometry_Msgs.Messages.Twist.Handling.Message;

   Have_Goal : Boolean := False;
   Goal      : VFH.Pose2D;

   Have_Pose : Boolean := False;
   Odom_Pose : VFH.Pose2D;

   ---------------
   -- Read_Goal --
   ---------------

   procedure Read_Goal (Node : in out Nodes.Node'Class;
                        Msg  : Geometry_Msgs.Messages.Posestamped.Message;
                        Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
   begin
      Have_Goal := True;
      Goal.X     := Msg.Pose.Position.X;
      Goal.Y     := Msg.Pose.Position.Y;
      Goal.Angle := 0.0;
      Logging.Warn ("BY YOUR COMMAND: "
                    & Common.Fixed (Goal.X)'image & " "
                    & Common.Fixed (Goal.Y)'image & " "
                    & Common.Fixed (Goal.Angle)'Image);
   end Read_Goal;

   ----------------
   -- Read_Laser --
   ----------------

   procedure Read_Laser (Node : in out Nodes.Node'Class;
                         Scan : Sensor_Msgs.Messages.Laserscan.Message;
                         Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
      use Common;

      ----------------------
      -- Convert_To_Cloud --
      ----------------------

      function Convert_To_Cloud return TF2.Point_Array is
         Cloud      : TF2.Point_Array (1 .. Natural (Scan.Ranges.Size));
         Cloud_Last : Natural := Cloud'First - 1;

         subtype Real is TF2.Real;
         use type Real;

         Scan_Angle : Real := Real (Scan.Angle_Min);

         --  Reference frames to use: from the laser one to the odometry one
         From       : constant String := Types.Get_String (Scan.Header.Frame_Id);
         Into       : constant String := "odom";
      begin
         for Scan_I in 1 .. Scan.Ranges.Size loop
            declare
               use type Types.Float32;
               Reading     : Types.Float32 renames Scan.Ranges.Data (Scan_I);

               --  We use this point to convert the laser polar reading into
               --  a cartesian 3D point. Then, we will transform this point
               --  to the odometry reference frame.
               Point : TF2.Point3D;
            begin
               if Reading'Valid and Then
                 Reading > Scan.Range_Min and then Reading < Scan.Range_Max
               Then
                  Cloud_Last := @ + 1; -- One more cloud point to store

                  --  Get the cartesian coordinates in the laser ref. frame
                  Point :=
                    (X => TF2.To_Point ((Bearing => TF2.Radians (Scan_Angle),
                                         Distance => TF2.Real (Reading))).X,
                     Y => TF2.To_Point ((Bearing => TF2.Radians (Scan_Angle),
                                         Distance => TF2.Real (Reading))).Y,
                     Z => 0.0);

                  --  Now transform the point to the odometry frame
                  Point := TF2.Transform (Point,
                                          Into => Into,
                                          From => From);

                  --  And assign it to the cloud array:
                  Cloud (Cloud_Last) := Point;
               end if;
            end;
            Scan_Angle := @ + Real (Scan.Angle_Increment);
         end loop;

         return Cloud;
      end Convert_To_Cloud;

   begin
      Logging.Warn ("FRAME is " & Types.Get_String (Scan.Header.Frame_Id));
      if Have_Goal and then Have_Pose then
         declare
            Cmd_Vel : constant VFH.Velocity2D := VFH.Steer
              (Odom   => Odom_Pose,
               Goal   => Goal,
               Cloud  => Convert_To_Cloud);
         begin
            Logging.Info ("VFH command: "
                          & Fixed (Cmd_Vel.Linear)'Image & " "
                          & Fixed (Cmd_Vel.Angular)'Image);
            Msg_Move.Data.Linear.X  := Types.Float64 (Cmd_Vel.Linear);
            Msg_Move.Data.Angular.Z := Types.Float64 (Cmd_Vel.Angular);
            Pub.Publish (Msg_Move);
         end;

         Logging.Info
           ("Pose: "
            & Fixed (Odom_Pose.X)'Image & " "
            & Fixed (Odom_Pose.Y)'Image & " "
            & Fixed (Odom_Pose.Angle)'Image & " "
            & " Distance:" & Fixed (VFH.Distance (Odom_Pose, Goal))'Image);
      else
         Pub.Publish (Msg_Stop);
      end if;
   end Read_Laser;

   ---------------
   -- Read_Odom --
   ---------------

   procedure Read_Odom (Node : in out Nodes.Node'Class;
                        Msg  : Nav_Msgs.Messages.Odometry.Message;
                        Info : ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
   begin
      Have_Pose   := True;
      Odom_Pose.X := Msg.Pose.Pose.Position.X;
      Odom_Pose.Y := Msg.Pose.Pose.Position.Y;
      Odom_Pose.Angle := VFH.Quat_To_Euler
        (VFH.Quaternion'(W => Msg.Pose.Pose.Orientation.W,
                         X => Msg.Pose.Pose.Orientation.X,
                         Y => Msg.Pose.Pose.Orientation.Y,
                         Z => Msg.Pose.Pose.Orientation.Z)).Yaw;
   end Read_Odom;

   procedure Subscribe_To_Goal is
     new Nodes.Typed_Subscribe (Handling => Geometry_Msgs.Messages.Posestamped.Handling,
                                Callback => Read_Goal);

   procedure Subscribe_To_Laser is
     new Nodes.Typed_Subscribe (Handling => Sensor_Msgs.Messages.Laserscan.Handling,
                                Callback => Read_Laser);

   procedure Subscribe_To_Odom is
     new Nodes.Typed_Subscribe (Handling => Nav_Msgs.Messages.Odometry.Handling,
                                Callback => Read_Odom);

begin
   Subscribe_To_Goal (Node, "/goal_pose");
   Subscribe_To_Laser (Node, "/scan",
                       Subscriptions.Defaults.Using (QoS.Profiles.Sensor_Data));
   Subscribe_To_Odom (Node, "/odom");
   Logging.Info ("Ready to see the world.");

   Node.Spin (During => Forever);
end Sol_Turtlebot_VFH_TF2;
