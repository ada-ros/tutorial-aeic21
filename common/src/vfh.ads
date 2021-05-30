with Ada.Numerics.Elementary_Functions; use Ada.Numerics;

with RCL.TF2;

with ROSIDL.Static.Tutorial_Common.Sensor_Msgs.Messages.Laserscan;
with ROSIDL.Types;

package VFH is

   use ROSIDL.Static.Tutorial_Common;

   subtype Real is ROSIDL.Types.Float64; use type Real;

   type Parameters is record
      --  An odd number is recommended so a bucket is always in front of the robot
      Buckets       : Positive := 19; -- Histogram buckets

      Threshold     : Real := 1.0; -- Tunable that marks free space density

      --  Angles covered by the buckets
      Angle_Min     : Real := -Pi / 2.0;
      Angle_Max     : Real :=  Pi / 2.0;

      Goal_Dist     : Real := 0.1; -- Stop when closer
      Near_Dist     : Real := 0.5; -- Closer than this, occupied
      Far_Dist      : Real := 0.9; -- Full speed ahead
      Max_Lin_Speed : Real := 0.1;          -- m/s
      Max_Ang_Speed : Real := 3.14159 / 3.0; -- rad/s

      Smooth_Weight : Real := 3.0; -- Histogram smoothing -- NOT WORKING

      Safety_Buckets : Natural := 3; -- How many buckets to leave for safety

      Flip : Boolean := True; -- Hack to avoid needing transforms with the turtlebot in webots
   end record;

   type Pose2D is record
      X, Y, Angle : Real := 0.0;
   end record;

   type Velocity2D is record
      Linear, Angular : Real := 0.0;
   end record;

   type Euler is record
      Roll, Pitch, Yaw : Real := 0.0;
   end record;

   type Quaternion is record
      X, Y, Z, W : Real;
   end record;

   function Distance (P, Q : Pose2D) return Real;

   function Quat_To_Euler (Q : Quaternion) return Euler;

   function Steer (Odom,
                   Goal   : Pose2D;
                   Scan   : Sensor_Msgs.Messages.Laserscan.Message;
                   Params : Parameters := (others => <>))
                   return Velocity2D;

   function Steer (Odom,
                   Goal   : Pose2D;
                   Cloud  : RCL.TF2.Point_Array;
                   Params : Parameters := (others => <>))
                      return Velocity2D;

private

   use Ada.Numerics.Elementary_Functions;

   --------------
   -- Distance --
   --------------

   function Distance (P, Q : Pose2D) return Real
   is (Real (Sqrt (Float (P.X - Q.X) * Float (P.X - Q.X)
                 + Float (P.Y - Q.Y) * Float (P.Y - Q.Y))));

end VFH;
