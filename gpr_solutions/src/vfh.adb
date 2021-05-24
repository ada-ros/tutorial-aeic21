with Ada.Numerics.Generic_Elementary_Functions;

with ANSI;

with Common;

with RCL.Logging; use RCL;

package body VFH is

   package Real_Funcs is new Ada.Numerics.Generic_Elementary_Functions (Real);

   ---------------
   -- Fix_Angle --
   ---------------

   function Fix_Angle (A : Real) return Real
   is
   begin
      return Result : Real := A do
         while Result > Pi loop
            Result := @ - (2.0 * Pi);
         end loop;
         while Result < -Pi loop
            Result := @ + (2.0 * Pi);
         end loop;
      end return;
   end Fix_Angle;

   -------------------
   -- Quat_To_Euler --
   -------------------

   function Quat_To_Euler (Q : Quaternion) return Euler
   --  From https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
   is
      use Real_Funcs;

      Angles : Euler;

      --  roll (x-axis rotation)
      Sinr_Cosp : constant Real := 2.0 * (Q.W * Q.X + Q.Y * Q.Z);
      Cosr_Cosp : constant Real := 1.0 - 2.0 * (Q.X * Q.X + Q.Y * Q.Y);
   begin
      Angles.Roll := Arctan (Sinr_Cosp, Cosr_Cosp);

      --  pitch (y-axis rotation)
      declare
         Sinp : constant Real := 2.0 * (q.w * q.y - q.z * q.x);
      begin
         Angles.Pitch :=
           (if abs (Sinp) >= 1.0
            then (if Sinp >= 0.0 then Pi / 2.0 else -Pi / 2.0)
            else Arcsin (Sinp));
      end;

      --  yaw (z-axis rotation)
      declare
         Siny_Cosp : constant Real := 2.0 * (Q.W * Q.Z + Q.X * Q.Y);
         Cosy_Cosp : constant Real := 1.0 - 2.0 * (Q.Y * Q.Y + Q.Z * Q.Z);
      begin
         Angles.Yaw := Arctan (Siny_Cosp, Cosy_Cosp);
      end;

      return Angles;
   end Quat_To_Euler;

   -----------
   -- Steer --
   -----------

   function Steer (Odom,
                   Goal   : Pose2D;
                   Scan   : Sensor_Msgs.Messages.Laserscan.Message;
                   Params : Parameters := (others => <>))
                   return Velocity2D
   is
      use Common;
      use Real_Funcs;

      type Bucket is record
         Weight : Real := 0.0; -- The more weight, the more "obstacley"
         Ang_Ini,
         Ang_End  : Real;
      end record;

      type Bucket_Array is array (Positive range <>) of Bucket;

      Buckets : Bucket_Array (1 .. Params.Buckets);

      Width : constant Real :=
                   (Params.Angle_Max - Params.Angle_Min) / Real (Params.Buckets);

      ---------------------
      -- Prepare_Buckets --
      ---------------------

      procedure Prepare_Buckets is
         Angle : Real := Params.Angle_Min;
      begin
         for B of Buckets loop
            B.Ang_Ini := Angle;
            B.Ang_End := Angle + Width;
            Angle := @ + Width;
         end loop;
      end Prepare_Buckets;

      ------------------
      -- Fill_Buckets --
      ------------------
      --  Use the scans to "fill" each bucket with the closest reading
      procedure Fill_Buckets is
         use type Types.Size_T;
         Scan_Angle : Real     := Real (Scan.Angle_Min);
         Scan_I     : Types.Size_T := (if Params.Flip then Scan.Ranges.Size else 1);
         Scan_Delta : constant Types.Size_T := (if Params.Flip then -1 else 1);
         Bucket_I   : Positive := 1;
      begin

         --  Drop too early scans

         while Scan_Angle < Buckets (Buckets'First).Ang_Ini loop
            Scan_I     := @ + Scan_Delta;
            Scan_Angle := @ + Real (Scan.Angle_Increment);
         end loop;

         --  Valid readings

         while Scan_Angle < Buckets (Buckets'Last).Ang_End and then
           Scan_I <= Types.Size_T (Scan.Ranges.Size)
         loop
            if Scan_Angle >= Buckets (Bucket_I).Ang_End then
               Bucket_I := @ + 1;
               exit when Bucket_I > Buckets'Last; -- Can't happen, but JIC
            end if;

            declare
               use type Types.Float32;
               Reading : Types.Float32 renames Scan.Ranges.Data (Scan_I);
            begin
               if Reading'Valid and Then
                 Reading > Scan.Range_Min and then Reading < Scan.Range_Max
                 and then Real (Reading) < Params.Far_Dist
               then
                  Buckets (Bucket_I).Weight := @ +
                    (Params.Far_Dist - Real (Reading)) / Params.Far_Dist * 3.0;
               end if;
            end;

            Scan_I := @ + Scan_Delta;
            Scan_Angle := @ + Real (Scan.Angle_Increment);
         end loop;

         --  Rest of scans is not useful either

      end Fill_Buckets;

      --------------------
      -- Smooth_Buckets --
      --------------------

      procedure Smooth_Buckets is
         Old : constant Bucket_Array := Buckets;
      begin
         for I in Old'Range loop
            declare
               function Fix (W : Integer) return Real
               is (1.0 / (Real (abs (W)) * Params.Smooth_Weight + 1.0));
               Used : Real := 0.0;
            begin
               Buckets (I).Weight := 0.0;
               for J in -2 .. 2 loop
                  if I + J in Old'Range then
                     Used := Used + Fix (J);
                     Buckets (I).Weight := Old (I + J).Weight * Fix (J);
                  end if;
               end loop;
               Buckets (I).Weight := @ * Used; -- Renormalize
            end;
         end loop;
      end Smooth_Buckets;

      -------------------
      -- Print_Buckets --
      -------------------

      procedure Print_Buckets (Steer : Real; Goal : Natural) is
      begin
         for I in reverse Buckets'Range loop
            declare
               use ANSI;
               Histo : constant String :=
                         String'(1 .. Natural (Buckets (I).Weight * 2.0) => '*');
            begin
               Logging.Info
                 ((if I < 10 then " " else "") & I'Img & ": "
                  & (if Steer >= Buckets (I).Ang_Ini and then Steer < Buckets (I).Ang_End
                    then "S" else " ")
                  & (if Goal = I then "G" else " ")
                  & " |"
                  & (if Buckets (I).Weight > Params.Threshold
                    then Color_Wrap (Histo, Foreground (Light_Red))
                    else Color_Wrap (Histo, Foreground (Light_Green))));
            end;
         end loop;
      end Print_Buckets;

      -----------------
      -- Pick_Bucket --
      -----------------

      function Pick_Bucket return Velocity2D is
         Ahead_Bucket : constant Positive := Params.Buckets / 2 + 1;

         Goal_Bearing : constant Real :=
                          Fix_Angle
                            (Arctan (Goal.Y - Odom.Y, Goal.X - Odom.X)
                             - Odom.Angle);

         Goal_Bucket  : Natural := 0;

         In_Valley    : Boolean := False;
         Valley_Ini,
         Valley_End : Natural;
         Best_Valley_Dist : Real := Real'Last;
         Best_Bucket_Ini,
         Best_Bucket_End : Natural;
      begin
         Logging.Info ("VFH goal bearing: " & Fixed (Goal_Bearing)'img);

         --  Locate valley containing the goal, and its middle point

         for I in Buckets'Range loop
            if Buckets (I).Weight < Params.Threshold and not In_Valley then
               In_Valley  := True;
               Valley_Ini := I;
            elsif In_Valley and
              (Buckets (I).Weight >= Params.Threshold or else I = Buckets'Last)
            then
               In_Valley  := False;

               if Buckets (I).Weight >= Params.Threshold then
                  Valley_End := I - 1;
               else
                  Valley_End := I; -- Which must also be 'Last
                  pragma Assert (I = Buckets'Last);
               end if;

               --  Use valley if big enough
               if Valley_Ini + Params.Safety_Buckets <= Valley_End - Params.Safety_Buckets then
                  declare
                     Valley_Dist : constant Real :=
                                     Real'Min
                                       (Real'Min
                                          (abs (Buckets (Valley_Ini).Ang_Ini - Goal_Bearing),
                                           abs (Buckets (Valley_Ini).Ang_End - Goal_Bearing)),
                                        Real'Min
                                          (abs (Buckets (Valley_End).Ang_Ini - Goal_Bearing),
                                           abs (Buckets (Valley_End).Ang_End - Goal_Bearing)));
                  begin
                     if Valley_Dist < Best_Valley_Dist then
                        Best_Valley_Dist := Valley_Dist;
                        Best_Bucket_Ini := Valley_Ini;
                        Best_Bucket_End := Valley_End;
                     end if;
                  end;
               end if;
            end if;

            if Goal_Bearing >= Buckets (I).Ang_Ini and then
              Goal_Bearing <  Buckets (I).Ang_End
            then
               Goal_Bucket := I;
            end if;
         end loop;

         --  Or, if out of range, pick just first/last

         if Goal_Bucket = 0 then
            if Goal_Bearing < 0.0 then
               Goal_Bucket := 1;
            else
               Goal_Bucket := Buckets'Last;
            end if;
         end if;

         --  Print_Buckets (0.0, Goal_Bucket);
         --  return (0.0, 0.0);

         --  If there's no valley, just turn in place

         if Best_Valley_Dist = Real'Last then
            Logging.Warn ("VFH found no valley");
            Print_Buckets (0.0, Goal_Bucket);
            return (0.0, Params.Max_Ang_Speed);
         end if;

         --  Use the middle of the valley closer to the goal

         Logging.Info ("Valley is" & Best_Bucket_Ini'Img & Best_Bucket_End'Img);
         Logging.Info ("Angles are "
                       & Fixed (Buckets (Best_Bucket_Ini).Ang_Ini)'Image & " "
                       & Fixed (Buckets (Best_Bucket_End).Ang_End)'Image);

         declare
            Steer : constant Real :=
                      (if Goal_Bucket = Best_Bucket_Ini and then Goal_Bucket = Buckets'First
                       then Goal_Bearing
                       elsif Goal_Bucket = Best_Bucket_End and then Goal_Bucket = Buckets'Last
                       then Goal_Bearing
                       elsif Goal_Bucket in Best_Bucket_Ini + Params.Safety_Buckets ..
                                            Best_Bucket_End - Params.Safety_Buckets
                       then Goal_Bearing
                       else
                          (if abs (Buckets (Best_Bucket_End - Params.Safety_Buckets).Ang_Ini - Goal_Bearing) <
                              abs (Buckets (Best_Bucket_Ini + Params.Safety_Buckets).Ang_End - Goal_Bearing)
                          then Buckets (Best_Bucket_End - Params.Safety_Buckets).Ang_Ini
                          else Buckets (Best_Bucket_Ini + Params.Safety_Buckets).Ang_End));
         begin
            Logging.Info ("VFH steer angle:" & Fixed (Steer)'Image);

            --  Debug output

            Print_Buckets (Steer, Goal_Bucket);

            return Result : Velocity2D do
               if Steer > Width then
                  Result.Angular := Params.Max_Ang_Speed;
               elsif Steer < -Width then
                  Result.Angular := -Params.Max_Ang_Speed;
               else
                  Result.Angular := Steer / Width * Params.Max_Ang_Speed;
               end if;

               if Buckets (Ahead_Bucket).Weight < Params.Threshold then
                  Result.Linear := Params.Max_Lin_Speed;
               else
                  Result.Linear := Params.Max_Lin_Speed
                    / Buckets (Ahead_Bucket).Weight;
               end if;
            end return;
         end;
      end Pick_Bucket;

   begin

      --  Stop when reached
      if Distance (Goal, Odom) <= Params.Goal_Dist then
         return (0.0, 0.0);
      end if;

      Prepare_Buckets;
      Fill_Buckets;
      --  Smooth_Buckets;

      return Pick_Bucket;
   end Steer;

end VFH;
