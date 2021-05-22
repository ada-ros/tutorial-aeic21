with ANSI; use ANSI;

package Common is

   --  Misc reusable thingies

   type Fixed is delta 0.01 digits 10;

   function Emph (Text : String) return String
   is (Color_Wrap (Text, Foreground (Light_Cyan)));

end Common;
