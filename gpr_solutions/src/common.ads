with ANSI; use ANSI;

package Common is

   --  Misc reusable thingies

   function Emph (Text : String) return String
   is (Color_Wrap (Text, Foreground (Light_Cyan)));

end Common;
