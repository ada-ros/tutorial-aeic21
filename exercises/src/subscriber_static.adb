pragma Warnings (Off); -- TODO: remove this pragma

with RCL.Logging;
with RCL.Nodes;

--  TODO: fix the import of the std_msgs/String message specification package,
--  already defined in the Rclada interfaces:
--  with ROSIDL.Static.Rclada.Std_Msgs. ...;
with ROSIDL.Types;
with ROSIDL.Typesupport;

use RCL;

with ROSIDL.Static.Rclada;
use ROSIDL.Static.Rclada;

procedure Subscriber_Static is

   Node : Nodes.Node'Class := Nodes.Init;

   --------------
   -- Callback --
   --------------
   --  TODO: uncomment and complete
   --  procedure Callback (Node : in out Nodes.Node'Class;
   --                      Msg  :        Std_Msgs.Messages.String.Message;
   --                      Info :        ROSIDL.Message_Info)
   --  is
   --     pragma Unreferenced (Node, Info);
   --  begin
   --     --  TODO: pass the appropriate argument to Get_String:
   --     Logging.Info ("Radio chatter: " & Types.Get_String (...));
   --  end Callback;

   --  TODO: uncomment and complete the arguments
   --  procedure Subscribe is new Nodes.Typed_Subscribe
   --    (Handling => ...,
   --     Callback => ...);

begin
   --  TODO: uncomment once the instance of Subscribe has been fixed
   --  Subscribe (Node, "/chatter");

   Node.Spin (During => Forever);
end Subscriber_Static;
