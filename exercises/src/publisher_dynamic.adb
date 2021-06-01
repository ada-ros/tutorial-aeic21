pragma Warnings (Off); -- TODO: remove this pragma

--  This exercise is a node that publishes once per second a string, using
--  the std_msgs/String predefined message. This message has a single field,
--  "data", of type String. We need to create a node, a publisher, and
--  a message of the std_msgs/String type. The publisher must publish in
--  the /chatter topic for it to be interoperable with other ROS2 standard
--  examples, but it can be anything else for your experiments.

with RCL; use RCL;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Publisher_Dynamic is

   --  TODO: initialize the node by calling the appropriate Init function in RCL.Nodes
   --  Node : Nodes.Node := ...;

   --  TODO: fix the Pkg parameter, which is the name of the package defining String
   Msg_Type : constant ROSIDL.Typesupport.Message_Support :=
                ROSIDL.Typesupport.Get_Message_Support
                  (Pkg => "...", -- This should be the package defining the String message
                   Msg => "String");

   Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Msg_Type);

   --  TODO: uncomment once the Node exists
   --  Pub : Publishers.Publisher := Node.Publish (Msg_Type,
   --                                              "/chatter");

begin
   for Count in Positive'Range loop
      Msg ("data").Set_String ("Hello from the Ada side" & Count'Image);

      --  TODO: uncomment once Pub exists
      --  Pub.Publish (Msg);

      Logging.Info ("I just said: " & Msg ("data").Get_String);

      --  We don't actually need to spin on a node just to publish:
      --  Node.Spin (During => 1.0);

      delay 1.0;
   end loop;
end Publisher_Dynamic;
