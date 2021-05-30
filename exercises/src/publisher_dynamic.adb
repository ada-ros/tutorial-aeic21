with RCL; use RCL;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Publisher_Dynamic is

   Node : Nodes.Node := Nodes.Init;

   Msg_Type : constant ROSIDL.Typesupport.Message_Support :=
                ROSIDL.Typesupport.Get_Message_Support ("std_msgs",
                                                        "String");

   Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Msg_Type);

   Pub : Publishers.Publisher := Node.Publish (Msg_Type,
                                               "/chatter");

begin
   for Count in Positive'Range loop
      Msg ("data").Set_String ("Hello from the Ada side" & Count'Image);
      Pub.Publish (Msg);
      Logging.Info ("I just said: " & Msg ("data").Get_String);

      --  We don't actually need to spin on a node just to publish:
      --  Node.Spin (During => 1.0);

      delay 1.0;
   end loop;
end Publisher_Dynamic;
