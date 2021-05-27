with RCL; use RCL;
with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Sol_Subscriber_Dynamic is

   Node : Nodes.Node := Nodes.Init;

   Msg_Type : constant ROSIDL.Typesupport.Message_Support :=
                ROSIDL.Typesupport.Get_Message_Support ("std_msgs",
                                                        "String");

   ------------
   -- Listen --
   ------------

   procedure Listen (Node : in out Nodes.Node'Class;
                     Msg  : in out ROSIDL.Dynamic.Message;
                     Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Info, Node);
   begin
      Logging.Info ("Radio chatter: " & Msg ("data").Get_String);
   end Listen;

begin
   Node.Subscribe (Msg_Type => Msg_Type,
                   Topic    => "/chatter",
                   Callback => Listen'Unrestricted_Access);

   Node.Spin (During => Forever);
end Sol_Subscriber_Dynamic;
