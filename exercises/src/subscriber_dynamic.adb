pragma Warnings (Off); -- TODO: remove this pragma

with RCL; use RCL;
with RCL.Logging;
with RCL.Nodes;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Subscriber_Dynamic is

   Node : Nodes.Node := Nodes.Init;

   Msg_Type : constant ROSIDL.Typesupport.Message_Support :=
                ROSIDL.Typesupport.Get_Message_Support
                  (Pkg => "",
                   Msg => "");
   --  TODO: fix the Pkg and Msg parameters to import the std_msgs/String type

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
   --  TODO: complete the subscription parameters and uncomment
   --  Node.Subscribe (Msg_Type => ...,
   --                  Topic    => "/chatter",
   --                  Callback => ...);

   Node.Spin (During => Forever);
end Subscriber_Dynamic;
