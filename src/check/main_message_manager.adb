--------------------------------------------------------------------------------
with Ada.Text_IO;
with Message_Manager;
use Ada.Text_IO;
use Message_Manager;

procedure Main_Message_Manager is

   Message, Message_2, Message_3, Message_4, Message_5, Message_6 : Message_Manager.Msg_Owner
     := new Message_Record'(Make_Empty_Message((1, 1), (1, 1), 0, (1, 1), 0));
   Message_Type : constant Message_Manager.Universal_Message_Type := (1, 1);
   Message_Type_2 : constant Message_Manager.Universal_Message_Type := (1, 2);
   Message_Status : Message_Manager.Status_Type;
   X : Integer := 1;
   Mailbox_1, Mailbox_2 : Message_Manager.Module_Mailbox;

   procedure Fetch is
      Y : Integer := 1;
   begin
      Put_Line("+++++ FETCHING MESSAGES FOR RECIEVER ID = 1 +++++");
      New_Line;
      while Y < 9 loop
         Message_Manager.Read_Next(Mailbox_1, Message);
         Put_Line("Message " & Integer'Image(Y) & " fetched ");
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;

         Y := Y + 1;
      end loop;
      Put_Line("+++++ Mailbox Is Full For Reciever ID = 1 +++++");
      New_Line;
   end Fetch;

   procedure Fetch_2 is
      Y : Integer := 1;
   begin
      Put_Line("+++++ FETCHING MESSAGES FOR RECIEVER ID = 2 +++++");
      New_Line;
      while Y < 9 loop
         Message_Manager.Read_Next(Mailbox_2, Message_5);
         Put_Line("Message " & Integer'Image(Y) & " fetched ");
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;

         Y := Y + 1;
      end loop;
      Put_Line("+++++ Mailbox Is Full For Reciever ID = 2 +++++");
      New_Line;
   end Fetch_2;

begin
   -- Register receiving mailboxes
   Message_Manager.Register_Module(1, 8, Mailbox_1, Unchecked_Type);
   Message_Manager.Register_Module(2, 8, Mailbox_2, Unchecked_Type);

   -- Test Get_Next_Request_ID
   Put_Line("Testing Get_Next_Request_ID");
   Get_Next_Request_ID(Request_ID => Message.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_2.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_3.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_4.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_5.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_6.Request_ID);
   New_Line;
   Put_Line("Message   -- Request ID: " & Request_ID_Type'Image (Message.Request_ID));
   Put_Line("Message_2 -- Request ID: " & Request_ID_Type'Image (Message_2.Request_ID));
   Put_Line("Message_3 -- Request ID: " & Request_ID_Type'Image (Message_3.Request_ID));
   Put_Line("Message_4 -- Request ID: " & Request_ID_Type'Image (Message_4.Request_ID));
   Put_Line("Message_5 -- Request ID: " & Request_ID_Type'Image (Message_5.Request_ID));
   Put_Line("Message_6 -- Request ID: " & Request_ID_Type'Image (Message_6.Request_ID));
   New_Line(2);

   -- Test Make_Empty_Message
   Message_3 := new Message_Record'(Make_Empty_Message
     (Sender_Address   => (0, 2),
      Receiver_Address => (0, 1),
      Request_ID       => 4,
      Message_Type     => Message_Type,
      Payload_Size     => 0,
      Priority         => 2));

   Message_4 := new Message_Record'(Make_Empty_Message
     (Sender_Address   => (1, 1),
      Receiver_Address => (1, 2),
      Request_ID       => 8,
      Message_Type       => Message_Type_2,
      Payload_Size     => 0,
      Priority         => 5));

   Message_5 := new Message_Record'(Make_Empty_Message
     (Sender_Address   => (2, 2),
      Receiver_Address => (1, 2),
      Request_ID       => 1,
      Message_Type       => Message_Type,
      Payload_Size     => 0,
      Priority         => 1));

   Message_6 := new Message_Record'(Make_Empty_Message
     (Sender_Address   => (1, 2),
      Receiver_Address => (1, 2),
      Request_ID       => 1,
      Message_Type       => Message_Type_2,
      Payload_Size     => 0,
      Priority         => 4));


   -- 20 messages should be attempted to be sent, only first eight should be sent
   Put_Line("=======ROUTING MESSAGES=========");
   New_Line;
   while X < 40 loop
      -- Ensure the Make_Empty_Message works properly
      if X = 12 or X = 31 then
         Route_Message(Message => Message_3.all, Status => Message_Status);
         Put_Line("Message" & Integer'Image(X) & " routed");
         Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;
         X := X + 1;

      elsif X = 15 or X = 22 then
         Route_Message(Message => Message_4.all, Status  => Message_Status);
         Put_Line("Message" & Integer'Image(X) & " routed");
         Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;
         X := X + 1;

      elsif X = 11 or X = 28 then
         Route_Message(Message => Message_5.all, Status  => Message_Status);
         Put_Line("Message" & Integer'Image(X) & " routed");
         Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;
         X := X + 1;


      elsif X = 4 or X = 23 then
         -- Test Route_Message without status parameter
         Route_Message(Message => Message_2.all);
         Put_Line("Message" & Integer'Image(X) & " routed");
         Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
         Put_Line(Message_Manager.Stringify_Message(Message.all));
         New_Line;
         X := X + 1;

      else
         -- Testing Route_Message with Status parameter
         if X < 22 then
            Route_Message(Message => Message.all, Status => Message_Status);
            Put_Line("Message" & Integer'Image(X) & " routed");
            Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
            Put_Line(Message_Manager.Stringify_Message(Message.all));
            New_Line;
            X := X + 1;
         end if;

         if X > 20 then
            Route_Message(Message => Message_6.all, Status => Message_Status);
            Put_Line("Message" & Integer'Image(X) & " routed");
            Put_Line("+++ Status      : " & Status_Type'Image(Message_Status));
            Put_Line(Message_Manager.Stringify_Message(Message.all));
            New_Line;
            X := X + 1;
         end if;
      end if;
   end loop;
   New_Line;

   -- Loop waiting to fetch messages until mailbox is full.
   -- Testing Fetch_Message
   Fetch;
   Fetch_2;

end Main_Message_Manager;
