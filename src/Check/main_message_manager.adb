--------------------------------------------------------------------------------
with Ada.Text_IO;
with Message_Manager;
use Ada.Text_IO;
use Message_Manager;

procedure Main_Message_Manager is

   Message, Message_2, Message_3, Message_4, Message_5, Message_6 : Message_Manager.Message_Record;
   Message_ID : constant Message_Manager.Message_ID_Type:= 1;
   Message_ID_2 : constant Message_Manager.Message_ID_Type:= 2;
   Message_Status : Message_Manager.Status_Type;
   X : Integer := 1;

   procedure Fetch is
      Y : Integer := 1;
   begin
      Put_Line("+++++ FETCHING MESSAGES FOR RECIEVER ID = 1 +++++");
      New_Line;
      while Y < 9 loop
      Fetch_Message(Module  => Message.Receiver, Message => Message);
      Put_Line("Message " & Integer'Image(Y) & " fetched ");
      Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message.Sender));
      Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message.Receiver));
      Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message.Message_ID));
      Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message.Request_ID));
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
      Fetch_Message(Module  => Message_5.Receiver, Message => Message_5);
      Put_Line("Message " & Integer'Image(Y) & " fetched ");
      Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_5.Sender));
      Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_5.Receiver));
      Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_5.Message_ID));
      Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_5.Request_ID));
      New_Line;

         Y := Y + 1;
      end loop;
      Put_Line("+++++ Mailbox Is Full For Reciever ID = 2 +++++");
      New_Line;
   end Fetch_2;

begin

   -- Test Get_Next_Request_ID
   Put_Line("Testing Get_Next_Request_ID");
   Get_Next_Request_ID(Request_ID => Message.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_2.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_3.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_4.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_5.Request_ID);
   Get_Next_Request_ID(Request_ID => Message_6.Request_ID);
   New_Line;
   Put_Line("Message -- Request ID: " & Request_ID_Type'Image (Message.Request_ID));
   Put_Line("Message_2 -- Request ID: " & Request_ID_Type'Image (Message_2.Request_ID));
   Put_Line("Message_3 -- Request ID: " & Request_ID_Type'Image (Message_3.Request_ID));
   Put_Line("Message_4 -- Request ID: " & Request_ID_Type'Image (Message_4.Request_ID));
   Put_Line("Message_5 -- Request ID: " & Request_ID_Type'Image (Message_5.Request_ID));
   Put_Line("Message_6 -- Request ID: " & Request_ID_Type'Image (Message_6.Request_ID));
   New_Line;New_Line;

   -- Test Make_Empty_Message
   Message_3 := Make_Empty_Message(Sender_Domain   => 0,
                                   Receiver_Domain => 0,
                                   Sender          => 2,
                                   Receiver        => 1,
                                   Request_ID      => 4,
                                   Message_ID      => Message_ID,
                                   Priority        => 2);

   Message_4 := Make_Empty_Message(Sender_Domain   => 1,
                                   Receiver_Domain => 1,
                                   Sender          => 1,
                                   Receiver        => 2,
                                   Request_ID      => 8,
                                   Message_ID      => Message_ID_2,
                                   Priority        => 5);


   Message_5 := Make_Empty_Message(Sender_Domain   => 2,
                                   Receiver_Domain => 1,
                                   Sender          => 2,
                                   Receiver        => 2,
                                   Request_ID      => 1,
                                   Message_ID      => 1,
                                   Priority        => 1);

   Message_6 := Make_Empty_Message(Sender_Domain   => 1,
                                   Receiver_Domain => 1,
                                   Sender          => 2,
                                   Receiver        => 2,
                                   Request_ID      => 1,
                                   Message_ID      => Message_ID_2,
                                   Priority        => 4);




   -- 20 messages should be attempted to be sent, only first eight should be sent
   Put_Line("=======ROUTING MESSAGES=========");New_Line;
   while X < 40 loop
        -- Ensure the Make_Empty_Message works properly
      if X = 12 or X = 31 then
           Route_Message(Message => Message_3, Status => Message_Status);
           Put("Message" & Integer'Image(X) & " routed");
           Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
           Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_3.Sender));
           Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_3.Receiver));
           Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_3.Message_ID));
           Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_3.Request_ID));
         New_Line;
         X := X + 1;

      elsif X = 15 or X = 22 then
         Route_Message(Message => Message_4,
                       Status  => Message_Status);
         Put("Message" & Integer'Image(X) & " routed");
           Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
           Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_4.Sender));
           Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_4.Receiver));
           Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_4.Message_ID));
           Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_4.Request_ID));
         New_Line;
         X := X + 1;

      elsif X = 11 or X = 28 then
         Route_Message(Message => Message_5,
                       Status  => Message_Status);
          Put("Message" & Integer'Image(X) & " routed");
           Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
           Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_5.Sender));
           Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_5.Receiver));
           Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_5.Message_ID));
           Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_5.Request_ID));
         New_Line;
         X := X + 1;


      elsif X = 4 or X = 23 then
       -- Test Route_Message without status parameter
           Route_Message(Message => Message_2);
           Put("Message" & Integer'Image(X) & " routed");
           Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
           Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_2.Sender));
           Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_2.Receiver));
           Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_2.Message_ID));
           Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_2.Request_ID));
         New_Line;
         X := X + 1;

      else
        --Testing Route_Message with Status parameter
         if X < 22 then
         Route_Message(Message => Message, Status => Message_Status);
          Put("Message" & Integer'Image(X) & " routed");
          Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
          Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message.Sender));
          Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message.Receiver));
          Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message.Message_ID));
          Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message.Request_ID));
         New_Line;
         X := X + 1;
         end if;

         if X > 20 then
         Route_Message(Message => Message_6, Status => Message_Status);
          Put("Message" & Integer'Image(X) & " routed");
          Put(" +++   Status : " & Status_Type'Image(Message_Status));New_Line;
          Put_Line("+++   Sender ID   : " & Module_ID_Type'Image (Message_6.Sender));
          Put_Line("+++   Reciever ID : " & Module_ID_Type'Image (Message_6.Receiver));
          Put_Line("+++   Message ID  : " & Message_ID_Type'Image (Message_6.Message_ID));
          Put_Line("+++   Request ID  : " & Request_ID_Type'Image(Message_6.Request_ID));
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
