--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Generic_Message_Manager;
with Ada.Real_Time; use type Ada.Real_Time.Time;
with GNAT.Sockets;   use GNAT.Sockets;
with Ada.Streams; 
with Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body CubedOS.Network.Messages is
    
   function Read_Stream_Message ( Data : Ada.Streams.Stream_Element_Array; Last : Ada.Streams.Stream_Element_Offset) return Message_Manager.Message_Record is
      Char : Character;
      Property_Digits : Integer := 0;
      Current_Digit : Integer := 0;
      Property_Position : Integer := 1;
      POW : Integer := 10;
      Sender_Domain : Message_Manager.Domain_ID_Type;
      Sender_Module : Message_Manager.Module_ID_Type;
      Receiver_Domain : Message_Manager.Domain_ID_Type;
      Receiver_Module :Message_Manager. Module_ID_Type;
      Request_ID : Message_Manager.Request_ID_Type;
      Message_ID : Message_Manager.Message_ID_Type;
   begin
      Ada.Text_IO.Put_Line("Incoming: ");
        
      for I in 1..Last loop
         Ada.Text_IO.Put("" & Character'Val(Data(I)));
         
         if Character'Val(Data(I)) = '!' then
            case Property_Position is
               when 1 => 
                 Sender_Domain := Message_Manager.Domain_ID_Type(Property_Digits);
               when 2 =>
                 Sender_Module := Message_Manager.Module_ID_Type(Property_Digits);
               when 3 =>
                 Receiver_Domain := Message_Manager.Domain_ID_Type(Property_Digits);
               when 4 =>
                 Receiver_Module := Message_Manager.Module_ID_Type(Property_Digits);
               when 5 =>
                 Request_ID := Message_Manager.Request_ID_Type(Property_Digits);
               when 6 =>
                 Message_ID := Message_Manager.Message_ID_Type(Property_Digits);
               when others => null;
             end case;
             Property_Position := Property_Position + 1;
             Property_Digits := 0;
         else
            Current_Digit := Integer'Value("" & Character'Val(Data(I)));
            while Current_Digit >= POW loop
               POW := POW * 10;
            end loop;
            Property_Digits := Property_Digits * POW + Current_Digit;
            POW := 10;
         end if;
      end loop;
   
      return Message_Manager.Make_Empty_Message
      (Sender_Domain   => Sender_Domain,
       Receiver_Domain => Receiver_Domain,
       Sender          => Sender_Module,
       Receiver        => Receiver_Module,
       Request_ID      => Request_ID,
       Message_ID      => Message_ID);
   end Read_Stream_Message;
    
   procedure Server_Loop is
      Server        : Socket_Type;
      Address, From : Sock_Addr_Type;
      Data          : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last          : Ada.Streams.Stream_Element_Offset;
      Message : Message_Manager.Message_Record;
      Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (1_000);
   begin
      Ada.Text_IO.Put_Line ("In Server Loop Task");
      Create_Socket (Server, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Address.Addr := Any_Inet_Addr;
      if Message_Manager.Domain_ID = 1 then
         Address.Port := 50000;
      else
         Address.Port := 50001;
      end if;
      Ada.Text_IO.Put_Line ("My Addr: " & Image (Address.Addr));
      Bind_Socket (Server, Address);
        
      loop
         begin
            -- Receive message from Socket
            GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
            Message := Read_Stream_Message(Data, Last);
            Ada.Text_IO.Put_Line ("from : " & Image (From.Addr));
            if Message.Sender_Domain = Message_Manager.Domain_ID then
               Ada.Text_IO.Put_Line ("This message was sent from this domain! Dropping Message");
            else
               Ada.Text_IO.Put_Line ("This message was sent from a different domain! Routing Message");
               Message_Manager.Route_Message(Message);
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Exception_Name (E) & ": " & Exception_Message (E));
         end;
         delay until Next_Release;
         Next_Release :=
           Next_Release + Ada.Real_Time.Milliseconds (1_000);
      end loop;
   end Server_Loop;

   procedure Send_Network_Message(Message_Description : in String )is
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Last : Ada.Streams.Stream_Element_Offset;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Message_Description'Length));
        
   begin
      if Message_Manager.Domain_ID = 1 then
         Address.Port := 50001;
      else
         Address.Port := 50000;
      end if;
      Address.Addr := Inet_Addr ("127.0.0.1");
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      -- I is Ada.Streams.Stream_Element_Offset
      for I in Buffer'Range loop
         Buffer (I) := Ada.Streams.Stream_Element (Character'Pos (Message_Description (Integer(I))));
      end loop; 
       -- Transmit a message to another socket.
      Send_Socket (Socket, Buffer, Last, Address);        
   end Send_Network_Message;
   
    -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Manager.Message_Record) is
      -- Convert Item to String (with no trailing space)
      Sender_Domain_String : constant String := Trim(Message_Manager.Domain_ID_Type'Image(Message.Sender_Domain), Left);
      Sender_Module_String : constant String := Trim(Message_Manager.Module_ID_Type'Image(Message.Sender), Left);
      Receiver_Domain_String : constant String := Trim(Message_Manager.Module_ID_Type'Image(Message.Receiver_Domain), Left);         
      Receiver_Module_String : constant String := Trim(Message_Manager.Module_ID_Type'Image(Message.Receiver), Left);
      Request_ID_String : constant String := Trim(Message_Manager.Request_ID_Type'Image(Message.Request_ID), Left);
      Message_ID_String : constant String := Trim(Message_Manager.Message_ID_Type'Image(Message.Message_ID), Left);
      Message_Image     : constant String := Sender_Domain_String & "!" & Sender_Module_String & "!" & Receiver_Domain_String & "!" & Receiver_Module_String & "!" & Request_ID_String & "!" & Message_ID_String & "!";
   begin
      -- should there be some check here?
      Ada.Text_IO.Put_Line("Sending from domain " & Message_Manager.Domain_ID_Type'Image(Message_Manager.Domain_ID));
      Ada.Text_IO.Put_Line(Message_Image);
      Send_Network_Message(Message_Image);
   end Process;
    
   task body Network_Loop is
   begin
      Ada.Text_IO.Put_Line ("Start Network Loop");
      Server_Loop;
   end Network_Loop;
   
   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      Ada.Text_IO.Put_Line ("Start Message Loop");
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;
   
   
end CubedOS.Network.Messages;
