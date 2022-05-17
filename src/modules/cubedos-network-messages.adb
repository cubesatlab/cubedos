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
with Name_Resolver;
with CubedOS.Lib.XDR;
use  CubedOS.Lib;
package body CubedOS.Network.Messages is
    
   function Read_Stream_Message ( Data : Ada.Streams.Stream_Element_Array; Last : Ada.Streams.Stream_Element_Offset) return Message_Manager.Message_Record is
      Sender_Domain : Message_Manager.Domain_ID_Type;
      Sender_Module : Message_Manager.Module_ID_Type;
      Receiver_Domain : Message_Manager.Domain_ID_Type;
      Receiver_Module :Message_Manager. Module_ID_Type;
      Request_ID : Message_Manager.Request_ID_Type;
      Message_ID : Message_Manager.Message_ID_Type;
      Message_Payload_Size : Integer;
      Message : Message_Manager.Message_Record;
      --
      Payload    : Message_Manager.Data_Array      := (others => 0);
      Position : Message_Manager.Data_Index_Type := 0;
      Last_XDR     : Message_Manager.Data_Index_Type;
      Value : XDR.XDR_Unsigned;
   begin

      Sender_Domain := Message_Manager.Domain_ID_Type(Data(0));
      Sender_Module := Message_Manager.Module_ID_Type(Data(1));
      Receiver_Domain := Message_Manager.Domain_ID_Type(Data(2));
      Receiver_Module := Message_Manager.Module_ID_Type(Data(3));
      Request_ID := Message_Manager.Request_ID_Type(Data(4));
      Message_ID := Message_Manager.Message_ID_Type(Data(5));
      
      Message := Message_Manager.Make_Empty_Message
        (Sender_Address => (Sender_Domain, Sender_Module),
         Receiver_Address => (Receiver_Domain, Receiver_Module),
         Request_ID      => Request_ID,
         Message_ID      => Message_ID);
      
      for I in 6 .. Last loop
         Message.Payload(Integer(I) - 6) := XDR.XDR_Octet(Data(I));
      end loop;

      return Message;
   end Read_Stream_Message;
    
   procedure Server_Loop is
      Server        : Socket_Type;
      Address, From : Sock_Addr_Type;
      Data          : Ada.Streams.Stream_Element_Array (0 .. (Ada.Streams.Stream_Element_Offset(Message_Manager.Data_Index_Type'Last + 6)));
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
            -- Decode Stream_Elements into a CubedOS Message_Record_Type
            Message := Read_Stream_Message(Data, Last);
            Ada.Text_IO.Put_Line ("from : " & Image (From.Addr));
            if Message.Sender_Address.Domain_ID = Message_Manager.Domain_ID then
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

   procedure Send_Network_Message(Message : in Message_Manager.Message_Record )is
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Last : Ada.Streams.Stream_Element_Offset;
      Buffer : Ada.Streams.Stream_Element_Array (0 .. Ada.Streams.Stream_Element_Offset (6 + Message_Manager.Data_Index_Type'Last));
      Message_Payload_Size : Integer := Message.Payload'Length;
      Payload    : Message_Manager.Data_Array      := (others => 0);
      Position : Message_Manager.Data_Index_Type := 0;
      Last_XDR     : Message_Manager.Data_Index_Type;
   begin
      
      -- Replace addresses with localhost for local testing
      if Message_Manager.Domain_ID = 1 then
         -- DomainA sending to DomainB
         Address.Port := 50001;
         Address.Addr := Inet_Addr ("54.87.1.233");
      else
         -- DomainB sending to DomainA
         Address.Port := 50000;
         Address.Addr := Inet_Addr ("54.226.181.101");
      end if;
      
      XDR.Encode(XDR.XDR_Unsigned(Integer'Pos(Message_Payload_Size)), Payload, Position, Last_XDR);      
      Position := Last_XDR + 1; -- currently not utilized

      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      
      Buffer (0) := Ada.Streams.Stream_Element (Message.Sender_Address.Domain_ID); -- Sender Domain
      Buffer (1) := Ada.Streams.Stream_Element (Message.Sender_Address.Module_ID); -- Sender Module
      Buffer (2) := Ada.Streams.Stream_Element (Message.Receiver_Address.Domain_ID); -- Receiver Domain
      Buffer (3) := Ada.Streams.Stream_Element (Message.Receiver_Address.Module_ID); -- Receiver Module
      Buffer (4) := Ada.Streams.Stream_Element (Message.Request_ID); -- Request ID
      Buffer (5) := Ada.Streams.Stream_Element (Message.Message_ID); -- Message ID 

      for I in 6 .. Message.Payload'Length loop
         Buffer (Ada.Streams.Stream_Element_Offset(I)) := Ada.Streams.Stream_Element(Message.Payload(I - 6));
      end loop;
   
      Send_Socket (Socket, Buffer, Last, Address);        
   end Send_Network_Message;
   
    -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Manager.Message_Record) is
   begin
      -- should there be some check here?
      Ada.Text_IO.Put_Line("Sending from domain " & Message_Manager.Domain_ID_Type'Image(Message_Manager.Domain_ID));
      Ada.Text_IO.Put_Line(Message_Manager.Stringify_Message(Message));
      Send_Network_Message(Message);
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
         Message_Manager.Fetch_Message(Name_Resolver.Network_Server, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;
   
   
end CubedOS.Network.Messages;
