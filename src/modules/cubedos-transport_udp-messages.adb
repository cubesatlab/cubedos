--------------------------------------------------------------------------------
-- FILE   : cubedos-transport_udp-messages.ads
-- SUBJECT: body package for the UDP implementation of the CubedOS Message Service.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with GNAT.Sockets;   use GNAT.Sockets;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with CubedOS.Lib.XDR;
use  CubedOS.Lib;

with CubedOS.Message_Types.Mutable; use CubedOS.Message_Types.Mutable;
with Network_Configuration;

package body CubedOS.Transport_UDP.Messages is
   use Message_Manager;
   use Ada.Streams;

   procedure Init is
   begin
      null;
   end Init;

   protected Send_Lock
   -- Here to prevent sending messages before we're
   -- ready to receive them over UDP.
      with SPARK_Mode
   is
      entry Wait;
      procedure Unlock;
   private
      Locked : Boolean := True;
   end Send_Lock;


   protected Send_Queue
     with SPARK_Mode
   is
      procedure Add(Msg : in out Msg_Owner)
        with Pre => Msg /= null,
        Post => Msg = null;
      entry Take(Msg : in out Msg_Owner)
        with Pre => Msg = null,
        Post => Msg /= null;
   private
      Queued : Msg_Owner;
      Waiting : Boolean := False;
   end Send_Queue;

   procedure Send(Msg : in out Msg_Owner) is
   begin
      Send_Queue.Add(Msg);
   end Send;

   protected body Send_Lock
      with SPARK_Mode
   is
      entry Wait when not Locked is
      begin
         null;
      end;
      procedure Unlock is
      begin
         Locked := False;
      end Unlock;
   end Send_Lock;

   protected body Send_Queue
     with SPARK_Mode
   is
      procedure Add(Msg : in out Msg_Owner) is
      begin
         if Queued = null then
            Queued := Msg;
            Waiting := True;
         else
            Delete(Msg);
         end if;
         Msg := null;
         pragma Unused(Msg);
      end Add;
      entry Take(Msg : in out Msg_Owner) when Waiting is
      begin
         pragma Assume(if Waiting then Queued /= null);
         Msg := Queued;
         Queued := null;
         Waiting := False;
      end Take;
   end Send_Queue;

   function Read_Stream_Message ( Data : Ada.Streams.Stream_Element_Array; Last : Ada.Streams.Stream_Element_Offset) return Message_Record
     with SPARK_Mode,
     Pre => Data'Length >= 7 -- Enough space at least for the metadata
     and Data'First = 0
     and Last >= 7
     and Last = Stream_Element_Offset(Data'Length - 1)
   is
      Sender_Domain : Domain_ID_Type;
      Raw_Module_ID : Octet;
      Sender_Module : Module_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Receiver_Module : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message_Type : Universal_Message_Type;
      Message : Mutable_Message_Record;
      Result : Message_Record;
   begin
      -- Read metadata
      Sender_Domain := Domain_ID_Type(Data(0));
      Raw_Module_ID := Octet(Data(1));
      if Raw_Module_ID = 0 then
         --TODO: Handle this case by refusing to decode a message completely
         Sender_Module := 255;
      else
         Sender_Module := Module_ID_Type(Raw_Module_ID);
      end if;
      Receiver_Domain := Domain_ID_Type(Data(2));
      Raw_Module_ID := Octet(Data(3));
      if Raw_Module_ID = 0 then
         --TODO: Handle this case
         Receiver_Module := 255;
      else
         Receiver_Module := Module_ID_Type(Raw_Module_ID);
      end if;
      Request_ID := Request_ID_Type(Data(4));
      Message_Type := (Module_ID_Type(Data(5)), Message_ID_Type(Data(6)));

      Message := Make_Empty_Message
        (Sender_Address => (Sender_Domain, Sender_Module),
         Receiver_Address => (Receiver_Domain, Receiver_Module),
         Request_ID      => Request_ID,
         Message_Type      => Message_Type,
         Payload_Size     => Positive(Last) - 6);

      -- Read payload
      for I in 7 .. Last loop
         Message.Payload(Integer(I) - 7) := XDR.XDR_Octet(Data(I));
      end loop;

      Result := Immutable(Message);
      Delete(Message);
      return Result;
   end Read_Stream_Message;

   procedure Server_Loop
   is
      Server        : Socket_Type;
      Address, From : Sock_Addr_Type;
      Data          : Ada.Streams.Stream_Element_Array (0 .. (Ada.Streams.Stream_Element_Offset(Max_Message_Size + 6)));
      Last          : Ada.Streams.Stream_Element_Offset;
      Message : Msg_Owner;
   begin
      Create_Socket (Server, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Address.Addr := Any_Inet_Addr;
      Address.Port := Network_Configuration.Get_Port(This_Domain.ID);
      Bind_Socket (Server, Address);
      Ada.Text_IO.Put_Line("UDP Transport module started listening " & Image(Address));
      Send_Lock.Unlock;
      loop
         begin
            GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
            Ada.TExt_IO.Put_Line("Received something");
            Message := new Message_Record'(Read_Stream_Message(Data, Last));
            Ada.Text_IO.Put_Line ("from : " & Image (From.Addr));
            if Sender_Address(Message).Domain_ID = Message_Manager.Domain_ID then
               Ada.Text_IO.Put_Line ("This message was sent from this domain! Dropping Message");
            else
               Message_Manager.Handle_Received(Message);
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Exception_Name (E) & ": " & Exception_Message (E));
         end;
      end loop;
   end Server_Loop;

   procedure Encode_Network_Message(Msg : in Message_Record; Buffer : in out Ada.Streams.Stream_Element_Array; Result_Last : out Ada.Streams.Stream_Element_Count)
     with SPARK_Mode,
     Pre => Payload(Msg) /= null
     and then Payload(Msg)'Length rem 4 = 0
     and then Buffer'Length =  7 + Max_Message_Size + 1
   is
      Last : Ada.Streams.Stream_Element_Offset := 0;
   begin
      -- Encode message metadata
      Buffer (0) := Ada.Streams.Stream_Element (Sender_Address(Msg).Domain_ID); -- Sender Domain
      Buffer (1) := Ada.Streams.Stream_Element (Sender_Address(Msg).Module_ID); -- Sender Module
      Buffer (2) := Ada.Streams.Stream_Element (Receiver_Address(Msg).Domain_ID); -- Receiver Domain
      Buffer (3) := Ada.Streams.Stream_Element (Receiver_Address(Msg).Module_ID); -- Receiver Module
      Buffer (4) := Ada.Streams.Stream_Element (Request_ID(Msg)); -- Request ID
      Buffer (5) := Ada.Streams.Stream_Element (Message_Type(Msg).Module_ID); -- Message type module id
      Buffer (6) := Ada.Streams.Stream_Element (Message_Type(Msg).Message_ID); -- Message type message id

      -- Encode message content
      for I in 7 .. Message_Types.Payload(Msg).all'Length loop
         Buffer (Ada.Streams.Stream_Element_Offset(I)) := Ada.Streams.Stream_Element(Message_Types.Payload(Msg)(I - 7));
         Last := Ada.Streams.Stream_Element_Offset(I);
      end loop;
      Result_Last := Last;
   end;

   procedure Send_Network_Message(Message : in Message_Record)
     with SPARK_Mode,
     Pre => Payload(Message) /= null
     and then Payload(Message)'Length rem 4 = 0
   is
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Buffer : Ada.Streams.Stream_Element_Array (0 .. Ada.Streams.Stream_Element_Offset (7 + Max_Message_Size));
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Address.Port :=  Network_Configuration.Get_Port(Receiver_Address(Message).Domain_ID);
      Address.Addr :=  Network_Configuration.Get_Address(Receiver_Address(Message).Domain_ID);
      Encode_Network_Message(Message, Buffer, Last);
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Send_Socket (Socket, Buffer, Last, Address);
      pragma Unreferenced(Last);
   end Send_Network_Message;

   procedure Process(Message : in Message_Record)
     with SPARK_Mode,
     Pre => Payload(Message) /= null
   is
   begin
      -- should there be some check here?
      Send_Network_Message(Message);
   end Process;

   task body Incoming_Loop
   with SPARK_Mode
   is
   begin
      Message_Manager.Wait;
      Server_Loop;
   end Incoming_Loop;

   task body Outgoing_Loop is
      To_Transmit : Msg_Owner;
   begin
      Message_Manager.Wait;
      Send_Lock.Wait;

      loop
         Send_Queue.Take(To_Transmit);
         Process(To_Transmit.all);
      end loop;
   end Outgoing_Loop;


end CubedOS.Transport_UDP.Messages;
