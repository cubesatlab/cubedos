--------------------------------------------------------------------------------
-- FILE   : cubedos-transport_udp-messages.ads
-- SUBJECT: body package for the UDP implementation of the CubedOS Message Service.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with GNAT.Sockets;   use GNAT.Sockets;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Network_Configuration;
with CubedOS.Lib.XDR;
use  CubedOS.Lib;

with CubedOS.Message_Types.Mutable; use CubedOS.Message_Types.Mutable;

package body CubedOS.Transport_UDP.Messages is

   procedure Init is
   begin
      null;
   end Init;

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
            pragma Unused(Msg);
         end if;
         Msg := null;
      end Add;
      entry Take(Msg : in out Msg_Owner) when Waiting is
      begin
         pragma Assert(if Waiting then Queued /= null);
         Msg := Queued;
         Queued := null;
         Waiting := False;
      end Take;
   end Send_Queue;

   function Read_Stream_Message ( Data : Ada.Streams.Stream_Element_Array; Last : Ada.Streams.Stream_Element_Offset) return Message_Record is
      Sender_Domain : Domain_ID_Type;
      Sender_Module : Module_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Receiver_Module : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message_Type : Universal_Message_Type;
      Message : Mutable_Message_Record;
   begin

      Sender_Domain := Domain_ID_Type(Data(0));
      Sender_Module := Module_ID_Type(Data(1));
      Receiver_Domain := Domain_ID_Type(Data(2));
      Receiver_Module := Module_ID_Type(Data(3));
      Request_ID := Request_ID_Type(Data(4));
      Message_Type := (Module_ID_Type(Data(5)), Message_ID_Type(Data(6)));

      Message := Make_Empty_Message
        (Sender_Address => (Sender_Domain, Sender_Module),
         Receiver_Address => (Receiver_Domain, Receiver_Module),
         Request_ID      => Request_ID,
         Message_Type      => Message_Type,
         Payload_Size     => Positive(Last) - 6);

      for I in 6 .. Last loop
         Message.Payload(Integer(I) - 6) := XDR.XDR_Octet(Data(I));
      end loop;

      return Immutable(Message);
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
      Address.Port := Network_Configuration.Get_Port(Message_Manager.Domain_ID);
      Bind_Socket (Server, Address);
      loop
         begin
            GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
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

   procedure Send_Network_Message(Message : in Message_Record )is
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Last : Ada.Streams.Stream_Element_Offset;
      Buffer : Ada.Streams.Stream_Element_Array (0 .. Ada.Streams.Stream_Element_Offset (6 + Data_Index_Type'Last));
      Message_Payload_Size : constant Integer := Payload(Message)'Length;
      Payload    : Data_Array(0 .. Message_Types.Payload(Message)'Length) := (others => 0);
      Position : Data_Index_Type := 0;
      Last_XDR     : Data_Index_Type;
   begin

      Address.Port :=  Network_Configuration.Get_Port(Receiver_Address(Message).Domain_ID);
      Address.Addr :=  Network_Configuration.Get_Address(Receiver_Address(Message).Domain_ID);

      XDR.Encode(XDR.XDR_Unsigned(Integer'Pos(Message_Payload_Size)), Payload, Position, Last_XDR);
      Position := Last_XDR + 1; -- currently not utilized

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      Buffer (0) := Ada.Streams.Stream_Element (Sender_Address(Message).Domain_ID); -- Sender Domain
      Buffer (1) := Ada.Streams.Stream_Element (Sender_Address(Message).Module_ID); -- Sender Module
      Buffer (2) := Ada.Streams.Stream_Element (Receiver_Address(Message).Domain_ID); -- Receiver Domain
      Buffer (3) := Ada.Streams.Stream_Element (Receiver_Address(Message).Module_ID); -- Receiver Module
      Buffer (4) := Ada.Streams.Stream_Element (Request_ID(Message)); -- Request ID
      Buffer (5) := Ada.Streams.Stream_Element (Message_Type(Message).Module_ID); -- Message type module id
      Buffer (6) := Ada.Streams.Stream_Element (Message_Type(Message).Message_ID); -- Message type message id

      for I in 7 .. Message_Types.Payload(Message).all'Length loop
         Buffer (Ada.Streams.Stream_Element_Offset(I)) := Ada.Streams.Stream_Element(Message_Types.Payload(Message)(I - 7));
      end loop;

      Send_Socket (Socket, Buffer, Last, Address);
   end Send_Network_Message;

   procedure Process(Message : in Message_Record)
     with SPARK_Mode
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
      Incoming_Message : Msg_Owner;
   begin
      Message_Manager.Wait;

      loop
         Send_Queue.Take(Incoming_Message);
         Process(Incoming_Message.all);
      end loop;
   end Outgoing_Loop;


end CubedOS.Transport_UDP.Messages;
