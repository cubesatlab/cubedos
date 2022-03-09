--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with CubedOS.Lib.Network;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;
with Ada.Strings;       use Ada.Strings;
with Ada.Streams;
with Ada.Strings;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO;
use Ada.Text_IO;
package body CubedOS.Generic_Message_Manager
  with Refined_State => (Mailboxes => Message_Storage, Request_ID_Generator => Request_ID_Gen)
is

   -- A protected object for generating request ID values.
   protected Request_ID_Gen is
      procedure Generate_Next_ID(Request_ID : out Request_ID_Type);
   private
      Next_Request_ID : Request_ID_Type := 1;
   end Request_ID_Gen;


  -- A protected type for holding messages.
   protected type Mailbox is

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the returned status indicates this.
      procedure Send(Message : in Message_Record; Status : out Status_Type);

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the message is lost.
      procedure Unchecked_Send(Message : in Message_Record);

      -- Returns the number of messages in the mailbox.
      function Message_Count return Message_Count_Type;

      -- Receive a message. This entry waits indefinitely for a message to be available.
      entry Receive(Message : out Message_Record);

   private
      Messages : Message_Array;
      Count    : Message_Count_Type := 0;
      Next_In  : Message_Index_Type := 1;
      Next_Out : Message_Index_Type := 1;
      Message_Waiting : Boolean := False;
   end Mailbox;

   -- One mailbox for each module.
   Message_Storage : array(Module_ID_Type) of Mailbox;

   ------------------
   -- Implementations
   ------------------

   protected body Request_ID_Gen is

      procedure Generate_Next_ID(Request_ID : out Request_ID_Type) is
      begin
         Request_ID := Next_Request_ID;
         Next_Request_ID := Next_Request_ID + 1;
      end Generate_Next_ID;

   end Request_ID_Gen;


   protected body Mailbox is

      procedure Send(Message : in Message_Record; Status : out Status_Type) is
      begin
         if Count = Mailbox_Size then
            Status := Mailbox_Full;
         else
            Messages(Next_In) := Message;
            if Next_In = Mailbox_Size then
               Next_In := 1;
            else
               Next_In := Next_In + 1;
            end if;
            Count := Count + 1;
            Message_Waiting := True;
            Status := Accepted;
         end if;
      end Send;


      procedure Unchecked_Send(Message : in Message_Record) is
      begin
         if Count /= Mailbox_Size then
            Messages(Next_In) := Message;
            if Next_In = Mailbox_Size then
               Next_In := 1;
            else
               Next_In := Next_In + 1;
            end if;
            Count := Count + 1;
            Message_Waiting := True;
         end if;
      end Unchecked_Send;


      function Message_Count return Message_Count_Type is
      begin
         return Count;
      end Message_Count;


      entry Receive(Message : out Message_Record) when Message_Waiting is
      begin
         Message := Messages(Next_Out);
         if Next_Out = Mailbox_Size then
            Next_Out := 1;
         else
            Next_Out := Next_Out + 1;
         end if;
         Count := Count - 1;
         if Count = 0 then
            Message_Waiting := False;
         end if;
      end Receive;

   end Mailbox;


   function Make_Empty_Message
     (Sender_Domain   : Domain_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message_ID : Message_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
   begin
      Message.Sender_Domain   := Sender_Domain;
      Message.Receiver_Domain := Receiver_Domain;
      Message.Sender     := Sender;
      Message.Receiver   := Receiver;
      Message.Request_ID := Request_ID;
      Message.Message_ID := Message_ID;
      Message.Priority   := Priority;
      Message.Size       := 0;
      Message.Payload := (others => 0);
      return Message;
   end Make_Empty_Message;


   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type) is
   begin
      Request_ID_Gen.Generate_Next_ID(Request_ID);
   end Get_Next_Request_ID;


   procedure Route_Message(Message : in Message_Record; Status : out Status_Type) is
   begin
      -- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage(Message.Receiver).Send(Message, Status);
   end Route_Message;

   function Create_Message_Image(Message : in Message_Record) return String
   is
      -- Convert Item to String (with no trailing space)
      Sender_Domain_String : String := Trim(Domain_ID_Type'Image(Message.Sender_Domain), Left);
      Sender_Module_String : String := Trim(Module_ID_Type'Image(Message.Sender), Left);
      Receiver_Domain_String : String := Trim(Module_ID_Type'Image(Message.Receiver_Domain), Left);         
      Receiver_Module_String : String := Trim(Module_ID_Type'Image(Message.Receiver), Left);
      Request_ID_String : String := Trim(Request_ID_Type'Image(Message.Request_ID), Left);
      Message_ID_String : String := Trim(Message_ID_Type'Image(Message.Message_ID), Left);
   begin 
      Ada.Text_IO.Put_Line("Outgoing: " & Sender_Domain_String & "!" & Sender_Module_String & "!" & Receiver_Domain_String & "!" & Receiver_Module_String & "!" & Request_ID_String & "!" & Message_ID_String & "!");
      return Sender_Domain_String & "!" & Sender_Module_String & "!" & Receiver_Domain_String & "!" & Receiver_Module_String & "!" & Request_ID_String & "!" & Message_ID_String & "!";
   end Create_Message_Image;
    
   function Parse_Message_Image(Message_String : String) return Message_Record
    is
      Property_Digits : Integer := 0;
      Current_Digit : Integer := 0;
      Property_Position : Integer := 1;
      POW : Integer := 10;
      Sender_Domain : Domain_ID_Type;
      Sender_Module : Module_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Receiver_Module : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message_ID : Message_ID_Type;
    begin
      for Index in Message_String'Range loop
        if Message_String(Index) = '!' then
          case Property_Position is
            when 1 => 
              Ada.Text_IO.Put("Sender Domain: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Sender_Domain := Domain_ID_Type(Property_Digits);
            when 2 =>
              Ada.Text_IO.Put("Sender Module: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Sender_Module := Module_ID_Type(Property_Digits);
            when 3 =>
              Ada.Text_IO.Put("Receiver Domain: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Receiver_Domain := Domain_ID_Type(Property_Digits);
            when 4 =>
              Ada.Text_IO.Put("Receiver Module: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Receiver_Module := Module_ID_Type(Property_Digits);
            when 5 =>
              Ada.Text_IO.Put("Request ID: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Request_ID := Request_ID_Type(Property_Digits);
            when 6 =>
              Ada.Text_IO.Put("Message ID: ");
              Ada.Text_IO.Put_Line(Integer'Image(Property_Digits));
              Message_ID := Message_ID_Type(Property_Digits);
            when others => null;
          end case;
          Property_Position := Property_Position + 1;
          Property_Digits := 0;
        else
          Current_Digit := Integer'Value("" & Message_String(Index));
          while Current_Digit >= POW loop
             POW := POW * 10;
          end loop;
          Property_Digits := Property_Digits * POW + Current_Digit;
          POW := 10;
        end if;
        -- Property_Digits := Integer'Value(Message_String(Index)'Image);
        -- Ada.Text_IO.Put("Digits: " & Integer'Image(Property_Digits));
        -- Property_Digits := 0;
      end loop;
      
      return Make_Empty_Message
      (Sender_Domain   => Sender_Domain,
       Receiver_Domain => Receiver_Domain,
       Sender          => Sender_Module,
       Receiver        => Receiver_Module,
       Request_ID      => Request_ID,
       Message_ID      => Message_ID);
    end Parse_Message_Image;

   procedure Route_Message(Message : in Message_Record) is
    X : Integer := 10;
    Parsed_Message : Message_Record;
   begin
      -- For now, let's ignore the domain and just use the receiver Module_ID only.
      if Message.Receiver_Domain /= Domain_ID then
        -- Parsed_Message := Parse_Message_Image(Create_Message_Image(Message));
        CubedOS.Lib.Network.Send_Network_Message(Create_Message_Image(Message));
      else 
        Message_Storage(Message.Receiver).Unchecked_Send(Message);
      end if;
   end Route_Message;

   procedure Fetch_Message(Module : in Module_ID_Type; Message : out Message_Record) is
   begin
      Message_Storage(Module).Receive(Message);
   end Fetch_Message;


   procedure Get_Message_Counts(Count_Array : out Message_Count_Array) is
   begin
      for I in Module_ID_Type loop
         Count_Array(I) := Message_Storage(I).Message_Count;
      end loop;
   end Get_Message_Counts;

end CubedOS.Generic_Message_Manager;
