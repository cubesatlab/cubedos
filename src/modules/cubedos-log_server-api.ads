--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server-api.ads
-- SUBJECT: Specification of a package that defines the Log_Server API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Name_Resolver;
with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;
with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.Log_Server.API is

   This_Module : constant Module_ID_Type := Name_Resolver.Log_Server;

   -- This type is not needed right now. It is intended to be used in replies to indicate success or failure
   -- of a request. However, the Log Server never sends any reply messages. Maybe later?
   -- type Status_Type is (Success, Failure);

   -- The maximum size of a log message.
   -- The other types here are for convenience only, but notice that Log_Message_Type is always 128 characters.
   -- In contrast, if a short string is sent to Log_Message, only a short string is encoded.
   Maximum_Log_Message_Size : constant := 128;
   subtype Log_Message_Size_Type is Natural range 0 .. Maximum_Log_Message_Size;
   subtype Log_Message_Index_Type is Positive range 1 .. Maximum_Log_Message_Size;
   subtype Log_Message_Type is String(1 .. Maximum_Log_Message_Size);

   -- Different log levels to indicate degree of "seriousness" of a message.
   type Log_Level_Type is (Debug, Informational, Warning, Error, Critical);

   type Message_Type is
      (Log_Text);

   Log_Text_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Log_Text));

   This_Receives: aliased constant Message_Type_Array := (0 => Log_Text_Msg);

   Mail_Target : aliased constant Module_Metadata := Declare_Receives(This_Module, This_Receives'Access);

   procedure Log_Text_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Level : Log_Level_Type;
      Msg_Content : String;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then (0 < Msg_Content'Length and Msg_Content'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Log_Text_Msg),
      Post => Message_Types.Message_Type(Result) = Log_Text_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Log_Text
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Level : Log_Level_Type;
      Msg_Content : String;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then (0 < Msg_Content'Length and Msg_Content'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Log_Text_Msg)
      ;

   function Is_Log_Text(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Log_Text_Msg);
   procedure Log_Text_Decode
      (Message : in  Message_Record;
      Level : out Log_Level_Type;
      Msg_Content : out String;
      Msg_Content_Size : out Natural;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Log_Text(Message) and Payload(Message) /= null,
      Depends => ((Level, Msg_Content, Msg_Content_Size, Decode_Status) => Message);

end CubedOS.Log_Server.API;
