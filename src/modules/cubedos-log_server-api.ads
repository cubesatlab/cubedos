--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server-api.ads
-- SUBJECT: Specification of the logger's API package
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;
with Name_Resolver;
with System;

package CubedOS.Log_Server.API is

   This_Module : constant Module_ID_Type := Name_Resolver.Log_Server.Module_ID;

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

   -- Convenience procedure that creates and sends a log message.
   procedure Log_Message
     (Sender_Address : in Message_Address;
      Log_Level      : in Log_Level_Type;
      Text           : in String)
     with Pre => Text'Length <= Maximum_Log_Message_Size;


   function Log_Text_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Log_Level      : in Log_Level_Type;
      Text           : in String;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre    => Text'Length <= Maximum_Log_Message_Size;


   function Is_A_Log_Text(Message : in Message_Record) return Boolean is
     (Message_Manager.Message_Type(Message) = (This_Module, Message_Type'Pos(Log_Text)));


   procedure Log_Text_Decode
     (Message   : in  Message_Record;
      Log_Level : out Log_Level_Type;
      Text      : out String;
      Size      : out Log_Message_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global  => null,
     Depends => (Text =>+ Message, (Log_Level, Size, Decode_Status) => Message),
     Pre     => Is_A_Log_Text(Message) and Text'Length >= Maximum_Log_Message_Size;

end CubedOS.Log_Server.API;
