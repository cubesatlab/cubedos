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
with System;

package CubedOS.Log_Server.API is

   -- This type is not needed right now. It is intended to be used in replies to indicate success or failure
   -- of a request. However, the Log Server never sends any reply messages. Maybe later?
   --type Status_Type is (Success, Failure);

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
     (Sender_Domain : in Domain_ID_Type;
      Sender        : in Module_ID_Type;
      Log_Level     : in Log_Level_Type;
      Text          : in String)
     with Pre => Text'Length <= Maximum_Log_Message_Size;


   function Log_Text_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Log_Level  : Log_Level_Type;
      Text       : String;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre    => Text'Length <= Maximum_Log_Message_Size;


   function Is_A_Log_Text(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Log_Text));


   procedure Log_Text_Decode
     (Message   : in  Message_Record;
      Log_Level : out Log_Level_Type;
      Text      : out String;
      Size      : out Log_Message_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global  => null,
     Pre     => Is_A_Log_Text(Message) and Text'Length >= Maximum_Log_Message_Size,
     Depends => (Text =>+ Message, (Log_Level, Size, Decode_Status) => Message);

end CubedOS.Log_Server.API;
