--------------------------------------------------------------------------------
-- FILE   : cubedos-logger-api.ads
-- SUBJECT: Specification of the logger's API package
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Logger.API is

   -- Define data types here that are only needed in this API (such as status codes). Data
   -- types that might also be used in the implementation should be in the top level package.
   type Status_Type is (Success, Failure);

   -- The maximum size of a log message.
   Maximum_Log_Message_Size : constant := 128;
   subtype Log_Message_Size_Type is Natural range 0 .. Maximum_Log_Message_Size;
   subtype Log_Message_Index_Type is Positive range 1 .. Maximum_Log_Message_Size;

   -- TODO: What messages should this package really send/receive?
   type Message_Type is
     (Log_Text);


   -- Convenience procedure that creates and sends a log message.
   procedure Log_Message
     (Sender_Domain : Domain_ID_Type; Sender : in Module_ID_Type; Text : in String);


   function Log_Text_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender   : Module_ID_Type;
      Text     : String;
      Priority : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre => Text'Length <= Maximum_Log_Message_Size;


   function Is_A_Log_Text(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Log_Text));


   procedure Log_Text_Decode
     (Message : in  Message_Record;
      Text : out String;
      Size : out Log_Message_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_A_Log_Text(Message) and Text'Length >= Maximum_Log_Message_Size,
     Depends => (Text =>+ Message, (Size, Decode_Status) => Message);

end CubedOS.Logger.API;
