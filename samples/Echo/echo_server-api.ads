--------------------------------------------------------------------------------
-- FILE   : echo_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;
with System;

package Echo_Server.API is

   type Status_Type is (Success, Failure);

   type Message_Type is (Ball, Init);

   function Init_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record;

   function Ponged_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type;
      Priority      : System.Priority := System.Default_Priority;
      Send_Return   : Boolean) return Message_Record with
      Global => null;

   function Is_Ponged (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos (Ball));

      function Is_Init (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos (Init));

   procedure Ponged_Decode
     (Message : Message_Manager.Message_Record; Send_Return : out Boolean);

end Echo_Server.API;
