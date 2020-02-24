--------------------------------------------------------------------------------
-- FILE   : pong-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;  use Message_Manager;
with System;

package Pong.API is

   type Status_Type is (Success, Failure);

   type Message_Type is
     (Ball);

   function Ponged_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender   : Module_ID_Type;
      Priority : System.Priority := System.Default_Priority;
      Request_ID : Request_ID_Type) return Message_Record
   with Global => null;

   function Is_Ponged(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Ball));

   procedure Ponged_Decode
     (Message : Message_Manager.Message_Record);

end Pong.API;
