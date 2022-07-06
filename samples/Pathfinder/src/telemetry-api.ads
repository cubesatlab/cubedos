------------------------------------------------------------------------------
--
--  FILE   : Telemetry-api.ads
--  SUBJECT: Specification of a package that simplifies use of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--  All the subprograms in this package must be task safe. They can be
--  simultaneously called from multiple tasks.  If possible, make
--  every subprogram here a pure function.
--
------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Message_Manager; use Message_Manager;
with System;

package Telemetry.API is
   type Status_Type is (Success, Failure);
   type Message_Type is (Telemetry_Request);

   function Is_Telemetry_Request (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and
        Message.Message_ID = Message_Type'Pos (Telemetry_Request));

   function Telemetry_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record with
      Global => null;
end Telemetry.API;
