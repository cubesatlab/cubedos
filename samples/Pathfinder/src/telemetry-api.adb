--------------------------------------------------------------------------------
--
--  FILE   : Telemetry-api.adb
--  SUBJECT: Body of a package that simplifies use of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

package body Telemetry.API is

   function Telemetry_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain, Domain_ID, Sender, ID, Request_ID,
           Message_Type'Pos (Telemetry_Request), Priority);
   begin
      --  Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Telemetry_Encode;
end Telemetry.API;
