--------------------------------------------------------------------------------
--
-- FILE    : SAMPLE_MODULE-api.adb
-- SUBJECT : Body of a package that simplifies use of the module.
-- AUTHOR  : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Random_Number_Generator.API;

package body System_Bus.API is

   -----------------
   -- Random Number
   -----------------
   function Random_Number_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain => Sender_Domain, Receiver_Domain => Domain_ID,
           Sender => Sender, Receiver => ID, Request_ID => Request_ID,
           Message_ID => Message_Type'Pos (Random_Number_Request),
           Priority => Priority);
   begin
      -- Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Random_Number_Request_Encode;

   function Random_Number_Reply_Encode
     (Receiver_Domain : Domain_ID_Type; Receiver : Module_ID_Type;
      Request_ID : Request_ID_Type; Status : Status_Type := Success;
      Priority : System.Priority := Pri)
      return Message_Record
   is
      pragma Unreferenced (Status);
      -- The skeletal message knows its sender (this module).
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain => Domain_ID, Receiver_Domain => Receiver_Domain,
           Sender => ID, Receiver => Receiver, Request_ID => Request_ID,
           Message_ID => Message_Type'Pos (Random_Number_Reply),
           Priority => Priority);
   begin
      return Message;
   end Random_Number_Reply_Encode;

   procedure Random_Number_Reply_Decode
     (Message : in Message_Record; Decode_Status : out Message_Status_Type;
      Value : out Positive)
   is
   begin
      Random_Number_Generator.API.Generate_Number_Reply_Decode
        (Message, Decode_Status, Value);
   end Random_Number_Reply_Decode;

   -------------
   -- Telemetry
   -------------
   function Telemetry_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain => Sender_Domain, Receiver_Domain => Domain_ID,
           Sender => Sender, Receiver => ID, Request_ID => Request_ID,
           Message_ID => Message_Type'Pos (Telemetry), Priority => Priority);
   begin
      -- Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Telemetry_Encode;

end System_Bus.API;
