--------------------------------------------------------------------------------
-- FILE   : ping-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------


package body Ping.API is

   function Pinged_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender   : Module_ID_Type;
      Priority : System.Priority := System.Default_Priority;
      Request_ID : Request_ID_Type) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Sender_Domain,
           Receiver_Domain => Domain_ID,
           Sender     => Sender,
           Receiver   => ID,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Ball),
           Priority   => Priority);
   begin
      return Message;
   end Pinged_Encode;


   procedure Pinged_Decode(Message : in  Message_Record) is
   begin
      -- In this case the entire message is in the header of the message record. The caller
      -- already has that information, so no additional decoding is needed (and there is
      -- nothing to decode anyway!)
      null;
   end Pinged_Decode;

  -- function Return_Request_ID(Message : in Message_Record) return Request_ID_Type
  -- is
   --begin
     --       Get_Next_Request_ID(Message.Request_ID);
       --  return Message.Request_ID;
   -- end Return_Request_ID;


end Ping.API;
