--------------------------------------------------------------------------------
-- FILE   : pong-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------


package body Pong.API is

   function Ponged_Encode
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
      -- Create an empty message to be recieved. Pong requires no additional information so
      -- there is no need to add anything to the message, the way the message is instantiated
      -- above is enough information for Pong to have a proper message recieved
      return Message;
   end Ponged_Encode;


   procedure Ponged_Decode(Message : in  Message_Record) is
   begin
      -- In this case the entire message is in the header of the message record. The caller
      -- already has that information, so no additional decoding is needed (and there is
      -- nothing to decode anyway!)
      null;
   end Ponged_Decode;

end pong.API;
