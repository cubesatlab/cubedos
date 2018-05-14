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
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message(Sender_Domain, Domain_ID, Sender, ID, Message_Type'Pos(Ball), Priority);
   begin
      return Message;
   end Ponged_Encode;


   procedure Ponged_Decode(Message : in  Message_Record) is
   begin
      -- Decode the given message and return via out parameters (not shown) the fields.
      null;
   end Ponged_Decode;

end pong.API;
