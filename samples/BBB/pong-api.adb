--------------------------------------------------------------------------------
-- FILE   : pong-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Lib.XDR;
use CubedOS.Lib;

package body Pong.API is
   use type XDR.XDR_Unsigned;

   function Init_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record := Make_Empty_Message
          (Sender_Domain => Sender_Domain, Receiver_Domain => Domain_ID,
           Sender        => Sender, Receiver => ID, Request_ID => Request_ID,
           Message_ID    => Message_Type'Pos (Init), Priority => Priority);
   begin
      return Message;
   end Init_Encode;

   function Ponged_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type;
      Priority      : System.Priority := System.Default_Priority;
      Send_Return   : Boolean) return Message_Record
   is
      Position : Data_Index_Type := 0;
      Last     : Data_Index_Type;

      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain => Sender_Domain, Receiver_Domain => Domain_ID,
           Sender        => Sender, Receiver => ID, Request_ID => Request_ID,
           Message_ID    => Message_Type'Pos (Ball), Priority => Priority);
   begin
      Position := 0;
      XDR.Encode
        (XDR.XDR_Unsigned (Boolean'Pos (Send_Return)), Message.Payload,
         Position, Last);
      Position := Last + 1;
      return Message;
   end Ponged_Encode;

   procedure Ponged_Decode
     (Message : in Message_Record; Send_Return : out Boolean)
   is
      Position        : Data_Index_Type;
      Last            : Data_Index_Type;
      Raw_Send_Return : XDR.XDR_Unsigned;
   begin
      -- need to add check if raw_send_return is valid.
      Position := 0;
      XDR.Decode (Message.Payload, Position, Raw_Send_Return, Last);
      Send_Return := Boolean'Val (Raw_Send_Return);

   end Ponged_Decode;

end Pong.API;
