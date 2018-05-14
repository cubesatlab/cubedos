--------------------------------------------------------------------------------
-- FILE   : cubedos-cfdp-api.adb
-- SUBJECT: Body of a package that declares the interface to the CFDP module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;

package body CubedOS.CFDP.API is
   use CubedOS.Lib;

   function Put_Request_Message
     (Sender_Domain : Domain_ID_Type;
      Sender      : Module_ID_Type;
      Destination : Entity_ID;
      Source_File : String;
      Destination_File : String) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => 0,
         Message_ID => Message_Type'Pos(Put_Request));

      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Destination), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Source_File'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Source_File, Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Destination_File'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Destination_File, Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Put_Request_Message;


end CubedOS.CFDP.API;
