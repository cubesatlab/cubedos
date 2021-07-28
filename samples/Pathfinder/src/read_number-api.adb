--------------------------------------------------------------------------------
--
--  FILE   : Read_Number-api.adb
--  SUBJECT: Body of a package that simplifies use of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Lib.XDR; use CubedOS.Lib;

package body Read_Number.API is

   function Read_Number_Reply_Encode
     (Receiver_Domain : Domain_ID_Type; Receiver : Module_ID_Type;
      Request_ID      : Request_ID_Type; Status : Status_Type := Success;
      Priority        : System.Priority := Pri; Value : Positive)
      return Message_Record
   is
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Domain_ID, Receiver_Domain, ID, Receiver, Request_ID,
           Message_Type'Pos (Read_Number_Reply), Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      --  Set a starting position.
      Position := 0;

      --  Encode one parameter (decoding logic must be consistent).
      --  Set Position to get ready for the next parameter.
      XDR.Encode
        (Value => XDR.XDR_Integer (Value),
      -- (Value => XDR.XDR_Unsigned (Status_Type'Pos (Status)),
      Data        => Message.Payload,
         Position => Position, Last => Last);
      Position := Last + 1;

      --  Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Read_Number_Reply_Encode;

   procedure Read_Number_Reply_Decode
     (Message : in     Message_Record; Decode_Status : out Message_Status_Type;
      Value   :    out Positive)
   is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
   begin
      --  Set a starting position.
      Position := 0;
      Value    := 42;

      --  Decode one parameter (encoding logic must be consistent).
      --  Set position to get ready for next parameter.
      XDR.Decode
        (Data => Message.Payload, Position => Position, Value => Raw_Value,
         Last => Last);

      Position := Last + 1;

      --  Convert raw XDR primitive type into appropriate result. Note
      --  runtime check needed!
      if Integer (Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value         := Positive (Raw_Value);
         Decode_Status := Success;
      end if;
   end Read_Number_Reply_Decode;

end Read_Number.API;
