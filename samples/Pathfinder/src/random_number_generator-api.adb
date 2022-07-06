--------------------------------------------------------------------------------
-- FILE   : random_number_generator-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with Ada.Text_IO;

with PNumbers;
with CubedOS.Lib.XDR; use CubedOS.Lib;

package body Random_Number_Generator.API is

   ----------
   -- Request
   ----------
   function Generate_Number_Request_Encode
     (Sender_Address : Message_Address;
      Request_ID     : Request_ID_Type;
      Priority       : System.Priority := Pri) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Address,
           Name_Resolver.Random_Number_Generator,
           Request_ID,
           Message_Type'Pos(Generate_Number_Request),
           Priority);
   begin
      --  Fill in the message by encoding the other parameters (not shown) as required.
      return Message;
   end Generate_Number_Request_Encode;

   --------
   -- Reply
   --------
   function Generate_Number_Reply_Encode
     (Receiver_Address : Message_Address;
      Request_ID       : Request_ID_Type;
      Priority         : System.Priority := Pri) return Message_Record
   is
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Name_Resolver.Random_Number_Generator,
           Receiver_Address,
           Request_ID,
           Message_Type'Pos (Generate_Number_Reply),
           Priority);

      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Value     : constant Positive := PNumbers.Get_Random_Number;
      Call_Sign : constant String   := "Low[" & Pri'Image & " ]: ";
   begin
      Ada.Text_IO.Put(Call_Sign & "The random number is");
      Ada.Text_IO.Put_Line(Value'Image);

      --  Set a starting position.
      Position := 0;

      --  Encode one parameter (decoding logic must be consistent).
      --  Set Position to get ready for the next parameter.
      XDR.Encode
        (Value    => XDR.XDR_Integer(Value),
         Data     => Message.Payload,
         Position => Position,
         Last     => Last);
      Position := Last + 1;

      --  Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Generate_Number_Reply_Encode;


   procedure Generate_Number_Reply_Decode
     (Message       : in     Message_Record;
      Decode_Status :    out Message_Status_Type;
      Value         :    out Positive)
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
      XDR.Decode(Data => Message.Payload, Position => Position, Value => Raw_Value, Last => Last);

      --  Convert raw XDR primitive type into appropriate result. Note
      --  runtime check needed!
      if Integer(Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value         := Positive(Raw_Value);
         Decode_Status := Success;
      end if;
   end Generate_Number_Reply_Decode;

end Random_Number_Generator.API;
