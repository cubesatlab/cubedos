--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
use  CubedOS.Lib;

package body CubedOS.Interpreter.API is

   function Clear_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.Interpreter, Request_ID, Message_Type'Pos(Clear_Request), Priority);
   begin
      -- Fill in the message by encoding the other parameters (not shown) as required.
      return Message;
   end Clear_Request_Encode;


   function Set_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.Interpreter, Request_ID, Message_Type'Pos(Set_Request), Priority);
   begin
      -- Fill in the message by encoding the other parameters (not shown) as required.
      return Message;
   end Set_Request_Encode;


   function Set_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Status           : in Status_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
   is
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Name_Resolver.Interpreter, Receiver_Address, Request_ID, Message_Type'Pos(Set_Reply), Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      -- Set a starting position.
      Position := 0;

      -- Encode one parameter (decoding logic must be consistent).
      -- Set Position to get ready for the next parameter.
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Position := Last + 1;

      -- Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Set_Reply_Encode;


   function Add_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.Interpreter, Request_ID, Message_Type'Pos(Add_Request), Priority);
   begin
      -- Fill in the message by encoding the other parameters (not shown) as required.
      return Message;
   end Add_Request_Encode;


   function Add_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Status           : in Status_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
   is
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Name_Resolver.Interpreter, Receiver_Address, Request_ID, Message_Type'Pos(Add_Reply), Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      -- Set a starting position.
      Position := 0;

      -- Encode one parameter (decoding logic must be consistent).
      -- Set Position to get ready for the next parameter.
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Position := Last + 1;

      -- Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Add_Reply_Encode;


   procedure Clear_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type) is
   begin
      -- Decode the given message and return via out parameters (not shown) the fields.
      null;
   end Clear_Request_Decode;


   procedure Set_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type) is
   begin
      -- Decode the given message and return via out parameters (not shown) the fields.
      null;
   end Set_Request_Decode;


   procedure Set_Reply_Decode (Message : in  Message_Record; Decode_Status : out Message_Status_Type) is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
      Value     : Positive; -- Commonly, this would be an out parameter.
   begin
      -- Set a starting position.
      Position := 0;

      -- Decode one parameter (encoding logic must be consistent).
      -- Set position to get ready for next parameter.
      XDR.Decode(Message.Payload, Position, Raw_Value, Last);
      Position := Last + 1;

      -- Convert raw XDR primitive type into appropriate result. Note runtime check needed!
      if Integer(Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value := Positive(Raw_Value);
         Decode_Status := Success;
      end if;
   end Set_Reply_Decode;


   procedure Add_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type) is
   begin
      -- Decode the given message and return via out parameters (not shown) the fields.
      null;
   end Add_Request_Decode;


   procedure Add_Reply_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type) is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
      Value     : Positive; -- Commonly, this would be an out parameter.
   begin
      -- Set a starting position.
      Position := 0;

      -- Decode one parameter (encoding logic must be consistent).
      -- Set position to get ready for next parameter.
      XDR.Decode(Message.Payload, Position, Raw_Value, Last);
      Position := Last + 1;

      -- Convert raw XDR primitive type into appropriate result. Note runtime check needed!
      if Integer(Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value := Positive(Raw_Value);
         Decode_Status := Success;
      end if;
   end Add_Reply_Decode;

end CubedOS.Interpreter.API;
