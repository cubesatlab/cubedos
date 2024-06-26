--------------------------------------------------------------------------------
-- FILE   : led_driver-api.adb
-- SUBJECT: Body of the LED driver API package.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
use  CubedOS.Lib;

package body LED_Driver.API is
   use type XDR.XDR_Unsigned;

   pragma Warnings (Off, "*may call Last_Chance_Handler");

   function On_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      LED            : in LED_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.LED_Driver, Request_ID, Message_Type'Pos(On_Request), Priority);
      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(LED_Type'Pos(LED)), Message.Payload, Position, Last);
      Position := Last + 1;

      Message.Size := Position;
      return Message;
   end On_Request_Encode;


   function Off_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      LED            : in LED_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.LED_Driver, Request_ID, Message_Type'Pos(Off_Request), Priority);
      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(LED_Type'Pos(LED)), Message.Payload, Position, Last);
      Position := Last + 1;

      Message.Size := Position;
      return Message;
   end Off_Request_Encode;


   function All_On_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.LED_Driver, Request_ID, Message_Type'Pos(All_On_Request), Priority);
   begin
      Message.Size := 0;
      return Message;
   end All_On_Request_Encode;


   function All_Off_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Address, Name_Resolver.LED_Driver, Request_ID, Message_Type'Pos(All_Off_Request), Priority);
   begin
      Message.Size := 0;
      return Message;
   end All_Off_Request_Encode;


   procedure On_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type;
      LED     : out LED_Type)
   is
      Position : Data_Index_Type;
      Last     : Data_Index_Type;
      Raw_LED  : XDR.XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_LED, Last);
      if Raw_LED > LED_Type'Pos(LED_Type'Last) then
         LED := Red;
         Decode_Status := Malformed;
      else
         LED := LED_Type'Val(Raw_LED);
         Decode_Status := Success;
      end if;
   end On_Request_Decode;


   procedure Off_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type;
      LED     : out LED_Type)
   is
      Position : Data_Index_Type;
      Last     : Data_Index_Type;
      Raw_LED  : XDR.XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_LED, Last);
      if Raw_LED > LED_Type'Pos(LED_Type'Last) then
         LED := Red;
         Decode_Status := Malformed;
      else
         LED := LED_Type'Val(Raw_LED);
         Decode_Status := Success;
      end if;
   end Off_Request_Decode;


   pragma Warnings(Off, "parameter ""Message"" is not referenced");

   procedure All_On_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   is
   begin
      Decode_Status := Success;
   end All_On_Request_Decode;


   procedure All_Off_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   is
   begin
      Decode_Status := Success;
   end All_Off_Request_Decode;

end LED_Driver.API;
