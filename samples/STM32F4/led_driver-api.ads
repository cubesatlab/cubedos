--------------------------------------------------------------------------------
-- FILE   : led_driver-api.ads
-- SUBJECT: Specification of the LED driver API package.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;
with Name_Resolver;
with System;

package LED_Driver.API is

   type Message_Type is (On_Request, Off_Request, All_On_Request, All_Off_Request);

   function On_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      LED            : in LED_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Off_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      LED            : in LED_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function All_On_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function All_Off_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;


   function Is_On_Request(Message : in Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.LED_Driver and
        Message.Message_ID = Message_Type'Pos(On_Request));

   function Is_Off_Request(Message : in Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.LED_Driver and
        Message.Message_ID = Message_Type'Pos(Off_Request));

   function Is_All_On_Request(Message : in Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.LED_Driver and
        Message.Message_ID = Message_Type'Pos(All_On_Request));

   function Is_All_Off_Request(Message : in Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.LED_Driver and
        Message.Message_ID = Message_Type'Pos(All_Off_Request));


   procedure On_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type;
      LED     : out LED_Type)
   with
     Global => null,
     Pre => Is_On_Request(Message),
     Depends => ((Decode_Status, LED) => Message);

   procedure Off_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type;
      LED     : out LED_Type)
   with
     Global => null,
     Pre => Is_Off_Request(Message),
     Depends => ((Decode_Status, LED) => Message);

   procedure All_On_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_All_On_Request(Message),
     Depends => (Decode_Status => null, null => Message);

   procedure All_Off_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_All_Off_Request(Message),
     Depends => (Decode_Status => null, null => Message);

end LED_Driver.API;
