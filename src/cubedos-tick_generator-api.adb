--------------------------------------------------------------------------------
-- FILE   : cubedos-tick_generator-api.adb
-- SUBJECT: Body of a package that simplifies use of the tick generator.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.Tick_Generator.API is
   use type XDR.XDR_Unsigned;

   function Relative_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender        : Module_ID_Type;
      Request_ID    : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type  : Series_Type;
      Series_ID     : Series_ID_Type;
      Priority      : System.Priority := System.Default_Priority) return Message_Record
   is
      Message    : Message_Record;
      Position   : XDR_Index_Type;
      Last       : XDR_Index_Type;
      Interval   : constant Duration := Ada.Real_Time.To_Duration(Tick_Interval);
   begin
      Message := Make_Empty_Message
        (Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Relative_Request),
         Priority   => Priority);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_Type'Pos(Request_Type)), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Relative_Request_Encode;


   function Absolute_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Tick_Time  : Ada.Real_Time.Time;
      Series_ID  : Series_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message    : Message_Record;
      Position   : XDR_Index_Type;
      Last       : XDR_Index_Type;
      Seconds    : Ada.Real_Time.Seconds_Count;
      Fraction   : Ada.Real_Time.Time_Span;
   begin
      Message := Make_Empty_Message
        (Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Absolute_Request),
         Priority   => Priority);

      Ada.Real_Time.Split(Tick_Time, Seconds, Fraction);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return message;
   end Absolute_Request_Encode;


   function Tick_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Series_ID  : Series_ID_Type;
      Count      : Series_Count_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Result : Message_Record := Make_Empty_Message
        (Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Tick_Reply),
         Priority   => Priority);

      Position : XDR_Index_Type;
      Last     : XDR_Index_Type;
   begin
      Position := 0;

      -- The only kind of messages coming from the tick generator are ticks.
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Result.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Count), Result.Payload, Position, Last);
      Result.Size := Last + 1;
      return Result;
   end Tick_Reply_Encode;


   function Cancel_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Series_ID  : Series_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message    : Message_Record;
      Position   : XDR_Index_Type;
      Last       : XDR_Index_Type;
   begin
      Message := Make_Empty_Message
        (Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Cancel_Request),
         Priority   => Priority);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return message;
   end Cancel_Request_Encode;


   procedure Relative_Request_Decode
     (Message       : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type  : out Series_Type;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position         : XDR_Index_Type;
      Raw_Interval     : XDR.XDR_Unsigned;
      Raw_Request_Type : XDR.XDR_Unsigned;
      Raw_Series_ID    : XDR.XDR_Unsigned;
      Last             : XDR_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Tick_Interval := Ada.Real_Time.Time_Span_First;
      Request_Type := Series_Type'First;
      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Interval, Last);
      Position := Last + 1;

      if Raw_Interval > XDR.XDR_Unsigned(Integer'Last) then
         Decode_Status := Malformed;
      else
         Tick_Interval := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));

         XDR.Decode(Message.Payload, Position, Raw_Request_Type, Last);
         Position := Last + 1;

         if Raw_Request_Type > Series_Type'Pos(Series_Type'Last) then
            Decode_Status := Malformed;
         else
            Request_Type := Series_Type'Val(Raw_Request_Type);

            XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);

            if Raw_Series_ID not in
              XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
               Decode_Status := Malformed;
            else
               Series_ID := Series_ID_Type(Raw_Series_ID);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Relative_Request_Decode;


   procedure Absolute_Request_Decode
     (Message       : in  Message_Record;
      Tick_Time     : out Ada.Real_Time.Time;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position      : XDR_Index_Type;
      Raw_Seconds   : XDR.XDR_Unsigned;
      Seconds       : Ada.Real_Time.Seconds_Count;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Last          : XDR_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Seconds, Last);
      Position := Last + 1;

      Seconds := Ada.Real_Time.Seconds_Count(Raw_Seconds);
      Tick_Time := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);

      XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Series_ID := Series_ID_Type(Raw_Series_ID);
         Decode_Status := Success;
      end if;
   end Absolute_Request_Decode;


   procedure Tick_Reply_Decode
     (Message   : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count     : out Natural;
      Decode_Status : out Message_Status_Type)
   is
      Position      : XDR_Index_Type;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Raw_Count     : XDR.XDR_Unsigned;
      Last          : XDR_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Series_ID := Series_ID_Type'First;
      Count := 0;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
      Position := Last + 1;
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Series_ID := Series_ID_Type(Raw_Series_ID);

         XDR.Decode(Message.Payload, Position, Raw_Count, Last);
         if Raw_Count > XDR.XDR_Unsigned(Natural'Last) then
            Decode_Status := Malformed;
         else
            Count := Natural(Raw_Count);
            Decode_Status := Success;
         end if;
      end if;
   end Tick_Reply_Decode;


   procedure Cancel_Request_Decode
     (Message       : in  Message_Record;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position      : XDR_Index_Type;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Last          : XDR_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");

      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Series_ID := Series_ID_Type(Raw_Series_ID);
         Decode_Status := Success;
      end if;
   end Cancel_Request_Decode;


end CubedOS.Tick_Generator.API;
