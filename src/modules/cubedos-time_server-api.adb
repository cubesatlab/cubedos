--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the time_server.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.Time_Server.API is
   use type XDR.XDR_Unsigned;

   pragma Warnings
     (GNATprove, Off, """Last"" is set by ""Decode"" but not used",
      Reason  => "The last value of Last is not needed");

   function Relative_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Tick_Interval  : in Ada.Real_Time.Time_Span;
      Request_Type   : in Series_Type;
      Series_ID      : in Series_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record := Make_Empty_Message
        (Sender_Address   => Sender_Address,
         Receiver_Address => Name_Resolver.Time_Server,
         Request_ID => Request_ID,
         Message_Type => (This_Module, Message_Type'Pos(Relative_Request)),
         Payload_Size => Message_Manager.Max_Message_Size,
         Priority   => Priority);
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      Interval   : constant Duration := Ada.Real_Time.To_Duration(Tick_Interval);
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_Type'Pos(Request_Type)), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Relative_Request_Encode;


   function Absolute_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Tick_Time      : in Ada.Real_Time.Time;
      Series_ID      : in Series_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record := Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Name_Resolver.Time_Server,
         Request_ID => Request_ID,
         Message_Type => (This_Module, Message_Type'Pos(Absolute_Request)),
         Payload_Size => Message_Manager.Max_Message_Size,
         Priority   => Priority);
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      Seconds    : Ada.Real_Time.Seconds_Count;
      Fraction   : Ada.Real_Time.Time_Span;
   begin
      Ada.Real_Time.Split(Tick_Time, Seconds, Fraction);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Absolute_Request_Encode;


   function Tick_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Series_ID        : in Series_ID_Type;
      Count            : in Series_Count_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
   is
      Result : constant Mutable_Message_Record := Make_Empty_Message
        (Sender_Address   => Name_Resolver.Time_Server,
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Message_Type => (This_Module, Message_Type'Pos(Tick_Reply)),
         Payload_Size => Message_Manager.Max_Message_Size,
         Priority   => Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      Position := 0;

      -- The only kind of messages coming from the tick generator are ticks.
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Result.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Count), Result.Payload.all, Position, Last);
      return Immutable(Result);
   end Tick_Reply_Encode;


   function Cancel_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Series_ID      : in Series_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record := Make_Empty_Message
        (Sender_Address   => Sender_Address,
         Receiver_Address => Name_Resolver.Time_Server,
         Request_ID => Request_ID,
         Message_Type => (This_Module, Message_Type'Pos(Cancel_Request)),
         Payload_Size => Message_Manager.Max_Message_Size,
         Priority   => Priority);
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Cancel_Request_Encode;


   procedure Relative_Request_Decode
     (Message       : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type  : out Series_Type;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position         : Data_Index_Type;
      Raw_Interval     : XDR.XDR_Unsigned;
      Raw_Request_Type : XDR.XDR_Unsigned;
      Raw_Series_ID    : XDR.XDR_Unsigned;
      Last             : Data_Index_Type;
   begin
      Tick_Interval := Ada.Real_Time.Time_Span_First;
      Request_Type := Series_Type'First;
      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload.all, Position, Raw_Interval, Last);
      Position := Last + 1;

      if Raw_Interval > XDR.XDR_Unsigned(Integer'Last) then
         Decode_Status := Malformed;
      else
         Tick_Interval := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));

         XDR.Decode(Message.Payload.all, Position, Raw_Request_Type, Last);
         Position := Last + 1;

         if Raw_Request_Type > Series_Type'Pos(Series_Type'Last) then
            Decode_Status := Malformed;
         else
            Request_Type := Series_Type'Val(Raw_Request_Type);

            XDR.Decode(Message.Payload.all, Position, Raw_Series_ID, Last);

            if Raw_Series_ID not in
              XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last)
            then
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
      Position      : Data_Index_Type;
      Raw_Seconds   : XDR.XDR_Unsigned;
      Seconds       : Ada.Real_Time.Seconds_Count;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Last          : Data_Index_Type;
   begin
      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload.all, Position, Raw_Seconds, Last);
      Position := Last + 1;

      Seconds := Ada.Real_Time.Seconds_Count(Raw_Seconds);
      Tick_Time := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);

      XDR.Decode(Message.Payload.all, Position, Raw_Series_ID, Last);
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last)
      then
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
      Position      : Data_Index_Type;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Raw_Count     : XDR.XDR_Unsigned;
      Last          : Data_Index_Type;
   begin
      Series_ID := Series_ID_Type'First;
      Count := 0;

      Position := 0;
      XDR.Decode(Message.Payload.all, Position, Raw_Series_ID, Last);
      Position := Last + 1;
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Series_ID := Series_ID_Type(Raw_Series_ID);

         XDR.Decode(Message.Payload.all, Position, Raw_Count, Last);
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
      Position      : Data_Index_Type;
      Raw_Series_ID : XDR.XDR_Unsigned;
      Last          : Data_Index_Type;
   begin
      Series_ID := Series_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload.all, Position, Raw_Series_ID, Last);
      if Raw_Series_ID not in
        XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Series_ID := Series_ID_Type(Raw_Series_ID);
         Decode_Status := Success;
      end if;
   end Cancel_Request_Decode;

end CubedOS.Time_Server.API;
