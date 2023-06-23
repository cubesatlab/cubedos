--------------------------------------------------------------------------------
-- FILE   : %FILENAME%
-- SUBJECT: Body of a package that implements the Time_Server API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
use Ada.Real_Time;

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;

package body CubedOS.Time_Server.API is

   procedure Relative_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
      Interval : constant Duration := Ada.Real_Time.To_Duration(Tick_Interval);
   begin

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_Type'Pos(Request_Type)), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Relative_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Relative_Request_Encode;

   procedure Send_Relative_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Relative_Request_Encode(
         Sender_Address => Address(Sender),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Tick_Interval => Tick_Interval,
         Request_Type => Request_Type,
         Series_ID => Series_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Relative_Request;
   procedure Relative_Request_Decode
      (Message : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type : out Series_Type;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Interval  : XDR.XDR_Unsigned;
      Raw_Request_Type : XDR.XDR_Unsigned;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Tick_Interval := Ada.Real_Time.Time_Span_First;
      Request_Type := Series_Type'First;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Interval, Last);
         Position := Last + 1;
         if Raw_Interval < XDR.XDR_Unsigned(Integer'Last) then
            Tick_Interval := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Request_Type, Last);
         Position := Last + 1;
         if Raw_Request_Type in Series_Type'Pos(Series_Type'First) .. Series_Type'Pos(Series_Type'Last) then
            Request_Type := Series_Type'Val(Raw_Request_Type);
         else
            Decode_Status := Malformed;
            Request_Type := Series_Type'First;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Relative_Request_Decode;

   procedure Absolute_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
      Seconds  : Ada.Real_Time.Seconds_Count;
      Fraction : Ada.Real_Time.Time_Span;
   begin
      Ada.Real_Time.Split(Tick_Time, Seconds, Fraction);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Seconds), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Absolute_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Absolute_Request_Encode;

   procedure Send_Absolute_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Absolute_Request_Encode(
         Sender_Address => Address(Sender),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Tick_Time => Tick_Time,
         Series_ID => Series_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Absolute_Request;
   procedure Absolute_Request_Decode
      (Message : in  Message_Record;
      Tick_Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Seconds : Ada.Real_Time.Seconds_Count;
      Raw_Tick_Time   : XDR.XDR_Unsigned;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Tick_Time := Ada.Real_Time.Time_First;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Tick_Time, Last);
         Position := Last + 1;
         if Raw_Tick_Time < XDR.XDR_Unsigned(Integer'Last) then
            Seconds := Ada.Real_Time.Seconds_Count(Raw_Tick_Time);
            Tick_Time := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Absolute_Request_Decode;

   procedure Tick_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Count), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Tick_Reply_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Tick_Reply_Encode;

   procedure Send_Tick_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Tick_Reply_Encode(
         Sender_Address => Address(Sender),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Series_ID => Series_ID,
         Count => Count,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Tick_Reply;
   procedure Tick_Reply_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count : out Series_Count_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Raw_Count   : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Series_ID := Series_ID_Type'First;
      Count := Series_Count_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Series_ID, Last);
         Position := Last + 1;
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Count, Last);
         if Raw_Count in XDR.XDR_Unsigned(Series_Count_Type'First) .. XDR.XDR_Unsigned(Series_Count_Type'Last) then
            Count := Series_Count_Type(Raw_Count);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Tick_Reply_Decode;

   procedure Cancel_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Cancel_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Cancel_Request_Encode;

   procedure Send_Cancel_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Cancel_Request_Encode(
         Sender_Address => Address(Sender),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Series_ID => Series_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Cancel_Request;
   procedure Cancel_Request_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Cancel_Request_Decode;


end CubedOS.Time_Server.API;
