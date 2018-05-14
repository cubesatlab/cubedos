--------------------------------------------------------------------------------
-- FILE   : cubedos-Tick_Generator-api.adb
-- SUBJECT: Body of a package that implements the Tick_Generator API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
use Ada.Real_Time;

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.Tick_Generator.API is

   function Relative_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Time_Span : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Interval : constant Duration := Ada.Real_Time.To_Duration(Time_Span);
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Relative_Request),
         Priority   => Priority);
      
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_Type'Pos(Request_Type)), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Relative_Request_Encode;

   procedure Relative_Request_Decode
      (Message : in  Message_Record;
      Time_Span : out Ada.Real_Time.Time_Span;
      Request_Type : out Series_Type;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Interval  : XDR.XDR_Unsigned;
      Raw_Request_Type : XDR.XDR_Unsigned;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Time_Span := Ada.Real_Time.Time_Span_First;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Interval, Last);
         Position := Last + 1;
         if Raw_Interval < XDR.XDR_Unsigned(Integer'Last) then
            Time_Span := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Request_Type, Last);
         Position := Last + 1;
         if Raw_Request_Type in Series_Type'Pos(Series_Type'First) .. Series_Type'Pos(Series_Type'Last) then
            Request_Type := Series_Type'Val(Raw_Request_Type);
         else
            Decode_Status := Malformed;
            Request_Type := Series_Type'First;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Relative_Request_Decode;

   function Absolute_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Seconds  : Ada.Real_Time.Seconds_Count;
      Fraction : Ada.Real_Time.Time_Span;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Absolute_Request),
         Priority   => Priority);
      Ada.Real_Time.Split(Time, Seconds, Fraction);

      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Absolute_Request_Encode;

   procedure Absolute_Request_Decode
      (Message : in  Message_Record;
      Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Seconds : Ada.Real_Time.Seconds_Count;
      Raw_Time   : XDR.XDR_Unsigned;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Time := Ada.Real_Time.Time_First;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Time, Last);
         Position := Last + 1;
         if Raw_Time < XDR.XDR_Unsigned(Integer'Last) then
            Seconds := Ada.Real_Time.Seconds_Count(Raw_Time);
            Time := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Absolute_Request_Decode;

   function Tick_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Tick_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Count), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Tick_Reply_Encode;

   procedure Tick_Reply_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count : out Series_Count_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Raw_Count   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Series_ID := Series_ID_Type'First;
      Count := Series_Count_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
         Position := Last + 1;
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Count, Last);
         if Raw_Count in XDR.XDR_Unsigned(Series_Count_Type'First) .. XDR.XDR_Unsigned(Series_Count_Type'Last) then
            Count := Series_Count_Type(Raw_Count);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Tick_Reply_Decode;

   function Cancel_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Cancel_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Cancel_Request_Encode;

   procedure Cancel_Request_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Series_ID   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Series_ID := Series_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Series_ID, Last);
         if Raw_Series_ID in XDR.XDR_Unsigned(Series_ID_Type'First) .. XDR.XDR_Unsigned(Series_ID_Type'Last) then
            Series_ID := Series_ID_Type(Raw_Series_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Cancel_Request_Decode;


end CubedOS.Tick_Generator.API;
