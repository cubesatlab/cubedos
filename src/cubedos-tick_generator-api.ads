--------------------------------------------------------------------------------
-- FILE   : cubedos-tick_generator-api.ads
-- SUBJECT: Specification of a package that simplifies use of the tick generator.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
with Message_Manager;
with System;

use Message_Manager;

package CubedOS.Tick_Generator.API is

   type Message_Type is
     (Relative_Request,
      Absolute_Request,
      Tick_Reply,
      Cancel_Request);

   type Series_Type is (One_Shot, Periodic);
   type Series_ID_Type is range 1 .. Natural'Last;
   type Series_Count_Type is range 0 .. Natural'Last;

   -- Return a message for requesting ticks that are scheduled relative to the current time.
   function Relative_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender        : Module_ID_Type;
      Request_ID    : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type  : Series_Type;
      Series_ID     : Series_ID_Type;
      Priority      : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   -- Return a message for requesting a single tick message at a specific, absolute time.
   function Absolute_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Tick_Time  : Ada.Real_Time.Time;
      Series_ID  : Series_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   -- The tick messages themselves.
   function Tick_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Series_ID  : Series_ID_Type;
      Count      : Series_Count_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record;

   -- Return a message for canceling previous tick requests (of any kind).
   function Cancel_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Series_ID  : Series_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;


   function Is_Relative_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Relative_Request));

   function Is_Absolute_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Absolute_Request));

   function Is_Tick_Reply(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Tick_Reply));

   function Is_Cancel_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Cancel_Request));


   procedure Relative_Request_Decode
     (Message       : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type  : out Series_Type;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Relative_Request(Message),
       Depends => ((Tick_Interval, Request_Type, Series_ID, Decode_Status) => Message);

   procedure Absolute_Request_Decode
     (Message   : in  Message_Record;
      Tick_Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Absolute_Request(Message),
       Depends => ((Tick_Time, Series_ID, Decode_Status) => Message);

   procedure Tick_Reply_Decode
     (Message   : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count     : out Natural;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Tick_Reply(Message),
       Depends => ((Series_ID, Count, Decode_Status) => Message);

   procedure Cancel_Request_Decode
     (Message   : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Cancel_Request(Message),
       Depends => ((Series_ID, Decode_Status) => Message);


end CubedOS.Tick_Generator.API;
