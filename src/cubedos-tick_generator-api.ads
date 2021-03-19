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

   -- Specifies the kinds of messages that can be sent to or received from the tick generator.
   -- @value Relative_Request A request for a tick using a time relative to when the request
   --   is made (e. g., "5 seconds from now").
   -- @value Absolute_Request A request for a tick using an absolute time that is independent
   --   from when the request is made (e. g., "13:25:30 on January 1, 2021").
   -- @value Tick_Reply The message type used for tick messages returned to client modules.
   -- @value Cancel_Request A request to cancel a previously defined series.
   type Message_Type is
     (Relative_Request,
      Absolute_Request,
      Tick_Reply,
      Cancel_Request);

   -- Two types of tick "series" are supported.
   -- @value One_Shot A single Tick_Reply message will be sent. The series is automatically
   --   canceled after that message.
   -- @value Periodic Tick_Reply messages will be sent periodically until explicitly canceled.
   type Series_Type is (One_Shot, Periodic);

   -- Each tick series has an ID number used to identify it in subsequent messages.
   type Series_ID_Type is range 1 .. Natural'Last;

   -- Tick_Reply messages contain a counter that indicates how many tick messages have been sent
   -- on a particular series. Although Tick_Reply messages start with a count of 1, the value
   -- 0 is allowed here so that variables that count the number of tick messages sent/received
   -- can be initialized in a reasonable way.
   type Series_Count_Type is range 0 .. Natural'Last;

   -- Return a message for requesting ticks that are scheduled relative to the current time.
   -- @param Tick_Interval The time interval until the first tick message (and between tick
   --   messages in the case of a periodic series).
   -- @param Request_Type Indicates if a One_Shot or Periodic series is requested.
   -- @param Series_ID The ID number for the series (selected by the caller and thus only
   --   meaningful to the requesting module).
   function Relative_Request_Encode
     (Sender_Domain : in Domain_ID_Type;
      Sender        : in Module_ID_Type;
      Request_ID    : in Request_ID_Type;
      Tick_Interval : in Ada.Real_Time.Time_Span;
      Request_Type  : in Series_Type;
      Series_ID     : in Series_ID_Type;
      Priority      : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   -- Return a message for requesting a single tick message at a specific, absolute time.
   -- @param Tick_Time The time when the tick message is requested. Absolute requests are always
   --   one shot requests so no Request_Type paramter is needed.
   -- @param Series_ID The ID number of the series (selected by the caller and thus only
   --   meaningful to the requesting module).
   function Absolute_Request_Encode
     (Sender_Domain : in Domain_ID_Type;
      Sender     : in Module_ID_Type;
      Request_ID : in Request_ID_Type;
      Tick_Time  : in Ada.Real_Time.Time;
      Series_ID  : in Series_ID_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   -- The tick messages themselves.
   -- @param Series_ID The ID number of the series to distinguish this tick message from those
   --   of other series being sent to the same module.
   -- @param Count The count for this tick reply. Counts start at one and increment for each
   --   message sent. The counts are scoped by their series (i. e., every series has its own
   --   count sequence). Should the count reach Series_Count_Type'Last, the series is canceled
   --   after a Tick_Reply with that count is sent.
   function Tick_Reply_Encode
     (Receiver_Domain : in Domain_ID_Type;
      Receiver   : in Module_ID_Type;
      Request_ID : in Request_ID_Type;
      Series_ID  : in Series_ID_Type;
      Count      : in Series_Count_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record;

   -- Return a message for canceling previous tick requests (of any kind).
   -- @param Series_ID The ID number of the series being canceled. If the series does not
   --   exist or has already been canceled or removed (due to being a one shot series that has
   --   already fired), there is no effect.
   function Cancel_Request_Encode
     (Sender_Domain : in Domain_ID_Type;
      Sender     : in Module_ID_Type;
      Request_ID : in Request_ID_Type;
      Series_ID  : in Series_ID_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Is_Relative_Request (Message : in Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos (Relative_Request));

   function Is_Absolute_Request (Message : in Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos (Absolute_Request));

   function Is_Tick_Reply (Message : in Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos (Tick_Reply));

   function Is_Cancel_Request (Message : in Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos (Cancel_Request));

   -- Decode Relative_Request messages. See Relative_Request_Encode for information about the
   -- parameters.
   procedure Relative_Request_Decode
     (Message       : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type  : out Series_Type;
      Series_ID     : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Relative_Request (Message),
       Depends => ((Tick_Interval, Request_Type, Series_ID, Decode_Status) => Message);

   -- Decode Absolute_Request messages. See Absolute_Request_Encode for information about the
   -- parameters.
   procedure Absolute_Request_Decode
     (Message   : in  Message_Record;
      Tick_Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Absolute_Request (Message),
       Depends => ((Tick_Time, Series_ID, Decode_Status) => Message);

   -- Decode Tick_Reply messages. See Tick_Reply_Encode for information about the parameters.
   procedure Tick_Reply_Decode
     (Message   : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count     : out Natural;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Tick_Reply (Message),
       Depends => ((Series_ID, Count, Decode_Status) => Message);

   -- Decode Cancel_Request messages. See Cancel_Request_Encode for information about the
   -- parameters.
   procedure Cancel_Request_Decode
     (Message   : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Cancel_Request (Message),
       Depends => ((Series_ID, Decode_Status) => Message);

end CubedOS.Tick_Generator.API;
