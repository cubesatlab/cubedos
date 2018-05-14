--------------------------------------------------------------------------------
-- FILE   : cubedos-Tick_Generator-api.ads
-- SUBJECT: Specification of a package that defines the Tick_Generator API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Tick_Generator.API is

   type Message_Type is
      (Relative_Request, 
      Absolute_Request, 
      Cancel_Request, 
      Tick_Reply);

   type Series_Type is 
         (One_Shot, 
         Periodoc);

   type Series_ID_Type is new Lib.Quadruple_Octet;

   type Series_Count_Type is new Lib.Quadruple_Octet;

   function Relative_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Time_Span : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Relative_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Relative_Request));

   procedure Relative_Request_Decode
      (Message : in  Message_Record;
      Time_Span : out Ada.Real_Time.Time_Span;
      Request_Type : out Series_Type;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Relative_Request(Message),
      Depends => ((Time_Span, Request_Type, Series_ID, Decode_Status) => Message);


   function Absolute_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Absolute_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Absolute_Request));

   procedure Absolute_Request_Decode
      (Message : in  Message_Record;
      Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Absolute_Request(Message),
      Depends => ((Time, Series_ID, Decode_Status) => Message);


   function Tick_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Tick_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Tick_Reply));

   procedure Tick_Reply_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count : out Series_Count_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Tick_Reply(Message),
      Depends => ((Series_ID, Count, Decode_Status) => Message);


   function Cancel_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Cancel_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Cancel_Request));

   procedure Cancel_Request_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Cancel_Request(Message),
      Depends => ((Series_ID, Decode_Status) => Message);



end CubedOS.Tick_Generator.API;
