--------------------------------------------------------------------------------
-- FILE   : cubedos-Spiral_Thruster-api.ads
-- SUBJECT: Specification of a package that defines the Spiral_Thruster API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Spiral_Thruster.API is

   type Message_Type is
      (Telemetry_Value_Request,
      Standby_Reply,
      Telemetry_Value_Reply,
      Standby_Request,
      Spiral_Thrust_Reply,
      Thrust_Command_Reply,
      Spiral_Thrust_Request,
      Thrust_Command_Request);

   type Spiral_Thrust_Status is
         (OK,
         Error);

   type Error_Table is
         (TBD);

   type Telem_Failure_Code is
         (OK,
         Failed);

   type Phi_Degrees_Type is new Float range 0.0 .. 10.0;

   type Theta_Degrees_Type is new Float range 0.0 .. 10.0;

   type Thrust_Force_Type is new Float range 0.0 .. 150.0;

   type Time_Type is new Lib.Quadruple_Octet;

   type Telemetry_Address_Type is new Lib.Quadruple_Octet;

   type Telemetry_Value_Type is new Float;

   function Thrust_Command_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Phi_Degrees : Phi_Degrees_Type;
      Theta_Degrees : Theta_Degrees_Type;
      Thrust_Force : Thrust_Force_Type;
      Time : Time_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => (in_out => Message_Manager.Request_ID_Generator);

   function Is_Thrust_Command_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Thrust_Command_Request));

   procedure Thrust_Command_Request_Decode
      (Message : in  Message_Record;
      Phi_Degrees : out Phi_Degrees_Type;
      Theta_Degrees : out Theta_Degrees_Type;
      Thrust_Force : out Thrust_Force_Type;
      Time : out Time_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Thrust_Command_Request(Message),
      Depends => ((Phi_Degrees, Theta_Degrees, Thrust_Force, Time, Decode_Status) => Message);


   function Thrust_Command_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Thrust_Command_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Thrust_Command_Reply));

   procedure Thrust_Command_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Thrust_Command_Reply(Message),
      Depends => ((Thrust_Status, Decode_Status) => Message);


   function Spiral_Thrust_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Phi_Degrees : Phi_Degrees_Type;
      Theta_Degrees : Theta_Degrees_Type;
      Thrust_Force : Thrust_Force_Type;
      Time : Time_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Spiral_Thrust_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Spiral_Thrust_Request));

   procedure Spiral_Thrust_Request_Decode
      (Message : in  Message_Record;
      Phi_Degrees : out Phi_Degrees_Type;
      Theta_Degrees : out Theta_Degrees_Type;
      Thrust_Force : out Thrust_Force_Type;
      Time : out Time_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Spiral_Thrust_Request(Message),
      Depends => ((Phi_Degrees, Theta_Degrees, Thrust_Force, Time, Decode_Status) => Message);


   function Spiral_Thrust_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Spiral_Thrust_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Spiral_Thrust_Reply));

   procedure Spiral_Thrust_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Spiral_Thrust_Reply(Message),
      Depends => ((Thrust_Status, Decode_Status) => Message);


   function Standby_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Standby_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Standby_Request));


   function Standby_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Standby_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Standby_Reply));

   procedure Standby_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Standby_Reply(Message),
      Depends => ((Thrust_Status, Decode_Status) => Message);


   function Telemetry_Value_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Telem_Addr : Telemetry_Address_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Telemetry_Value_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Telemetry_Value_Request));

   procedure Telemetry_Value_Request_Decode
      (Message : in  Message_Record;
      Telem_Addr : out Telemetry_Address_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Telemetry_Value_Request(Message),
      Depends => ((Telem_Addr, Decode_Status) => Message);


   function Telemetry_Value_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Response_Val : Telemetry_Value_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Telemetry_Value_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Telemetry_Value_Reply));

   procedure Telemetry_Value_Reply_Decode
      (Message : in  Message_Record;
      Response_Val : out Telemetry_Value_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Telemetry_Value_Reply(Message),
      Depends => ((Response_Val, Decode_Status) => Message);



end CubedOS.Spiral_Thruster.API;
