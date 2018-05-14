--------------------------------------------------------------------------------
-- FILE   : cubedos-Positioning_System-api.ads
-- SUBJECT: Specification of a package that defines the Positioning_System API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Positioning_System.API is

   type Message_Type is
      (Z_Rot_Reply, 
      X_Rot_Request, 
      Z_Rot_Request, 
      Y_Rot_Reply, 
      X_Rot_Reply, 
      Y_Rot_Request);

   type X_Rotation is new Float range 0.0 .. 359.0;

   type Y_Rotation is new Float range 0.0 .. 359.0;

   type Z_Rotation is new Float range 0.0 .. 359.0;

   function X_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_X_Rot_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(X_Rot_Request));


   function X_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      X_Rot : X_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_X_Rot_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(X_Rot_Reply));

   procedure X_Rot_Reply_Decode
      (Message : in  Message_Record;
      X_Rot : out X_Rotation;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_X_Rot_Reply(Message),
      Depends => ((X_Rot, Decode_Status) => Message);


   function Y_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Y_Rot_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Y_Rot_Request));


   function Y_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Y_Rot : Y_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Y_Rot_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Y_Rot_Reply));

   procedure Y_Rot_Reply_Decode
      (Message : in  Message_Record;
      Y_Rot : out Y_Rotation;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Y_Rot_Reply(Message),
      Depends => ((Y_Rot, Decode_Status) => Message);


   function Z_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Z_Rot_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Z_Rot_Request));


   function Z_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Z_Rot : Z_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Z_Rot_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Z_Rot_Reply));

   procedure Z_Rot_Reply_Decode
      (Message : in  Message_Record;
      Z_Rot : out Z_Rotation;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Z_Rot_Reply(Message),
      Depends => ((Z_Rot, Decode_Status) => Message);



end CubedOS.Positioning_System.API;
