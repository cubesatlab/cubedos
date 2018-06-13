--------------------------------------------------------------------------------
-- FILE   : cubedos-Code_Generation_For_Paper-api.ads
-- SUBJECT: Specification of a package that defines the Code_Generation_For_Paper API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Code_Generation_For_Paper.API is

   type Message_Type is
      (Update, 
      File_Receipt, 
      File_Accept);

   Min: constant := 0;

   Max: constant := 9;

   type Memory_Locale is new Lib.U_Hyper_Type;

   type Module_ID is new Integer;

   type CubeSat_Intermediary is
      record
         Address : Memory_Locale;
         Module : Module_ID;
         Swarm_Position : Integer;
      end record;

   type CubeSat is array (0 .. 10) of CubeSat_Intermediary;

   function File_Accept_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Module_Destination_ID : Integer;
      Swarm : CubeSat;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_File_Accept(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(File_Accept));

   procedure File_Accept_Decode
      (Message : in  Message_Record;
      Module_Destination_ID : out Integer;
      Swarm : out CubeSat;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_File_Accept(Message),
      Depends => ((Module_Destination_ID, Swarm, Decode_Status) => Message);


   function File_Receipt_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message : String;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null,
      Pre => (0 < Message'Length and Message'Length <= XDR_Size_Type'Last - 12);

   function Is_File_Receipt(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(File_Receipt));

   procedure File_Receipt_Decode
      (Message : in  Message_Record;
      Message : out String;
      Message_Size : out Natural;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_File_Receipt(Message),
      Depends => ((Message, Message_Size, Decode_Status) => Message);


   function Update_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Module_Destination_ID : Integer;
      Swarm : CubeSat;
      Errors : Boolean;
      Check_Swarm : Boolean;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null,
      Pre => Module_Destination_ID <= Max,
      Pre => Module_Destination_ID >= Min;

   function Is_Update(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Update));

   procedure Update_Decode
      (Message : in  Message_Record;
      Module_Destination_ID : out Integer;
      Swarm : out CubeSat;
      Errors : out Boolean;
      Check_Swarm : out Boolean;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Update(Message),
      Depends => ((Module_Destination_ID, Swarm, Errors, Check_Swarm, Decode_Status) => Message),
      Post => Module_Destination_ID <= Max,
      Post => Module_Destination_ID >= Min;



end CubedOS.Code_Generation_For_Paper.API;
