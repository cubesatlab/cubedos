--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the file server.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;
with System;

use Message_Manager;

package CubedOS.File_Server.API is

   type Message_Type is
     (Open_Request,
      Open_Reply,
      Read_Request,
      Read_Reply,
      Write_Request,
      Write_Reply,
      Close_Request);

   type Mode_Type is (Read, Write);
   type File_Handle_Type is range 0 .. 64;
   subtype Valid_File_Handle_Type is File_Handle_Type range 1 .. File_Handle_Type'Last;

   Invalid_Handle : constant File_Handle_Type := 0;
   Maximum_Read_Size : constant := 256;
   Maximum_Write_Size : constant := 256;

   -- Attempted reads and writes must be for at least one octet. The reply messages contain the
   -- number of octets successfully read or written and must allow for zero to handle the error
   -- or end-of-file case.
   --
   subtype Read_Result_Size_Type is Natural range 0 .. Maximum_Read_Size;
   subtype Read_Size_Type is Natural range 1 .. Read_Result_Size_Type'Last;
   subtype Write_Result_Size_Type is Natural range 0 .. Maximum_Write_Size;
   subtype Write_Size_Type is Natural range 1 .. Write_Result_Size_Type'Last;

   function Open_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Mode       : Mode_Type;
      Name       : String;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre => (0 < Name'Length and Name'Length <= Data_Size_Type'Last - 12);

   function Open_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : File_Handle_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Read_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Read_Size_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Read_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Read_Result_Size_Type;
      Data       : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre => Amount <= Data'Length;

   function Write_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Write_Size_Type;
      Data       : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Pre => Amount <= Data'Length;

   function Write_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Write_Result_Size_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Close_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;



   function Is_Open_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Open_Request));

   function Is_Open_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Open_Reply));

   function Is_Read_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Read_Request));

   function Is_Read_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Read_Reply));

   function Is_Write_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Write_Request));

   function Is_Write_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Write_Reply));

   function Is_Close_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Close_Request));



   procedure Open_Request_Decode
     (Message    : in  Message_Record;
      Mode       : out Mode_Type;
      Name       : out String;
      Name_Size  : out Natural;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Open_Request(Message),
       Depends => ((Mode, Name_Size, Decode_Status) => Message, Name =>+ Message);

   procedure Open_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out File_Handle_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Open_Reply(Message),
       Depends => ((Handle, Decode_Status) => Message);

   procedure Read_Request_Decode
     (Message    : in Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Read_Request(Message),
       Depends => ((Handle, Amount, Decode_Status) => Message);

   procedure Read_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Read_Result_Size_Type;
      Data       : out CubedOS.Lib.Octet_Array;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Read_Reply(Message) and Data'Length = Read_Result_Size_Type'Last,
       Depends => ((Handle, Amount, Decode_Status) => Message, Data =>+ Message);

   procedure Write_Request_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Write_Size_Type;
      Data       : out CubedOS.Lib.Octet_Array;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Write_Request(Message) and Data'Length = Read_Result_Size_Type'Last,
       Depends => ((Handle, Amount, Decode_Status) => Message, Data =>+ Message);

   procedure Write_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Write_Reply(Message),
       Depends => ((Handle, Amount, Decode_Status) => Message);

   procedure Close_Request_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Close_Request(Message),
       Depends => ((Handle, Decode_Status) => Message);


 end CubedOS.File_Server.API;
