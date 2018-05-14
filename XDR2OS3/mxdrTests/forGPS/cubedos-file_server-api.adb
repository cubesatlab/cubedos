--------------------------------------------------------------------------------
-- FILE   : cubedos-File_Server-api.adb
-- SUBJECT: Body of a package that implements the File_Server API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.File_Server.API is

   function Open_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Mode : Mode_Type;
      Name : String;
      Request_ID : Request_ID_Type;
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
         Message_ID => Message_Type'Pos(Open_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Mode_Type'Pos(Mode)), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Name'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Name, Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Request_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Open_Request_Encode;

   procedure Open_Request_Decode
      (Message : in  Message_Record;
      Mode : out Mode_Type;
      Name : out String;
      Name_Size : out Natural;
      Request_ID : out Request_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Mode : XDR.XDR_Unsigned;
      Raw_Name_Size : XDR.XDR_Unsigned;
      Raw_Request_ID   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Name := (others => ' ');
      Request_ID := Request_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Mode, Last);
         Position := Last + 1;
         if Raw_Mode in Mode_Type'Pos(Mode_Type'First) .. Mode_Type'Pos(Mode_Type'Last) then
            Mode := Mode_Type'Val(Raw_Mode);
         else
            Decode_Status := Malformed;
            Mode := Mode_Type'First;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Name_Size, Last);
         Position := Last + 1;
         if Raw_Name_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then
            Name_Size := Natural(Raw_Name_Size);
         else
            Name_Size := 0;
         end if;
         if Name_Size < 1 then
            XDR.Decode(Message.Payload, Position, Name(Name'First .. Name'First + (Name_Size - 1)), Last);
            Position := Last + 1;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Request_ID, Last);
         if Raw_Request_ID in XDR.XDR_Unsigned(Request_ID_Type'First) .. XDR.XDR_Unsigned(Request_ID_Type'Last) then
            Request_ID := Request_ID_Type(Raw_Request_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Open_Request_Decode;

   function Open_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Handle : File_Handle_Type;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Open_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Request_ID), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Open_Reply_Encode;

   procedure Open_Reply_Decode
      (Message : in  Message_Record;
      Handle : out File_Handle_Type;
      Request_ID : out Request_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Raw_Request_ID   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := File_Handle_Type'First;
      Request_ID := Request_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         Position := Last + 1;
         if Raw_Handle in XDR.XDR_Unsigned(File_Handle_Type'First) .. XDR.XDR_Unsigned(File_Handle_Type'Last) then
            Handle := File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Request_ID, Last);
         if Raw_Request_ID in XDR.XDR_Unsigned(Request_ID_Type'First) .. XDR.XDR_Unsigned(Request_ID_Type'Last) then
            Request_ID := Request_ID_Type(Raw_Request_ID);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Open_Reply_Decode;

   function Read_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Handle : File_Handle_Type;
      Amount : Read_Size_Type;
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
         Message_ID => Message_Type'Pos(Read_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Read_Request_Encode;

   procedure Read_Request_Decode
      (Message : in  Message_Record;
      Handle : out File_Handle_Type;
      Amount : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Raw_Amount   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := File_Handle_Type'First;
      Amount := Read_Size_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         Position := Last + 1;
         if Raw_Handle in XDR.XDR_Unsigned(File_Handle_Type'First) .. XDR.XDR_Unsigned(File_Handle_Type'Last) then
            Handle := File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
         if Raw_Amount in XDR.XDR_Unsigned(Read_Size_Type'First) .. XDR.XDR_Unsigned(Read_Size_Type'Last) then
            Amount := Read_Size_Type(Raw_Amount);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Read_Request_Decode;

   function Read_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Read_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Read_Reply_Encode;

   procedure Read_Reply_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Read_Result_Size_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Raw_Amount   : XDR.XDR_Unsigned;
      Raw_Size : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := Valid_File_Handle_Type'First;
      Amount := Read_Result_Size_Type'First;
      Message_Data := (others => 0);
      Size := 0;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         Position := Last + 1;
         if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) .. XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
            Handle := Valid_File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
         Position := Last + 1;
         if Raw_Amount in XDR.XDR_Unsigned(Read_Result_Size_Type'First) .. XDR.XDR_Unsigned(Read_Result_Size_Type'Last) then
            Amount := Read_Result_Size_Type(Raw_Amount);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Size, Last);
         Position := Last + 1;
         if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
         if Size < Message_Data'Length then
            XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Read_Reply_Decode;

   function Write_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
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
         Message_ID => Message_Type'Pos(Write_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Write_Request_Encode;

   procedure Write_Request_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Write_Size_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Raw_Amount   : XDR.XDR_Unsigned;
      Raw_Size : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := Valid_File_Handle_Type'First;
      Amount := Write_Size_Type'First;
      Message_Data := (others => 0);
      Size := 0;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         Position := Last + 1;
         if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) .. XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
            Handle := Valid_File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
         Position := Last + 1;
         if Raw_Amount in XDR.XDR_Unsigned(Write_Size_Type'First) .. XDR.XDR_Unsigned(Write_Size_Type'Last) then
            Amount := Write_Size_Type(Raw_Amount);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Size, Last);
         Position := Last + 1;
         if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
         if Size < Message_Data'Length then
            XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Write_Request_Decode;

   function Write_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Write_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Write_Reply_Encode;

   procedure Write_Reply_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Raw_Amount   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := Valid_File_Handle_Type'First;
      Amount := Write_Result_Size_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         Position := Last + 1;
         if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) .. XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
            Handle := Valid_File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
         if Raw_Amount in XDR.XDR_Unsigned(Write_Result_Size_Type'First) .. XDR.XDR_Unsigned(Write_Result_Size_Type'Last) then
            Amount := Write_Result_Size_Type(Raw_Amount);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Write_Reply_Decode;

   function Close_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
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
         Message_ID => Message_Type'Pos(Close_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Close_Request_Encode;

   procedure Close_Request_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Handle   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Handle := Valid_File_Handle_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
         if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) .. XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
            Handle := Valid_File_Handle_Type(Raw_Handle);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Close_Request_Decode;


end CubedOS.File_Server.API;
