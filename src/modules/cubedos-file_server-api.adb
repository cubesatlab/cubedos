--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the file server.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.File_Server.API is
   use type XDR.XDR_Unsigned;

   -- Encodes:

   procedure Open_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Mode : Mode_Type;
      Name : String;
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
      XDR.Encode(XDR.XDR_Unsigned(Mode_Type'Pos(Mode)), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Name'Length), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(Name, Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Open_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Open_Request_Encode;

   procedure Send_Open_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Mode : Mode_Type;
      Name : String;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Open_Request_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Mode => Mode,
         Name => Name,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Open_Request;

   procedure Open_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : File_Handle_Type;
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
      XDR.Encode(XDR.XDR_Unsigned(Handle), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Open_Reply_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Open_Reply_Encode;

   procedure Send_Open_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : File_Handle_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Open_Reply_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Open_Reply;

   procedure Read_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Size_Type;
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
      XDR.Encode(XDR.XDR_Unsigned(Amount), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Read_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Read_Request_Encode;

   procedure Send_Read_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Size_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Read_Request_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Amount => Amount,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Read_Request;


   -- Server-side use.
   procedure Read_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_TYpe;
      File_Data : CubedOS.Lib.Octet_Array;
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
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Read_Reply_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Read_Reply_Encode;

   procedure Send_Read_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_TYpe;
      File_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Read_Reply_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Amount => Amount,
         File_Data => File_Data,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Read_Reply;

   procedure Write_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      File_Data : CubedOS.Lib.Octet_Array;
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
      XDR.Encode(XDR.XDR_Unsigned(Amount), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Write_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Write_Request_Encode;

   procedure Send_Write_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      File_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Write_Request_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Amount => Amount,
         File_Data => File_Data,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Write_Request;

   procedure Write_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
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
      XDR.Encode(XDR.XDR_Unsigned(Amount), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Write_Reply_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Write_Reply_Encode;

   procedure Send_Write_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Write_Reply_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Amount => Amount,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Write_Reply;

   procedure Close_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
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
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Close_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Close_Request_Encode;

   procedure Send_Close_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Close_Request_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Handle => Handle,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Close_Request;

   -- Decodes:

   -- Server-side Use
   procedure Open_Request_Decode
     (Message    : in  Message_Record;
      Mode       : out Mode_Type;
      Name       : out String;
      Name_Size  : out Natural;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Mode : XDR.XDR_Unsigned;
      Raw_Name_Size : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;
      Name := (others => ' ');

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Mode, Last);
      Position := Last + 1;
      if Raw_Mode <= Mode_Type'Pos(Mode_Type'Last) then
         Mode := Mode_Type'Val(Raw_Mode);
      else
         Decode_Status := Malformed;
         Mode := Mode_Type'First; -- appropriate?
      end if;
      XDR.Decode(Payload(Message).all, Position, Raw_Name_Size, Last);
      Position := Last + 1;
      if Raw_Name_Size <= XDR.XDR_Unsigned(Natural'Last) then
         Name_Size := Natural(Raw_Name_Size);
      else
         Name_Size := 0;
      end if;
      XDR.Decode(Payload(Message).all, Position, Name(Name'First .. Name'First + (Name_Size - 1)), Last);
   end Open_Request_Decode;


   -- Requester-side use.
   procedure Open_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      if Raw_Handle <= XDR.XDR_Unsigned(File_Handle_Type'Last)
      then
         Handle := File_Handle_Type(Raw_Handle);
         Decode_Status := Success;
      else
         Handle := File_Handle_Type'First;
         Decode_Status := Malformed;
      end if;
   end Open_Reply_Decode;


   -- Server-side use.
   procedure Read_Request_Decode
     (Message : in Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      Position := Last + 1;
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
                       XDR.XDR_Unsigned(Valid_File_Handle_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First;
      end if;
      XDR.Decode(Payload(Message).all, Position, Raw_Amount, Last);
      if Raw_Amount in XDR.XDR_Unsigned(Read_Size_Type'First) ..
                       XDR.XDR_Unsigned(Read_Size_Type'Last)
      then
         Amount := Read_Size_Type(Raw_Amount);
      else
         Decode_Status := Malformed;
         Amount := Read_Size_Type'First; -- What type is this?
      end if;
   end Read_Request_Decode;


   procedure Read_Reply_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Read_Result_Size_Type;
      Data    : out Octet_Array;
      Decode_Status : out Message_Status_Type)
   is
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Amount, Last);
      Position := Last + 1;
      Data := (others => 0);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
          XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) and
         Raw_Amount <= XDR.XDR_Unsigned(Read_Result_Size_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
         Amount := Read_Result_Size_Type(Raw_Amount);
         XDR.Decode(Payload(Message).all, Position, Data(Data'First .. Data'First + (Amount - 1)), Last);
         Decode_Status := Success;
      else
         Handle := Valid_File_Handle_Type'First;
         Amount := Read_Result_Size_Type'First;
         Decode_Status := Malformed;
      end if;
   end Read_Reply_Decode;


   -- Server-side use.
   procedure Write_Request_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Write_Size_Type;
      Data    : out CubedOS.Lib.Octet_Array;
      Decode_Status  : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
                       XDR.XDR_Unsigned(Valid_File_Handle_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First; -- Is that what this should be?
      end if;
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Amount, Last);
      if Raw_Amount in XDR.XDR_Unsigned(Write_Result_Size_Type'First + 1) ..
                       XDR.XDR_Unsigned(Write_Result_Size_Type'Last)
      then
         Amount := Write_Result_Size_Type(Raw_Amount);
      else
         Amount := 1;
      end if;
      Position := Last + 1;
      Data := (others => 0);
      XDR.Decode(Payload(Message).all, Position, Data(Data'First .. Data'First + (Amount - 1)), Last);
   end Write_Request_Decode;


   procedure Write_Reply_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Amount, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
          XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) and
         Raw_Amount <= XDR.XDR_Unsigned(Write_Result_Size_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
         Amount := Write_Result_Size_Type(Raw_Amount);
         Decode_Status := Success;
      else
         Handle := Valid_File_Handle_Type'First;
         Amount := Write_Result_Size_Type'First;
         Decode_Status := Malformed;
      end if;
   end Write_Reply_Decode;


   -- Server-side use.
   procedure Close_Request_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position   : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Last       : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Handle, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
                       XDR.XDR_Unsigned(Valid_File_Handle_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First; -- Again, is that what this should be?
      end if;
   end Close_Request_Decode;

end CubedOS.File_Server.API;
