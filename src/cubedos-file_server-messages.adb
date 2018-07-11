--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the file server.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(Off);

-- For debugging...
with Ada.Exceptions;
with Ada.Text_IO;

with Ada.Sequential_IO;
with CubedOS.File_Server.API;
with CubedOS.Lib;

use Ada.Text_IO;

package body CubedOS.File_Server.Messages is
   use Message_Manager;
   use type API.File_Handle_Type;
   use type API.Mode_Type;

   package Octet_IO is new Ada.Sequential_IO(Element_Type => CubedOS.Lib.Octet);

   type File_Record is
      record
         -- This record may hold additional components in the future.
         Underlying : Octet_IO.File_Type;
      end record;

   Files : array(API.Valid_File_Handle_Type) of File_Record;


   procedure Process_Open_Request(Incoming_Message : in Message_Record)
     with Pre => API.Is_Open_Request(Incoming_Message)
   is

      -- A linear search should be fine. This produces the lowest available handle.
      function Find_Free_Handle return API.File_Handle_Type is
      begin
         for I in API.Valid_File_Handle_Type loop
            if not Octet_IO.Is_Open(Files(I).Underlying) then
               return I;
            end if;
         end loop;
         return API.Invalid_Handle;
      end Find_Free_Handle;

      Mode       : API.Mode_Type;
      Status     : Message_Status_Type;
      Name       : String(1 .. 256);  -- Somewhat arbitrary restriction on file name size.
      Name_Size  : Natural;
      Underlying_Mode : Octet_IO.File_Mode;
      Handle     : constant API.File_Handle_Type := Find_Free_Handle;
   begin
      API.Open_Request_Decode(Incoming_Message, Mode, Name, Name_Size, Status);

      -- Don't even bother if there are no available handles.
      if Handle = API.Invalid_Handle then
         Message_Manager.Route_Message
           (API.Open_Reply_Encode
              (Receiver_Domain => Incoming_Message.Sender_Domain,
               Receiver   => Incoming_Message.Sender,
               Request_ID => Incoming_Message.Request_ID,
               Handle     => API.Invalid_Handle));
         return;
      end if;

      if Status = Malformed then
         Message_Manager.Route_Message
           (API.Open_Reply_Encode
              (Receiver_Domain=> Incoming_Message.Sender_Domain,
               Receiver   => Incoming_Message.Sender,
               Request_ID => Incoming_Message.Request_ID,
               Handle     => API.Invalid_Handle));
      else
         case Mode is
            when API.Read =>
               Underlying_Mode := Octet_IO.In_File;
               Octet_IO.Open(Files(Handle).Underlying, Underlying_Mode, Name(1 .. Name_Size));

            when API.Write =>
               Underlying_Mode := Octet_IO.Out_File;
               Octet_IO.Create(Files(Handle).Underlying, Underlying_Mode, Name(1 .. Name_Size));
         end case;

         Message_Manager.Route_Message
           (API.Open_Reply_Encode
              (Receiver_Domain => Incoming_Message.Sender_Domain,
               Receiver   => Incoming_Message.Sender,
               Request_ID => Incoming_Message.Request_ID,
               Handle     => Handle));
      end if;

   exception
      when others =>
         -- Open failed. Send back an invalid handle.
         Message_Manager.Route_Message
           (API.Open_Reply_Encode
              (Receiver_Domain => Incoming_Message.Sender_Domain,
               Receiver   => Incoming_Message.Sender,
               Request_ID => Incoming_Message.Request_ID,
               Handle     => API.Invalid_Handle));
   end Process_Open_Request;


   procedure Process_Read_Request(Incoming_Message : in Message_Record)
     with Pre => API.Is_Read_Request(Incoming_Message)
   is
      Handle : API.File_Handle_Type;
      Amount : API.Read_Size_Type;
      Status : Message_Status_Type;
      Size   : API.Read_Result_Size_Type;
      Data   : CubedOS.Lib.Octet_Array(0 .. API.Read_Size_Type'Last - 1);
   begin
      -- If the read request doesn't decode properly we just don't send a reply at all?
      API.Read_Request_Decode(Incoming_Message, Handle, Amount, Status);
      if Status = Success then
         if Octet_IO.Is_Open(Files(Handle).Underlying) then
            Size := 0;
            begin
               while Size < Amount loop
                  Octet_IO.Read(Files(Handle).Underlying, Data(Size));
                  Size := Size + 1;
               end loop;
            exception
               when Octet_IO.End_Error =>
                  null;
            end;
            -- Send what we have (could be zero octets!).
            Message_Manager.Route_Message
              (API.Read_Reply_Encode
                 (Receiver_Domain => Incoming_Message.Sender_Domain,
                  Receiver   => Incoming_Message.Sender,
                  Request_ID => Incoming_Message.Request_ID,
                  Handle     => Handle,
                  Amount     => Size,
                  Data       => Data));
         end if;
      end if;
   end Process_Read_Request;


   procedure Process_Write_Request(Incoming_Message : in Message_Record)
     with Pre => API.Is_Write_Request(Incoming_Message)
   is
      Handle : API.File_Handle_Type;
      Amount : API.Read_Size_Type;
      Status : Message_Status_Type;
      Size   : API.Read_Result_Size_Type;
      Data   : CubedOS.Lib.Octet_Array(0 .. API.Read_Size_Type'Last - 1);
   begin
      -- If the read request doesn't decode properly we just don't send a reply at all?
      API.Write_Request_Decode(Incoming_Message, Handle, Amount, Data, Status);
      if Status = Success then
         if Octet_IO.Is_Open(Files(Handle).Underlying) then
            Size := 0;
            begin
	    -- Loop thorugh data to write each character
	       while Size < Amount loop
	          Octet_IO.Write(Files(Handle).Underlying, Data(Size));
                  Size := Size + 1;
               end loop;
            exception
               when Octet_IO.End_Error =>
                  null;
            end;
            Message_Manager.Route_Message
              (API.Write_Reply_Encode
                 (Receiver_Domain => Incoming_Message.Sender_Domain,
                  Receiver   => Incoming_Message.Sender,
                  Request_ID => Incoming_Message.Request_ID,
                  Handle     => Handle,
                  Amount     => Size));
         end if;
      end if;
   end Process_Write_Request;


   procedure Process_Close_Request(Incoming_Message : in Message_Record)
     with Pre => API.Is_Close_Request(Incoming_Message)
   is
      Handle : API.Valid_File_Handle_Type;
      Status : Message_Status_Type;
   begin
      API.Close_Request_Decode(Incoming_Message, Handle, Status);
      if Status = Success then
         if Octet_IO.Is_Open(Files(Handle).Underlying) then
            Octet_IO.Close(Files(Handle).Underlying);
         end if;
      end if;
   end Process_Close_Request;


-- TODO --
   procedure Process(Incoming_Message : in Message_Record) is
   begin
      if API.Is_Open_Request(Incoming_Message) then
         Process_Open_Request(Incoming_Message);
      elsif API.Is_Read_Request(Incoming_Message) then
         Process_Read_Request(Incoming_Message);
      elsif API.Is_Write_Request(Incoming_Message) then
	 Process_Write_Request(Incoming_Message);
      elsif API.Is_Close_Request(Incoming_Message) then
         Process_Close_Request(Incoming_Message);
      else
         -- TODO: What should be done about malformed/unrecognized messages?
         null;
      end if;

   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line("Unhandled exception in File_Server message processor...");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(Ex));
   end Process;


   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;


end CubedOS.File_Server.Messages;
