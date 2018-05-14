--------------------------------------------------------------------------------
-- FILE   : main_file.adb
-- SUBJECT: The main file to test the file server package.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- In order to test the file server module, you must run the produced executable
-- in the command prompt, and ^C out of it when a suitable time has passed. Then,
-- observe the behavior to ensure the desired effects are happening.
--------------------------------------------------------------------------------

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with CubedOS.Lib;
with CubedOS.File_Server.API;
with CubedOS.File_Server.Messages;
pragma Unreferenced(CubedOS.File_Server.Messages);
with Message_Manager;

use Ada.Integer_Text_IO;
use Ada.Text_IO;
use CubedOS;
use CubedOS.File_Server.API;
use Message_Manager;

procedure Main_File is

   -- Be sure this module ID doesn't conflict with any of the CubedOS core modules.
   My_Module_ID : constant Message_Manager.Module_ID_Type := 16;

   Incoming_Message : Message_Manager.Message_Record;
   -- Outgoing_Message : Message_Manager.Message_Record;
   Handle       : File_Handle_Type;
   Read_Handle  : File_Handle_Type;
   Write_Handle : File_Handle_Type;
   Amount_Read  : Read_Result_Size_Type;
   Amount_Write : Write_Result_Size_Type;
   Data   : Lib.Octet_Array(0 .. Maximum_Read_Size - 1);
   Data_Write : constant Lib.Octet_Array(0 .. Maximum_Write_Size - 1) :=
     (others => Character'Pos('x'));
   Status : Message_Manager.Message_Status_Type;
   -- Request_ID : Request_ID_Type;
begin
   -- TEST : Open two files. Read from one file, and write to another file.

   -- Try to open file for writing
   Message_Manager.Route_Message
     (Open_Request_Encode(Domain_ID, My_Module_ID, 2, Write, "write_test.txt"));
   Put_Line("TX : Open File message for 'write_test.txt'");


   -- Try to open a file to read.
  -- Message_Manager.Mailboxes(File_Server.ID).Unchecked_Send
    -- (Open_Request_Encode(My_Module_ID, Read, 1, "example.txt"));
  -- Put_Line("TX : Open File message for 'example.txt'");

   loop
      Message_Manager.Fetch_Message(My_Module_ID, Incoming_Message);
      Put_Line("+++ Fetch returned!");
      Put("+++    Sender    : "); Put(Incoming_Message.Sender); New_Line;
      Put("+++    Receiver  : "); Put(Incoming_Message.Receiver); New_Line;
      Put("+++    Message_ID: "); Put(Incoming_Message.Message_ID); New_Line(2);

      -- Process open reply messages.
      if Is_Open_Reply(Incoming_Message) then
         Put_Line("RX : Open File reply message");

         Open_Reply_Decode(Incoming_Message, Handle, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
         elsif Handle = Invalid_Handle then
            Put_Line("     *** File failed to open!");
         else
            Put_Line("     +++ File opened successfully!");

	    if Incoming_Message.Request_ID = 1 then
               -- Request to read file
               Read_Handle := Handle;
	       Message_Manager.Route_Message
		 (Read_Request_Encode(Domain_ID, My_Module_ID, 0, Read_Handle, Maximum_Read_Size));
	       Put_Line("TX : Read File message requesting " & Integer'Image(Maximum_Read_Size) & " octets");
	    else
               -- Request to write to file
               Write_Handle := Handle;
	       Message_Manager.Route_Message
		 (Write_Request_Encode(Domain_ID, My_Module_ID, 0, Write_Handle, Maximum_Write_Size, Data_Write));
	       Put_Line("TX : Write File message writing " & Integer'Image(Maximum_Write_Size) & " octets");
	    end if;
         end if;

      -- Process reply messages.
      elsif Is_Read_Reply(Incoming_Message) then
         Put_Line("RX : Read File reply message");

         Read_Reply_Decode(Incoming_Message, Handle, Amount_Read, Data, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
	    Message_Manager.Route_Message
	      (Close_Request_Encode(Domain_ID, My_Module_ID, 0, Read_Handle));
	    Put_Line("TX : Close File message");
         else
            Put_Line
              ("     +++ Successfully read " & Read_Size_Type'Image(Amount_Read) & " octets");
            if Amount_Read > 0 then
               -- Print the data (we are reading a text file).
               for I in 0 .. Amount_Read - 1 loop
                  Put(Character'Val(Data(I)));
               end loop;
               New_Line;  -- Just to make the output look a little nicer.

               Message_Manager.Route_Message
                 (Read_Request_Encode(Domain_ID, My_Module_ID, 0, Read_Handle, Maximum_Read_Size));
               Put_Line("TX : Read File message requesting " & Integer'Image(Maximum_Read_Size) & " octets");
            else
               Message_Manager.Route_Message
                 (Close_Request_Encode(Domain_ID, My_Module_ID, 0, Read_Handle));
               Put_Line("TX : Close File message");
            end if;
         end if;

      elsif Is_Write_Reply(Incoming_Message) then
	 Put_Line("RX : Write File reply message");

         Write_Reply_Decode(Incoming_Message, Handle, Amount_Write, Status);
	 if Status = Malformed then
	    Put_Line("     *** Message is malformed!");
	    Message_Manager.Route_Message
	      (Close_Request_Encode(Domain_ID, My_Module_ID, 0, Write_Handle));
	    Put_Line("TX : Close File message");
	 else
	 -- have only write to file once
	    Put_Line
              ("     +++ Successfully wrote " & Write_Result_Size_Type'Image(Amount_Write) & " octets");

            Message_Manager.Route_Message
              (Close_Request_Encode(Domain_ID, My_Module_ID, 0, Write_Handle));
            Put_Line("TX : Close File message");
	 end if;

      end if;
   end loop;
end Main_File;
