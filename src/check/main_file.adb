--------------------------------------------------------------------------------
-- FILE   : main_file.adb
-- SUBJECT: The main file to test the file server package.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
-- In order to test the file server module, you must run the produced executable in a console
-- window, and ^C out of it when a suitable time has passed. Then observe the behavior to ensure
-- the desired effects are happening.
--------------------------------------------------------------------------------
with Ada.Text_IO;
with CubedOS.Lib;
with CubedOS.File_Server.API;
with CubedOS.File_Server.Messages;
with Message_Manager;

use Ada.Text_IO;
use CubedOS;
use CubedOS.File_Server.API;
use Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;

procedure Main_File is

   -- Be sure this module ID doesn't conflict with any of the CubedOS core modules.
   My_Module_ID : constant Module_ID_Type := Module_ID_Type'Last;
   Metadata : constant Module_Metadata := Declare_Receives(My_Module_ID, Empty_Type_Array_Ptr'Access);
   My_Mailbox : constant Module_Mailbox := Make_Module_Mailbox(My_Module_ID, Metadata);

   Incoming_Message : Message_Manager.Message_Record;
   Handle       : File_Handle_Type;
   Read_Handle  : File_Handle_Type;
   Write_Handle : File_Handle_Type;
   Amount_Read  : Read_Result_Size_Type;
   Amount_Write : Write_Result_Size_Type;
   Data   : Lib.Octet_Array(0 .. Maximum_Read_Size - 1);
   Status : Message_Manager.Message_Status_Type;
begin
   CubedOS.File_Server.Messages.Init;
   Register_Module(My_Mailbox, 8);

   -- TEST : Open two files. Read from one file, and write to another file.

   -- Try to open a file to read.
   Send_Open_Request
     (My_Mailbox, (Domain_ID, My_Module_ID), 1, Read, "example.txt");
   Put_Line("TX : Open_Request message sent for reading 'example.txt'");

   -- Try to open file for writing
   Send_Open_Request
     (My_Mailbox, (Domain_ID, My_Module_ID), 2, Write, "write_test.txt");
   Put_Line("TX : Open_Request message sent for writing 'write_test.txt'");

   loop
      Message_Manager.Read_Next(My_Mailbox, Incoming_Message);
      Put_Line("+++ Fetch returned!");
      Put_Line(Message_Manager.Stringify_Message(Incoming_Message));
      New_Line;

      -- Process open reply messages.
      if Is_Open_Reply(Incoming_Message) then
         Put_Line
           ("RX : Open_Reply message (Request_ID = " &
              Request_ID_Type'Image(Request_ID(Incoming_Message)) & ")");

         Open_Reply_Decode(Incoming_Message, Handle, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
         elsif Handle = Invalid_Handle then
            Put_Line("     *** File failed to open!");
         else
            Put_Line("     +++ File opened successfully!");

            case Request_ID(Incoming_Message) is
               when 1 =>
                  -- We got a reply to our open-for-reading request.
                  Read_Handle := Handle;
                  Send_Read_Request
                    (My_Mailbox, (Domain_ID, My_Module_ID), 0, Read_Handle, Maximum_Read_Size);
                  Put_Line("TX : Read_Request message sent requesting " & Integer'Image(Maximum_Read_Size) & " octets");

               when 2 =>
                  -- We got a reply to our open-for-writing request.
                  Write_Handle := Handle;

               when others =>
                  -- Should never happen. Let's just ignore this.
                  null;
            end case;
         end if;

      -- Process reply messages.
      elsif Is_Read_Reply(Incoming_Message) then
         Put_Line("RX : Read_Reply message");

         Read_Reply_Decode(Incoming_Message, Handle, Amount_Read, Data, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
            Send_Close_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Read_Handle);
            Put_Line("TX : Close_Request message sent (input file)");
         else
            Put_Line("     +++ Successfully read " & Read_Size_Type'Image(Amount_Read) & " octets");

            if Amount_Read = 0 then
               -- Close the files (both of them).
               Send_Close_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Read_Handle);
               Put_Line("TX : Close_Request message sent (input file)");

               Send_Close_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Write_Handle);
               Put_Line("TX : Close_Request message sent (output file)");
            else
               -- Print the data we just read (it's a text file).
               for I in 0 .. Amount_Read - 1 loop
                  Put(Character'Val(Data(I)));
               end loop;
               New_Line;  -- Just to make the output look a little nicer.

               -- Write this data to the output file too.
               Amount_Write := Amount_Read;
               Send_Write_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Write_Handle, Amount_Write, Data);
               Put_Line("TX : Write_Request message sent writing " & Integer'Image(Amount_Write) & " octets");

               -- Request the next chunk from the file.
               Send_Read_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Read_Handle, Maximum_Read_Size);
               Put_Line("TX : Read_Request message sent requesting " & Integer'Image(Maximum_Read_Size) & " octets");
            end if;
         end if;

      elsif Is_Write_Reply(Incoming_Message) then
         Put_Line("RX : Write_Reply message");

         Write_Reply_Decode(Incoming_Message, Handle, Amount_Write, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
            Send_Close_Request(My_Mailbox, (Domain_ID, My_Module_ID), 0, Write_Handle);
            Put_Line("TX : Close_Request message sent (output file)");
         else
            Put_Line("     +++ Successfully wrote " & Write_Result_Size_Type'Image(Amount_Write) & " octets");
         end if;
      end if;
   end loop;

end Main_File;
