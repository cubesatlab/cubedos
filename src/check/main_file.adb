--------------------------------------------------------------------------------
-- FILE   : main_file.adb
-- SUBJECT: The main file to test the file server package.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- In order to test the file server module, you must run the produced executable in a console
-- window, and ^C out of it when a suitable time has passed. Then observe the behavior to ensure
-- the desired effects are happening.
--------------------------------------------------------------------------------
with Ada.Text_IO;
with CubedOS.File_Server.API;
with CubedOS.File_Server.Messages;
with Message_Manager;
with Test_Constants;

use Ada.Text_IO;
use CubedOS;
use CubedOS.File_Server.API;
use Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;

with Name_Resolver;

procedure Main_File is

   -- Use the time server's module id because it isn't needed in this check
   My_Module_ID : constant Module_ID_Type := Name_Resolver.Time_Server;
   Metadata : constant Module_Metadata := Define_Module(My_Module_ID, Test_Constants.Receives_File_Messages'Access);
   My_Mailbox : constant Module_Mailbox := Make_Module_Mailbox(My_Module_ID, Metadata);

   Incoming_Message : Message_Record;
   Handle       : File_Handle_Type;
   Read_Handle  : Valid_File_Handle_Type;
   Write_Handle : File_Handle_Type;
   Amount_Write : Write_Result_Size_Type;
   Data   : File_Server.API.Octet_Array_Ptr;
   Status : Message_Status_Type;
begin
   CubedOS.File_Server.Messages.Init;
   Register_Module(My_Mailbox, 8);

   Message_Manager.Skip_Mailbox_Initialization;

   Message_Manager.Wait;

   -- TEST : Open two files. Read from one file, and write to another file.

   -- Try to open a file to read.
   Send_Open_Request
     (My_Mailbox, File_Server.API.Mail_Target, 1, Read, "example.txt");
   Put_Line("TX : Open_Request message sent for reading 'example.txt'");

   -- Try to open file for writing
   Send_Open_Request
     (My_Mailbox, File_Server.API.Mail_Target, 2, Write, "write_test.txt");
   Put_Line("TX : Open_Request message sent for writing 'write_test.txt'");

   loop
      Message_Manager.Read_Next(My_Mailbox, Incoming_Message);
      Put_Line("+++ Fetch returned!");
      Put_Line(Stringify_Message(Incoming_Message));
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
                  Read_Handle := Valid_File_Handle_Type(Handle);
                  Send_Read_Request
                    (My_Mailbox, File_Server.API.Mail_Target, 0, Read_Handle, Read_Size_Type'Last);
                  Put_Line("TX : Read_Request message sent requesting " & Integer'Image(Max_Read_Size) & " octets");

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

         Read_Reply_Decode(Incoming_Message, Handle, Data, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
            Send_Close_Request(My_Mailbox, File_Server.API.Mail_Target, 0, Read_Handle);
            Put_Line("TX : Close_Request message sent (input file)");
         else
            Put_Line("     +++ Successfully read " & Integer'Image(Data'Length) & " octets");

            if Data'Length = 0 then
               -- Close the files (both of them).
               Send_Close_Request(My_Mailbox, Message_Address'(Domain_ID, My_Module_ID), 0, Read_Handle);
               Put_Line("TX : Close_Request message sent (input file)");

               Send_Close_Request(My_Mailbox, File_Server.API.Mail_Target, 0, Write_Handle);
               Put_Line("TX : Close_Request message sent (output file)");
            else
               -- Print the data we just read (it's a text file).
               for I in 0 .. Natural(Data'Length) - 1 loop
                  Put(Character'Val(Data(I)));
               end loop;
               New_Line;  -- Just to make the output look a little nicer.

               -- Write this data to the output file too.
               declare
                  Write_Amount : constant Write_Size_Type :=  Write_Size_Type(Data'Size);
               begin
                  Send_Write_Request(My_Mailbox, File_Server.API.Mail_Target, 0, Write_Handle, Data.all);
                  Put_Line("TX : Write_Request message sent writing " & Write_Size_Type'Image(Write_Amount) & " octets");
               end;
               -- Request the next chunk from the file.
               Send_Read_Request(My_Mailbox, File_Server.API.Mail_Target, 0, Read_Handle, Read_Size_Type'Last);
               Put_Line("TX : Read_Request message sent requesting " & Read_Size_Type'Image(Read_Size_Type'Last) & " octets");
            end if;
         end if;

      elsif Is_Write_Reply(Incoming_Message) then
         Put_Line("RX : Write_Reply message");

         Write_Reply_Decode(Incoming_Message, Handle, Amount_Write, Status);
         if Status = Malformed then
            Put_Line("     *** Message is malformed!");
            Send_Close_Request(My_Mailbox, File_Server.API.Mail_Target, 0, Write_Handle);
            Put_Line("TX : Close_Request message sent (output file)");
         else
            Put_Line("     +++ Successfully wrote " & Write_Result_Size_Type'Image(Amount_Write) & " octets");
         end if;
      end if;
   end loop;

end Main_File;
