--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Generic_Message_Manager;
with Ada.Real_Time; use type Ada.Real_Time.Time;
with GNAT.Sockets;   use GNAT.Sockets;
with Ada.Streams; 
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
package body CubedOS.Lib.Network is
    
   function Read_Stream_Message ( Data : Ada.Streams.Stream_Element_Array; Last : Ada.Streams.Stream_Element_Offset) return Integer is
      Char : Character;
   begin
      Ada.Text_IO.Put_Line ("Incoming Message Data");
        
      for I in 1..Last loop
         Ada.Text_IO.Put_Line("" & Character'Val(Data(I)));
      end loop;
    
      return 1;
   end Read_Stream_Message;
    
   procedure Server_Loop is
      Server        : Socket_Type;
      Address, From : Sock_Addr_Type;
      Data          : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last          : Ada.Streams.Stream_Element_Offset;
      Temp : Integer;
      Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (1_000);
   begin
      Ada.Text_IO.Put_Line ("In Server Loop Task");
      Create_Socket (Server, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Address.Addr := Any_Inet_Addr;
      Address.Port := 50_001;
      Ada.Text_IO.Put_Line ("My Addr: " & Image (Address.Addr));
      Bind_Socket (Server, Address);
        
      loop
         begin
            GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
            Temp := Read_Stream_Message(Data, Last);
            Ada.Text_IO.Put_Line ("last : " & Last'Img);
            Ada.Text_IO.Put_Line ("from : " & Image (From.Addr));
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Exception_Name (E) & ": " & Exception_Message (E));
         end;
         delay until Next_Release;
         Next_Release :=
           Next_Release + Ada.Real_Time.Milliseconds (1_000);
      end loop;
   end Server_Loop;

   procedure Send_Network_Message(Message_Description : in String )is
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Last : Ada.Streams.Stream_Element_Offset;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Message_Description'Length));
        
   begin
      Address.Port := 50001;
      Address.Addr := Inet_Addr ("127.0.0.1");
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      -- Copy Descr to Buffer
      for I in Buffer'Range loop
         Buffer (I) := Ada.Streams.Stream_Element (Character'Pos (Message_Description (Integer (I))));
      end loop; 
        
      Send_Socket (Socket, Buffer, Last, Address);        
   end Send_Network_Message;
    
    
   task body Network_Loop is
   begin
      Ada.Text_IO.Put_Line ("Start Network Loop");
      Server_Loop;
   end Network_Loop;

end CubedOS.Lib.Network;
