--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Real_Time;
with GNAT.Sockets;   use GNAT.Sockets;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body CubedOS.Lib.Network is

    procedure Server_Loop_Task is
        task Server_Loop;
        Server        : Socket_Type;
        Address, From : Sock_Addr_Type;
        Data          : Ada.Streams.Stream_Element_Array (1 .. 512);
        Last          : Ada.Streams.Stream_Element_Offset;
        use type Ada.Real_Time.Time;
        Next_Release : Ada.Real_Time.Time :=
           Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (1_000);
        task body Server_Loop is
        begin
            Create_Socket (Server, Family_Inet, Socket_Datagram);
            Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
            Address.Addr := Any_Inet_Addr;
            Address.Port := 50_001;
            Ada.Text_IO.Put_Line ("My Addr: " & Image (Address.Addr));
            Bind_Socket (Server, Address);
            loop
                begin
                    GNAT.Sockets.Receive_Socket (Server, Data, Last, From);
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
    begin
        Ada.Text_IO.Put_Line ("In Server Loop Task Procedure");
    end Server_Loop_Task;

    procedure Send_Network_Message is
    begin
        Ada.Text_IO.Put_Line ("Send message");
    end Send_Network_Message;

end CubedOS.Lib.Network;
