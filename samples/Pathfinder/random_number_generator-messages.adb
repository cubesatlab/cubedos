--------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

--  Needed so that the types in the API can be used here.
with Random_Number_Generator.API;


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

package body Random_Number_Generator.Messages is
   type Random_Range is range 1..100;
   package Random_Integer is new Ada.Numerics.Discrete_Random(Random_Range);

   function Get_Random_Number return Random_Range is
      Number_Generator : Random_Integer.Generator;
   begin
      Random_Integer.Reset(Number_Generator);
      return Random_Integer.Random(Number_Generator);
   end Get_Random_Number;
   
   use Message_Manager;
   ML1 : Message_Loop;   
   Count : Positive := 1;
   
   --  The package initializer, if needed.  This procedure might be
   --  called as the message loop (see below) is starting, or perhaps
   --  during package elaboration.  If this procedure is not needed,
   --  it should be removed to avoid SPARK flow issues.
   --
   procedure Initialize is
   begin
      null;
   end Initialize;
   
   --------------- Message Handling ---------------
   
   --  Here is where the details of handing the messages is done.
   --  Probably there will be a separate subprogram for each message,
   --  but the exact organization is, obviously, dependent on the
   --  needs of the module.  It might be useful to put these message
   --  handling subprograms into a private sibling package.  That
   --  would move the complex details of message handling to a
   --  different file and reduce clutter in this file.  This file is
   --  really just about decoding and dispatching the messages.  We
   --  recommend that if a single internal package is used that it
   --  sould be called Sample_Module.Core (for example).
   
   procedure Handle_Generate_Number_Request(Message : in Message_Record)
     with Pre => Random_Number_Generator.API.Is_Generate_Number_Request(Message)
   is
      
      Random_Number : Random_Range;
      Status : Message_Status_Type;
      Fib_Seed : constant Natural := 40;
      Fib_Number : Natural;
      Msg : constant String := "L: Generating Fibonacci (" & Fib_Seed'Image & " ) to waste time...";
      
      function Fibonacci (N : in Natural) return Natural is
      begin
         if N = 0 then 
            return 0;
         end if;
         if N = 1 then
            return 1;   
         end if;
         
         return Fibonacci(N-1) + Fibonacci(N-2);         
      end Fibonacci;
      
   begin
      Random_Number_Generator.API.Generate_Number_Request_Decode(Message, Status);
      --  Act on the request message.
      Random_Number := Get_Random_Number;
      Ada.Text_IO.Put_Line(Msg);
      
      Fib_Number := Fibonacci(Fib_Seed);
      
      Ada.Text_IO.Put("L: Fibonacci (" & Fib_Seed'Image & " ) is: ");
      Ada.Text_IO.Put_Line(Fib_Number'Image);
      
      Ada.Text_IO.Put("L: (" & Count'Image & " ) ");
      Ada.Text_IO.Put("+++ Random Number: ");
      Ada.Text_IO.Put_Line(Random_Range'Image(Random_Number));
      Ada.Text_IO.New_Line;
      
      Count := Count + 1;

   end Handle_Generate_Number_Request;
   
   -----------------------------------
   --  Message Decoding and Dispatching
   -----------------------------------
   
   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Random_Number_Generator.API.Is_Generate_Number_Request(Message) then
         Handle_Generate_Number_Request(Message);
      else
	 --  An unknown message type has been received. What should be
	 --  done about that?
         null;
      end if;
      --  When this procedure returns the message loop will
      --  immediately try to receive the next message.  Note that all
      --  CubedOS send operations are non-blocking so sending an
      --  outgoing message will not delay execution.
   end Process;
   
   ---------------
   -- Message Loop
   ---------------
   
   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      --  Initialize the internal workings of the module (if required)
      --  before processing any of its messages.  It may instead be
      --  appropriate for the package to initialize itself at
      --  elaboration time.  Note that SPARK requires that
      --  applications use the configuration pragma of
      --  Partition_Elaboration_Policy set to Sequential.  This
      --  ensures that all packages are elaborated before any library
      --  level tasks start.
      --
      Initialize;

      --  Process messages as they arrive.  This simple loop may be
      --  all that is needed.  It may also be appropriate to do some
      --  prechecks or preprocessing of messages before calling
      --  Process_Message.
      --
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Random_Number_Generator.Messages;
