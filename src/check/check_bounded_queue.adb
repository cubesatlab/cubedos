---------------------------------------------------------------------------
-- FILE    : check_bounded_queue.adb
-- SUBJECT : Package containing tests of bounded queue.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;
with CubedOS.Lib.Bounded_Queues;

package body Check_Bounded_Queue is

   package Bool_Queues is new CubedOS.Lib.Bounded_Queues(Boolean);
   Queue1 : Bool_Queues.Bounded_Queue := Bool_Queues.Make(3);
   use Bool_Queues;
   type Bool_Owner is access Boolean;
   Bool : Data_Owner := new Boolean'(False);
   Bool_False : constant Data_Owner := new Boolean'(False);
   Bool_True : constant Data_Owner := new Boolean'(True);
   procedure Take_Next is
   begin
      Bool := null;
      Bool_Queues.Next(Queue1, Bool);
   end Take_Next;

   procedure Put_Another is
      Value : Data_Owner := new Boolean'(True);
   begin
      Put(Queue1, Value);
   end Put_Another;

   -- Make some instantiations in SPARK
   procedure SPARK_Inits
     with SPARK_Mode => On
   is
      package Bool_Queues is new CubedOS.Lib.Bounded_Queues(Boolean);
   begin
      null;
   end SPARK_Inits;


   procedure Test_Queue(T : in out AUnit.Test_Cases.Test_Case'Class) is
      False_Value : Data_Owner := new Boolean'(False);
   begin

      Assert(Size(Queue1) = 3, "Created queue with wrong size");
      Assert(Count(Queue1) = 0, "Created queue with elements");

      Assert_Exception(Take_Next'Access, "Queue allowed user to take nonexistant element");

      Put(Queue1, False_Value); -- The first element out should be False

      Assert(Count(Queue1) = 1, "Failed to add element");

      Put_Another; -- The next two elements are True
      Put_Another;

      Assert_Exception(Put_Another'Access, "Queue took element even though it is full");

      Assert(Count(Queue1) = Size(Queue1), "When queue is full, count should equal size");
      Assert(Size(Queue1) = 3, "Queue size changed");

      Next(Queue1, Bool);
      Assert(Bool.all = False, "Queue fetched eroneous element 1");
      Bool := null;
      Next(Queue1, Bool);
      Assert(Bool.all = True, "Queue fetched eroneous element 2");
      Bool := null;

      Assert(Count(Queue1) = 1, "Queue failed to remove elements");

      Next(Queue1, Bool);
      Bool := null;
      Assert_Exception(Take_Next'Access, "Queue allowed user to take nonexistant element");

      -- Now add two more so the queue has to wrap around its array
      Put_Another;
      Put_Another;
      Put_Another;

      Assert(Count(Queue1) = Size(Queue1), "When queue is full, count should equal size");
      Assert(Size(Queue1) = 3, "Queue size changed");

   end Test_Queue;


   procedure Register_Tests(T : in out Queue_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Queue'Access, "Test_Bounded_Queue");
   end Register_Tests;


   function Name(T : in Queue_Test) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("Bounded Queue");
   end Name;

end Check_Bounded_Queue;