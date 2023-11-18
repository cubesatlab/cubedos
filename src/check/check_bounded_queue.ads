---------------------------------------------------------------------------
-- FILE    : check_bounded_queue.ads
-- SUBJECT : Package containing tests the bounded queue.
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Bounded_Queue is

   type Queue_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Queue_Test);
   function Name(T : in Queue_Test) return AUnit.Message_String;

end Check_Bounded_Queue;
