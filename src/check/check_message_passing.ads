---------------------------------------------------------------------------
-- FILE    : check_message_passing.ads
-- SUBJECT : Package containing tests for the message system.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
-- Tests intra-domain message passing.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Message_Passing is

   type Message_Passing_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Message_Passing_Test);
   function Name(T : in Message_Passing_Test) return AUnit.Message_String;

end Check_Message_Passing;
