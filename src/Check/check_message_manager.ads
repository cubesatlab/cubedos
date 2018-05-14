--------------------------------------------------------------------------------
-- FILE   : check_message_manager.ads
-- SUBJECT: Package containing unit tests of the message manager.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

use AUnit.Test_Cases;

package Check_Message_Manager is

   type Message_Manager_Test is new Test_Case with null record;

   overriding procedure Register_Tests(T : in out Message_Manager_Test);
   overriding function Name(T : Message_Manager_Test) return AUnit.Message_String;

end Check_Message_Manager;
