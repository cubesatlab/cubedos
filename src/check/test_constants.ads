
with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.Time_Server.API;

-- Ada is really picky about where access constants are defined so we have to
-- add a file for them.
package Test_Constants is

   Receives_Tick_Messages : aliased constant Message_Type_Array := (1 => CubedOS.Time_Server.API.Tick_Reply_Msg);

end Test_Constants;
