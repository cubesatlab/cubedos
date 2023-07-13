--------------------------------------------------------------------------------
-- FILE   : test_constants.ads
-- SUBJECT: Some constants used in tests.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- Ada is very picky about when it is safe to 'Access things, so this file
-- defines some constants which are aliased in various test programs.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.Time_Server.API;
with CubedOS.File_Server.API;

-- Ada is really picky about where access constants are defined so we have to
-- add a file for them.
package Test_Constants is

   Receives_Tick_Messages : aliased constant Message_Type_Array := (1 => CubedOS.Time_Server.API.Tick_Reply_Msg);

   Receives_File_Messages : aliased constant Message_Type_Array := (CubedOS.File_Server.API.Open_Reply_Msg,
                                                                    CubedOS.File_Server.API.Open_Reply_Msg,
                                                                    CubedOS.File_Server.API.Read_Reply_Msg,
                                                                    CubedOS.File_Server.API.Write_Reply_Msg);
end Test_Constants;
