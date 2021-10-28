--------------------------------------------------------------------------------
-- FILE   : cubedos-interpreter.ads
-- SUBJECT: Top level package of the Interpreter module
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Interpreter is

   ID : constant Message_Manager.Module_ID_Type := 8;

end CubedOS.Interpreter;
