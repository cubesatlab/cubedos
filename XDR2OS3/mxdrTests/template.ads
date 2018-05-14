--------------------------------------------------------------------------------
-- FILE   : cubedos-%MODULENAME%-api.ads
-- SUBJECT: Specification of a package that defines the %MODULENAME% API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.%MODULENAME%.API is

   %BULK%

end CubedOS.%MODULENAME%.API;
