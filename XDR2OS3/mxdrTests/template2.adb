--------------------------------------------------------------------------------
-- FILE   : cubedos-%MODULENAME%-api.adb
-- SUBJECT: Body of a package that implements the %MODULENAME% API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.%MODULENAME%.API is

   %BULK%

end CubedOS.%MODULENAME%.API;
