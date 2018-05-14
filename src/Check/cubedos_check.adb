---------------------------------------------------------------------------
-- FILE   : cubedos_check.adb
-- SUBJECT: The main entry point for the CubedOS unit test program.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- Please send comments or bug reports to
--
--      CubeSat Laboratory
--      c/o Dr. Carl Brandon
--      Vermont Technical College
--      Randolph Center, VT 05061 USA
--      CBrandon@vtc.vsc.edu
---------------------------------------------------------------------------
with CubedOS.CFDP.Internals.Check;
with CubedOS.Lib.Bounded_Strings.Check;
with CubedOS.Lib.CRC.Check;
with CubedOS.Lib.XDR.Check;

procedure CubedOS_Check is
begin
   CubedOS.CFDP.Internals.Check.Run_Tests;
   CubedOS.Lib.Bounded_Strings.Check.Run_Tests;
   CubedOS.Lib.CRC.Check.Run_Tests;
   CubedOS.Lib.XDR.Check.Run_Tests;
end CubedOS_Check;
