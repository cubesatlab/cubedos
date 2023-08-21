--------------------------------------------------------------------------------
-- FILE   : ping_client-api.adb
-- SUBJECT: Body of a package that implements the Ping_Client API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Warnings(Off);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;
with CubedOS.Message_Types.Mutable; use CubedOS.Message_Types.Mutable;

package body Ping_Client.API is

   procedure Free is new Ada.Unchecked_Deallocation(String, String_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Octet_Array, Octet_Array_Ptr);

end Ping_Client.API;
