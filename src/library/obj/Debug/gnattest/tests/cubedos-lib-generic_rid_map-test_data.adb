--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body CubedOS.Lib.Generic_rID_Map.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      User_Set_Up (X);
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      User_Tear_Down (X);
   end Tear_Down;

   procedure User_Set_Up (Gnattest_T : in out Test) is
   begin
      null;
   end User_Set_Up;

   procedure User_Tear_Down (Gnattest_T : in out Test) is
   begin
      null;
   end User_Tear_Down;

end CubedOS.Lib.Generic_rID_Map.Test_Data;
