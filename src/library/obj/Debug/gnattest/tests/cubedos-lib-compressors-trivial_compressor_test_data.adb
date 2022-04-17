--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data is

   Local_Trivial_Compressor : aliased GNATtest_Generated.GNATtest_Standard.CubedOS.Lib.Compressors.Trivial_Compressor;
   procedure Set_Up (Gnattest_T : in out Test_Trivial_Compressor) is
   begin
      Gnattest_T.Fixture := Local_Trivial_Compressor'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Trivial_Compressor) is
   begin
      null;
   end Tear_Down;

end CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data;
