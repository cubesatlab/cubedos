--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data is

   type Trivial_Compressor_Access is access all GNATtest_Generated.GNATtest_Standard.CubedOS.Lib.Compressors.Trivial_Compressor'Class;

--  begin read only
   type Test_Trivial_Compressor is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Trivial_Compressor_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Trivial_Compressor);
   procedure Tear_Down (Gnattest_T : in out Test_Trivial_Compressor);

end CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data;
