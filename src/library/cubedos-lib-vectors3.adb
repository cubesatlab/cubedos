--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-vectors3.adb
-- SUBJECT: Body of a package for manipulating 3D vectors of floating point numbers.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Numerics.Generic_Elementary_Functions;

package body CubedOS.Lib.Vectors3 is

   package Float_Type_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions(Float_Type);
   use Float_Type_Elementary_Functions;

   function Cross(Left, Right : in Vector3) return Vector3 is
      New_X : Float_Type;
      New_Y : Float_Type;
      New_Z : Float_Type;
   begin
      New_X := Left.Y * Right.Z - Left.Z * Right.Y;
      New_Y := Left.Z * Right.X - Left.X * Right.Z;
      New_Z := Left.X * Right.Y - Left.Y * Right.X;
      return (New_X, New_Y, New_Z);
   end Cross;


   function Magnitude(V : in Vector3) return Float_Type is
      Result : Float_Type;
   begin
      Result := Sqrt( (V.X * V.X) + (V.Y * V.Y) + (V.Z * V.Z) );
      return Result;
   end Magnitude;


   function Unit(V : in Vector3) return Vector3 is
      Length : constant Float_Type := Magnitude(V);
   begin
      return ( X => V.X/Length, Y => V.Y/Length, Z => V.Z/Length );
   end Unit;

end CubedOS.Lib.Vectors3;
