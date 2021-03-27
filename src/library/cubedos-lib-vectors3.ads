--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-vectors3.ads
-- SUBJECT: Specification of a package for manipulating 3D vectors of floating point numbers.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

generic
   type Float_Type is digits <>;
package CubedOS.Lib.Vectors3 is

   -- There seems to be little point in trying to hide the representation of this basic type.
   -- Clients can use aggregates to construct Vector3s and directly access the components.
   --
   type Vector3 is
      record
         X, Y, Z : Float_Type;
      end record;

   -- Arithmetic Operators
   -----------------------
   -- Only pre-multiplication of a scalar is supported.

   function "+"(Left, Right : Vector3) return Vector3 is
     ( (X => Left.X + Right.X, Y => Left.Y + Right.Y, Z => Left.Z + Right.Z) )
     with Inline;

   function "-"(Left, Right : Vector3) return Vector3 is
     ( (X => Left.X - Right.X, Y => Left.Y - Right.Y, Z => Left.Z - Right.Z) )
     with Inline;

   function "*"(Left : Float_Type; Right : Vector3) return Vector3 is
     ( (X => Left * Right.X, Y => Left * Right.Y, Z => Left * Right.Z) )
     with Inline;

   function "/"(Left : Vector3; Right : Float_Type) return Vector3 is
     ( (X => Left.X / Right, Y => Left.Y / Right, Z => Left.Z / Right) )
     with Inline;

   -- Vector Operations
   --------------------

   -- Computes the dot product of two vectors.
   function Dot(Left, Right : Vector3) return Float_Type is
     (Left.X * Right.X + Left.Y * Right.Y + Left.Z * Right.Z);

   -- Computes the cross product of two vectors.
   function Cross(Left, Right : Vector3) return Vector3;

   -- Computes the magnitude of a vector.
   function Magnitude(V : Vector3) return Float_Type;

   -- Computes a unit vector in the same direction as the given vector.
   function Unit(V : Vector3) return Vector3;

end CubedOS.Lib.Vectors3;
