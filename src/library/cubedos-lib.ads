--------------------------------------------------------------------------------
-- FILE   : cubedos-lib.ads
-- SUBJECT: Specification of a package serving as a parent to the CubedOS runtime library.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- This package also defines a number of useful, library-wide data types.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package CubedOS.Lib is
   pragma Pure;

   type Octet is mod 2**8;
   type Double_Octet is mod 2**16;
   type Quadruple_Octet is mod 2**32;
   type Octuple_Octet is mod 2**64;

   -- Limiting the maximum size of arrays simplfies proving freedom from overflow in array index
   -- computations. There is no other particular reason for this limit; it is arbitrary. The
   -- limit should be high enough to satisfy any reasonable application.
   --
   Maximum_Array_Size : constant := 2**16;

   -- Starting the index type at 0 is consistent with low level expectations.
   subtype Octet_Array_Index is Natural range 0 .. Maximum_Array_Size - 1;
   subtype Octet_Array_Count is Natural range 0 .. Maximum_Array_Size;
   type Octet_Array is array (Octet_Array_Index range <>) of Octet
     with Component_Size => 8;
   Zero_Width_Octet_Array : aliased constant Octet_Array(1..0) := (others => 0);

   -- Declare intrinsic shift functions for these types.

   function Shift_Left(Value : in Octet; Count : in Natural) return Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : in Octet; Count : in Natural) return Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Left(Value : in Double_Octet; Count : in Natural) return Double_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : in Double_Octet; Count : in Natural) return Double_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Left(Value : in Quadruple_Octet; Count : in Natural) return Quadruple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : in Quadruple_Octet; Count : in Natural) return Quadruple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

end CubedOS.Lib;
