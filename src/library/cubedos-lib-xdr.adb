--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-xdr.adb
-- SUBJECT: Body of an XDR encoding/decoding package.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Unchecked_Conversion;

package body CubedOS.Lib.XDR is

   function XDR_Integer_To_Unsigned is
     new Ada.Unchecked_Conversion(Source => XDR_Integer, Target => XDR_Unsigned);
   function XDR_Unsigned_To_Integer is
     new Ada.Unchecked_Conversion(Source => XDR_Unsigned, Target => XDR_Integer);

   function XDR_Hyper_To_Unsigned is
     new Ada.Unchecked_Conversion(Source => XDR_Hyper, Target => XDR_Unsigned_Hyper);
   function XDR_Unsigned_To_Hyper is
     new Ada.Unchecked_Conversion(Source => XDR_Unsigned_Hyper, Target => XDR_Hyper);

   -- Floating point types are "not suitable as the target of an unchecked conversion" because
   -- not every possible bit pattern is a distinct, valid value of a floating point type
   -- (specifically, the bit pattern corresponding to NaN is not a "valid" number). See the
   -- SPARK Reference Manual, section 13.9.
   --
   function XDR_Float_To_Unsigned is
     new Ada.Unchecked_Conversion(Source => XDR_Float, Target => XDR_Unsigned);

   function XDR_Double_To_Unsigned is
     new Ada.Unchecked_Conversion(Source => XDR_Double, Target => XDR_Unsigned_Hyper);
   -- function XDR_Unsigned_To_Double is
   --   new Ada.Unchecked_Conversion(Source => XDR_Unsigned_Hyper, Target => XDR_Double);


   procedure Encode
     (Value    : in     XDR_Integer;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
   begin
      Encode(XDR_Integer_To_Unsigned(Value), Data, Position, Last);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Unsigned;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
      Temporary_1 : XDR_Unsigned := Value;
      Temporary_2 : XDR_Unsigned;
   begin
      for I in 1 .. 4 loop
         Temporary_2 := Temporary_1 rem 256;
         Data(Position + (4 - I)) := XDR_Octet(Temporary_2);
         Temporary_1 := Temporary_1 / 256;
      end loop;
      Last := Position + (4 - 1);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Boolean;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
      Temporary : XDR_Unsigned;
   begin
      if Value = XDR_False then
         Temporary := 0;
      else
         Temporary := 1;
      end if;
      Encode(Temporary, Data, Position, Last);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Hyper;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
   begin
      Encode(XDR_Hyper_To_Unsigned(Value), Data, Position, Last);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Unsigned_Hyper;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
      Temporary_1 : XDR_Unsigned_Hyper := Value;
      Temporary_2 : XDR_Unsigned_Hyper;
   begin
      for I in 1 .. 8 loop
         Temporary_2 := Temporary_1 rem 256;
         Data(Position + (8 - I)) := XDR_Octet(Temporary_2);
         Temporary_1 := Temporary_1 / 256;
      end loop;
      Last := Position + (8 - 1);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Float;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
   begin
      Encode(XDR_Float_To_Unsigned(Value), Data, Position, Last);
   end Encode;


   procedure Encode
     (Value    : in     XDR_Double;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
   is
   begin
      Encode(XDR_Double_To_Unsigned(Value), Data, Position, Last);
   end Encode;


   procedure Encode
     (Value    : in     Octet_Array;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Extended_Index_Type)
   is
      Padding_Start : XDR_Index_Type;
      Padding_Stop : XDR_Index_Type;
   begin
      for I in Value'Range loop
         Data(Position + (I - Value'First)) := XDR_Octet(Value(I));
      end loop;

      -- Add padding if necessary.
      if Value'Length rem 4 /= 0 then
         Padding_Start := Position + Value'Length;
         Padding_Stop := Padding_Start + (3 - Value'Length rem 4);
         Data(Padding_Start .. Padding_Stop) := (others => 0);
      end if;
      Last := Position + (Length_With_Padding(Value'Length) - 1);
   end Encode;


   procedure Encode
     (Value    : in     String;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Extended_Index_Type)
   is
      Temporary_Array : Octet_Array(0 .. Value'Length - 1);
   begin
      -- Convert the string to an octet array and then encode the octet array.
      --
      -- This approch puts an artificial restriction on the size of the strings we can encode. Since
      -- XDR_Arrays can be huge, in theory it should be possible to encode a huge string into one. Going
      -- through Octet_Array inhibits that ability since Octet_Arrays have moderate length.
      Temporary_Array := (others => 0);
      for I in Value'Range loop
         Temporary_Array(I - Value'First) := Character'Pos(Value(I));
      end loop;
      Encode(Temporary_Array, Data, Position, Last);
   end Encode;

   ----------------------
   -- Decoding Procedures
   ----------------------

   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Integer;
      Last     : out XDR_Index_Type)
   is
      Temp : XDR_Unsigned;
   begin
      Decode(Data, Position, Temp, Last);
      Value := XDR_Unsigned_To_Integer(Temp);
   end Decode;


   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Unsigned;
      Last     : out XDR_Index_Type)
   is
      Temporary_1 : XDR_Unsigned := XDR_Unsigned(Data(Position));
      Temporary_2 : XDR_Unsigned;
   begin
      for I in 1 .. 3 loop
         Temporary_2 := Temporary_1 * 256;
         Temporary_2 := Temporary_2 + XDR_Unsigned(Data(Position + I));
         Value := Temporary_2;
         Temporary_1 := Value;
      end loop;
      Last := Position + 3;
   end Decode;


   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Boolean;
      Last     : out XDR_Index_Type)
   is
      Temporary : XDR_Unsigned;
   begin
      Decode(Data, Position, Temporary, Last);
      -- TODO: Any non-zero value is taken to be True. Is that what we really want?
      if Temporary /= 0 then
         Value := XDR_True;
      else
         Value := XDR_False;
      end if;
   end Decode;


   -- TODO: Account for negatives.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Hyper;
      Last     : out XDR_Index_Type)
   is
      Temporary : XDR_Unsigned_Hyper;
   begin
      Decode(Data, Position, Temporary, Last);
      Value := XDR_Unsigned_To_Hyper(Temporary);
   end Decode;


   -- Decodes an unsigned hyper integer from Data starting at Position up to and including Last.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Unsigned_Hyper;
      Last     : out XDR_Index_Type)
   is
      Temporary_1 : XDR_Unsigned_Hyper := XDR_Unsigned_Hyper(Data(Position));
      Temporary_2 : XDR_Unsigned_Hyper;
   begin
      for I in 1 .. 7 loop
         Temporary_2 := Temporary_1 * 256;
         Temporary_2 := Temporary_2 + XDR_Unsigned_Hyper(Data(Position + I));
         Value := Temporary_2;
         Temporary_1 := Value;
      end loop;
      Last := Natural(Position + 7);
   end Decode;

   subtype Bit is Natural range 0 .. 1;
   type Bit_List is array(Positive range <>) of Bit
     with Component_Size => 1;

   -- https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

   procedure Decode
    (Data     : in  XDR_Array;
     Position : in  XDR_Index_Type;
     Value    : out XDR_Float;
     Last     : out XDR_Index_Type;
     Special  : out Special_Float_Value)
   is
      Raw : XDR_Unsigned;
      type Bit_List_32 is new Bit_List(1..32);
      Bits : Bit_List_32;
      procedure To_Bits(Int : in out XDR_Unsigned; Result : out Bit_List_32) is
      begin
         for I in reverse 1..32 loop
            Result(I) := Bit(Int rem 2);
            Int := Int / 2;
         end loop;
      end;
      Sign : Bit;
      type Float_Exp_Bits is mod 2**8;
      Raw_Exp : Float_Exp_Bits := 0;
      -- Note that not all of these exponent values are valid
      subtype Float_Exponent is Integer range -127 .. 128;
      Biased_Exp : Float_Exponent;
      type Float_Fraction_Part is mod 2**23;
      Raw_Frac : Float_Fraction_Part := 0;
      Frac : XDR_Float;
      Normalized : Boolean := True;
   begin
      Decode(Data, Position, Raw, Last);
      Special := None;
      To_Bits(Raw, Bits);
      Sign := Bits(1);

      -- Big endian, early bits are more significant
      for I in 2 .. 9 loop
         Raw_Exp := Raw_Exp + Float_Exp_Bits(Bits(I)) * 2**(8 - (I - 1));
      end loop;

      for I in 10 .. 32 loop
         Raw_Frac := Raw_Frac + Float_Fraction_Part(Natural(Bits(I))) * 2**(23 - (I - 9));
      end loop;

      -- Check for special values
      if Raw_Exp = 255 then
         if Raw_Frac = 0 then
            if Sign = 1 then
               Special := Negative_Infinity;
            else
               Special := Positive_Infinity;
            end if;
         else
            Special := NaN;
         end if;
         Value := XDR_Float(0.0); -- Arbitrary
         return;
      elsif Raw_Exp = 0 then
         if Raw_Frac = 0 then
            Value := XDR_Float(0.0 * Float(-1.0) ** Sign);
            return;
         else
            Normalized := False;
         end if;
      end if;


      if Normalized then
         -- Subtract the bias
         Biased_Exp := Float_Exponent(Raw_Exp - 127);
         Frac := 1.0 + XDR_Float(Raw_Frac) / 2.0**23;
      else
         Biased_Exp := (-126);
         Frac := XDR_Float(Raw_Frac) / 2.0**23;
      end if;

      -- (-1)**S * 2**(E-Bias) * 1.F
      -- https://www.ibm.com/docs/en/aix/7.2?topic=types-single-precision-floating-point

      Value := XDR_Float((-1.0) ** Sign * (2.0)**Biased_Exp * Frac);
   end Decode;

   -- TODO: Decode compliant with IEEE standard (XDR calls for this)
   procedure Decode
    (Data     : in  XDR_Array;
     Position : in  XDR_Index_Type;
     Value    : out XDR_Double;
     Last     : out XDR_Index_Type;
     Special  : out Special_Float_Value)
   is
      Raw : XDR_Unsigned_Hyper;
      type Bit_List_64 is new Bit_List(1..64);
      Bits : Bit_List_64;
      procedure To_Bits(Int : in out XDR_Unsigned_Hyper; Result : out Bit_List_64) is
      begin
         for I in reverse 1..64 loop
            Result(I) := Bit(Int rem 2);
            Int := Int / 2;
         end loop;
      end;
      Sign : Bit;
      type Float_Exp_Bits is mod 2**11;
      Raw_Exp : Float_Exp_Bits := 0;
      -- Note that not all of these exponent values are valid
      subtype Float_Exponent is Integer range -1023 .. 1024;
      Biased_Exp : Float_Exponent;
      type Float_Fraction_Part is mod 2**52;
      Raw_Frac : Float_Fraction_Part := 0;
      Frac : XDR_Double;
      Normalized : Boolean := True;
   begin
      Decode(Data, Position, Raw, Last);
      Special := None;
      To_Bits(Raw, Bits);
      Sign := Bits(1);

      -- Big endian, early bits are more significant
      for I in 2 .. 12 loop
         Raw_Exp := Raw_Exp + Float_Exp_Bits(Bits(I)) * 2**(11 - (I - 1));
      end loop;

      for I in 13 .. 64 loop
         Raw_Frac := Raw_Frac + Float_Fraction_Part(Natural(Bits(I))) * 2**(52 - (I - 12));
      end loop;

      -- Check for special values
      if Raw_Exp = 2047 then
         if Raw_Frac = 0 then
            if Sign = 1 then
               Special := Negative_Infinity;
            else
               Special := Positive_Infinity;
            end if;
         else
            Special := NaN;
         end if;
         Value := XDR_Double(0.0); -- Arbitrary
         return;
      elsif Raw_Exp = 0 then
         if Raw_Frac = 0 then
            Value := XDR_Double(0.0 * Float(-1.0) ** Sign);
            return;
         else
            Normalized := False;
         end if;
      end if;


      if Normalized then
         -- Subtract the bias
         Biased_Exp := Float_Exponent(Raw_Exp - 1023);
         Frac := 1.0 + XDR_Double(Raw_Frac) / 2.0**52;
      else
         Biased_Exp := (-126);
         Frac := XDR_Double(Raw_Frac) / 2.0**52;
      end if;

      -- (-1)**S * 2**(E-Bias) * 1.F

      Value := XDR_Double((-1.0) ** Sign * (2.0)**Biased_Exp * Frac);
    end Decode;


   -- Decodes a fixed length array of opaque data from Data starting at Position.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out Octet_Array;
      Last     : out XDR_Extended_Index_Type)
   is
   begin
      for I in Value'Range loop
         Value(I) := Octet(Data(Position + (I - Value'First)));
      end loop;
      Last := Position + (Length_With_Padding(Value'Length) - 1);
   end Decode;


   -- Decodes a fixed length string from Data starting at Position.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out String;
      Last     : out XDR_Extended_Index_Type)
   is
   begin
      Value := (others => ' ');
      for I in Position .. (Position + Value'Length - 1) loop
         Value(Value'First + (I - Position)) := Character'Val(Data(I));
      end loop;
      Last := Position + (Length_With_Padding(Value'Length) - 1);
   end Decode;


end CubedOS.Lib.XDR;
