--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-xdr.ads
-- SUBJECT: Specification of an XDR encoding/decoding package.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- XDR is a standard for converting typed data into an octet stream suitable for exchange
-- between communicating partners. Unlike ASN.1, the data is not self describing so the
-- receiver needs to know what information to expect in what order. See RFC-4506 for more
-- information: http://tools.ietf.org/html/rfc4506.html.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;

package CubedOS.Lib.XDR is

   -------------------
   -- Type Definitions
   -------------------

   -- The primitive unit of data stored in an XDR message.
   type XDR_Octet is mod 2**8;

   subtype XDR_Index_Type is Natural;
   subtype XDR_Extended_Index_Type is Integer range -1 .. XDR_Index_Type'Last;
   subtype XDR_Size_Type is Natural range 0 .. XDR_Index_Type'Last;
   type XDR_Array is array(XDR_Index_Type range <>) of XDR_Octet;

   -- TODO: Should we remove the XDR prefix?
   -- The names would be nicer, but they might be confused with those in package Standard.
   type XDR_Integer is range -2**31 .. 2**31 - 1;
   type XDR_Unsigned is mod 2**32;
   type XDR_Boolean is (XDR_False, XDR_True);
   type XDR_Hyper is range -2**63 .. 2**63 - 1;
   type XDR_Unsigned_Hyper is mod 2**64;
   type XDR_Float is new Float;
   type XDR_Double is new Long_Float;
   -- TODO: Add support for XDR floating point types? See comments in the body.

   use Ada.Real_Time;
   Max_Time_Span : constant Time_Span := Seconds(2**31 - 1);
   -- The longest time span which the library supports encoding.

   Max_Time : constant Time := Time_First + Max_Time_Span;
   -- The longest time which the library supports encoding.

   ----------------------
   -- Encoding Procedures
   ----------------------

   -- Encodes an XDR integer into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Integer;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Encodes an XDR unsigned integer into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Unsigned;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Encodes an XDR Boolean into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Boolean;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Encodes an XDR hyper integer into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Hyper;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Position <= Data'Last - (8 - 1),
       Post => Last = Position + (8 - 1);

   -- Encodes an XDR unsigned hyper integer into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Unsigned_Hyper;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Position <= Data'Last - (8 - 1),
       Post => Last = Position + (8 - 1);

   -- Encodes an XDR float into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Float;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Encodes an XDR double into Data starting at Position.
   procedure Encode
     (Value    : in     XDR_Double;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Index_Type)
     with
      Global  => null,
       Depends => (Data =>+ (Value, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Position <= Data'Last - (8 - 1),
       Post => Last = Position + (8 - 1);

   function Length_With_Padding(Length : Octet_Array_Count) return Octet_Array_Count is
     (Length + (if Length rem 4 = 0 then 0 else (4 - Length rem 4)));

   -- Encodes XDR fixed length opaque data into Data starting at Position.
   procedure Encode
     (Value    : in     Octet_Array;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Extended_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => (Value, Position)),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Length_With_Padding(Value'Length) <= (Data'Last - Position) + 1,
       Post => Last = Position + (Length_With_Padding(Value'Length) - 1);

   -- Encodes XDR fixed length string into Data starting at Position.
   procedure Encode
     (Value    : in     String;
      Data     : in out XDR_Array;
      Position : in     XDR_Index_Type;
      Last     :    out XDR_Extended_Index_Type)
     with
       Global  => null,
       Depends => (Data =>+ (Value, Position), Last => (Value, Position)),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Value'Length <= Octet_Array_Count'Last and then
         Length_With_Padding(Value'Length) <= (Data'Last - Position) + 1,
       Post => Last = Position + (Length_With_Padding(Value'Length) - 1);

   ----------------------
   -- Decoding Procedures
   ----------------------

   -- Decodes an integer from Data starting at Position up to and including Last.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Integer;
      Last     : out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Value => (Data, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Decodes an unsigned integer from Data starting at Position up to and including Last.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Unsigned;
      Last     : out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Value => (Data, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Decodes a Boolean from Data starting at Position up to and including Last.
   -- TODO: What should be done if the information in Data is invalid?
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Boolean;
      Last     : out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Value => (Data, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0,
       Post => Last = Position + (4 - 1);

   -- Decodes a hyper integer from Data starting at Position up to and including Last.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Hyper;
      Last     : out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Value => (Data, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Position <= Data'Last - (8 - 1),
       Post => Last = Position + (8 - 1);

   -- Decodes an unsigned hyper integer from Data starting at Position up to and including Last.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out XDR_Unsigned_Hyper;
      Last     : out XDR_Index_Type)
     with
       Global  => null,
       Depends => (Value => (Data, Position), Last => Position),
       Pre =>
         Position in Data'Range            and then
         (Position - Data'First) rem 4 = 0 and then
         Data'Length             rem 4 = 0 and then
         Position <= Data'Last - (8 - 1),
       Post => Last = Position + (8 - 1);

   type Special_Float_Value is (None, Positive_Infinity, Negative_Infinity, NaN);
   -- Ada can't handle these floating point values directly.

   -- Decodes an float from Data starting at Position up to and including Last.
   -- Conforms to the IEEE standard for normalized single-precision floats.
   -- Ada doesn't support the special IEEE float values so we return an additional
   -- Special parameter.
   procedure Decode
    (Data     : in  XDR_Array;
     Position : in  XDR_Index_Type;
     Value    : out XDR_Float;
     Last     : out XDR_Index_Type;
     Special  : out Special_Float_Value)
    with
      Global  => null,
      Depends => ((Value, Special) => (Data, Position), Last => Position),
      Pre =>
         Position rem 4 = 0 and
         Data'Length rem 4 = 0 and
         Position + (4 - 1) <= Data'Last and
         (Position - Data'First) rem 4 = 0 and
         Position in Data'Range,
      Post => Last = Position + (4 - 1);

   -- Decodes a double from Data starting at Position up to and including Last.
   -- Conforms to the IEEE standard for normalized double-precision floats.
   procedure Decode
    (Data     : in  XDR_Array;
     Position : in  XDR_Index_Type;
     Value    : out XDR_Double;
     Last     : out XDR_Index_Type;
     Special  : out Special_Float_Value)
    with
      Global  => null,
      Depends => ((Value, Special) => (Data, Position), Last => Position),
      Pre =>
        Position rem 4 = 0 and
        Data'Length rem 4 = 0 and
        Position <= Data'Last - (8 - 1) and
        (Position - Data'First) rem 4 = 0 and
        Position in Data'Range,
      Post => Last = Position + (8 - 1);

   -- Decodes a fixed length array of opaque data from Data starting at Position.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out Octet_Array;
      Last     : out XDR_Extended_Index_Type)
     with
       Global  => null,
       Depends => (Value =>+ (Data, Position), Last => (Position, Value)),
       Pre =>
         Position rem 4 = 0 and
         Data'Length rem 4 = 0 and
         Position + (Length_With_Padding(Value'Length) - 1) <= Data'Last,
       Post => Last = Position + (Length_With_Padding(Value'Length) - 1);

   -- Decodes a fixed length string from Data starting at Position.
   -- Ada doesn't support NaN types, so an eroneous value will be
   -- produced if NaN.
   procedure Decode
     (Data     : in  XDR_Array;
      Position : in  XDR_Index_Type;
      Value    : out String;
      Last     : out XDR_Extended_Index_Type)
     with
       Global  => null,
       Depends => (Value =>+ (Data, Position), Last => (Position, Value)),
       Pre =>
         Position rem 4 = 0 and then
         Data'Length > 0 and then
         Data'Length rem 4 = 0 and then
         Value'Length <= Octet_Array_Count'Last and then
         Position <= Data'Last - (Length_With_Padding(Value'Length) - 1),
       Post => Last = Position + (Length_With_Padding(Value'Length) - 1);

end CubedOS.Lib.XDR;
