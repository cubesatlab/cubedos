--------------------------------------------------------------------------------
-- FILE   : cfdp.adb
-- SUBJECT: Body of the CFDP module's main package.
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body CubedOS.CFDP.Internals is

   -- I'm not sure what to do with Procedure Text_Tx, but I'm 100% sure it doesn't belong here.
   --
   -- This procedure is for testing only. It does not (and need not) pass SPARK analysis.
   --
   --procedure Test_Tx
   --  with SPARK_Mode => Off
   --is
   --   Message              : Request_Record;
   --   Header_Information   : Fixed_Header_Record;
   --   File_Data_Temp       : Filedata_Record;
   --   EOF_Information      : EOF_Record;
   --   Finished_Information : Finished_Record;
   --   Finished_PDU         : Complete_Finished_PDU_Array;
   --   Filedata             : Complete_Filedata_PDU_Array;
   --   TLV_Information      : TLV_Record;
   --   TLV_Options          : TLV_Options_Array;
   --   EOF                  : Complete_EOF_PDU_Array;
   --   Status               : PDU_Status_Type;
   --   Test_File            : Octet_Array(0 .. 9999);
   --   Temp_Octet           : Octet;
   --   Octets_Sent          : Segment_Offset_Type;
   --
   --   -- Incoming_PDU is used only for test purposes.
   --   pragma Warnings(Off, "variable ""Incoming_PDU"" is assigned but never read");
   --   Incoming_PDU : PDU_Category;
   --begin
   --   Mailbox.Get_Message(Message);
   --   Put_Line(Entity_ID'Image(Message.Destination));
   --
   --   Octets_Sent := 0;
   --
   --   --Fill "file" with phony data
   --   for I in 0 .. 9999 loop
   --      Temp_Octet := Octet((I / 100) + 55);
   --      Test_File(I) := Temp_Octet;
   --   end loop;

   --   --phony fixed header
   --   Header_Information.Version := 0;
   --   Header_Information.PDU := File_Directive;
   --   Header_Information.Direction := Toward_Receiver;
   --   Header_Information.Transmission_Mode := Unacknowledged;
   --   Header_Information.CRC_Flag := CRC_Not_Present;
   --   Header_Information.Data_Length := 1024;
   --   Header_Information.Source_Entity := 0;
   --   Header_Information.Transaction := 0;
   --   Header_Information.Destination_Entity := Message.Destination;
   --
   --   while Octets_Sent <= 10000 loop
   --
   --      --sample of how we might set up file data
   --      Header_Information.Version := 0;
   --      Header_Information.PDU := File_Data;
   --      Header_Information.Direction := Toward_Receiver;
   --      Header_Information.Transmission_Mode := Unacknowledged;
   --      Header_Information.CRC_Flag := CRC_Not_Present;
   --      Header_Information.Data_Length := 1024;
   --      Header_Information.Source_Entity := 0;
   --      Header_Information.Transaction := 0;
   --      Header_Information.Destination_Entity := Message.Destination;
   --
   --      File_Data_Temp.Segment_Offset := Octets_Sent;
   --      for I in File_Segment_Type'Range loop
   --         if I + Natural(Octets_Sent) < 10000 then
   --            File_Data_Temp.Filedata_Segment(I) := Test_File(I + Natural(Octets_Sent));
   --         end if;
   --      end loop;
   --
   --      if Octets_Sent + 1024 >= 10000 then
   --         Filedata := Construct_Filedata_PDU(Header_Information, File_Data_Temp, True);
   --      else
   --         Filedata := Construct_Filedata_PDU(Header_Information, File_Data_Temp, False);
   --      end if;
   --
   --      Octets_Sent := Octets_Sent + 1024;
   --   end loop;
   --
   --   Header_Information.Version := 0;
   --   Header_Information.PDU := File_Directive;
   --   Header_Information.Direction := Toward_Receiver;
   --   Header_Information.Transmission_Mode := Unacknowledged;
   --   Header_Information.CRC_Flag := CRC_Not_Present;
   --   Header_Information.Data_Length := 1024;
   --   Header_Information.Source_Entity := 0;
   --   Header_Information.Transaction := 0;
   --   Header_Information.Destination_Entity := Message.Destination;
   --
   --   EOF_Information.Condition_Code := No_Error;
   --   EOF_Information.File_Checksum := File_Checksum(Test_File);
   --   EOF_Information.File_Size := 10000;
   --
   --   EOF := Construct_EOF_PDU(Header_Information, EOF_Information);
   --
   --   --check to see that finished pdus can be sent
   --   Finished_Information.Condition_Code := No_Error;
   --   Finished_Information.End_System_Status := Generated_By_System;
   --   Finished_Information.Delivery_Code := Data_Complete;
   --   Finished_Information.File_Status_Code := Retained_Successfully;
   --
   --   TLV_Information.TLV_Category := Filestore_Response;
   --   TLV_Information.Filestore_Action_Code := Append_File;
   --   TLV_Information.Source_File_Name := Message.Source_File;
   --   TLV_Information.Dest_File_Name := Message.Destination_File;
   --   TLV_Information.Filestore_Message := Bounded_Strings.Make(Max_TLV_Message_Length, "Hello");
   --   TLV_Information.Filestore_Status_Code := Successful;
   --
   --   TLV_Options(0) := TLV_Information;
   --
   --   Finished_PDU := Construct_Finished_PDU(Header_Information, Finished_Information, TLV_Options, 1);
   --   Deconstruct_Finished_PDU(Finished_PDU, Header_Information, Finished_Information, TLV_Options, Status);
   --
   --   -- The following assignments are done for test purposes (for visibility in the debugger).
   --   pragma Warnings(Off, "useless assignment to ""Incoming_PDU""");
   --   Incoming_PDU := Interpret_PDU(Filedata);
   --   Incoming_PDU := Interpret_PDU(EOF);
   --   Incoming_PDU := Interpret_PDU(Finished_PDU);
   --
   --end Test_Tx;


   function Shift_Left(X : Data_Length_Type; Distance : Natural) return Data_Length_Type
     with Global => null, Import, Convention => Intrinsic;

   -- Not referenced...
   -- function Shift_Right(X : Data_Length_Type; Distance : Natural) return Data_Length_Type
   --   with Global => null, Import, Convention => Intrinsic;

   function Shift_Left(X : Entity_ID; Distance : Natural) return Entity_ID
     with Global => null, Import, Convention => Intrinsic;

   -- Not references...
   -- function Shift_Right(X : Entity_ID; Distance : Natural) return Entity_ID
   --   with Global => null, Import, Convention => Intrinsic;

   function Shift_Left(X : Transaction_ID; Distance : Natural) return Transaction_ID
     with Global => null, Import, Convention => Intrinsic;

   -- Not referenced...
   -- function Shift_Right(X : Transaction_ID; Distance : Natural) return Transaction_ID
   --   with Global => null, Import, Convention => Intrinsic;


   -- Computes a checksum for the given file. This is a mod-32 addition of all
   -- 4-octet words in the file, sequentially. The file is padded with zeroes
   -- to be a multiple of 4 bits in length.
   function File_Checksum(File_Array : File_Octets) return Segment_Offset_Type is
      Word_Temp : Quadruple_Octet;
      Result : Segment_Offset_Type := 0;
      --in case we need to pad, add 3
      Counter : Natural range 0 .. File_Array'Last + 1;
   begin
      Counter := 0;

      while Counter <= File_Array'Last loop
         Word_Temp := (Shift_Left(Quadruple_Octet(File_Array(Counter)), 24)
                       and 2#1111_1111_0000_0000_0000_0000_0000_0000#);
         Counter := Counter + 1;

         if Counter <= File_Array'Last then
            Word_Temp := (Shift_Left(Quadruple_Octet(File_Array(Counter)), 16)
                         and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Word_Temp;
            Counter := Counter + 1;
         else
            Word_Temp := (Shift_Left(Quadruple_Octet(0), 16)
                          and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Word_Temp;
         end if;

         if Counter <= File_Array'Last then
            Word_Temp := (Shift_Left(Quadruple_Octet(File_Array(Counter)), 8)
                          and 2#1111_1111_1111_1111_1111_1111_0000_0000#) or Word_Temp;
            Counter := Counter + 1;
         else
            Word_Temp := (Shift_Left(Quadruple_Octet(0), 8)
                          and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Word_Temp;
         end if;

         if Counter <= File_Array'Last then
            Word_Temp := (Quadruple_Octet(File_Array(Counter))
                          and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Word_Temp;
            Counter := Counter + 1;
         else
            Word_Temp := (Quadruple_Octet(0)
                          and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Word_Temp;
         end if;

         Result := Result + Segment_Offset_Type(Word_Temp);

      end loop;

      return Result;
   end File_Checksum;


   --This handles every obnoxious possible different case for the
   --filestore status value
   --Note: if something about the status code isn't right, this returns
   --as though the action wasn't performed
   function Get_Filestore_Status_Val
     (Filestore_Action_Code : Filestore_Action_Code_Type;
      Filestore_Status_Code : Filestore_Status_Code_Type) return Filestore_Status_Value
   is
      Result : Filestore_Status_Value;
   begin
      case Filestore_Action_Code is

         when Create_File =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when Create_Not_Allowed =>
                  Result := 2#0001#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Delete_File =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when File_Does_Not_Exist =>
                  Result := 2#0001#;
               when Delete_Not_Allowed =>
                  Result := 2#0010#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Rename_File =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when Old_File_Not_Exist =>
                  Result := 2#0001#;
               when New_File_Already_Exist =>
                  Result := 2#0010#;
               when Rename_Not_Allowed =>
                  Result := 2#0011#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Append_File =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when File_1_Not_Exist =>
                  Result := 2#0001#;
               when File_2_Not_Exist =>
                  Result := 2#0010#;
               when Append_Not_Allowed =>
                  Result := 2#0011#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Replace_File =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when File_1_Not_Exist =>
                  Result := 2#0001#;
               when File_2_Not_Exist =>
                  Result := 2#0010#;
               when Replace_Not_Allowed =>
                  Result := 2#0011#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Create_Dir =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when Directory_Cannot_Create =>
                  Result := 2#0001#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Remove_Dir =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when Directory_Not_Exist =>
                  Result := 2#0001#;
               when Delete_Not_Allowed =>
                  Result := 2#0010#;
               when others =>
                  Result := 2#1111#;
            end case;

         when Deny_File .. Deny_Dir =>
            case Filestore_Status_Code is
               when Successful =>
                  Result := 2#0000#;
               when Delete_Not_Allowed =>
                  Result := 2#0001#;
               when others =>
                  Result := 2#1111#;
            end case;

      end case;

      return Result;

   end Get_Filestore_Status_Val;


   --This handles every obnoxious possible different case for the
   --filestore status value
   --Note: if something about the status code isn't right, this returns
   --as though the action wasn't performed
   function Get_Filestore_Status_Code
     (Filestore_Action_Code : Filestore_Action_Code_Type;
      Filestore_Status_Val : Filestore_Status_Value) return Filestore_Status_Code_Type
   is
      Result : Filestore_Status_Code_Type;
   begin
      case Filestore_Action_Code is

         when Create_File =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := Create_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Delete_File =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := File_Does_Not_Exist;
               when 2#0010# =>
                  Result := Delete_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Rename_File =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := Old_File_Not_Exist;
               when 2#0010# =>
                  Result := New_File_Already_Exist;
               when 2#0011# =>
                  Result := Rename_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Append_File =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := File_1_Not_Exist;
               when 2#0010# =>
                  Result := File_2_Not_Exist;
               when 2#0011# =>
                  Result := Append_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Replace_File =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := File_1_Not_Exist;
               when 2#0010# =>
                  Result := File_2_Not_Exist;
               when 2#0011# =>
                  Result := Replace_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Create_Dir =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := Directory_Cannot_Create;
               when others =>
                  Result := Not_Performed;
            end case;

         when Remove_Dir =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := Directory_Not_Exist;
               when 2#0010# =>
                  Result := Delete_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

         when Deny_File .. Deny_Dir =>
            case Filestore_Status_Val is
               when 2#0000# =>
                  Result := Successful;
               when 2#0001# =>
                  Result := Delete_Not_Allowed;
               when others =>
                  Result := Not_Performed;
            end case;

      end case;

      return Result;

   end Get_Filestore_Status_Code;


   -- Function that returns the fixed PDU header required by CFDP PDUs.
   function Encode_Fixed_PDU_Header
     (Header_Information : Fixed_Header_Record) return Fixed_Header_Array
   is
      Result       : Fixed_Header_Array := (others => 0);
      Version      : Octet;
      PDU          : Octet;
      Direction    : Octet;
      Transmission : Octet;
      CRC_Flag     : Octet;
      Entity_ID_Length   : Octet;
      Transaction_Length : Octet;
      Data_Length_MSB    : Octet;
      Data_Length_LSB    : Octet;
      Source_Entity_MSB  : Octet;
      Source_Entity_LSB  : Octet;
      Transaction_MSB    : Octet;
      Transaction_LSB    : Octet;
      Destination_MSB    : Octet;
      Destination_LSB    : Octet;
      Temp_Entity        : Entity_ID;
      Temp_Transaction   : Transaction_ID;
   begin
      -- Encode the Version into the first 3 bits of the array
      Version := Octet(Header_Information.Version);
      Result(0) := ((Result(0) and 2#0001_1111#) or Shift_Left(Version, 5));

      -- Encode the PDU into the next bit of the array
      PDU := Octet(PDU_Type'Pos(Header_Information.PDU));
      Result(0) := ((Result(0) and 2#1110_1111#) or Shift_Left(PDU, 4));

      -- Encode the Direction into the next bit of the array
      Direction := Octet(Direction_Type'Pos(Header_Information.Direction));
      Result(0) := ((Result(0) and 2#1111_0111#) or Shift_Left(Direction, 3));

      -- Encode the Transmission into the next bit of the array
      Transmission :=
        Octet(Transmission_Mode_Type'Pos(Header_Information.Transmission_Mode));
      Result(0) := ((Result(0) and 2#1111_1011#) or Shift_Left(Transmission, 2));

      -- Encode the CRC_Flag into the next bit of the array
      CRC_Flag := Octet(CRC_Flag_Type'Pos(Header_Information.CRC_Flag));
      Result(0) := ((Result(0) and 2#1111_1101#) or Shift_Left(CRC_Flag, 1));

      -- Put in a 0 for the reserved bit
      Result(0) := (Result(0) and 2#1111_1110#);

      -- Encode the data length into the next two octets.
      Data_Length_MSB := Octet(Header_Information.Data_Length / 256);
      Data_Length_LSB := Octet(
                                     Shift_Left(Double_Octet(
                                       Header_Information.Data_Length), 8) / 256);
      Result(1) := ((Result(1) and 2#0000_0000#) or Data_Length_MSB);
      Result(2) := ((Result(2) and 2#0000_0000#) or Data_Length_LSB);

      -- Put in a 0 for the reserved bit
      Result(3) := (Result(3) and 2#0111_1111#);

      -- Encode 1 for the length of the entity ID
      Entity_ID_Length := Octet(1);
      Result(3) := ((Result(3) and 2#1000_1111#) or Shift_Left(Entity_ID_Length, 4));

      -- Encode 1 for the length of the transaction seq number
      Transaction_Length := Octet(1);
      Result(3) := ((Result(3) and 2#1111_1000#) or Transaction_Length);

      -- Put in a 0 for the reserved bit
      Result(3) := (Result(3) and 2#1111_0111#);

      -- Encode the source entity ID into the next two octets.
      Temp_Entity := Header_Information.Source_Entity and 2#1111_1111_0000_0000#;
      Source_Entity_MSB := Octet(Temp_Entity / 256);
      Temp_Entity := Header_Information.Source_Entity and 2#0000_0000_1111_1111#;
      Temp_Entity := Shift_Left(Temp_Entity, 8);
      Source_Entity_LSB := Octet(Temp_Entity / 256);
      Result(4) := ((Result(4) and 2#0000_0000#) or Source_Entity_MSB);
      Result(5) := ((Result(5) and 2#0000_0000#) or Source_Entity_LSB);

      -- Encode the transaction seq number into the next two octets.
      Temp_Transaction := Header_Information.Transaction and 2#1111_1111_0000_0000#;
      Transaction_MSB := Octet(Temp_Transaction / 256);
      Temp_Transaction := Header_Information.Transaction and 2#0000_0000_1111_1111#;
      Temp_Transaction := Shift_Left(Temp_Transaction, 8);
      Transaction_LSB := Octet(Temp_Transaction / 256);
      Result(6) := ((Result(6) and 2#0000_0000#) or Transaction_MSB);
      Result(7) := ((Result(7) and 2#0000_0000#) or Transaction_LSB);

      -- Encode the destination entity ID into the next two octets.
      Temp_Entity := Header_Information.Destination_Entity and 2#1111_1111_0000_0000#;
      Destination_MSB := Octet(Temp_Entity / 256);
      Temp_Entity := Header_Information.Destination_Entity and 2#0000_0000_1111_1111#;
      Temp_Entity := Shift_Left(Temp_Entity, 8);
      Destination_LSB := Octet(Temp_Entity / 256);
      Result(8) := ((Result(8) and 2#0000_0000#) or Destination_MSB);
      Result(9) := ((Result(9) and 2#0000_0000#) or Destination_LSB);

      return Result;
   end Encode_Fixed_PDU_Header;


   -- Procedure that returns a fixed header record based on what is found in the fixed header.
   procedure Decode_Fixed_PDU_Header
     (Header             : in  Fixed_Header_Array;
      Header_Information : out Fixed_Header_Record;
      Status             : out PDU_Status_Type)
   is
      Entity_ID_Length : Octet;
      Transaction_Length : Octet;
   begin
      -- Decode the first octet: version, PDU type, direction, transmission, CRC
      Header_Information.Version :=
        Version_Type((Header(0) and 2#1110_0000#) / 32);
      Header_Information.PDU :=
        PDU_Type'Val(Shift_Left((Header(0) and 2#0001_0000#), 3) / 128);
      Header_Information.Direction :=
        Direction_Type'Val(Shift_Left((Header(0) and 2#0000_1000#), 4) / 128);
      Header_Information.Transmission_Mode :=
        Transmission_Mode_Type'Val(Shift_Left((Header(0) and 2#0000_0100#), 5) / 128);
      Header_Information.CRC_Flag :=
        CRC_Flag_Type'Val(Shift_Left((Header(0) and 2#0000_0010#), 6) / 128);

      -- Decode the next two octets: data length
      Header_Information.Data_Length := Data_Length_Type(Header(1));
      Header_Information.Data_Length :=
        Shift_Left(Header_Information.Data_Length, 8) or Data_Length_Type(Header(2));

      -- Decode the next octet: entity length, transaction length
      Entity_ID_Length := Shift_Left((Header(3) and 2#0111_0000#), 1) / 32;
      Transaction_Length := Shift_Left((Header(3) and 2#0000_0111#), 5) / 32;

      -- TODO: Handle larger entity and transaction lengths better.
      if Entity_ID_Length > Octet(1) then
         Status := Invalid_PDU;
      elsif Transaction_Length > Octet(1) then
         Status := Invalid_PDU;
      else
         Status := Success;
      end if;

      -- Decode next two octets: source entity ID
      Header_Information.Source_Entity := Entity_ID(Header(4));
      Header_Information.Source_Entity :=
        Shift_Left(Header_Information.Source_Entity, 8) or Entity_ID(Header(5));

      -- Decode next two octets: transactions seq number
      Header_Information.Transaction := Transaction_ID(Header(6));
      Header_Information.Transaction :=
        Shift_Left(Header_Information.Transaction, 8) or Transaction_ID(Header(7));

      -- Decode next two octets: destination ID
      Header_Information.Destination_Entity := Entity_ID(Header(8));
      Header_Information.Destination_Entity :=
        Shift_Left(Header_Information.Destination_Entity, 8) or Entity_ID(Header(9));

   end Decode_Fixed_PDU_Header;


   function Encode_Metadata_PDU
     (Metadata_Information : Metadata_PDU_Record) return Metadata_PDU_Array
   is
      Result : Metadata_PDU_Array := (others => 0);
      Next_Empty_Octet : Natural range 0 .. (Max_Metadata_Size - 1) := 0;
      Segmentation_Control : Octet;
      Length_Temp : Length_Indicator_Type;
      Source_Length : Octet;
      Char_Temp : Octet;
      Destination_Length : Octet;
   begin
      --The File Directive Code for metadata is always 0x07
      --We start with a condition code of 0000 to indicate no errors
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#) or 2#0111_0000#;
      Next_Empty_Octet := Next_Empty_Octet + 1;

      --One-bit seg control, followed by 7 zeroes
      Segmentation_Control :=
        Octet(Segmentation_Control_Type'Pos(Metadata_Information.Segmentation_Control));
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
        or Shift_Left(Segmentation_Control, 7);
      Next_Empty_Octet := Next_Empty_Octet + 1;

      --File size is encoded in 4 octets.
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
        or Octet(Metadata_Information.File_Size / 256 / 256 / 256);
      Next_Empty_Octet := Next_Empty_Octet + 1;
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Metadata_Information.File_Size), 8) / 256 / 256 / 256);
      Next_Empty_Octet := Next_Empty_Octet + 1;
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Metadata_Information.File_Size), 16) / 256 / 256 / 256);
      Next_Empty_Octet := Next_Empty_Octet + 1;
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Metadata_Information.File_Size), 24) / 256 / 256 / 256);
      Next_Empty_Octet := Next_Empty_Octet + 1;

      --Length of filename at source
      Length_Temp := Metadata_Information.Source_File_Length;
      Source_Length := Octet(Length_Temp);
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#) or Source_Length;
      Next_Empty_Octet := Next_Empty_Octet + 1;

      --Source Filename (the variable length of filenames is why we access the result array dynamically)
      for I in 1 .. Bounded_Strings.Length(Metadata_Information.Source_File_Name) loop
         Char_Temp := Octet(Character'Pos(Bounded_Strings.Element(Metadata_Information.Source_File_Name, I)));
         Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
           or Char_Temp;
         if Next_Empty_Octet + 1 < Max_Metadata_Size - 1 then
            Next_Empty_Octet := Next_Empty_Octet + 1;
         end if;
      end loop;

      --Length of filename at destination
      Length_Temp := Metadata_Information.Dest_File_Length;
      Destination_Length := Octet(Length_Temp);
      Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#) or Destination_Length;
      if Next_Empty_Octet + 1 < Max_Metadata_Size - 1 then
         Next_Empty_Octet := Next_Empty_Octet + 1;
      end if;

      --Dest Filename (the variable length of filenames is why we access the result array dynamically)
      for I in 1 .. Bounded_Strings.Length(Metadata_Information.Dest_File_Name) loop
         Char_Temp := Octet(Character'Pos(Bounded_Strings.Element(Metadata_Information.Dest_File_Name, I)));
         Result(Next_Empty_Octet) := (Result(Next_Empty_Octet) and 2#0000_0000#)
           or Char_Temp;
         if Next_Empty_Octet + 1 < Max_Metadata_Size - 1 then
            Next_Empty_Octet := Next_Empty_Octet + 1;
         end if;
      end loop;

      return Result;
   end Encode_Metadata_PDU;


   procedure Decode_Metadata_PDU
     (Metadata             : in  Metadata_PDU_Array;
      Next_Octet           : out Next_Metadata_Octet_Type;
      Metadata_Information : out Metadata_PDU_Record;
      Status               : out PDU_Status_Type)
   is
      Temp_Size : Quadruple_Octet;
      Source_Length : Length_Indicator_Type;
      Dest_Length : Length_Indicator_Type;
      Temp_Source : Bounded_Strings.Bounded_String(Max_Name_Length);
      Temp_Dest  : Bounded_Strings.Bounded_String(Max_Name_Length);
      Char_Temp : Octet;
   begin
      Status := Success;
      Next_Octet := 1;

      -- Skip the first octet. It's just a signifier that it's a metadata PDU.
      Metadata_Information.Segmentation_Control := Segmentation_Control_Type'Val(Metadata(Natural(Next_Octet)) / 128);
      Next_Octet := Next_Octet + 1;

      --Next four octets are the total size of the file
      Temp_Size := Shift_Left(Quadruple_Octet(Metadata(Natural(Next_Octet))), 24)
        and 2#1111_1111_0000_0000_0000_0000_0000_0000#;
      Next_Octet := Next_Octet + 1;

      Temp_Size := (Temp_Size or Shift_Left(Quadruple_Octet(Metadata(Natural(Next_Octet))), 16))
        and 2#1111_1111_1111_1111_0000_0000_0000_0000#;
      Next_Octet := Next_Octet + 1;

      Temp_Size := (Temp_Size or Shift_Left(Quadruple_Octet(Metadata(Natural(Next_Octet))), 8))
        and 2#1111_1111_1111_1111_1111_1111_0000_0000#;
      Next_Octet := Next_Octet + 1;

      Temp_Size := (Temp_Size or Quadruple_Octet(Metadata(Natural(Next_Octet))));
      Next_Octet := Next_Octet + 1;

      Metadata_Information.File_Size := File_Size_Type(Temp_Size);

      --Length of the source file
      Source_Length := Length_Indicator_Type(Metadata(Natural(Next_Octet)));
      Metadata_Information.Source_File_Length := Source_Length;
      Next_Octet := Next_Octet + 1;

      --Decode the source file name from the length
      --if length is 0, we just skip this
      Temp_Source := Bounded_Strings.Make(Max_Name_Length, "");

      if Source_Length > 0 then
         for I in 0 .. Source_Length - 1 loop
            Char_Temp := Metadata(Natural(Next_Octet));
            if (Bounded_Strings.Length(Temp_Source) + 1 <= Temp_Source.Bound) then
               Bounded_Strings.Append(Temp_Source, Character'Val(Char_Temp));
            else
               Status := Invalid_PDU;
            end if;
            if Next_Octet + 1 < Max_Metadata_Size - 1 then
               Next_Octet := Next_Octet + 1;
            else
               Status := Invalid_PDU;
            end if;
         end loop;
      end if;

      Metadata_Information.Source_File_Name := Temp_Source;

      --Length of the destination file
      Dest_Length := Length_Indicator_Type(Metadata(Natural(Next_Octet)));
      Metadata_Information.Dest_File_Length := Dest_Length;
      if Next_Octet + 1 < Max_Metadata_Size - 1 then
         Next_Octet := Next_Octet + 1;
      else
         Status := Invalid_PDU;
      end if;

      --Decode the destination file name from the length
      --if length is 0, we just skip this
      Temp_Dest := Bounded_Strings.Make(Max_Name_Length, "");

      if Dest_Length > 0 then
         for I in 0 .. Dest_Length - 1 loop
            Char_Temp := Metadata(Natural(Next_Octet));
            if (Bounded_Strings.Length(Temp_Dest) + 1 <= Temp_Dest.Bound) then
               Bounded_Strings.Append(Temp_Dest, Character'Val(Char_Temp));
            else
               Status := Invalid_PDU;
            end if;
            if Next_Octet + 1 < Max_Metadata_Size - 1 then
               Next_Octet := Next_Octet + 1;
            else
               Status := Invalid_PDU;
            end if;
         end loop;
      end if;

      Metadata_Information.Dest_File_Name := Temp_Dest;
   end Decode_Metadata_PDU;


   function Encode_TLV
     (TLV_Information : TLV_Record) return TLV_Array
   is
      Result : TLV_Array := (others => 0);
      Next_Octet : Natural range 0 .. (Max_TLV_Size - 1) := 0;
      Length : Natural range 0 .. 255;
      Source_Length : Natural range 0 .. Max_Name_Length;
      Dest_Length : Natural range 0 .. Max_Name_Length;
      TLV_Message_Length : Natural range 0 .. Max_TLV_Message_Length := 0;
      Has_Dest : Boolean;
      Char_Temp : Octet;
      Temp_Code : Octet range 0 .. 15;
      Message_Length : Natural range 0 .. 255;
   begin
      case TLV_Information.TLV_Category is
         when Filestore_Request =>
            --Filestore request corresponds with a TLV type octet of 0x00
            Result(Next_Octet) := Result(Next_Octet) or 2#0000_0000#;
            Next_Octet := Next_Octet + 1;

            --Check to see if we're using the second filename (dest)
            case TLV_Information.Filestore_Action_Code is
               when Rename_File .. Replace_File =>
                  Has_Dest := True;
               when others =>
                  Has_Dest := False;
            end case;

            --Store the length in the next octet
            Source_Length := Bounded_Strings.Length(TLV_Information.Source_File_Name);
            Dest_Length := Bounded_Strings.Length(TLV_Information.Dest_File_Name);
            Length := 1 + 1 + Source_Length;

            if Has_Dest then
               Length := Length + 1 + Dest_Length;
            end if;

            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Length);
            Next_Octet := Next_Octet + 1;

            --First octet of the value is always the action code followed by 0000
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Shift_Left(
                                  Octet(Filestore_Action_Code_Type'Pos(
                                    TLV_Information.Filestore_Action_Code)), 4);
            Next_Octet := Next_Octet + 1;

            --Encode the source filename (always used)
            --First the length of it
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Source_Length);
            Next_Octet := Next_Octet + 1;

            --Now the filename
            if Source_Length > 0 then
               for I in 1 .. Source_Length loop
                  Char_Temp := Octet(
                                           Character'Pos(
                                             Bounded_Strings.Element(
                                               TLV_Information.Source_File_Name, I)));
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  end if;
               end loop;
            end if;

            --Encode the dest filename
            if Has_Dest then
               --First the length of it
               Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                 or Octet(Dest_Length);
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               end if;

               --Now the filename
               if Dest_Length > 0 then
                  for I in 1 .. Dest_Length loop
                     Char_Temp := Octet(
                                              Character'Pos(
                                                Bounded_Strings.Element(
                                                  TLV_Information.Dest_File_Name, I)));
                     Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                       or Char_Temp;
                     if Next_Octet + 1 < Max_TLV_Size - 1 then
                        Next_Octet := Next_Octet + 1;
                     end if;
                  end loop;
               end if;
            end if;

         when Filestore_Response =>

            --Filestore response corresponds with a TLV type octet of 0x01
            Result(Next_Octet) := Result(Next_Octet) or 2#0000_0001#;
            Next_Octet := Next_Octet + 1;

            --Check to see if we're using the second filename (dest)
            case TLV_Information.Filestore_Action_Code is
               when Rename_File .. Replace_File =>
                  Has_Dest := True;
               when others =>
                  Has_Dest := False;
            end case;

            --Store the length in the next octet
            Source_Length := Bounded_Strings.Length(TLV_Information.Source_File_Name);
            Dest_Length := Bounded_Strings.Length(TLV_Information.Dest_File_Name);
            if Bounded_Strings.Length(TLV_Information.Filestore_Message) <= 60 then
               TLV_Message_Length := Bounded_Strings.Length(TLV_Information.Filestore_Message);
            end if;

            Length := 1 + 1 + Source_Length;

            if Has_Dest then
               Length := Length + 1 + Dest_Length;
            end if;

            if TLV_Message_Length > 0 then
               Length := Length + 1 + TLV_Message_Length;
            end if;

            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Length);
            Next_Octet := Next_Octet + 1;

            --First octet of the value is always the action code followed by status code
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Shift_Left(
                                  Octet(Filestore_Action_Code_Type'Pos(
                                    TLV_Information.Filestore_Action_Code)), 4);
            Result(Next_Octet) := Result(Next_Octet) or
              Octet(Get_Filestore_Status_Val(TLV_Information.Filestore_Action_Code,
                                       TLV_Information.Filestore_Status_Code));
            Next_Octet := Next_Octet + 1;

            --Encode the source filename (always used)
            --First the length of it
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Source_Length);
            Next_Octet := Next_Octet + 1;

            --Now the filename
            if Source_Length > 0 then
               for I in 1 .. Source_Length loop
                  Char_Temp := Octet(
                                           Character'Pos(
                                             Bounded_Strings.Element(
                                               TLV_Information.Source_File_Name, I)));
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  end if;
               end loop;
            end if;

            --Encode the dest filename
            if Has_Dest then
               --First the length of it
               Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                 or Octet(Dest_Length);
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               end if;

               --Now the filename
               if Dest_Length > 0 then
                  for I in 1 .. Dest_Length loop
                     Char_Temp := Octet(
                                              Character'Pos(
                                                Bounded_Strings.Element(
                                                  TLV_Information.Dest_File_Name, I)));
                     Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                       or Char_Temp;
                     if Next_Octet + 1 < Max_TLV_Size - 1 then
                        Next_Octet := Next_Octet + 1;
                     end if;
                  end loop;
               end if;
            end if;

            if TLV_Message_Length > 0 then
               --First the length of it
               Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                 or Octet(TLV_Message_Length);
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               end if;

               --Now the message
               for I in 1 .. TLV_Message_Length loop
                  Char_Temp := Octet(
                                           Character'Pos(
                                             Bounded_Strings.Element(
                                               TLV_Information.Filestore_Message, I)));
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  end if;
               end loop;
               null;
            end if;

         when Message_To_User =>

            --Message to user request corresponds with a TLV type octet of 0x02
            Result(Next_Octet) := Result(Next_Octet) or 2#0000_0010#;
            Next_Octet := Next_Octet + 1;

            --Store the length in the next octet
            Message_Length := Bounded_Strings.Length(TLV_Information.User_Message);
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Message_Length);
            Next_Octet := Next_Octet + 1;

            --Write the message, one octet at a time
            if Message_Length > 0 then
               for I in 1 .. Message_Length loop
                  Char_Temp := Octet(
                                           Character'Pos(
                                             Bounded_Strings.Element(
                                               TLV_Information.User_Message, I)));
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  end if;
               end loop;
            end if;

         when Fault_Handler_Override =>
            --FHO request corresponds with a TLV type octet of 0x04
            Result(Next_Octet) := Result(Next_Octet) or 2#0000_0100#;
            Next_Octet := Next_Octet + 1;

            --Length is always one octet
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0001#);
            Next_Octet := Next_Octet + 1;

            --Condition code goes in first four bytes
            case TLV_Information.FHO_Condition_Code is
               when ACK_Limit_Reached .. Check_Limit_Reached =>
                  Temp_Code := Condition_Code_Type'Pos(TLV_Information.FHO_Condition_Code);
                  Char_Temp := Shift_Left(Temp_Code, 4);
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  --TODO: ensure we never reach this
               when others =>
                  null;
            end case;

            --Handler code goes in last four bytes
            --Add one to the 'Pos to account for the reserved value at 0x00
            --that we don't include in the enum
            Temp_Code := FHO_Handler_Code_Type'Pos(TLV_Information.FHO_Handler_Code);
            Char_Temp := Temp_Code + 1;
            Result(Next_Octet) := (Result(Next_Octet) and 2#1111_0000#)
              or Char_Temp;

         when Flow_Label =>
            --Flow_Label request corresponds with a TLV type octet of 0x05
            Result(Next_Octet) := Result(Next_Octet) or 2#0000_0101#;
            Next_Octet := Next_Octet + 1;

            --Store the length in the next octet
            Message_Length := Bounded_Strings.Length(TLV_Information.Flow_Label_Message);
            Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
              or Octet(Message_Length);
            Next_Octet := Next_Octet + 1;

            --Write the message, one octet at a time
            if Message_Length > 0 then
               for I in 1 .. Message_Length loop
                  Char_Temp := Octet(
                                           Character'Pos(
                                             Bounded_Strings.Element(
                                               TLV_Information.Flow_Label_Message, I)));
                  Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
                    or Char_Temp;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  end if;
               end loop;
            end if;
         when others =>
            null;
      end case;
      return Result;
   end Encode_TLV;

   procedure Decode_TLV
     (TLV_Segment     : in  TLV_Array;
      Next_Octet      : out Next_TLV_Octet_Type;
      TLV_Information : out TLV_Record;
      Status          : out PDU_Status_Type)
   is
      TLV_Category : TLV_Type;
      Temp_Category : Octet range 0 .. 15;
      Remaining_Length : Natural range 0 .. 255;
      Has_Dest : Boolean;
      Temp_Octet : Octet;
      Source_Length : Natural range 0 .. Max_Name_Length;
      Dest_Length : Natural range 0 .. Max_Name_Length;
      Message_Length : Natural range 0 .. Max_TLV_Message_Length;
   begin

      Status := Success;
      Next_Octet := 0;

      --Check the first octet... what kind of TLV is this?
      Temp_Category := TLV_Segment(Natural(Next_Octet)) and 2#0000_0111#;
      --Check that the temp_category is within range
      if Temp_Category <= 6 then
         TLV_Category := TLV_Type'Val(Temp_Category);
      else
         TLV_Category := NOT_USED;
         Status := Invalid_PDU;
      end if;

      TLV_Information.TLV_Category := TLV_Category;
      Next_Octet := Next_Octet + 1;

      -- Initialize everything else to default values to satisfy SPARK. This is necessary
      -- despite TLV_Information having full default initialization because SPARK does not
      -- consider that fact for out parameters. This is intended behavior. See Ticket #PA04-039
      -- in our GNATtracker account.
      --
      -- TODO: Order these assignments to match the order of the component declarations.
      TLV_Information.Filestore_Action_Code := Create_File;
      TLV_Information.Source_File_Name := Bounded_Strings.Make(Max_Name_Length, "");
      TLV_Information.Dest_File_Name := Bounded_Strings.Make(Max_Name_Length, "");
      TLV_Information.FHO_Condition_Code := No_Error;
      TLV_Information.Filestore_Status_Code := Successful;
      TLV_Information.FHO_Handler_Code := Notice_Cancellation;
      TLV_Information.User_Message := Bounded_Strings.Make(Max_Message_Length, "");
      TLV_Information.Flow_Label_Message := Bounded_Strings.Make(Max_Message_Length, "");
      Bounded_Strings.Clear(TLV_Information.Filestore_Message);

      case TLV_Category is
         when Filestore_Request =>
            --Check the length. If it looks invalid, mark the pdu as invalid.
            Remaining_Length := Natural(TLV_Segment(Natural(Next_Octet)));
            Next_Octet := Next_Octet + 1;

            if Remaining_Length > 1 + 1 + Max_Name_Length + 1 + Max_Name_Length then
               Status := Invalid_PDU;
               return;
            end if;

            --Check the filestore action code (must be 0 .. 8)
            Temp_Category := (TLV_Segment(Natural(Next_Octet)) / 16) and 2#0000_1111#;
            Next_Octet := Next_Octet + 1;

            --Only proceed if the action code is valid
            if Temp_Category > 8 then
               Status := Invalid_PDU;
               return;
            end if;
            TLV_Information.Filestore_Action_Code :=
              Filestore_Action_Code_Type'Val(Temp_Category);

            --check to see if we expect a destination
            case Filestore_Action_Code_Type'Val(Temp_Category) is
               when Rename_File .. Replace_File =>
                  Has_Dest := True;
               when others =>
                  Has_Dest := False;
            end case;

            --check the source file length
            Temp_Octet := TLV_Segment(Natural(Next_Octet)) and 2#0001_1111#;
            Next_Octet := Next_Octet + 1;

            if Natural(Temp_Octet) > Max_Name_Length then
               Status := Invalid_PDU;
               return;
            end if;

            Source_Length := Natural(Temp_Octet);

            --Read the source file name
            for I in 0 .. Source_Length - 1 loop
               Temp_Octet := TLV_Segment(Natural(Next_Octet));
               if (Bounded_Strings.Length(TLV_Information.Source_File_Name)
                   + 1 <= TLV_Information.Source_File_Name.Bound) then
                  Bounded_Strings.Append(TLV_Information.Source_File_Name,
                                         Character'Val(Temp_Octet));
               else
                  Status := Invalid_PDU;
               end if;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;
            end loop;

            --If we need to read the destination, do so here
            if Has_Dest then
               --check the destination file length
               Temp_Octet := TLV_Segment(Natural(Next_Octet)) and 2#0001_1111#;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;

               if Natural(Temp_Octet) > Max_Name_Length then
                  Status := Invalid_PDU;
                  return;
               end if;

               Dest_Length := Natural(Temp_Octet);

               --Read the destination file name
               for I in 0 .. Dest_Length - 1 loop
                  Temp_Octet := TLV_Segment(Natural(Next_Octet));
                  if (Bounded_Strings.Length(TLV_Information.Dest_File_Name)
                      + 1 <= TLV_Information.Dest_File_Name.Bound) then
                     Bounded_Strings.Append(TLV_Information.Dest_File_Name,
                                            Character'Val(Temp_Octet));
                  else
                     Status := Invalid_PDU;
                  end if;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  else
                     Status := Invalid_PDU;
                  end if;
               end loop;
            end if;

         when Filestore_Response =>
            --Check the length. If it looks invalid, mark the pdu as invalid.
            Remaining_Length := Natural(TLV_Segment(Natural(Next_Octet)));
            Next_Octet := Next_Octet + 1;

            if Remaining_Length > 1 + 1 + Max_Name_Length + 1 + Max_Name_Length + 1 + Max_TLV_Message_Length then
               Status := Invalid_PDU;
               return;
            end if;

            --Check the filestore action code (must be 0 .. 8)
            Temp_Category := (TLV_Segment(Natural(Next_Octet)) / 16) and 2#0000_1111#;


            --Only proceed if the action code is valid
            if Temp_Category > 8 then
               Status := Invalid_PDU;
               return;
            end if;
            TLV_Information.Filestore_Action_Code :=
              Filestore_Action_Code_Type'Val(Temp_Category);

            --check to see if we expect a destination
            case Filestore_Action_Code_Type'Val(Temp_Category) is
               when Rename_File .. Replace_File =>
                  Has_Dest := True;
               when others =>
                  Has_Dest := False;
            end case;

            --check the filestore status code
            Temp_Category := TLV_Segment(Natural(Next_Octet)) and 2#0000_1111#;
            TLV_Information.Filestore_Status_Code :=
              Get_Filestore_Status_Code(TLV_Information.Filestore_Action_Code,
                                        Filestore_Status_Value(Temp_Category));

            Next_Octet := Next_Octet + 1;

            --check the source file length
            Temp_Octet := TLV_Segment(Natural(Next_Octet)) and 2#0001_1111#;
            Next_Octet := Next_Octet + 1;

            if Natural(Temp_Octet) > Max_Name_Length then
               Status := Invalid_PDU;
               return;
            end if;

            Source_Length := Natural(Temp_Octet);

            --Read the source file name
            for I in 0 .. Source_Length - 1 loop
               Temp_Octet := TLV_Segment(Natural(Next_Octet));
               if (Bounded_Strings.Length(TLV_Information.Source_File_Name)
                   + 1 <= TLV_Information.Source_File_Name.Bound) then
                  Bounded_Strings.Append(TLV_Information.Source_File_Name,
                                         Character'Val(Temp_Octet));
               else
                  Status := Invalid_PDU;
               end if;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;
            end loop;

            --If we need to read the destination, do so here
            if Has_Dest then
               --check the destination file length
               Temp_Octet := TLV_Segment(Natural(Next_Octet)) and 2#0001_1111#;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;

               if Natural(Temp_Octet) > Max_Name_Length then
                  Status := Invalid_PDU;
                  return;
               end if;

               Dest_Length := Natural(Temp_Octet);

               --Read the destination file name
               for I in 0 .. Dest_Length - 1 loop
                  Temp_Octet := TLV_Segment(Natural(Next_Octet));
                  if (Bounded_Strings.Length(TLV_Information.Dest_File_Name)
                      + 1 <= TLV_Information.Dest_File_Name.Bound) then
                     Bounded_Strings.Append(TLV_Information.Dest_File_Name,
                                            Character'Val(Temp_Octet));
                  else
                     Status := Invalid_PDU;
                  end if;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  else
                     Status := Invalid_PDU;
                  end if;
               end loop;
            end if;

            if TLV_Segment(Natural(Next_Octet)) > 0 and TLV_Segment(Natural(Next_Octet)) <= Max_TLV_Message_Length then
               Message_Length := Natural(TLV_Segment(Natural(Next_Octet)));

               --Bounded_Strings.Clear(TLV_Information.Filestore_Message);

               for I in 0 .. Message_Length - 1 loop
                  Temp_Octet := TLV_Segment(Natural(Next_Octet));
                  if (Bounded_Strings.Length(TLV_Information.Filestore_Message)
                      + 1 <= TLV_Information.Filestore_Message.Bound) then
                     Bounded_Strings.Append(TLV_Information.Filestore_Message,
                                            Character'Val(Temp_Octet));
                  else
                     Status := Invalid_PDU;
                  end if;
                  if Next_Octet + 1 < Max_TLV_Size - 1 then
                     Next_Octet := Next_Octet + 1;
                  else
                     Status := Invalid_PDU;
                  end if;
               end loop;
            end if;

         when Message_To_User =>
            --Check the length. If it looks invalid, mark the pdu as invalid.
            Remaining_Length := Natural(TLV_Segment(Natural(Next_Octet)));
            Next_Octet := Next_Octet + 1;

            --Read the message to user
            for I in 0 .. Remaining_Length - 1 loop
               Temp_Octet := TLV_Segment(Natural(Next_Octet));
               if (Bounded_Strings.Length(TLV_Information.User_Message)
                   + 1 <= TLV_Information.User_Message.Bound) then
                  Bounded_Strings.Append(TLV_Information.User_Message,
                                         Character'Val(Temp_Octet));
               else
                  Status := Invalid_PDU;
               end if;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;
            end loop;

         when Fault_Handler_Override =>
            --check to make sure length is 1
            if TLV_Segment(Natural(Next_Octet)) /= 1 then
               Status := Invalid_PDU;
               return;
            end if;

            Next_Octet := Next_Octet + 1;

            --read the condition code (max possible is 10 in this case)
            Temp_Category := TLV_Segment(Natural(Next_Octet)) / 16;
            if Temp_Category > 10 or Temp_Category < 1 then
               Status := Invalid_PDU;
               return;
            end if;

            TLV_Information.FHO_Condition_Code := Condition_Code_Type'Val(Temp_Category);

            --read the handler code (can be from 1 to 4, everything else is unused)
            Temp_Category := TLV_Segment(Natural(Next_Octet)) and 2#0000_1111#;
            if Temp_Category > 4 or Temp_Category < 1 then
               Status := Invalid_PDU;
               return;
            end if;

            TLV_Information.FHO_Handler_Code := FHO_Handler_Code_Type'Val(Temp_Category - 1);

         when Flow_Label =>
            --Check the length. If it looks invalid, mark the pdu as invalid.
            Remaining_Length := Natural(TLV_Segment(Natural(Next_Octet)));
            Next_Octet := Next_Octet + 1;

            --Read the flow label message
            for I in 0 .. Remaining_Length - 1 loop
               Temp_Octet := TLV_Segment(Natural(Next_Octet));
               if (Bounded_Strings.Length(TLV_Information.Flow_Label_Message)
                   + 1 <= TLV_Information.Flow_Label_Message.Bound) then
                  Bounded_Strings.Append(TLV_Information.Flow_Label_Message,
                                         Character'Val(Temp_Octet));
               else
                  Status := Invalid_PDU;
               end if;
               if Next_Octet + 1 < Max_TLV_Size - 1 then
                  Next_Octet := Next_Octet + 1;
               else
                  Status := Invalid_PDU;
               end if;
            end loop;

         when others =>
            Status := Invalid_PDU;

      end case;
   end Decode_TLV;


   function Encode_Filedata_PDU
     (Filedata_Information : Filedata_Record) return Filedata_PDU_Array
   is
      Result : Filedata_PDU_Array := (others => 0);
   begin
      --File offset is encoded in 4 octets.
      Result(0) := (Result(0) and 2#0000_0000#)
        or Octet(Filedata_Information.Segment_Offset / 256 / 256 / 256);
      Result(1) := (Result(1) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Filedata_Information.Segment_Offset), 8) / 256 / 256 / 256);
      Result(2) := (Result(2) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Filedata_Information.Segment_Offset), 16) / 256 / 256 / 256);
      Result(3) := (Result(3) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         Filedata_Information.Segment_Offset), 24) / 256 / 256 / 256);

      --For now, file segment is 1024 bytes. Not sure if correct, but it works for now
      for I in 4 .. Max_Filedata_Segment_Size - 1 loop
         Result(I) := Filedata_Information.Filedata_Segment(I - 4);
      end loop;
      return Result;
   end Encode_Filedata_PDU;


   procedure Decode_Filedata_PDU
     (Filedata_Segment     : in Filedata_PDU_Array;
      Filedata_Information : out Filedata_Record;
      Status               : out PDU_Status_Type)
   is
      Temp_Offset : Quadruple_Octet;
   begin
      Status := Success;

      --offset is encoded in first four octets
      Temp_Offset := (Shift_Left(Quadruple_Octet(Filedata_Segment(0)), 24)
                      and 2#1111_1111_0000_0000_0000_0000_0000_0000#);
      Temp_Offset := (Shift_Left(Quadruple_Octet(Filedata_Segment(1)), 16)
                      and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Temp_Offset;
      Temp_Offset := (Shift_Left(Quadruple_Octet(Filedata_Segment(2)), 8)
                      and 2#1111_1111_1111_1111_1111_1111_0000_0000#) or Temp_Offset;
      Temp_Offset := (Quadruple_Octet(Filedata_Segment(3))
                      and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Temp_Offset;
      Filedata_Information.Segment_Offset := Segment_Offset_Type(Temp_Offset);

      --Get all 1024 bytes of raw file data
      for I in 0 .. Max_Segment_Size - 1 loop
         Filedata_Information.Filedata_Segment(I) := Filedata_Segment(I + 4);
      end loop;
   end Decode_Filedata_PDU;


   function Encode_NAK_PDU
     (NAK_Information : NAK_Record) return NAK_PDU_Array
   is
      Result : NAK_PDU_Array := (others => 0);
      Temp_Segment : NAK_Segment_Record;
      Next_Octet : Natural range 9 .. Max_NAK_Segment_Size - 1;
   begin
      -- first octet is just the directive code of 0x08
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_1000#;

      --start scope is encoded in 4 octets.
      Result(1) := (Result(1) and 2#0000_0000#)
        or Octet(NAK_Information.Start_Scope / 256 / 256 / 256);
      Result(2) := (Result(2) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.Start_Scope), 8) / 256 / 256 / 256);
      Result(3) := (Result(3) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.Start_Scope), 16) / 256 / 256 / 256);
      Result(4) := (Result(4) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.Start_Scope), 24) / 256 / 256 / 256);

      --end scope is encoded in 4 octets.
      Result(5) := (Result(5) and 2#0000_0000#)
        or Octet(NAK_Information.End_Scope / 256 / 256 / 256);
      Result(6) := (Result(6) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.End_Scope), 8) / 256 / 256 / 256);
      Result(7) := (Result(7) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.End_Scope), 16) / 256 / 256 / 256);
      Result(8) := (Result(8) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         NAK_Information.End_Scope), 24) / 256 / 256 / 256);

      Next_Octet := 9;

      --Encode all the NAK Segments
      for I in NAK_Information.NAK_Segments'Range loop
         Temp_Segment := NAK_Information.NAK_Segments(I);
         --start offset is encoded in 4 octets.
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Temp_Segment.Start_Offset / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.Start_Offset), 8) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.Start_Offset), 16) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.Start_Offset), 24) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;

         --end offset is encoded in 4 octets.
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Temp_Segment.End_Offset / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.End_Offset), 8) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.End_Offset), 16) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
         Result(Next_Octet) := (Result(Next_Octet) and 2#0000_0000#)
           or Octet(Shift_Left(
                          Quadruple_Octet(
                            Temp_Segment.End_Offset), 24) / 256 / 256 / 256);
         if Next_Octet + 1 < Max_NAK_Segment_Size - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;
      return Result;
   end Encode_NAK_PDU;

   --TODO: Holding off on Decode_NAK while we figure out how it'll work


   function Encode_Prompt_PDU
     (Prompt_Information : Prompt_Record) return Prompt_PDU_Array
   is
      Result : Prompt_PDU_Array := (others => 0);
      NAK_Or_KA : Octet;
   begin
      --Encode the prompt directive code: 0x09
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_1001#;

      --Encode the NAK/KA in the next (and last) octet
      NAK_Or_KA :=
        Octet(NAK_Or_Keep_Alive_Type'Pos(Prompt_Information.NAK_Or_Keep_Alive));
      Result(1) := (Result(1) and 2#0000_0000#)
        or Shift_Left(NAK_Or_KA, 7);

      return Result;
   end Encode_Prompt_PDU;


   procedure Decode_Prompt_PDU
     (Prompt_PDU         : in Prompt_PDU_Array;
      Prompt_Information : out Prompt_Record)
   is
     NAK_Or_KA : NAK_Or_Keep_Alive_Type;
   begin
      --This is all the info we need
      NAK_Or_KA := NAK_Or_Keep_Alive_Type'Val(Prompt_PDU(1) / 128);
      Prompt_Information.NAK_Or_Keep_Alive := NAK_Or_KA;
   end Decode_Prompt_PDU;

   function Encode_EOF_PDU
     (EOF_Information : EOF_Record) return EOF_Array
   is
      Result : EOF_Array := (others => 0);
      Condition_Code : Octet range 0 .. 15;
   begin
      --Encode the EOF directive code: 0x04
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_0100#;

      --Encode the condition code in the first four bits
      Condition_Code := Condition_Code_Type'Pos(EOF_Information.Condition_Code);
      Result(1) := Shift_Left(Condition_Code, 4);

      --File checksum is encoded in 4 octets.
      Result(2) := (Result(2) and 2#0000_0000#)
        or Octet(EOF_Information.File_Checksum / 256 / 256 / 256);
      Result(3) := (Result(3) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                        EOF_Information.File_Checksum), 8) / 256 / 256 / 256);
      Result(4) := (Result(4) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         EOF_Information.File_Checksum), 16) / 256 / 256 / 256);
      Result(5) := (Result(5) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         EOF_Information.File_Checksum), 24) / 256 / 256 / 256);

      --file size is encoded in 4 octets.
      Result(6) := (Result(6) and 2#0000_0000#)
        or Octet(EOF_Information.File_Size / 256 / 256 / 256);
      Result(7) := (Result(7) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         EOF_Information.File_Size), 8) / 256 / 256 / 256);
      Result(8) := (Result(8) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         EOF_Information.File_Size), 16) / 256 / 256 / 256);
      Result(9) := (Result(9) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         EOF_Information.File_Size), 24) / 256 / 256 / 256);
      return Result;
   end Encode_EOF_PDU;


   procedure Decode_EOF_PDU
     (EOF_PDU         : in EOF_Array;
      EOF_Information : out EOF_Record)
   is
      Condition_Code : Condition_Code_Type;
      Temp_Offset : Quadruple_Octet;
   begin
      --Condition code
      Condition_Code := Condition_Code_Type'Val(EOF_PDU(1) / 16);
      EOF_Information.Condition_Code := Condition_Code;

      --checksum is encoded in next four octets
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(2)), 24)
                      and 2#1111_1111_0000_0000_0000_0000_0000_0000#);
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(3)), 16)
                      and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Temp_Offset;
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(4)), 8)
                      and 2#1111_1111_1111_1111_1111_1111_0000_0000#) or Temp_Offset;
      Temp_Offset := (Quadruple_Octet(EOF_PDU(5))
                      and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Temp_Offset;
      EOF_Information.File_Checksum := Segment_Offset_Type(Temp_Offset);

      --filesize is encoded in next four octets
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(6)), 24)
                      and 2#1111_1111_0000_0000_0000_0000_0000_0000#);
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(7)), 16)
                      and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Temp_Offset;
      Temp_Offset := (Shift_Left(Quadruple_Octet(EOF_PDU(8)), 8)
                      and 2#1111_1111_1111_1111_1111_1111_0000_0000#) or Temp_Offset;
      Temp_Offset := (Quadruple_Octet(EOF_PDU(9))
                      and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Temp_Offset;
      EOF_Information.File_Size := Segment_Offset_Type(Temp_Offset);

   end Decode_EOF_PDU;


   function Encode_Keep_Alive_PDU
     (KA_Information : Keep_Alive_Record) return Keep_Alive_Array
   is
      Result : Keep_Alive_Array := (others => 0);
   begin
      --Encode the keep alive directive code: 0x0C
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_1100#;

     --Progress is encoded in 4 octets.
      Result(1) := (Result(1) and 2#0000_0000#)
        or Octet(KA_Information.Progress / 256 / 256 / 256);
      Result(2) := (Result(2) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         KA_Information.Progress), 8) / 256 / 256 / 256);
      Result(3) := (Result(3) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         KA_Information.Progress), 16) / 256 / 256 / 256);
      Result(4) := (Result(4) and 2#0000_0000#)
        or Octet(Shift_Left(
                       Quadruple_Octet(
                         KA_Information.Progress), 24) / 256 / 256 / 256);
      return Result;
   end Encode_Keep_Alive_PDU;


   procedure Decode_Keep_Alive_PDU
     (KA_PDU         : in Keep_Alive_Array;
      KA_Information : out Keep_Alive_Record)
   is
      Progress : Quadruple_Octet;
   begin
      --progress through transmission is encoded in the last four bytes of the KA PDU
      Progress := (Shift_Left(Quadruple_Octet(KA_PDU(1)), 24)
                      and 2#1111_1111_0000_0000_0000_0000_0000_0000#);
      Progress := (Shift_Left(Quadruple_Octet(KA_PDU(2)), 16)
                      and 2#1111_1111_1111_1111_0000_0000_0000_0000#) or Progress;
      Progress := (Shift_Left(Quadruple_Octet(KA_PDU(3)), 8)
                      and 2#1111_1111_1111_1111_1111_1111_0000_0000#) or Progress;
      Progress := (Quadruple_Octet(KA_PDU(4))
                      and 2#1111_1111_1111_1111_1111_1111_1111_1111#) or Progress;
      KA_Information.Progress := Segment_Offset_Type(Progress);
   end Decode_Keep_Alive_PDU;


   function Encode_Finished_PDU
     (Finished_Information : Finished_Record) return Finished_PDU_Array
   is
      Result : Finished_PDU_Array := (others => 0);
      Condition_Code : Octet range 0 .. 15;
      System_Status : Octet range 0 .. 1;
      Delivery_Code : Octet range 0 .. 1;
      File_Status_Code : Octet range 0 .. 3;
   begin
      --encode the directive code of 0x05
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_0101#;

      --encode the condition code
      Condition_Code := Condition_Code_Type'Pos(Finished_Information.Condition_Code);
      Result(1) := (Result(1) and 2#0000_0000#) or Shift_Left(Condition_Code, 4);

      --encode the end system status
      System_Status := End_System_Status_Type'Pos(Finished_Information.End_System_Status);
      Result(1) := Result(1) or Shift_Left(System_Status, 3);

      --encode the delivery code
      Delivery_Code := Delivery_Code_Type'Pos(Finished_Information.Delivery_Code);
      Result(1) := Result(1) or Shift_Left(Delivery_Code, 2);

      --encode the file status code
      File_Status_Code := File_Status_Code_Type'Pos(Finished_Information.File_Status_Code);
      Result(1) := Result(1) or File_Status_Code;

      --there's a bunch of TLVs following all this for filestore_responses

      return Result;
   end Encode_Finished_PDU;


   procedure Decode_Finished_PDU
     (Finished_PDU         : in Finished_PDU_Array;
      Finished_Information : out Finished_Record)
   is
      Condition_Code : Condition_Code_Type;
      Temp_Bit : Octet range 0 .. 1;
      Temp_File_Status : Octet range 0 .. 3;
   begin
      --grab the condition code
      Condition_Code := Condition_Code_Type'Val(Finished_PDU(1) / 16);
      Finished_Information.Condition_Code := Condition_Code;

      --grab the end system status
      Temp_Bit := (Finished_PDU(1) and 2#0000_1000#) / 8;
      Finished_Information.End_System_Status := End_System_Status_Type'Val(Temp_Bit);

      --grab the delivery code
      Temp_Bit := (Finished_PDU(1) and 2#0000_0100#) / 4;
      Finished_Information.Delivery_Code := Delivery_Code_Type'Val(Temp_Bit);

      --grab the file status code
      Temp_File_Status := Finished_PDU(1) and 2#0000_0011#;
      Finished_Information.File_Status_Code := File_Status_Code_Type'Val(Temp_File_Status);

   end Decode_Finished_PDU;


   function Encode_ACK_PDU
     (ACK_Information : ACK_Record) return ACK_PDU_Array
   is
      Result : ACK_PDU_Array := (others => 0);
      Directive_Code : Octet range 0 .. 15;
      Subtype_Code : Octet range 0 .. 1;
      Condition_Code : Octet range 0 .. 15;
      Transaction_Status : Octet range 0 .. 3;
   begin
      --encode the file directive code of 0x06
      Result(0) := (Result(0) and 2#0000_0000#) or 2#0000_0110#;

      --encode the directive code
      --add 4 to account for reserved values
      Directive_Code := File_Directive_Code_Type'Pos(ACK_Information.Directive_Code) + 4;
      Result(1) := (Result(1) and 2#0000_0000#) or
        Shift_Left(Directive_Code, 4);

      --encode the directive subtype
      Subtype_Code := Directive_Subtype_Code_Type'Pos(ACK_Information.Directive_Subtype_Code);
      Result(1) := Result(1) or Subtype_Code;

      --encode the condition code
      Condition_Code := Condition_Code_Type'Pos(ACK_Information.Condition_Code);
      Result(2) := (Result(2) and 2#0000_0000#) or
        Shift_Left(Condition_Code, 4);

      --next two bits are spare

      --encode the transaction status
      Transaction_Status := Transaction_Status_Type'Pos(ACK_Information.Transaction_Status);
      Result(2) := Result(2) or Transaction_Status;

      return Result;
   end Encode_ACK_PDU;


   procedure Decode_ACK_PDU
     (ACK_PDU         : in ACK_PDU_Array;
      ACK_Information : out ACK_Record;
      Status          : out PDU_Status_Type)
   is
      Directive_Code : File_Directive_Code_Type;
      Has_Subtype : Boolean;
      Transaction_Status : Octet range 0 .. 3;
   begin
      Status := Success;

      --get the directive code, and check that it's valid
      Directive_Code := File_Directive_Code_Type'Val((ACK_PDU(1) / 16) - 4);

      case Directive_Code is
         when EOF_PDU =>
            Has_Subtype := False;
         when Finished_PDU =>
            Has_Subtype := True;
         when others =>
            Has_Subtype := False;
            Status := Invalid_PDU;
      end case;

      ACK_Information.Directive_Code := Directive_Code;

      --get the directive subtype, if present (otherwise set to default value)
      if Has_Subtype then
         ACK_Information.Directive_Subtype_Code :=
           Directive_Subtype_Code_Type'Val(ACK_PDU(1) and 2#0000_0001#);
      else
         ACK_Information.Directive_Subtype_Code := Generated_By_Waypoint;
      end if;

      --get the condition code
      ACK_Information.Condition_Code := Condition_Code_Type'Val(ACK_PDU(2) / 16);

      --get the transaction status
      Transaction_Status := ACK_PDU(2) and 2#0000_0011#;
      ACK_Information.Transaction_Status := Transaction_Status_Type'Val(Transaction_Status);

   end Decode_ACK_PDU;


   function Interpret_PDU(Complete_PDU : Unknown_PDU_Array) return PDU_Category is
      Result : PDU_Category;
      Dir_Or_Dat : PDU_Type;
      File_Directive_Code : File_Directive_Code_Type;
   begin

      Dir_Or_Dat := PDU_Type'Val(((Complete_PDU(0) and 2#0001_0000#) / 16));

      case Dir_Or_Dat is

         when File_Data =>
            Result := Filedata_PDU;

         when File_Directive =>
            File_Directive_Code :=
              File_Directive_Code_Type'Val(Complete_PDU(10));
            case File_Directive_Code is

            when Metadata_PDU =>
               Result := Metadata_PDU;

            when EOF_PDU =>
               Result := EOF_PDU;

               when Finished_PDU =>
                  Result := Finished_PDU;

               when others =>
                  Result := Error;
            end case;
      end case;

      return Result;
   end Interpret_PDU;


   function Construct_Metadata_PDU
     (Header_Information   : Fixed_Header_Record;
      Metadata_Information : Metadata_PDU_Record;
      TLV_Options          : TLV_Options_Array) return Complete_Metadata_PDU_Array
   is
      Result : Complete_Metadata_PDU_Array := (others => 0);
      Header_PDU : Fixed_Header_Array;
      Metadata_PDU : Metadata_PDU_Array;
      Temp_TLV : TLV_Array;
      Next_Octet : Natural range 0 .. Complete_Metadata_PDU_Array'Last - 1 := 0;
      Metadata_End : Natural range 0 .. Metadata_PDU_Array'Last := Metadata_PDU_Array'Last;
      Temp_TLV_End : Natural range 0 .. TLV_Array'Last := TLV_Array'Last;
      Has_Found_End : Boolean;
   begin
      --encode the first two PDUs
      Header_PDU := Encode_Fixed_PDU_Header(Header_Information);
      Metadata_PDU := Encode_Metadata_PDU(Metadata_Information);

      --header size never varies, so we just copy it
      for I in Fixed_Header_Array'Range loop
         Result(Next_Octet) := Header_PDU(I);
         if Next_Octet + 1 < Complete_Metadata_PDU_Array'Last - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      --calculate the last array element with real data in the metadata PDU
      Has_Found_End := False;
      for I in reverse Metadata_PDU'Range loop
         if not Has_Found_End then
            if Metadata_PDU(I) /= 0 then
               Has_Found_End := True;
               Metadata_End := I;
            end if;
         end if;
      end loop;

      --add the metadata PDU to the result array
      for I in 0 .. Metadata_End loop
         Result(Next_Octet) := Metadata_PDU(I);
         if Next_Octet + 1 < Complete_Metadata_PDU_Array'Last - 1 then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      --handle each TLV Option : first calculate length of the PDU, then add it to result
      for A in TLV_Options'Range loop
         Temp_TLV := Encode_TLV(TLV_Options(A));

         if A = 0 then

            --calculate the last array element with real data in the TLV Option
            Has_Found_End := False;
            for I in reverse Temp_TLV'Range loop
               if not Has_Found_End then
                  if Temp_TLV(I) /= 0 then
                     Has_Found_End := True;
                     Temp_TLV_End := I;
                  end if;
               end if;
            end loop;

            --add the TLV Option to the result array
            for I in 0 .. Temp_TLV_End loop
               Result(Next_Octet) := Temp_TLV(I);
               if Next_Octet + 1 < Complete_Metadata_PDU_Array'Last - 1 then
                  Next_Octet := Next_Octet + 1;
               end if;
            end loop;

         end if;
      end loop;

      return Result;
   end Construct_Metadata_PDU;


   procedure Deconstruct_Metadata_PDU
     (Complete_Metadata    : in  Complete_Metadata_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Metadata_Information : out Metadata_PDU_Record;
      TLV_Information      : out TLV_Record;
      Status               : out PDU_Status_Type)
   is
      Header_Array : Fixed_Header_Array;
      Metadata_Array : Metadata_PDU_Array;
      TLV_Option : TLV_Array;
      Next_After_Metadata : Next_Metadata_Octet_Type;
      Next_After_TLV : Next_TLV_Octet_Type;
   begin
      -- Initialize everything else to default values to satisfy SPARK. This is necessary
      -- despite TLV_Information having full default initialization because SPARK does not
      -- consider that fact for out parameters. This is intended behavior. See Ticket #PA04-039
      -- in our GNATtracker account.
      --
      -- TODO: Order these assignments to match the order of the component declarations.
      TLV_Information.Filestore_Action_Code := Create_File;
      TLV_Information.Source_File_Name := Bounded_Strings.Make(Max_Name_Length, "");
      TLV_Information.Dest_File_Name := Bounded_Strings.Make(Max_Name_Length, "");
      TLV_Information.FHO_Condition_Code := No_Error;
      TLV_Information.Filestore_Status_Code := Successful;
      TLV_Information.FHO_Handler_Code := Notice_Cancellation;
      TLV_Information.User_Message := Bounded_Strings.Make(Max_Message_Length, "");
      TLV_Information.Flow_Label_Message := Bounded_Strings.Make(Max_Message_Length, "");
      Bounded_Strings.Clear(TLV_Information.Filestore_Message);
      TLV_Information.TLV_Category := Filestore_Request;

      Metadata_Information.Segmentation_Control := Boundaries_Respected;
      Metadata_Information.File_Size := 0;
      Metadata_Information.Source_File_Length := 0;
      Bounded_Strings.Clear(Metadata_Information.Source_File_Name);
      Metadata_Information.Dest_File_Length := 0;
      Bounded_Strings.Clear(Metadata_Information.Dest_File_Name);

      -- Decode the header first. Check that it was valid.
      for I in 0 .. Max_Header_Length - 1 loop
         Header_Array(I) := Complete_Metadata(I);
      end loop;

      Decode_Fixed_PDU_Header(Header_Array, Header_Information, Status);

      if Status = Invalid_PDU then
         return;
      end if;

      --remove the header (first 10 octets) from the array, truncate to fit metadata pdu, decode result
      for I in 0 .. Max_Metadata_Size - 1 loop
         Metadata_Array(I) := Complete_Metadata(I + 10);
      end loop;

      Decode_Metadata_PDU(Metadata_Array, Next_After_Metadata, Metadata_Information, Status);

      if Status = Invalid_PDU then
         return;
      end if;

      --Handle any TLV Options (for now assuming one)

      for I in 0 .. Max_TLV_Size - 1 loop
         TLV_Option(I) := Complete_Metadata(I + 10 + Natural(Next_After_Metadata));
      end loop;

      pragma Warnings(Off, "unused assignment to ""Next_After_TLV""",
                      Reason => "Next_After_TLV's value matters in all other cases, but the final run of Decode_TLV's results can be ignored");
      Decode_TLV(TLV_Option, Next_After_TLV, TLV_Information, Status);
      pragma Warnings(On, "unused assignment to ""Next_After_TLV""");
      -- TODO: Next_After_TLV is given a value by Decode_TLV. That value is then ignored.
      -- Is this correct behavior?
   end Deconstruct_Metadata_PDU;


   function Construct_Filedata_PDU
     (Header_Information   : Fixed_Header_Record;
      Filedata_Information : Filedata_Record;
      Is_Last              : Boolean) return Complete_Filedata_PDU_Array
   is
      Result : Complete_Filedata_PDU_Array := (others => 0);
      Header_PDU : Fixed_Header_Array;
      Filedata_PDU : Filedata_PDU_Array;
      Next_Octet : Natural range 0 .. Complete_Filedata_PDU_Array'Last;
      Has_Found_End : Boolean;
      Filedata_End : Natural range 0 .. Filedata_PDU_Array'Last := Filedata_PDU_Array'Last;
   begin
      Next_Octet := 0;

      --encode each piece of the pdu
      Header_PDU := Encode_Fixed_PDU_Header(Header_Information);
      Filedata_PDU := Encode_Filedata_PDU(Filedata_Information);

      --header size never varies, so we just copy it
      for I in Fixed_Header_Array'Range loop
         Result(Next_Octet) := Header_PDU(I);
         if Next_Octet + 1 <= Complete_Filedata_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      --calculate the last array element with real data in the filedata PDU if this is the last one
      Has_Found_End := False;
      if Is_Last then
         for I in reverse Filedata_PDU'Range loop
            if not Has_Found_End then
               if Filedata_PDU(I) /= 0 then
                  Has_Found_End := True;
                  Filedata_End := I;
               end if;
            end if;
         end loop;
      end if;

      --add the filedata PDU to the result array
      for I in 0 .. Filedata_End loop
         Result(Next_Octet) := Filedata_PDU(I);
         if Next_Octet + 1 <= Complete_Filedata_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      return Result;
   end Construct_Filedata_PDU;


   procedure Deconstruct_Filedata_PDU
     (Complete_Filedata    : in  Complete_Filedata_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Filedata_Information : out Filedata_Record;
      Status               : out PDU_Status_Type)
   is
      Header_Array : Fixed_Header_Array;
      Filedata_Array : Filedata_PDU_Array;
   begin

      --Defaults for Filedata_Information (this ensures it's all initialized)
      Filedata_Information.Segment_Offset := 0;
      Filedata_Information.Filedata_Segment := (others => 0);

      --decode the header first, check that it was valid
      for I in 0 .. Max_Header_Length - 1 loop
         Header_Array(I) := Complete_Filedata(I);
      end loop;

      Decode_Fixed_PDU_Header(Header_Array, Header_Information, Status);

      if Status = Invalid_PDU then
         return;
      end if;

      for I in 0 .. Max_Filedata_Segment_Size - 1 loop
            Filedata_Array(I) := Complete_Filedata(I + 10);
      end loop;

      Decode_Filedata_PDU(Filedata_Array, Filedata_Information, Status);
   end Deconstruct_Filedata_PDU;


   function Construct_EOF_PDU
     (Header_Information : Fixed_Header_Record;
      EOF_Information : EOF_Record) return Complete_EOF_PDU_Array
   is
      Result : Complete_EOF_PDU_Array := (others => 0);
      Header_PDU : Fixed_Header_Array;
      EOF_PDU : EOF_Array;
      Next_Octet : Natural range 0 .. Complete_EOF_PDU_Array'Last;
   begin
      Next_Octet := 0;

      Header_PDU := Encode_Fixed_PDU_Header(Header_Information);
      EOF_PDU := Encode_EOF_PDU(EOF_Information);

      --fixed header
      for I in Fixed_Header_Array'Range loop
         Result(Next_Octet) := Header_PDU(I);
         if Next_Octet + 1 <= Complete_EOF_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      --eof part
      for I in EOF_Array'Range loop
         Result(Next_Octet) := EOF_PDU(I);
         if Next_Octet + 1 <= Complete_EOF_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      return Result;
   end Construct_EOF_PDU;


   procedure Deconstruct_EOF_PDU
     (Complete_EOF       : in Complete_EOF_PDU_Array;
      Header_Information : out Fixed_Header_Record;
      EOF_Information    : out EOF_Record;
      Status             : out PDU_Status_Type)
   is
      Header_Array : Fixed_Header_Array;
      EOF_PDU_Array : EOF_Array;
   begin
      --Default Inits
      EOF_Information.Condition_Code := No_Error;
      EOF_Information.File_Checksum := 0;
      EOF_Information.File_Size := 0;

      --first, decode the header
      for I in Header_Array'Range loop
         Header_Array(I) := Complete_EOF(I);
      end loop;

      Decode_Fixed_PDU_Header(Header_Array, Header_Information, Status);

      --if something went wrong, just exit now
      if Status = Invalid_PDU then
         return;
      end if;

      --next, decode the eof part of the pdu (fixed header is always ten octets)
      for I in EOF_Array'Range loop
         EOF_PDU_Array(I) := Complete_EOF(I + 10);
      end loop;

      Decode_EOF_PDU(EOF_PDU_Array, EOF_Information);

   end Deconstruct_EOF_PDU;


   function Construct_Finished_PDU
     (Header_Information   : Fixed_Header_Record;
      Finished_Information : Finished_Record;
      TLV_Options          : TLV_Options_Array;
      Num_File_Requests    : Num_File_Request_Type) return Complete_Finished_PDU_Array
   is
      Result : Complete_Finished_PDU_Array := (others => 0);
      Header_Array : Fixed_Header_Array;
      Finished_Array : Finished_PDU_Array;
      TLV_Option : TLV_Array;
      Next_Octet : Natural range 0 .. Complete_Finished_PDU_Array'Last;
   begin
      Next_Octet := 0;

      --populate the headers
      Header_Array := Encode_Fixed_PDU_Header(Header_Information);
      Finished_Array := Encode_Finished_PDU(Finished_Information);

      for I in Header_Array'Range loop
         Result(Next_Octet) := Header_Array(I);
         if Next_Octet + 1 <= Complete_Finished_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      for I in Finished_Array'Range loop
         Result(Next_Octet) := Finished_Array(I);
         if Next_Octet + 1 <= Complete_Finished_PDU_Array'Last then
            Next_Octet := Next_Octet + 1;
         end if;
      end loop;

      for I in TLV_Options'Range loop
         if Natural(I) < Natural(Num_File_Requests) then
            TLV_Option := Encode_TLV(TLV_Options(I));
            for J in TLV_Option'Range loop
               Result(Next_Octet) := TLV_Option(J);
               if Next_Octet + 1 <= Complete_Finished_PDU_Array'Last then
                  Next_Octet := Next_Octet + 1;
               end if;
            end loop;
         end if;
      end loop;

      return Result;
   end Construct_Finished_PDU;


   procedure Deconstruct_Finished_PDU
     (Complete_Finished    : in Complete_Finished_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Finished_Information : out Finished_Record;
      TLV_Options          : out TLV_Options_Array;
      Status               : out PDU_Status_Type)
   is
      Header_Array   : Fixed_Header_Array;
      Finished_Array : Finished_PDU_Array;
      TLV_Temp_Array : TLV_Array := (others => 0);
      Check_TLV_Type : Natural;
      Next_After_TLV : Next_TLV_Octet_Type;
      Temp_After_TLV : Natural range 0 .. (Natural(Next_TLV_Octet_Type'Last) * 3);
      TLV_Init : TLV_Record;
   begin

      --Default inits
      Finished_Information.File_Status_Code := Retained_Successfully;
      Finished_Information.Condition_Code := No_Error;
      Finished_Information.End_System_Status := Generated_By_System;
      Finished_Information.Delivery_Code := Data_Complete;

      Temp_After_TLV := 0;
      TLV_Options := (others => TLV_Init);
      for I in Header_Array'Range loop
         Header_Array(I) := Complete_Finished(I);
      end loop;

      Decode_Fixed_PDU_Header(Header_Array, Header_Information, Status);

      if Status = Invalid_PDU then
         return;
      end if;

      for I in Finished_Array'Range loop
         Finished_Array(I) := Complete_Finished(I + 10);
      end loop;

      Decode_Finished_PDU(Finished_Array, Finished_Information);

      for I in TLV_Options'Range loop
         for J in TLV_Array'Range loop
            if 10 + 12 + Temp_After_TLV + J <= Complete_Finished'Last then
               TLV_Temp_Array(J) := Complete_Finished(10 + 12 + Temp_After_TLV + J);
            end if;
         end loop;
         Check_TLV_Type := Natural(TLV_Temp_Array(0));
         if Check_TLV_Type = 1 then
            Decode_TLV(TLV_Temp_Array, Next_After_TLV, TLV_Options(I), Status);
            if Natural(Temp_After_TLV) + Natural(Next_After_TLV) <= Natural(Next_TLV_Octet_Type'Last) * 3 then
               Temp_After_TLV := Natural(Temp_After_TLV) + Natural(Next_After_TLV);
            end if;
         end if;
         if Status = Invalid_PDU then
            return;
         end if;
      end loop;
   end Deconstruct_Finished_PDU;

end CubedOS.CFDP.Internals;
