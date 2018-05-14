--------------------------------------------------------------------------------
-- FILE   : cfdp.ads
-- SUBJECT: Top level package of a CFDP implementation for IceCube.
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
-- This module implements the CCSDS File Delivery Protocol (CFDP).
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.CFDP.API;
with CubedOS.Lib;
with CubedOS.Lib.Bounded_Strings;

package CubedOS.CFDP.Internals is
   use CubedOS.CFDP.API;
   use CubedOS.Lib;

   -- In metadata, messages to users can't be more than 255 chars
   Max_Message_Length : constant Bounded_Strings.Index_Type := 255;

   -- Max length of pdu header
   Max_Header_Length : constant := 10;

   -- Max size of the filestore message in a TLV
   Max_TLV_Message_Length : constant := 60;

   -- Maximum length of a TLV PDU. This includes an octet Type, an
   -- octet Length, and a variable
   Max_TLV_Size : constant := 1 + 1 + 255;

   -- The maximum metadata size possible, in octets. See CFDP spec 727.B-4,
   -- table 5-9. We're using a filename length of 8 bits (so the max number of
   -- characters in a filename is 255.)
   Max_Metadata_Size : constant := 1 + 1 + 4 + 1 + 255 + 1
     + 255;

   -- Number of octets per file segment
   Max_Segment_Size : constant := 1024;

   -- Max size of filedata segment
   Max_Filedata_Segment_Size : constant := 4 + Max_Segment_Size;

   -- Max Number of NAK requests in buffer
   Max_NAK_Buffer_Size : constant := 8 * 64;

   -- Max size of a complete NAK PDU
   Max_NAK_Segment_Size : constant := 1 + 4 + 4 + Max_NAK_Buffer_Size;

   -- Max size of EOF segment
   Max_EOF_Segment_Size : constant := 1 + 1 + 4 + 4;

   -- Max size of finished segment
   Max_Finished_Segment_Size : constant := 1 + 1;

   -- Max size of complete metadata pdu
   Max_Complete_Metadata_Size : constant :=
     Max_Header_Length + Max_Metadata_Size + (Max_TLV_Size * 3);

   -- Max size of complete filedata pdu
   Max_Complete_Filedata_Size : constant :=
     Max_Header_Length + Max_Filedata_Segment_Size;

   -- Max size of complete eof pdu
   Max_Complete_EOF_Size : constant :=
     Max_Header_Length + Max_EOF_Segment_Size;

   -- Max size of complete finished pdu
   Max_Complete_Finished_Size : constant :=
     Max_Header_Length + Max_Finished_Segment_Size + (Max_TLV_Size * 3);

   -- Condition codes used by various headers
   type Condition_Code_Type is
     (No_Error,
      ACK_Limit_Reached,
      Keep_Alive_Limit_Reached,
      Invalid_Transmission_Mode,
      Filestore_Rejection,
      File_Checksum_Failure,
      File_Size_Error,
      NAK_Limit_Reached,
      Inactivity_Detected,
      Invalid_File_Structure,
      Check_Limit_Reached,
      Reserved1,
      Reserved2,
      Reserved3,
      Suspend_Received,
      Cancel_Received);

   type PDU_Status_Type is (Success, Invalid_PDU);

   type File_Directive_Code_Type is
     (Reserved1,
      Reserved2,
      Reserved3,
      Reserved4,
      EOF_PDU,
      Finished_PDU,
      ACK_PDU,
      Metadata_PDU,
      NAK_PDU,
      Prompt_PDU,
      Keep_Alive_PDU);

   type Segment_Offset_Type is mod 2**32;

   -- Exclusive to generic PDU Header
   type Version_Type is range 0 .. 7;
   type PDU_Type is (File_Directive, File_Data);
   type Direction_Type is (Toward_Receiver, Toward_Sender);
   type Transmission_Mode_Type is (Acknowledged, Unacknowledged);
   type CRC_Flag_Type is (CRC_Not_Present, CRC_Present);
   type Data_Length_Type is mod 2**16;
   subtype Fixed_Header_Array is Octet_Array(0 .. 9);

   -- Exclusive to Metadata segment
   type Segmentation_Control_Type is (Boundaries_Respected, Boundaries_Not_Respected);
   type File_Size_Type is mod 2**32;
   type Length_Indicator_Type is mod 2**8;
   subtype Metadata_PDU_Array is Octet_Array(0 .. Max_Metadata_Size - 1);
   type Next_Metadata_Octet_Type is new Integer range 0 .. Max_Metadata_Size - 1;

   -- Exclusive to TLV segment
   type TLV_Type is
     (Filestore_Request,
      Filestore_Response,
      Message_To_User,
      NOT_USED,
      Fault_Handler_Override,
      Flow_Label,
      Entity_ID_TLV);

   type Filestore_Action_Code_Type is
     (Create_File,
      Delete_File,
      Rename_File,
      Append_File,
      Replace_File,
      Create_Dir,
      Remove_Dir,
      Deny_File,
      Deny_Dir);

   type Filestore_Status_Code_Type is
     (Successful,
      Create_Not_Allowed,
      Not_Performed,
      File_Does_Not_Exist,
      Delete_Not_Allowed,
      Old_File_Not_Exist,
      New_File_Already_Exist,
      Rename_Not_Allowed,
      File_1_Not_Exist,
      File_2_Not_Exist,
      Append_Not_Allowed,
      Replace_Not_Allowed,
      Directory_Cannot_Create,
      Directory_Not_Exist);

   type FHO_Handler_Code_Type is
     (Notice_Cancellation,
      Notice_Suspension,
      Ignore_Error,
      Abandon_Transaction);

   subtype TLV_Array is Octet_Array(0 .. Max_TLV_Size - 1);
   type Next_TLV_Octet_Type is new Natural range 0 .. Max_TLV_Size - 1;

   -- Exclusive to Filedata segment
   subtype File_Segment_Type is Octet_Array(0 .. Max_Segment_Size - 1);
   subtype Filedata_PDU_Array is Octet_Array(0 .. Max_Filedata_Segment_Size - 1);

   -- Exclusive to NAK segment
   type Scope_Offset_Type is mod 2**32;
   subtype NAK_PDU_Array is Octet_Array(0 .. Max_NAK_Segment_Size - 1);

   -- Exclusive to Prompt segment
   type NAK_Or_Keep_Alive_Type is (NAK, Keep_Alive);
   subtype Prompt_PDU_Array is Octet_Array(0 .. 1);

   -- Exclusive to EOF segement
   subtype EOF_Array is Octet_Array(0 .. Max_EOF_Segment_Size - 1);

   -- Exclusive to keep alive segment
   subtype Keep_Alive_Array is Octet_Array(0 .. 4);

   -- Exclusive to finished segment
   type End_System_Status_Type is (Generated_By_Waypoint, Generated_By_System);
   type Delivery_Code_Type is (Data_Complete, Data_Incomplete);

   type File_Status_Code_Type is
     (Discarded_Deliberately,
      Discarded_Rejection,
      Retained_Successfully,
      Status_Unreported);

   subtype Finished_PDU_Array is Octet_Array(0 .. 1);

   --exclusive to ACK segment
   type Directive_Subtype_Code_Type is (Generated_By_Waypoint, Generated_By_System);
   type Transaction_Status_Type is (Undefined, Active, Terminated, Unrecognized);
   subtype ACK_PDU_Array is Octet_Array(0 .. 2);


   --Final, full PDU arrays of different types
   subtype Complete_Metadata_PDU_Array is Octet_Array(0 .. Max_Complete_Metadata_Size - 1);
   subtype Complete_Filedata_PDU_Array is Octet_Array(0 .. Max_Complete_Filedata_Size - 1);
   subtype Complete_EOF_PDU_Array is Octet_Array(0 .. Max_Complete_EOF_Size - 1);
   subtype Complete_Finished_PDU_Array is Octet_Array(0 .. Max_Complete_Finished_Size - 1);
   subtype Unknown_PDU_Array is Octet_Array(0 .. Max_Complete_Filedata_Size - 1);

   --File type (for now we're pretending that files are up to 64 KiB)
   subtype File_Octets is Octet_Array(0 .. 65535);

   -- This record embodies the information stored in a Fixed PDU header.
   type Fixed_Header_Record is
      record
         Version            : Version_Type;
         PDU                : PDU_Type;
         Direction          : Direction_Type;
         Transmission_Mode  : Transmission_Mode_Type;
         CRC_Flag           : CRC_Flag_Type;
         Data_Length        : Data_Length_Type;
         Source_Entity      : Entity_ID;
         Transaction        : Transaction_ID;
         Destination_Entity : Entity_ID;
      end record;

   type Metadata_PDU_Record is
      record
         Segmentation_Control : Segmentation_Control_Type := Boundaries_Respected;
         File_Size            : File_Size_Type := 0;
         Source_File_Length   : Length_Indicator_Type := 0;
         Source_File_Name     : Bounded_Strings.Bounded_String(Max_Name_Length);
         Dest_File_Length     : Length_Indicator_Type  := 0;
         Dest_File_Name       : Bounded_Strings.Bounded_String(Max_Name_Length);
      end record;

   type TLV_Record is
      record
         TLV_Category : TLV_Type := Filestore_Request;
         --all below are optional depending on the TLV_Category
         Filestore_Action_Code : Filestore_Action_Code_Type := Create_File;
         Source_File_Name : Bounded_Strings.Bounded_String(Max_Name_Length);
         Dest_File_Name : Bounded_Strings.Bounded_String(Max_Name_Length);
         Filestore_Message : Bounded_Strings.Bounded_String(Max_TLV_Message_Length);
         Filestore_Status_Code : Filestore_Status_Code_Type := Successful;
         --FHO is Fault Handler Override
         FHO_Condition_Code : Condition_Code_Type := No_Error;
         FHO_Handler_Code : FHO_Handler_Code_Type := Notice_Cancellation;
         User_Message : Bounded_Strings.Bounded_String(Max_Message_Length);
         Flow_Label_Message : Bounded_Strings.Bounded_String(Max_Message_Length);
      end record;

   -- For now, we just limit the user to 3 TLV options (see CFDP spec for more)
   type TLV_Options_Array_Index is new Natural range 0 .. 2;
   type TLV_Options_Array is array(TLV_Options_Array_Index) of TLV_Record;
   type Num_File_Request_Type is new Natural range 1 .. Natural(TLV_Options_Array'Last + 1);
   type Filestore_Status_Value is new Octet range 0 .. 15;

   type Filedata_Record is
      record
         Segment_Offset : Segment_Offset_Type;
         Filedata_Segment : File_Segment_Type;
      end record;

   type NAK_Segment_Record is
      record
         Start_Offset : Segment_Offset_Type;
         End_Offset : Segment_Offset_Type;
      end record;

   -- Just here so this can see the nak_segment_record
   type NAK_Buffer_Index is new Natural range 0 .. 63;  --allowing for 64 nak segment requests
   type NAK_Buffer_Array is array (NAK_Buffer_Index) of NAK_Segment_Record;

   type NAK_Record is
      record
         Start_Scope : Scope_Offset_Type;
         End_Scope : Scope_Offset_Type;
         NAK_Segments : NAK_Buffer_Array;
      end record;

   type Prompt_Record is
      record
         NAK_Or_Keep_Alive : NAK_Or_Keep_Alive_Type;
      end record;

   type EOF_Record is
      record
         Condition_Code : Condition_Code_Type;
         File_Checksum : Segment_Offset_Type;
         File_Size : Segment_Offset_Type;
         --a TLV follows this
      end record;

   type Keep_Alive_Record is
      record
         Progress : Segment_Offset_Type;
      end record;

   type Finished_Record is
      record
         Condition_Code : Condition_Code_Type;
         End_System_Status : End_System_Status_Type;
         Delivery_Code : Delivery_Code_Type;
         File_Status_Code : File_Status_Code_Type;
         -- Followed by one filestore response TLV per filestore request TLV that was originally
         -- sent
      end record;

   type ACK_Record is
      record
         Directive_Code : File_Directive_Code_Type;
         Directive_Subtype_Code : Directive_Subtype_Code_Type;
         Condition_Code : Condition_Code_Type;
         Transaction_Status : Transaction_Status_Type;
      end record;

   type Transaction_Record is
      record
         Is_Used : Boolean := False;
      end record;

   function File_Checksum(File_Array : File_Octets) return Segment_Offset_Type;

   function Get_Filestore_Status_Val
     (Filestore_Action_Code : Filestore_Action_Code_Type;
      Filestore_Status_Code : Filestore_Status_Code_Type) return Filestore_Status_Value;

   function Get_Filestore_Status_Code
     (Filestore_Action_Code : Filestore_Action_Code_Type;
      Filestore_Status_Val : Filestore_Status_Value) return Filestore_Status_Code_Type;

   function Encode_Fixed_PDU_Header
     (Header_Information : Fixed_Header_Record) return Fixed_Header_Array;

   procedure Decode_Fixed_PDU_Header
     (Header             : in  Fixed_Header_Array;
      Header_Information : out Fixed_Header_Record;
      Status             : out PDU_Status_Type);

   function Encode_Metadata_PDU
     (Metadata_Information : Metadata_PDU_Record) return Metadata_PDU_Array
     with
       Pre => Bounded_Strings.Length(Metadata_Information.Source_File_Name) <= Metadata_Information.Source_File_Name.Bound and
              Bounded_Strings.Length(Metadata_Information.Dest_File_Name) <= Metadata_Information.Dest_File_Name.Bound;

   procedure Decode_Metadata_PDU
     (Metadata             : in  Metadata_PDU_Array;
      Next_Octet           : out Next_Metadata_Octet_Type;
      Metadata_Information : out Metadata_PDU_Record;
      Status               : out PDU_Status_Type)
     with
       Global => null,
       Depends => ((Next_Octet, Metadata_Information, Status) => Metadata);

   function Encode_TLV
     (TLV_Information : TLV_Record) return TLV_Array
     with
       Pre => Bounded_Strings.Length(TLV_Information.Source_File_Name) <= TLV_Information.Source_File_Name.Bound and
              Bounded_Strings.Length(TLV_Information.Dest_File_Name) <= TLV_Information.Dest_File_Name.Bound and
              Bounded_Strings.Length(TLV_Information.User_Message) <= TLV_Information.User_Message.Bound and
     Bounded_Strings.Length(TLV_Information.Flow_Label_Message) <= TLV_Information.Flow_Label_Message.Bound;

   procedure Decode_TLV
     (TLV_Segment     : in TLV_Array;
      Next_Octet      : out Next_TLV_Octet_Type;
      TLV_Information : out TLV_Record;
      Status          : out PDU_Status_Type)
     with
       Global => null,
       Depends => ( Next_Octet => TLV_Segment,
                   (TLV_Information, Status) => (TLV_Information, TLV_Segment));

   function Encode_Filedata_PDU
     (Filedata_Information : Filedata_Record) return Filedata_PDU_Array;

   procedure Decode_Filedata_PDU
     (Filedata_Segment     : in Filedata_PDU_Array;
      Filedata_Information : out Filedata_Record;
      Status               : out PDU_Status_Type);

   function Encode_NAK_PDU
     (NAK_Information : NAK_Record) return NAK_PDU_Array;

   --TODO: Decode NAK (holding off while we work out details)

   function Encode_Prompt_PDU
     (Prompt_Information : Prompt_Record) return Prompt_PDU_Array;

   procedure Decode_Prompt_PDU
     (Prompt_PDU         : in Prompt_PDU_Array;
      Prompt_Information : out Prompt_Record);

   function Encode_EOF_PDU
     (EOF_Information : EOF_Record) return EOF_Array;

   procedure Decode_EOF_PDU
     (EOF_PDU         : in EOF_Array;
      EOF_Information : out EOF_Record);

   function Encode_Keep_Alive_PDU
     (KA_Information : Keep_Alive_Record) return Keep_Alive_Array;

   procedure Decode_Keep_Alive_PDU
     (KA_PDU         : in Keep_Alive_Array;
      KA_Information : out Keep_Alive_Record);

   function Encode_Finished_PDU
     (Finished_Information : Finished_Record) return Finished_PDU_Array;

   procedure Decode_Finished_PDU
     (Finished_PDU         : in Finished_PDU_Array;
      Finished_Information : out Finished_Record);

   function Encode_ACK_PDU
     (ACK_Information : ACK_Record) return ACK_PDU_Array;

   procedure Decode_ACK_PDU
     (ACK_PDU         : in ACK_PDU_Array;
      ACK_Information : out ACK_Record;
      Status          : out PDU_Status_Type)
     with
       Pre => (ACK_PDU(1) / 16) >= 4 and
              (ACK_PDU(1) / 16) <= 10;

   function Interpret_PDU(Complete_PDU : in Unknown_PDU_Array) return PDU_Category
     with
       Pre => Complete_PDU'Size > 10 and
              Complete_PDU(10) <= 10;

   function Construct_Metadata_PDU
     (Header_Information   : Fixed_Header_Record;
      Metadata_Information : Metadata_PDU_Record;
      TLV_Options          : TLV_Options_Array) return Complete_Metadata_PDU_Array
      with
       Pre => Bounded_Strings.Length(Metadata_Information.Source_File_Name) <= Metadata_Information.Source_File_Name.Bound and
              Bounded_Strings.Length(Metadata_Information.Dest_File_Name) <= Metadata_Information.Dest_File_Name.Bound;

   procedure Deconstruct_Metadata_PDU
     (Complete_Metadata    : in Complete_Metadata_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Metadata_Information : out Metadata_PDU_Record;
      TLV_Information      : out TLV_Record;
      Status               : out PDU_Status_Type)
     with
       Global => null,
       Depends =>
         ( Header_Information => Complete_Metadata,
           Metadata_Information => (Metadata_Information, Complete_Metadata),
          (TLV_Information, Status) => (TLV_Information, Complete_Metadata));

   function Construct_Filedata_PDU
     (Header_Information   : Fixed_Header_Record;
      Filedata_Information : Filedata_Record;
      Is_Last              : Boolean) return Complete_Filedata_PDU_Array;

   procedure Deconstruct_Filedata_PDU
     (Complete_Filedata    : in Complete_Filedata_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Filedata_Information : out Filedata_Record;
      Status               : out PDU_Status_Type)
     with
       Global => null,
       Depends => ((Header_Information, Filedata_Information, Status) => Complete_Filedata);

   function Construct_EOF_PDU
     (Header_Information : Fixed_Header_Record;
      EOF_Information    : EOF_Record) return Complete_EOF_PDU_Array;

   procedure Deconstruct_EOF_PDU
     (Complete_EOF       : in Complete_EOF_PDU_Array;
      Header_Information : out Fixed_Header_Record;
      EOF_Information    : out EOF_Record;
      Status             : out PDU_Status_Type)
     with
       Global => null,
       Depends => ((Header_Information, EOF_Information, Status) => Complete_EOF);

   function Construct_Finished_PDU
     (Header_Information   : Fixed_Header_Record;
      Finished_Information : Finished_Record;
      TLV_Options          : TLV_Options_Array;
      Num_File_Requests    : Num_File_Request_Type) return Complete_Finished_PDU_Array;

   procedure Deconstruct_Finished_PDU
     (Complete_Finished    : in Complete_Finished_PDU_Array;
      Header_Information   : out Fixed_Header_Record;
      Finished_Information : out Finished_Record;
      TLV_Options          : out TLV_Options_Array;
      Status               : out PDU_Status_Type)
     with
       Global => null,
       Depends =>
         ((Header_Information, Finished_Information, TLV_Options, Status) => Complete_Finished);

end CubedOS.CFDP.Internals;
