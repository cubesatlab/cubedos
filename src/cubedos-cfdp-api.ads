--------------------------------------------------------------------------------
-- FILE   : cubedos-cfdp-api.ads
-- SUBJECT: Specification of a package that declares the interface to the CFDP module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

use Message_Manager;

package CubedOS.CFDP.API is

   -- We have a maximum filename length of 16 characters
   Max_Name_Length : constant := 16;

   -- This implementation uses 16 bit entity IDs.
   type Entity_ID is mod 2**16;

   -- This implementation uses 16 bit transaction IDs.
   type Transaction_ID is mod 2**16;

   -- Various request messages sent by the CFDP user to the CFDP entity.
   type Message_Type is
     (Put_Request,
      Cancel_Request,
      Suspend_Request,
      Resume_Request,
      Report_Request);

   -- Various indication messages sent by the CFDP entity to the CFDP user.
   type Indication_Type is
     (Transaction_Indication,
      Metadata_Received_Indication,
      File_Segment_Received_Indication,
      Suspended_Indication,
      Resumed_Indication,
      EOF_Sent_Indication,
      Transaction_Finished_Indication,
      Transfer_Consigned_Indication,
      Report_Indication,
      Fault_Indication,
      Abandoned_Indication,
      EOF_Received_Indication);

   type PDU_Category is
     (Metadata_PDU, Filedata_PDU, EOF_PDU, Finished_PDU, Error);

   -- This is only an approximation.
   function Put_Request_Message
     (Sender_Domain : Domain_ID_Type;
      Sender      : Module_ID_Type;
      Destination : Entity_ID;
      Source_File : String;
      Destination_File : String) return Message_Record
     with
       Global => null,
       Pre =>
         Source_File'Length > 0 and
         Destination_File'Length > 0 and
         Source_File'Last <= XDR_Size_Type'Last/2 and  --might want to do something similar to this
         Destination_File'Last <= XDR_Size_Type'Last/2; --to get provers passing??
         --(XDR.Length_With_Padding(Source_File'Length) <=
                  --XDR_Size_Type'Last - 16 - XDR.Length_With_Padding(Destination_File'Length));

end CubedOS.CFDP.API;
