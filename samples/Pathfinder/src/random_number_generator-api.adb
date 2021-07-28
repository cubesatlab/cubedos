--------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-api.adb
--  SUBJECT: Body of a package that simplifies use of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with PNumbers;

package body Random_Number_Generator.API is

   function Generate_Number_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record
   is
      --  Create a skeletal message based on the given sender and
      --  priority.  This function knows what module ID will receive
      --  the message and knows what message ID is approriate (there
      --  are different functions for different messages) so it can
      --  fill in those values on its own.
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain, Domain_ID, Sender, ID, Request_ID,
           Message_Type'Pos (Generate_Number_Request), Priority);
   begin
      --  Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Generate_Number_Request_Encode;

   procedure Generate_Number_Request_Decode
     (Message : in     Message_Record; Decode_Status : out Message_Status_Type;
      Value   :    out Positive)
   is
   begin
      --  Decode the given message and return via out parameters (not
      --  shown) the fields.
      Value         := PNumbers.Get_Random_Number;
      Decode_Status := Success;
   end Generate_Number_Request_Decode;

end Random_Number_Generator.API;
