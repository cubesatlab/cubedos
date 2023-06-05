pragma SPARK_Mode (On);

with CubedOS.Message_System; use CubedOS.Message_System;

package Test_Module.API is

   Mailbox : constant access Public_Mailbox_Interface := Public_Mailbox;

   type Custom_Message_Type is new Message_Type with private;

private

   type Custom_Message_Type is new Message_Type with
      record
         Custom_Data : Positive;
      end record;


   overriding
   function From_Byte_Array (Data : Data_Array) return Custom_Message_Type;

   overriding
   function To_Byte_Array_Impl (Msg : Custom_Message_Type) return Data_Array;



end Test_Module.API;
