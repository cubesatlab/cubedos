pragma SPARK_Mode (On);

package body Test_Module.API is

overriding
   function To_Byte_Array_Impl (Msg : Custom_Message_Type) return Data_Array
   is
   begin
      null
   end;

end Test_Module.API;
