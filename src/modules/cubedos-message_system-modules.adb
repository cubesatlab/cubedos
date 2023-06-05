pragma SPARK_Mode (On);

package body CubedOS.Message_System.Modules is

   function Make_Module
     (Message_Types : Message_Discriminant_Array;
      Message_Queue_Size : Positive; Domain_ID : Domain_ID_Type;
      Module_ID : Module_ID_Type;
      Read_Message_Proc : not null access procedure
        (Msg : in Message_Type'Class))
      return access Module_Type
   is
      Module : Module_Type;
   begin
      Module.Mail := new Mailbox_Type (Message_Queue_Size);

      return Module;
   end Make_Module;

end CubedOS.Message_System.Modules;
