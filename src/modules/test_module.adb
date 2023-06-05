      pragma SPARK_Mode (On);

      with Test_Module.API;

      package body Test_Module is

         function Public_Mailbox
            return not null access Public_Mailbox_Interface'Class
         is
         begin
            return m_Public_Mailbox;
         end Public_Mailbox;

         procedure Handle_Message (Msg : Message_Type'Class) is
         begin
            null;
         end Handle_Message;

      end Test_Module;
