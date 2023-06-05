      pragma SPARK_Mode (On);

      with CubedOS.Message_System;         use CubedOS.Message_System;
      with CubedOS.Message_System.Modules; use CubedOS.Message_System.Modules;

      package Test_Module is

         function Public_Mailbox
            return not null access Public_Mailbox_Interface'Class;

      private

         procedure Handle_Message (Msg : Message_Type'Class);

         Message_Types : Message_Discriminant_Array (Positive range 1 .. 2) :=
           (others => 1);

         Module : Module_Type :=
           Make_Module (Message_Types, 10, 2, 1, Handle_Message'Access);

         m_Public_Mailbox : access Public_Mailbox_Interface'Class :=
           Modules.Public_Mailbox (Module);
         Module_Mailbox : access Module_Mailbox_Interface'Class :=
           Modules.Module_Mailbox (Module);

      end Test_Module;
