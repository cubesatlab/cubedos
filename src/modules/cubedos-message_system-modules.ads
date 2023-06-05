pragma SPARK_Mode (On);

package CubedOS.Message_System.Modules is

   type Module_Type is private;

   type Message_Discriminant_Array is array (Positive range <>) of Message_Discriminant_Type;

   -- Creates a module, registers it with the Message_Info_Type system.
   function Make_Module (
                                    Message_Types : Message_Discriminant_Array;
                                    Message_Queue_Size : Positive;
                                    Domain_ID : Domain_ID_Type;
                         Module_ID : Module_ID_Type;
                         Read_Message_Proc : not null access procedure (Msg : in Message_Type'Class)
                                    ) return Module_Type;

   -- Gets the public view of the module's mailbox.
   -- This should be made visible to other modules.
   function Public_Mailbox (Comp : Module_Type) return access Public_Mailbox_Interface'Class;

   -- Gets the private view of the module's mailbox.
   -- This should only be accessible inside your module.
   function Module_Mailbox (Comp : Module_Type) return access Module_Mailbox_Interface'Class;

private

   type Module_Type is
      record
         Mail : not null access Module_Mailbox_Interface;
      end record;


end CubedOS.Message_System.Modules;
