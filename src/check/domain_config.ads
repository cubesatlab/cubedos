
pragma SPARK_Mode (On);

with Name_Resolver;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Domain_Config is

   procedure Send (Msg : in out Msg_Owner)
     with Pre => Msg /= null,
       Post => Msg = null;

end Domain_Config;
