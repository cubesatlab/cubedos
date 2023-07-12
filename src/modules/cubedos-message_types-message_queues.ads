pragma SPARK_Mode(On);

with CubedOS.Lib.Bounded_Queues;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.Message_Types.Message_Queues is new CubedOS.Lib.Bounded_Queues(Message_Record, Msg_Owner);
