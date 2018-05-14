--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe.ads
-- SUBJECT: Top level package of a CubedOS Publish/Subscribe server.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- This module supports a publish/subscribe message passing paradigm on top of the base CubedOS
-- point-to-point message system. Publish/subscribe is often a useful way to broadcast or
-- multicast messages throughout the system.
--
-- In this system any module can publish or subscribe (or both) to any channel. If a message is
-- published to a channel for which there are no subscribers, the message is lost. Subscribers
-- only receive messages that are published after they subscribe; they do not see old messages.
-- The number of messages queued on a channel is connected to the depth of the CubedOS mailbox
-- used by the publish/subscribe server; Publish_Result messages can back up in that mailbox.
-- However, since the CubedOS mailbox is shared by all channels (and also used for request
-- messages), it's difficult to say how many backlogged messages are allowed for any given
-- channel.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Publish_Subscribe is

   ID : constant Message_Manager.Module_ID_Type := 2;

   Maximum_Channel_Count : constant := 8;
   type Channel_ID_Type is range 1 .. Maximum_Channel_Count;

end CubedOS.Publish_Subscribe;
