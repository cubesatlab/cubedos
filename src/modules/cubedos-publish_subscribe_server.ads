--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server.ads
-- SUBJECT: Top level package of a CubedOS Publish/Subscribe server.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
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

pragma Warnings (Off, "unit ""Message_Manager"" is not referenced");
with Message_Manager;

package CubedOS.Publish_Subscribe_Server is

end CubedOS.Publish_Subscribe_Server;
