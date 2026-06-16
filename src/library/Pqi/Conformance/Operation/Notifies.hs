-- | Coverage for 'Pqi.notifies': @LISTEN@\/@NOTIFY@ delivery, queueing,
-- and that @UNLISTEN@ stops delivery.
--
-- The backend PID carried by a notification is connection-specific — the
-- candidate and the reference are distinct backends — so 'bePid' is omitted
-- from the cross-adapter comparison. Each scenario that receives a
-- notification instead asserts independently (per adapter) that
-- @notification.bePid == backendPID connection@, verifying that the PID field
-- is correctly populated without comparing it across adapters.
module Pqi.Conformance.Operation.Notifies
  ( spec,
  )
where

import Pqi (IsConnection (..), Notify (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "notifies" do
    it "is empty with no pending notifications" \conninfo ->
      differential proxy conninfo \connection ->
        fmap channelAndPayload <$> notifies connection

    it "delivers a listen/notify round-trip and then drains" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "listen conformance_channel"
        _ <- exec connection "notify conformance_channel, 'payload-1'"
        notification <- notifies connection
        pid <- backendPID connection
        for_ notification \n -> n.bePid `shouldBe` pid
        drained <- fmap channelAndPayload <$> notifies connection
        pure (fmap channelAndPayload notification, drained)

    it "queues notifications in order" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "listen conformance_channel"
        _ <- exec connection "notify conformance_channel, 'first'"
        _ <- exec connection "notify conformance_channel, 'second'"
        first <- notifies connection
        second <- notifies connection
        third <- notifies connection
        pid <- backendPID connection
        for_ first \n -> n.bePid `shouldBe` pid
        for_ second \n -> n.bePid `shouldBe` pid
        pure (fmap channelAndPayload first, fmap channelAndPayload second, fmap channelAndPayload third)

    it "stops delivery after unlisten" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "listen conformance_channel"
        _ <- exec connection "unlisten conformance_channel"
        _ <- exec connection "notify conformance_channel, 'lost'"
        fmap channelAndPayload <$> notifies connection
  where
    channelAndPayload notification = (notification.relname, notification.extra)
