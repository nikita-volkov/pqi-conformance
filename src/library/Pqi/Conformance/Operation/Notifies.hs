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

import Pqi (Notify (..))
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "notifies" do
    it "is empty with no pending notifications" \conninfo ->
      differential adapter conninfo \connection ->
        fmap channelAndPayload <$> connection.notifies

    it "delivers a listen/notify round-trip and then drains" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "listen conformance_channel"
        _ <- connection.exec "notify conformance_channel, 'payload-1'"
        notification <- connection.notifies
        pid <- connection.backendPID
        for_ notification \n -> n.bePid `shouldBe` pid
        drained <- fmap channelAndPayload <$> connection.notifies
        pure (fmap channelAndPayload notification, drained)

    it "queues notifications in order" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "listen conformance_channel"
        _ <- connection.exec "notify conformance_channel, 'first'"
        _ <- connection.exec "notify conformance_channel, 'second'"
        first <- connection.notifies
        second <- connection.notifies
        third <- connection.notifies
        pid <- connection.backendPID
        for_ first \n -> n.bePid `shouldBe` pid
        for_ second \n -> n.bePid `shouldBe` pid
        pure (fmap channelAndPayload first, fmap channelAndPayload second, fmap channelAndPayload third)

    it "stops delivery after unlisten" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "listen conformance_channel"
        _ <- connection.exec "unlisten conformance_channel"
        _ <- connection.exec "notify conformance_channel, 'lost'"
        fmap channelAndPayload <$> connection.notifies
  where
    channelAndPayload notification = (notification.relname, notification.extra)
