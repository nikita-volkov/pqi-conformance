-- | Coverage for 'Pqi.notifies': @LISTEN@\/@NOTIFY@ delivery, queueing,
-- and that @UNLISTEN@ stops delivery.
--
-- The backend PID carried by a notification is connection-specific - the
-- candidate and the reference are distinct backends - so 'notifyBePid' is omitted
-- from the cross-adapter comparison. Each scenario that receives a
-- notification instead asserts independently (per adapter) that
-- @Pqi.notifyBePid notification == backendPID connection@, verifying that the PID field
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
        fmap channelAndPayload <$> Pqi.notifies connection

    it "delivers a listen/notify round-trip and then drains" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "listen conformance_channel"
        _ <- Pqi.exec connection "notify conformance_channel, 'payload-1'"
        notification <- Pqi.notifies connection
        pid <- Pqi.backendPID connection
        for_ notification \n -> Pqi.notifyBePid n `shouldBe` pid
        drained <- fmap channelAndPayload <$> Pqi.notifies connection
        pure (fmap channelAndPayload notification, drained)

    it "queues notifications in order" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "listen conformance_channel"
        _ <- Pqi.exec connection "notify conformance_channel, 'first'"
        _ <- Pqi.exec connection "notify conformance_channel, 'second'"
        first <- Pqi.notifies connection
        second <- Pqi.notifies connection
        third <- Pqi.notifies connection
        pid <- Pqi.backendPID connection
        for_ first \n -> Pqi.notifyBePid n `shouldBe` pid
        for_ second \n -> Pqi.notifyBePid n `shouldBe` pid
        pure (fmap channelAndPayload first, fmap channelAndPayload second, fmap channelAndPayload third)

    it "stops delivery after unlisten" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "listen conformance_channel"
        _ <- Pqi.exec connection "unlisten conformance_channel"
        _ <- Pqi.exec connection "notify conformance_channel, 'lost'"
        fmap channelAndPayload <$> Pqi.notifies connection
  where
    channelAndPayload notification = (Pqi.notifyRelname notification, Pqi.notifyExtra notification)
