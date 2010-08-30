-- Based on http://gafferongames.wordpress.com/networking-for-game-programmers/reliability-and-flow-control/
-- The Acknowledgement builds on Connection to provide sequence numbers and
-- acks as a mean to detect lost packets. A packet is considered lost when no
-- ack has been received for it during, say, 1 second.
module Network.Game.Acknowledgement where

import Data.Word (Word16)

-- sequence number with wrapping

newtype Sequence = Sequence Word16
  deriving (Eq, Show)

zeroSequence :: Sequence
zeroSequence = Sequence 0

succSequence :: Sequence -> Sequence
succSequence (Sequence i) = Sequence (i + 1)

-- s1 > s2
greaterSequence :: Sequence -> Sequence -> Bool
greaterSequence (Sequence s1) (Sequence s2) =
  (s1 > s2 && s1 - s2 <= m) ||
  (s2 > s1 && s2 - s1 > m)
  where
  m :: Word16
  m = 2 ^ (15 :: Word16)

