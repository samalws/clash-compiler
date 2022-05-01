module DcFifoTop where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.DcFifo.Explicit

import DcFifo

topEntity
  :: Clock P50
  -> Clock P30
  -> Signal P50 (BitVector 32)
  -> Signal P50 Bool
  -> Signal P30 Bool
  -> XilinxFifo P30 P50 7 32
topEntity wClk rdClk = dcFifo defConfig wClk rdClk resetGen
{-# NOINLINE topEntity #-}

testBench :: Signal P30 Bool
testBench = tb $(listToVecTH $ sampleN 20 $ tbOutput clockGen clockGen)
