{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Clash.Cores.Floating.Xilinx.Internal
  ( addFloat#
  , conditionFloat
  , conditionFloatF
  , divFloat#
  , expFloat#
  , FloatingArchOpt(..)
  , FloatingConfig(..)
  , FloatingDspUsage(..)
  , mulFloat#
  , subFloat#
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Floating.Xilinx.Annotations

-- | Customize Xilinx floating point IP.
--
-- These customizations influence how the IP will be synthesized. They do not
-- affect behavior.
data FloatingConfig = FloatingConfig
  { floatingArchOpt :: !FloatingArchOpt
    -- ^ Architecture optimizations
  , floatingDspUsage :: !FloatingDspUsage
    -- ^ DSP slice usage
  , floatingBMemUsage :: !Bool
    -- ^ Use block memory for exponential function
  } deriving (Eq, Show)

-- | Architecture optimizations.
--
-- For those operations that support different architecture optimizations.
data FloatingArchOpt
  = SpeedArch
    -- ^ Speed-optimized architecture
  | LatencyArch
    -- ^ Low latency architecture
  deriving (Eq, Show)

-- | DSP slice usage.
--
-- For those operations that can use DSP slices. Not all such operations support
-- all variants.
data FloatingDspUsage
  = NoDspUsage
    -- ^ Do not use any DSP slices.
  | MediumDspUsage
  | FullDspUsage
  | MaxDspUsage
  deriving (Eq, Show)

-- | NaN as generated by Xilinx.
--
-- Quiet NaN, no payload, positive.
xilinxNaN :: Float
xilinxNaN = unpack 0x7FC00000

-- | Perform the same conditioning on a Float as the Xilinx IP does.
--
-- All NaNs are mapped to 'xilinxNaN', all subnormal numbers to zero.
--
-- Note that there are a few exceptions where the Xilinx IP does not condition
-- Float values. Those functions are documented as such.
conditionFloat
  :: Float
  -> Float
conditionFloat x
  | isNaN x          = xilinxNaN
  | isDenormalized x = if x > 0 then 0 else -0
  | otherwise        = x

conditionFloatF
  :: Functor f
  => f Float
  -> f Float
conditionFloatF = fmap conditionFloat

addFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addFloat# !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x + y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of addFloat undefined"
{-# NOINLINE addFloat# #-}
{-# ANN addFloat#
      (vhdlBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.addFloat#"
         "addFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.addFloatTclTF"
      ) #-}
{-# ANN addFloat#
      (veriBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.addFloat#"
         "addFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.addFloatTclTF"
      ) #-}

subFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subFloat# !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x - y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of subFloat undefined"
{-# NOINLINE subFloat# #-}
{-# ANN subFloat#
      (vhdlBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.subFloat#"
         "subFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.subFloatTclTF"
      ) #-}
{-# ANN subFloat#
      (veriBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.subFloat#"
         "subFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.subFloatTclTF"
      ) #-}

mulFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulFloat# !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x * y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of mulFloat undefined"
{-# NOINLINE mulFloat# #-}
{-# ANN mulFloat#
      (vhdlBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.mulFloat#"
         "mulFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.mulFloatTclTF"
      ) #-}
{-# ANN mulFloat#
      (veriBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.mulFloat#"
         "mulFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.mulFloatTclTF"
      ) #-}

divFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divFloat# !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x / y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of divFloat undefined"
{-# NOINLINE divFloat# #-}
{-# ANN divFloat#
      (vhdlBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.divFloat#"
         "divFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.divFloatTclTF"
      ) #-}
{-# ANN divFloat#
      (veriBinaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.divFloat#"
         "divFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.divFloatTclTF"
      ) #-}

expFloat#
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => FloatingConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
expFloat# !_ clk en (conditionFloatF -> x) =
  delayI und en clk . conditionFloatF $ exp <$> x
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of expFloat undefined"
{-# NOINLINE expFloat# #-}
{-# ANN expFloat#
      (vhdlUnaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.expFloat#"
         "expFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.expFloatTclTF"
      ) #-}
{-# ANN expFloat#
      (veriUnaryPrim
         "Clash.Cores.Floating.Xilinx.Internal.expFloat#"
         "expFloat"
         "Clash.Cores.Floating.Xilinx.BlackBoxes.expFloatTclTF"
      ) #-}
