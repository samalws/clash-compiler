{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           Clash.Driver
import           Clash.Driver.Types

import           Clash.GHC.PartialEval
import           Clash.GHC.Evaluator
import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)

import           Clash.Netlist.Types          (TopEntityT(topId))

import           Criterion.Main

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (NFData(..), rwhnf)
import           Data.List                    (isPrefixOf, partition)
import           System.Environment           (getArgs, withArgs)

import BenchmarkCommon

-- | Run benchmark the normalization process
--
-- You can provide you own test cases as commandline arguments.
--
-- All arguments from the first one starting with a '-' are given to criterion.
-- All argument before that are interpreted as test cases.
main :: IO ()
main = do
  args <- getArgs
  let (idirs0,rest)         = partition ((== "-i") . take 2) args
      idirs1                = ".":map (drop 2) idirs0
      (fileArgs,optionArgs) = break (isPrefixOf "-") rest
      tests | null fileArgs = defaultTests
            | otherwise     = fileArgs

  withArgs optionArgs (defaultMain $ fmap (benchFile idirs1) tests)

benchFile :: [FilePath] -> FilePath -> Benchmark
benchFile idirs src =
  env (setupEnv idirs src) $
    \ ~(clashEnv, clashDesign, supplyN) -> do
      bench ("normalization of " ++ src)
            (nfIO
              (normalizeEntity
                 clashEnv
                 (designBindings clashDesign)
                 (ghcTypeToHWType (opt_intWidth (envOpts clashEnv)))
                 ghcEvaluator
                 evaluator
                 (fmap topId (designEntities clashDesign))
                 supplyN
                 (topId (head (designEntities clashDesign)))))

setupEnv
  :: [FilePath]
  -> FilePath
  -> IO (ClashEnv, ClashDesign, Supply.Supply)
setupEnv idirs src = do
  (clashEnv, clashDesign) <- runInputStage idirs src
  supplyN <- Supply.newSupply
  return (clashEnv, clashDesign ,supplyN)

instance NFData Supply.Supply where
  rnf = rwhnf
