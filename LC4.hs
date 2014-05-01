{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module LC4 where

import Prelude
import Data.Vector (Vector, (!), (//), update, singleton, replicate, fromList, filter)
import Data.Map as Map
import Data.Word (Word16)
--import Data.Int (Int16)
import DataModel
import Control.Monad.State
import Control.Monad.Error

type MLC4 = ErrorT LC4Error (State MachineState)

data MachineState = 
     MachineState { pc :: Word16,
                    nzp :: (Bool, Bool, Bool),
                    regs :: Vector Word16,
                    priv :: Bool,
                    mem :: Vector MemVal,
                    labels :: Map String Word16 }
                    deriving (Show, Eq)

data LC4Error = SomeError String
              | NoSuchInstruction String
              | IllegalMemAccess
              | IllegalInsnAccess
              deriving (Eq)

instance Error LC4Error where
  noMsg = SomeError "Unknown error"
  strMsg s = SomeError s

instance Show LC4Error where
  show (NoSuchInstruction _) = "There was no such instruction!"
  show (IllegalMemAccess) = "Cannot access memory"
  show (IllegalInsnAccess) = "Cannot access Insn"
  show (SomeError msg) = msg

type Delta = [Change]

data Change = SetPC Word16
            | IncPC
            | SetNZP (Bool, Bool, Bool)
            | SetReg Int Word16
            | SetPriv Bool
            | SetMem Int MemVal
            | SetLabel String Word16

loop :: MLC4 ()
loop = do i <- fetch
          d <- decode i
          _ <- modify (apply d)
          loop

fetch :: (MonadState MachineState m) => m Insn
fetch = undefined

decode :: (MonadError LC4Error m) => Insn -> m Delta
decode = undefined

apply :: Delta -> MachineState -> MachineState
apply [] ms     = ms
apply (x:xs) ms = 
      case x of
       (SetPC v)      -> apply xs (ms { pc = v })
       (IncPC)        -> apply xs (ms { pc = (pc ms) + 1 })
       (SetNZP v)     -> apply xs (ms { nzp = v })
       (SetReg r v)   -> apply xs (ms { regs = (regs ms) // [(r, v)] })
       (SetPriv v)    -> apply xs (ms { priv = v })
       (SetMem i v)   -> apply xs (ms { mem = (mem ms) // [(i, v)] })
       (SetLabel l v) -> apply xs (ms { labels = (Map.insert l v (labels ms)) })