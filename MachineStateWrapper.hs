module MachineStateWrapper ( MachineState(pc, nzp, regs, priv, memory, labels), 
                             StateM, get, aPut,
                             Delta,
                             Change(SetPC, IncPC, SetNZP, SetReg, SetPriv, SetMem, SetLabel))where

import Prelude
import Control.Monad
import LC4Parser
import Data.Vector (Vector, (!), (//), update, singleton, replicate)
import Data.Map as Map
import Data.Word (Word16)
import Data.Int (Int16)

data MachineState = 
     MachineState { pc :: Word16,
                    nzp :: Word16,
                    regs :: Vector Word16,
                    priv :: Bool,
                    memory :: Vector MemVal,
                    labels :: Map String Word16 }

type Delta = [Change]

data Change = SetPC Word16
            | IncPC
            | SetNZP Word16
            | SetReg Int Word16
            | SetPriv Bool
            | SetMem Int MemVal
            | SetLabel String Word16

newtype MachineStateWrapper = StateM MachineState

newtype StateM s a = S {runState :: s -> (a, s)}

state :: (s -> (a,s)) -> StateM s a
state = S

instance Monad (StateM s) where
          return x   =  state $ \s -> (x,s)
          st >>= f   =  state $ \s -> let (a,s') = runState st s in runState (f a) s'

evalState :: StateM s a -> s -> a
evalState st = fst . runState st 

execState :: StateM s a -> s -> s
execState st = snd . runState st

get :: StateM s s
get = state $ \s -> (s,s)

put :: s -> StateM s ()
put s' = S $ \s -> ( (), s' )

getNZP :: StateM MachineState a -> MachineState -> Word16
getNZP st s = nzp (execState st s)

-- | Atomic put function
aPut :: Delta -> StateM MachineState ()
aPut []     = return ()
aPut (x:xs) = do ms <- get
                 case x of
                     SetPC v      -> put $ ms { pc = v }
                     IncPC        -> put $ ms { pc = (pc ms) + 1 }
                     SetNZP v     -> put $ ms { nzp = v }
                     SetReg r v   -> put $ ms { regs = (regs ms) // [(r, v)] }
                     SetPriv v    -> put $ ms { priv = v }
                     SetMem i v   -> put $ ms { memory = (memory ms) // [(i, v)] }
                     SetLabel l v -> put $ ms { labels = (Map.insert l v (labels ms)) }
                 aPut xs