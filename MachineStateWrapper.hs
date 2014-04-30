module MachineStateWrapper ( MachineState(pc, nzp, regs, priv, memory, labels), 
                             StateM, get, aPut, execState,
                             Delta, simpMachine, emptyMachine,
                             Change(SetPC, IncPC, SetNZP, SetReg, SetPriv, SetMem, SetLabel))where

import Prelude
import Control.Monad
import LC4Parser
import Data.Vector (Vector, (!), (//), update, singleton, replicate, fromList, filter)
import Data.Map as Map
import Data.Word (Word16)
import Data.Int (Int16)
import DataModel

data MachineState = 
     MachineState { pc :: Word16,
                    nzp :: (Bool, Bool, Bool),
                    regs :: Vector Word16,
                    priv :: Bool,
                    memory :: Vector MemVal,
                    labels :: Map String Word16 }
                    deriving(Eq)

instance Show MachineState where
    show (MachineState pc nzp regs priv memory labels) = 
        "\npc: " ++ show pc ++
        "\npriv: " ++ show priv ++
        "\nnzp: " ++ (if n then "1" else "0") ++
        (if z then "1" else "0") ++
        (if p then "1" else "0") ++
        "\nregs: " ++ show regs ++
        "\nlabels: " ++ show labels ++
        "\nmemory: " ++ printPopulatedMemory memory ++
        "\n--------------------------------------"
        where (n, z, p) = nzp

printPopulatedMemory :: Vector MemVal -> String
printPopulatedMemory mem = let filt = Data.Vector.filter (\x -> if x == DataVal 0 then False else True) mem in
                           show filt

simpMachine :: MachineState
simpMachine = MachineState {
                 pc = 0,
                 nzp = (False, False, False),
                 regs = Data.Vector.fromList [0..7],
                 priv = True,
                 memory = Data.Vector.replicate 10 (DataVal 0),
                 labels = Map.fromList [("lab", 10)]
                  }

emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = (False, False, False),
                 regs = Data.Vector.replicate 7 0,
                 priv = False,
                 memory = Data.Vector.replicate 10 (DataVal 0),
                 labels = Map.empty
                }

type Delta = [Change]

data Change = SetPC Word16
            | IncPC
            | SetNZP (Bool, Bool, Bool)
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

-- | Atomic put function
aPut :: Delta -> StateM MachineState ()
aPut []     = return ()
aPut (x:xs) = do ms <- get
                 case x of
                     SetPC v         -> put $ ms { pc = v }
                     IncPC           -> put $ ms { pc = (pc ms) + 1 }
                     SetNZP v        -> put $ ms { nzp = v }
                     SetReg r v      -> put $ ms { regs = (regs ms) // [(r, v)] }
                     SetPriv v       -> put $ ms { priv = v }
                     SetMem i v      -> put $ ms { memory = (memory ms) // [(i, v)] }
                     SetLabel l v    -> put $ ms { labels = (Map.insert l v (labels ms)) }
                 aPut xs             
