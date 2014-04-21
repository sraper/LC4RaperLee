{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where
import Data.Array.IO
import Data.Map as Map
import Control.Monad.State
import Main
import Numeric
import Data.Word
import Data.Bits
import Data.Array.IO

hexToDec :: String -> Int
hexToDec = fst . head . readHex

getRegVal :: MachineState -> Int -> Int
getRegVal ms r = let registers = regs ms in
                 do a <- readArray registers r
                    return a 

--setRegVal :: Int -> Int -> State MachineState ()
--setRegVal r v = do ms <- get
--                   put $ ms { regs = (Map.insert r v (regs ms)) }

incPC :: State MachineState ()
incPC = do ms <- get
           put $ ms { pc = (pc ms) + 1 }

matchNZP :: MachineState -> Int -> Bool
matchNZP ms v = v == (nzp ms)

wordToInt :: Word -> Int
wordToInt = fromIntegral
                

execute :: Insn -> State MachineState ()
execute (Single NOP) = incPC
execute (Single RTI) = do ms <- get
                          put $ ms { priv = False }
execute _ = do return ()

execS :: Insn -> MachineState -> MachineState
execS insn = execState (execute insn)

run :: Insn -> IO ()
run insn = let ms = execS insn emptyMachine in
           do putStrLn "Output priv:"
              print $ priv ms

emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = 0,
                 regs = Map.empty,
--                 regs = newArray (0, 7) 0,
                 priv = True,
                 memory = Map.empty,
--                 memory = newArray (0, 7) 0,
                 labels = Map.empty
                  }