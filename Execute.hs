{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where
import Data.Array.IO
import Data.Map as Map
import Control.Monad.State
import Main
import Numeric

hexToDec :: String -> Int
hexToDec = fst . head . readHex

binToDec :: Int -> Int
binToDec =

getRegVal :: MachineState -> Int -> Int
getRegVal ms r = let registers = regs ms in
                 Map.findWithDefault 0 r registers

matchNZP :: MachineState -> Int -> Bool
matchNZP ms v = v == (nzp ms)
                

execute :: Insn -> State MachineState ()
execute (Single NOP) = do return ()
execute (Single RTI) = do ms <- get
                          put $ ms { priv = False }
execute (Unary JSRR (R rs)) = do ms <- get
                                 let rsv = getRegVal ms rs
                                     pcv = pc ms
                                 put $ ms { pc = rsv, regs = (Map.insert 7 (pcv + 1) (regs ms)) }
execute (Unary JMPR (R rs)) = do ms <- get
                                 let rsv = getRegVal ms rs
                                 put $ ms { pc = rsv }
execute (Unary TRAP (IMM imm)) = do ms <- get
                                    let pcv = pc ms
                                        newpcv = imm + (hexToDec "8000")
                                    put $ ms { regs = (Map.insert 7 (pcv + 1) (regs ms)), pc = newpcv, priv = True}
execute (Binary BRn (IMM i) (LABEL l))
                                = do ms <- get

                                        
                                    
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