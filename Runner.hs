{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Runner where
import Data.Array.IO
import Data.Map as Map
import Control.Monad.State
import Main
import Execute
import Numeric
import Data.Word
import Data.Bits
import ParserCombinators
import Debug.Trace

execS :: Insn -> MachineState -> MachineState
execS insn = execState (execute insn)

populateMemory :: LC4 -> MachineState -> MachineState
populateMemory [] ms = ms
populateMemory xs ms = Prelude.foldl pop (ms { pc = 0 }) xs where
    pop acc i = case i of
        (Comment s) -> acc
        _           -> acc { memory = Map.insert (pc acc) i (memory acc), pc = (pc acc) + 1 }

runLC4 :: LC4 -> IO ()
runLC4 insns = let ms = populateMemory insns emptyMachine
                   ms' = execProg (ms { pc = 0 }) in
               printMS ms'

execProg :: MachineState -> MachineState
execProg ms = let insn = Map.findWithDefault (Single NOP) (pc ms) (memory ms) in
              case insn of
                (Single NOP) -> ms -- MIGHT NOT BE NOP THAT WE WANT AS FINAL
                _ -> execProg $ execState (execute insn) ms

main :: IO ()
main = do s <- parseFromFile lc4P "sample.asm"
          case s of
            (Left _) -> print "f up"
            (Right x) -> runLC4 $ reverse x
          return ()

testPopulateMemory :: IO ()
testPopulateMemory = do s <- parseFromFile lc4P "sample.asm"
                        case s of
                          (Left _) -> print "f up"
                          (Right x) -> printMS $ populateMemory x emptyMachine
                        return ()

testExecuteJSRR :: IO ()
testExecuteJSRR = let a = Unary JSRR (R 5)
                  in runOneInsn a

runOneInsn :: Insn -> IO ()
runOneInsn insn = let ms = execS insn emptyMachine in
           printMS ms

printMS :: MachineState -> IO ()
printMS ms = do putStr "pc: "
                print $ pc ms
                putStr "priv: "
                print $ priv ms
                putStr "nzp: "
                print $ nzp ms
                putStr "regs: "
                print $ regs ms
                putStr "labels: "
                print $ labels ms
                putStr "memory: "
                print $ memory ms

emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = 0,
                 regs = Map.fromList [(x,x) | x <- [0..7]],
--                 regs = newArray (0, 7) 0,
                 priv = True,
                 memory = Map.empty,
--                 memory = newArray (0, 7) 0,
                 labels = Map.empty
                  }