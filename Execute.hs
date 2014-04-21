{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where
import Data.Array.IO
import Data.Map as Map
import Control.Monad.State
import Main
import Numeric
import Data.Word
import Data.Bits
import ParserCombinators

hexToDec :: String -> Int
hexToDec = fst . head . readHex

getRegVal :: MachineState -> Int -> Int
getRegVal ms r = let registers = regs ms in
                 Map.findWithDefault 0 r registers

setRegVal :: Int -> Int -> State MachineState ()
setRegVal r v = do ms <- get
                   put $ ms { regs = (Map.insert r v (regs ms)) }

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
execute (Unary JSRR (R rs)) = do ms <- get
                                 let rsv = getRegVal ms rs
                                     pcv = pc ms
                                 setRegVal 7 $ pcv + 1
                                 put $ ms { pc = rsv }
execute (Unary JMPR (R rs)) = do ms <- get
                                 let rsv = getRegVal ms rs
                                 put $ ms { pc = rsv }
execute (Unary TRAP (IMM imm)) = do ms <- get
                                    let pcv = pc ms
                                        newpcv = imm + (hexToDec "8000")
                                    setRegVal 7 $ pcv + 1
                                    put $ ms { pc = newpcv, priv = True}
execute (Binary BRn (IMM i) (LABEL l))
                                = do ms <- get
                                     return ()
execute (Binary CMP (R rs) (R rt))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rtv = getRegVal ms rt
                                     return ()
execute (Binary NOT (R r1) (R r2))
                                = do ms <- get
                                     let r1v = getRegVal ms r1
                                         r2v = getRegVal ms r2
                                     return ()
-- HOW DO YOU BITS
execute (Unary JMP l)
                                = do ms <- get
                                     let pcv = pc ms
                                         add = case l of
                                            LABEL l -> Map.findWithDefault 0 l $ labels ms
                                            IMM i   -> i
                                            -- ERROR HERE
                                     put $ ms { pc = pcv + 1 + add }
execute (Binary CONST (R rd) (IMM i))
                                = do ms <- get
                                     setRegVal rd i
execute (Binary LEA (R r1) (LABEL l))
                                = do ms <- get
                                     let addr = Map.findWithDefault 0 l $ labels ms
                                     setRegVal r1 addr
execute (Binary LC (R r1) (LABEL l))
                                = do ms <- get
                                     let addr = Map.findWithDefault 0 l $ labels ms
                                         val = Map.findWithDefault (DataVal 0) addr $ memory ms
                                     case val of
                                        DataVal d -> setRegVal r1 d
                                        _ -> return () -- NEED ERROR
execute (Ternary ADD (R rd) (R rs) (R rt))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv + rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary MUL (R rd) (R rs) (R rt))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv * rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary SUB (R rd) (R rs) (R rt)) 
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv - rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary DIV (R rd) (R rs) (R rt)) 
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv `div` rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary ADD (R rd) (R rs) (IMM imm))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         pcv = pc ms
                                     setRegVal rdv $ rsv + imm
                                     put $ ms { pc = pcv + 1 }
execute (Ternary AND (R rd) (R rs) (R rt))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv .&. rtv
                                     put $ ms { pc = pcv + 1 }
execute (Binary NOT (R rd) (R rs)) -- i don't think this works complement 8 = -9???
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         pcv = pc ms
                                     setRegVal rdv $ complement rsv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary OR (R rd) (R rs) (R rt)) 
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv .|. rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary XOR (R rd) (R rs) (R rt)) 
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         rtv = getRegVal ms rt
                                         pcv = pc ms
                                     setRegVal rdv $ rsv `xor` rtv
                                     put $ ms { pc = pcv + 1 }
execute (Ternary AND (R rd) (R rs) (IMM i)) 
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         rdv = getRegVal ms rd
                                         pcv = pc ms
                                     setRegVal rdv $ rsv .&. i
                                     put $ ms { pc = pcv + 1 }
execute (Ternary LDR (R rd) (R rs) (IMM i))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         addr = rsv + i
                                         val = Map.findWithDefault (DataVal 0) addr $ memory ms
                                     case val of
                                        DataVal d -> setRegVal rd d
                                        _ -> return () -- NEED ERROR
execute (Ternary STR (R rd) (R rs) (IMM i))
                                = do ms <- get
                                     let rsv = getRegVal ms rs
                                         addr = rsv + i
                                         val = getRegVal ms rd
                                     put $ ms { memory = Map.insert addr (DataVal val) (memory ms) }

execute _ = do return ()

execS :: Insn -> MachineState -> MachineState
execS insn = execState (execute insn)

populateMemory :: LC4 -> MachineState -> MachineState
populateMemory [] ms = ms
populateMemory xs ms = Prelude.foldr pop ms xs where
    pop i acc = case i of
        (Comment s) -> acc
        _           -> acc { memory = Map.insert (pc acc) i (memory acc), pc = (pc acc) + 1 }

runLC4 :: LC4 -> IO ()
runLC4 insns = let ms = execProg $ populateMemory insns emptyMachine in
               printMS ms

execProg :: MachineState -> MachineState
execProg ms = let insn = Map.findWithDefault (Single NOP) (pc ms) (memory ms) in
              case insn of
                (Single NOP) -> ms -- MIGHT NOT BE NOP THAT WE WANT AS FINAL
                _ -> execProg $ execState (execute insn) ms

main :: IO ()
main = do s <- parseFromFile lc4P "sample.asm"
          case s of
            (Left _) -> print "f up"
            (Right x) -> runLC4 x
          return ()

testPopulateMemory :: IO ()
testPopulateMemory = do s <- parseFromFile lc4P "sample.asm"
                        case s of
                          (Left _) -> print "f up"
                          (Right x) -> printMS $ populateMemory x emptyMachine
                        return ()

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
                 pc = hexToDec "8200",
                 nzp = 0,
                 regs = Map.empty,
--                 regs = newArray (0, 7) 0,
                 priv = True,
                 memory = Map.empty,
--                 memory = newArray (0, 7) 0,
                 labels = Map.empty
                  }