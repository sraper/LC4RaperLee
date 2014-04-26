module MachineStateWrapper.Monad (MachineStateWrapper()) where

import Prelude
import Control.Monad
import Control.Monad.State
import Main

newtype MachineStateWrapper a = MSW {run :: MachineState a}

-- return :: a -> MachineStateWrapper a
return x = MSW

-- (>>=) :: MachineStateWrapper a -> (a -> MachineStateWrapper b) -> MachineStateWrapper b
ms >>= f = MSW $ f (run ms)