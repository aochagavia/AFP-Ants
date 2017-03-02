module Language.Function (
    Instruction(..),
    Function,
    NewState,
    times,
    sequenceT,
    declare,
    defineAs
) where

import Prelude hiding (Left, Right)
import Control.Monad.State

import Instruction hiding (Instruction(..))

data Function = Function [Instruction] deriving Show

data Instruction
    = Sense SenseDir NewState NewState Condition
    | Mark MarkerNumber NewState
    | Unmark MarkerNumber NewState
    | PickUp NewState NewState
    | Drop NewState
    | Turn LeftOrRight NewState
    | Move NewState NewState
    | Flip InvChance NewState NewState
    | Inline Function NewState
    deriving Show

data NewState = Goto InternalLabel | Return deriving Show
data InternalLabel = InternalLabel Int deriving Show

{- Basic template combinators -}

-- Since forever cannot have any end states, it is a fragment. However, it doesn't have a name

times :: Int -> Function -> Function
times i = sequenceT . replicate i

sequenceT :: [Function] -> Function
sequenceT [] = error "cannot sequenceT 0 templates"
sequenceT (t:[]) = undefined
sequenceT (t:ts) = undefined

{- Example templates -}

walkUntilBaseFound, walkUntilFoodFound :: Function
walkUntilBaseFound = walkUntilCond Home
walkUntilFoodFound = walkUntilCond Food

data FnBuilder = FnBuilder Int [(Int, Instruction)]

declare :: State FnBuilder InternalLabel
declare = do (FnBuilder u is) <- get
             put $ FnBuilder (u+1) is
             return $ InternalLabel u

defineAs :: InternalLabel -> Instruction -> State FnBuilder ()
defineAs (InternalLabel d) i = do
    (FnBuilder u is) <- get
    put $ FnBuilder u ((d,i):is)

walkUntilCond :: Condition -> Function
walkUntilCond cond = mkFunction $ do
    start <- declare
    walk <- declare
    start `defineAs` Sense Here Return (Goto walk) cond
    walk `defineAs` Move (Goto start) (Goto walk)

turnAround :: Function
turnAround = times 3 $ Function [ Turn Left Return ]

mkFunction :: State FnBuilder () -> Function
mkFunction = undefined

{- Checks

* If a template with no TEnds is called by a function, show a warning that the template will never return

-}
