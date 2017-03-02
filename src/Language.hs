module Language where

import Prelude hiding (Left, Right)
import Instruction hiding (Instruction(..))

newtype Name = Name String deriving Show

-- Note: templates are inlined into instructions.
-- Therefore, we don't need to keep a list of them
-- On the other hand, fragments can link to each other,
-- so we use names to avoid cycles in Haskell objects
data Program = Program {
    entryPoint :: Fragment,
    fragments :: [Fragment]
} deriving Show

newtype AnonFragment = AnonFragment [FragmentInstruction] deriving Show
data Fragment = Fragment Name [FragmentInstruction] deriving Show
data Template = Template [TemplateInstruction] deriving Show

data GenInstruction newState
    = GotoFragment Name
    | InlineTemplate Template newState
    | Sense SenseDir newState newState Condition
    | Mark MarkerNumber newState
    | Unmark MarkerNumber newState
    | PickUp newState newState
    | Drop newState
    | Turn LeftOrRight newState
    | Move newState newState
    | Flip InvChance newState newState
    deriving Show

type FragmentInstruction = GenInstruction FNewState
type TemplateInstruction = GenInstruction TNewState

-- Note: fragments don't have end states
data FNewState = FInternalGoto Int deriving Show
data TNewState = TInternalGoto Int
               | TEnd
               deriving Show

{- Basic template combinators -}

-- Since forever cannot have any end states, it is a fragment. However, it doesn't have a name
nameFragment :: Name -> AnonFragment -> Fragment
nameFragment name (AnonFragment instrs) = Fragment name instrs

forever :: Template -> AnonFragment
forever template = AnonFragment [ InlineTemplate template (FInternalGoto 0) ]

times :: Int -> Template -> Template
times i t = sequenceT $ map (const t) [1..i]

sequenceT :: [Template] -> Template
sequenceT [] = error "cannot sequenceT 0 templates"
sequenceT (t:[]) = undefined
sequenceT (t:ts) = undefined

{- Example templates -}

walkUntilBaseFound, walkUntilFoodFound :: Template
walkUntilBaseFound = walkUntilCond Home
walkUntilFoodFound = walkUntilCond Food

walkUntilCond :: Condition -> Template
walkUntilCond cond = Template instructions
    where
    instructions = [ Sense Here TEnd (TInternalGoto 1) cond
                   , Move (TInternalGoto 0) (TInternalGoto 1)
                   ]

turnAround :: Template
turnAround = times 3 $ Template [ Turn Left TEnd ]

{- Example program -}

program :: Program
program = Program main []

main :: Fragment
main = nameFragment (Name "main") $ forever $ sequenceT instructions
    where
    instructions = [ walkUntilFoodFound
                   , Template [ PickUp TEnd TEnd ]
                   , turnAround
                   , walkUntilBaseFound
                   , Template [ Drop TEnd ]
                   ]

{- Checks

* All internal gotos are correct
* If a template with no TEnds is called by a function, show a warning that the template will never return


-}
