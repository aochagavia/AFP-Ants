{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module MultilineStrings (multiline) where

import Simulator.ReadWorld

-- The code below is heavily based on http://harry.garrood.me/blog/qq-literals/
import Language.Haskell.TH (varE, Name)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

multiline :: QuasiQuoter
multiline = QuasiQuoter {..}
  where
  quoteExp str = [| str |]
  quotePat  = unsupported "pattern"
  quoteType = unsupported "type"
  quoteDec  = unsupported "declaration"

  unsupported context = fail $
    "Unsupported operation: this QuasiQuoter can not be used in a " ++ context ++ " context."
