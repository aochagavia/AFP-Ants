import qualified Worlds

main :: IO ()
main = definedWorlds

definedWorlds :: IO ()
definedWorlds = sequence_ worlds
    where worlds = [Worlds.sample0]
