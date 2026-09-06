-- data ZipList a = ZipList [a]
-- data ZipList a = ZipList { getZipList :: [a] }

newtype ZipList a = ZipList { getZipList :: [a] }

data Profession = Fighter | Archer | Accountant
data Race = Human | Elf | Orc | Goblin
data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair  { getPair :: (a, b) }
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"