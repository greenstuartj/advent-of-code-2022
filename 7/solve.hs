import qualified Data.Set as S

type File = (String, Integer)

data System = Dir String (S.Set System) (S.Set File)
            deriving (Show, Eq)

instance Ord System where
  compare (Dir nameA _ _)  (Dir nameB _ _)  = compare nameA nameB

data Instruction = MkDir String
                 | CdUp
                 | Touch String Integer
                 | NoOp
                 deriving Show

parseLine :: String -> Instruction
parseLine line = aux $ words line
  where aux ["$", "cd", ".."] = CdUp
        aux ["$", "cd", name] = MkDir name
        aux ["$", "ls"]       = NoOp
        aux ["dir", name]     = NoOp
        aux [size, name]      = Touch name (read size :: Integer)
        aux _                 = NoOp

build :: [Instruction] -> System -> (System, [Instruction])
build [] system = (system, [])
build (MkDir name : instructions) (Dir parent system files) =
  build remainingInstructions (Dir parent (S.insert subdir system) files)
  where (subdir, remainingInstructions) =
          build instructions (Dir name S.empty S.empty)
build (CdUp : instructions) system = (system, instructions)
build (Touch name size : instructions) (Dir parent system files) =
  build instructions (Dir parent system (S.insert (name, size) files))
build (NoOp : instructions) system = build instructions system

flatSystem :: System -> [System]
flatSystem d@(Dir _ dirs _) = d : (S.toList dirs >>= flatSystem)

du :: System -> Integer
du (Dir name dirs files) = fileSize + dirSize
  where fileSize = sum $ fmap snd $ S.toList files
        dirSize  = sum $ fmap du $ S.toList dirs

part1 :: System -> Integer
part1 system = sum $ filter (<= 100000) $ fmap du $ flatSystem system

part2 :: System -> Integer
part2 system = minimum $ filter (>= delete) sizes
  where total    = 70000000
        atLeast  = 30000000
        sizes    = fmap du $ flatSystem system
        homeSize = head sizes
        delete   = atLeast - (total - homeSize)

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let
    system = (\(sys,_) -> sys)
             $ build (fmap parseLine $ tail $ lines contents) (Dir "/" S.empty S.empty)
    in do
    putStrLn $ Prelude.show $ part1 system
    putStrLn $ Prelude.show $ part2 system
