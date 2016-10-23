import Data.Char

data Slider a = Slider [a] [a] deriving (Show)

complete :: Slider a -> [a]
complete (Slider xs ys) = reverse ys ++ xs

lookF (Slider (x:xs) _) = xs
lookB (Slider _ ys) = ys

current :: Slider a -> a
current (Slider (x:xs) _) = x

setCurrent :: Slider a -> a -> Slider a
setCurrent (Slider (f:fs) b) c = Slider (c:fs) b

initSlider :: [a] -> Slider a
initSlider x = Slider x []

slideBack (Slider xs (y:ys)) = Slider (y:xs) ys
slideBack (Slider xs ys) = error (show $ length xs)

slideForward :: Slider a -> Slider a
slideForward (Slider [] ys) = Slider [] ys
slideForward (Slider (x:xs) ys) = Slider xs (x:ys)

slideForward' :: Slider a -> a -> Slider a
slideForward' (Slider (x:[]) ys) d = Slider [d] (x:ys)
slideForward' slider _ = slideForward slider 

emptySlider :: Slider a -> Bool
emptySlider (Slider [] _) = True
emptySlider _ = False

type Instruction = State -> State

data State = State {
	instructions :: (Slider Instruction),
	memory :: (Slider Int),
	input :: [Char],
	output :: [Char],
	count :: Int
}

currentMem = current.memory
showCurrentMem = chr.currentMem
showMem = show.complete.memory

instance Show State where
  show state = "input: " ++ input state ++ ", output:" ++ output state ++ ", mem: " ++ showMem state ++ ", count: " ++ show (count state)

transformValue :: (a -> a) -> Slider a -> Slider a
transformValue f (Slider (x:xs) ys) = Slider ((f x):xs) ys

inc x
	| x >= 255 = 0
	| otherwise = x + 1

dec x
	| x <= 0 = 255
	| otherwise = x - 1

-- iterateN?
again :: Int -> (a -> a) -> a -> a
again 0 _ d = d
again n f d = again (n-1) f (f d)

inputAsInt :: String -> Int
inputAsInt (x:xs) = ord x

-- Instructions

write state = state { output = output state ++ [showCurrentMem state] }
read' state = state { input = tail (input state), memory = setCurrent (memory state) (inputAsInt (input state)) }

forward state = state { memory = slideForward' (memory state) 0 }
back state = state { memory = slideBack (memory state) }

up state = state { memory = transformValue inc (memory state) }
down state = state { memory = transformValue dec (memory state) }

jumpf n state
	| currentMem state == 0 = again n moveInstruction state
	| otherwise = state
jumpb n state
	| currentMem state /= 0 = again n moveInstruction' state
	| otherwise = state
-- 

currentInstruction :: State -> Instruction
currentInstruction = current.instructions

transformInstruction :: (Slider Instruction -> Slider Instruction) -> State -> State
transformInstruction f state = state { instructions = (f) (instructions state) }

moveInstruction = transformInstruction slideForward
moveInstruction' = transformInstruction slideBack

applyInstruction :: State -> State
applyInstruction state = state `currentInstruction` state

incExec state = state { count = (count state) + 1 }

timeout state = state { output = output state ++ "\nPROCESS TIME OUT. KILLED!!!",
	instructions = initSlider [] }

step :: State -> State
step state
	| count state >= 100000 = timeout state
	| otherwise = (incExec.moveInstruction.applyInstruction) state

finished = emptySlider.instructions
next = execute.step

execute :: State -> State
execute state
	| finished state = state
        | otherwise = next state

calcJumpDistScope (x:xs) open close scope
	| x == close && scope == 0 = 1
	| x == close = 1 + (calcJumpDistScope xs open close (scope - 1))
	| x == open = 1 + (calcJumpDistScope xs open close (scope + 1))
	| otherwise = 1 + (calcJumpDistScope xs open close scope)

calcJumpDist xs open close = calcJumpDistScope xs open close 0

parseInstruction :: Char -> [Char] -> [Char] -> Maybe Instruction
parseInstruction '>' _ _ = Just forward
parseInstruction '<' _ _ = Just back
parseInstruction '+' _ _ = Just up
parseInstruction '-' _ _ = Just down
parseInstruction ',' _ _ = Just read'
parseInstruction '.' _ _ = Just write
parseInstruction '[' f _ = Just $ jumpf $ calcJumpDist f '[' ']'
parseInstruction ']' _ b = Just $ jumpb $ (calcJumpDist b ']' '[') + 1
parseInstruction _ _ _ = Nothing

condAdd :: Maybe a -> [a] -> [a]
condAdd Nothing xs = xs
condAdd (Just x) xs = x:xs 

addNextInstruction p is = condAdd (parseInstruction (current p) (lookF p) (lookB p)) is

parseProcess :: Slider Char -> [Instruction] -> (Slider Char, [Instruction])
parseProcess p is
	| emptySlider p = (p, is)
	| otherwise = parseProcess (slideForward p) (addNextInstruction p is)

runParse p = parseProcess (initSlider p) []
extractInstructions (p, is) = is
string2InstSlider = initSlider.reverse.extractInstructions.runParse

emptyMem = initSlider [0]

string2State :: String -> String -> State
string2State inp p = State (string2InstSlider p) emptyMem inp "" 0

brainf :: String -> String -> String
brainf inp = output.execute.string2State inp

withoutDollar = reverse.tail.reverse

bftokens = "<>+-,.[]"
bfonly p = filter (\x -> elem x bftokens) p

main = do
	trash <- getLine
	inputString <- getLine
	prog <- getContents
	putStrLn $ brainf (withoutDollar inputString) (bfonly prog)
	return ()

