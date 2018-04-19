-- Function that takes in a guess, score and possible chords,
-- then removes all inconsistent possible targets, that is possible targets that
-- do not yield the same score. 
removeInvalidTargets :: [String] -> (Int, Int, Int) -> [[String]] -> [[String]]
removeInvalidTargets guess score [] = []
removeInvalidTargets guess score (t:ts)
    -- Keep that chord if the score is the same. 
    | (score == (scoreCalc guess t)) = [t] ++ removeInvalidTargets guess score ts
    -- otherwise, don't keep it and keep going through the chords recursively. 
    | otherwise                      = removeInvalidTargets guess score ts

-- Our functions require that the guess and target be sorted first. 
-- TESTED
scoreCalc :: [String] -> [String] -> (Int, Int, Int)
scoreCalc guess target = (pitchScore,y,z)
    where pitchScore   = length (intersect guess target)
          y            = noteScore (removeCorrectPitches guess target) (removeCorrectPitches target guess)
          z            = octaveScore (removeCorrectPitches guess target) (removeCorrectPitches target guess) 

--TESTED
noteScore :: [String] -> [String] -> Int
noteScore cleanGuess cleanTarget  = length (nub ((intersect (extractNotes cleanGuess) (extractNotes cleanTarget))))


-- Takes a string of pitches and returns the notes in order. 
-- TESTED
extractNotes :: [String] -> [Char]
extractNotes []     = [] 
extractNotes (x:xs) = [x!!0] ++ (extractNotes xs)  

-- Takes a string of pitches and returns the octaves in order. 
-- TESTED
extractOctaves :: [String] -> [Char]
extractOctaves []     = [] 
extractOctaves (x:xs) = [x!!1] ++ (extractOctaves xs)  

-- Takes guess and target chord then removes correct ptches from the guess chord. 
-- TESTED
removeCorrectPitches :: [String] -> [String] -> [String]
removeCorrectPitches (x:xs) [] = (x:xs) 
removeCorrectPitches (x:xs) (y:ys) = [z | z<-(x:xs), isPresent z (y:ys) == False]

-- TESTED
isPresent :: String -> [String] -> Bool
isPresent x [] = False
isPresent x (y:ys) 
    | x == y = True
    | otherwise = False ||  (isPresent x ys)



-- Calculates score for when the note is wrong but the octave is right
-- TESTED
octaveScore :: [String] -> [String] -> Int
octaveScore x y = (length x) - length (deleteFirstsBy equality (extractOctaves x) (extractOctaves y) )

equality :: Char -> Char -> Bool 
equality x y = x==y