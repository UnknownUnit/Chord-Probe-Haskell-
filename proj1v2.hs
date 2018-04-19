-- =========================================================
-- Written by Jia Shun Low (743436) 
-- jlow3@student.unimelb.edu.au
-- Declarative Programming is fun!
-- TODO go through the coding standards list. 
-- TODO check char limit on one line. 
-- =========================================================

-- This program implements the performer part of the game ChordProbe, 
-- where it has to guess the chord that the composer part of the game
-- has given as a target. Further details are in the specification 
-- of the project. 

module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List

-- It holds every possible chord that has not been eliminated by the program. 
data GameState = Empty | GameState {remaining :: [[String]]
    }deriving Show
 
data ScoreTracker = Empty1 | ScoreTracker {score::(Int,Int,Int), count :: Int}

data ChordTracker = ChordTracker {chord :: [String], avgFreq :: Double}

-- Currently our initial guess is just the first chord in our list. 
-- We then remove that chord from the list. 
initialGuess :: ([String], GameState)
initialGuess = (firstGuess, GameState (removeChord firstGuess chordList))
    where chordList = combinations 3 pitchList
          firstGuess = ["A1", "B2", "C3"] 

-- After improving initialGuess, change this to mirror that then test it out!
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (previousGuess, prevGameState) prevScore
    = (head consistentChords, GameState (tail consistentChords) )
        where consistentChords     = removeInvalidTargets previousGuess prevScore (remaining prevGameState)
              chordTrackerList     = chordTrackerListCreator consistentChords consistentChords
              maxFreq'             = maximum (avgFreqListCreator chordTrackerList) 
              nextTryChordTracker  = chordTrackerKey chordTrackerList maxFreq'
              nextTryChord         = chord nextTryChordTracker

--Function that looks through a chordTrackerList and creates a list of doubles. 
avgFreqListCreator :: [ChordTracker] -> [Double] 
avgFreqListCreator [] = [] 
avgFreqListCreator (x:xs) = [avgFreq x] ++ avgFreqListCreator xs

-- Function that looks though a chordTrackerList and finds the first entry with a double key. 
chordTrackerKey :: [ChordTracker] -> Double -> ChordTracker
chordTrackerKey (x:xs) key 
    | (avgFreq x == key) = x
    | otherwise          = chordTrackerKey xs key

-- Function that creates a list of chordtrackers from all possible targets and guesses.
chordTrackerListCreator :: [[String]] -> [[String]] -> [ChordTracker]
chordTrackerListCreator [] y         = [] 
chordTrackerListCreator (x:xs) y 
    = [chordTrackerCreator x (removeChord x y)] ++ (chordTrackerListCreator xs y)

-- Function that takes in a chord, possible targets and  returns a chordTracker with the chord and it's averageFrequency
chordTrackerCreator :: [String] -> [[String]] -> chordTracker
chordTrackerCreator x (y:ys) = ChordTracker x (averageFrequency freqList (fromIntegral (sumFrequency freqList)) )
    where freqList = (frequencyList( (averageLeftList x (y:ys) []) ) )

--Function that takes in a list of frequencies and returns the sum 
sumFrequency :: [Int] -> Int
sumFrequency [] = 0
sumFrequency (x:xs) = x + (sumFrequency xs)

-- Function that takes in a list of frequencies, and returns the average number of frequency. 
averageFrequency :: [Int] -> Double -> Double 
averageFrequency [] x             = 0.0 
averageFrequency (x:xs) totalFreq = (x'*x'/totalFreq) + (averageFrequency xs totalFreq)
    where x' = fromIntegral x

-- Function that creates a list of counts (frequencies) given a ScoreTracker list. 
frequencyList :: [ScoreTracker] -> [Int] 
frequencyList [] = [] 
frequencyList (x:xs) = [count x] ++ (frequencyList xs) 

--Function that takes in one chord, and the remaining possibile chords that might 
-- be answers, then returns a list of ScoreTrackers, a data type that has a score and frequency 
-- associated with the score. 
averageLeftList :: [String] -> [[String]] -> [ScoreTracker] -> [ScoreTracker]
averageLeftList x [] scoreTrackerList = scoreTrackerList
averageLeftList x (y:ys) scoreTrackerList 
    -- if score is not on that list, add that score to the list then add that target
    -- to the list of possible targets. 
    | (checkScoreOnList (scoreCalc x y) scoreTrackerList) == False 
    = (averageLeftList x ys (scoreTrackerList ++ [ScoreTracker (scoreCalc x y) 1]))
    --If score is on the list, +1 to that particular score. 
    | (checkScoreOnList (scoreCalc x y) scoreTrackerList) == True  
    = averageLeftList x ys (updateScoreTrackerList (scoreCalc x y) scoreTrackerList)
        where updatedList = updateScoreTrackerList (scoreCalc x y) scoreTrackerList

updateScoreTrackerList :: (Int, Int, Int) -> [ScoreTracker] -> [ScoreTracker]
updateScoreTrackerList x [] = []
updateScoreTrackerList scoreToChange (x:xs)  
    -- If we are at the index of the score to change the frequency. change the frequency. 
    | (scoreToChange == (score x)) = [plusOne x] ++ (xs)
    -- If we are not yet there, just keep going. 
    | otherwise                    = [x] ++ (updateScoreTrackerList scoreToChange xs)

-- Edits the count on a ScoreTracker to +1
plusOne :: ScoreTracker -> ScoreTracker
plusOne x = ( ScoreTracker (score x) ((count x) + 1) )

-- Helperfunction that helps check if a particular score is already included on the list. 
checkScoreOnList :: (Int, Int, Int) -> [ScoreTracker] -> Bool
checkScoreOnList x [] = False
checkScoreOnList x (y:ys) 
    | x == (score y) = True
    | otherwise      = (False || (checkScoreOnList x ys))

























--Remove a chord from a list of chords. 
removeChord :: [String] -> [[String]] -> [[String]]
removeChord x [] = [] 
removeChord x (y:ys) 
    | x == y = removeChord x ys
    | otherwise = [y] ++ removeChord x ys

-- Creates initial list of pitches in the form of String. 
pitchList :: [String]
pitchList
    = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3",
    "E1", "E2", "E3", "F1", "F2", "F3", "G1", "G2", "G3"]

-- TODO Rewrite. Maybe write a version that uses only list comprehension or something. 
-- Taken from the Haskell wiki 99 Questions solution 26. 
combinations :: Int-> [String] -> [[String]]
combinations 0 _ = [[]]
combinations n xs = [xs !! i : x | i <- [0..length(xs) - 1], x <- combinations (n-1) (drop (i+1) xs)]

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