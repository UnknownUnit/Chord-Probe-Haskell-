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

-- Currently our initial guess is just the first chord in our list. 
-- We then remove that chord from the list. 
initialGuess :: ([String], GameState)
initialGuess = (firstGuess, GameState (removeChord firstGuess chordList))
    where chordList = combinations 3 pitchList
          firstGuess = ["E3", "F1", "G2"] -- TODO change ths to match hnt 5

--Function that takes in a chord and a chordlist, then computes the average number of 
-- guesses needed if we used this guess. 



removeChord :: [String] -> [[String]] -> [[String]]
removeChord x [] = [] 
removeChord x (y:ys) 
    | x == y = removeChord x ys
    | otherwise = [y] ++ removeChord x ys

-- After improving initialGuess, change this to mirror that then test it out!
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (previousGuess, prevGameState) prevScore
    = (head consistentChords, GameState (tail consistentChords))
        where consistentChords = removeInvalidTargets previousGuess prevScore (remaining prevGameState)


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