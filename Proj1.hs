-- =========================================================
-- Written by Jia Shun Low (743436) 
-- jlow3@student.unimelb.edu.au
-- Declarative Programming is fun!
-- =========================================================

-- This program implements the performer part of the game ChordProbe, 
-- where it has to guess the chord that the composer part of the game
-- has given as a target. Further details are in the specification 
-- of the project. The bulf of the description of our algorithm to choose
-- which chords to select as our next guess is located in the comments for 
-- initialGuess and nextGuess. 

module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List

-- It holds every possible chord that has not been eliminated by the program. 
data GameState = Empty | GameState {remaining :: [[String]]}
    deriving Show
 
-- A data type that is used in our algorithm to choose which possible guess 
-- to use next, based on the frequency of it appearing. 
data ScoreTracker = ScoreTracker {score::(Int,Int,Int), frequency :: Int} 
    deriving Show

-- A data type that is used to store all remaining chords and the average 
-- frequency if we chose that chord. 
data ChordTracker = ChordTracker {chord :: [String], avgFreq :: Double}
    deriving Show

-- Our initial guess just has a hard-coded first guess inside, as implementing 
-- our algorithm in initialGuess might make it too slow to feasibly use within
-- time limits. 
initialGuess :: ([String], GameState)
initialGuess = (firstGuess, GameState (removeChord firstGuess chordList))
    where chordList = combinations pitchList
          firstGuess = ["A1","B2","C3"]

-- Our next guess is chosen based on a simple algorithm. For all the remaining 
-- chords left, we check (on average) how many chords will be left if we choose 
-- that chord as our next guess. We do this by taking for each chord, a list 
-- of scores that appear and the frequency at which they appear. Since every
-- chord left in the potential target list has equal probability of being 
-- the real target, we just take the average frequency for each chord. When we 
--  have all the average frequencies for each chord, we proceed to pick the c
-- chord with the lowest average frequency as our next guess, because that chord
-- has the highest chance of eliminating many inconsistent chords if it is the
-- wrong choice. 
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (previousGuess, prevGameState) prevScore
    = (nextTryChord, GameState (removeChord nextTryChord consistentChords) )
        where consistentChords    = removeInvalidTargets previousGuess 
                                    prevScore (remaining prevGameState)
              chordTrackerList    = chordTrackerListCreator consistentChords 
                                    consistentChords
              minFreq'            = minimum (avgFreqListCreator 
                                    chordTrackerList) 
              nextTryChordTracker = chordTrackerSearch chordTrackerList minFreq'
              nextTryChord        = chord nextTryChordTracker

-- Returns every combination of chord possible with our list of pitches as the
-- argument, using list comprehension. 
combinations :: [String] -> [[String]]
combinations pitchList = [[x,y,z] | x<-pitchList, y<-pitchList,z<-pitchList, 
                         (x/=y) && (y/=z) && (x/=z), (x<y)&&(y<z)]

-- Function that takes in a guess, score and possible chords,
-- then removes all inconsistent possible targets, that is possible targets that
-- do not yield the same score when our previous guess is applied to it. 
removeInvalidTargets :: [String] -> (Int, Int, Int) -> [[String]] -> [[String]]
removeInvalidTargets guess score [] = []
removeInvalidTargets guess score (t:ts)
    | score == (scoreCalc guess t) = [t] ++ removeInvalidTargets guess score ts
    | otherwise                    = removeInvalidTargets guess score ts

-- Creates initial list of pitches in the form of String. 
pitchList :: [String]
pitchList
    = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3",
    "E1", "E2", "E3", "F1", "F2", "F3", "G1", "G2", "G3"]

-- Function to calculate scores, using various helper functions. 
scoreCalc :: [String] -> [String] -> (Int, Int, Int)
scoreCalc guess target = (pitchScore,y,z)
    where pitchScore   = length (intersect guess target)
          y            = noteScore (removeCorrectPitches guess target) 
                         (removeCorrectPitches target guess)
          z            = octaveScore (removeCorrectPitches guess target) 
                         (removeCorrectPitches target guess) 

-- Helper function for scoreCa;c, calculates the note score. 
noteScore :: [String] -> [String] -> Int
noteScore cleanGuess cleanTarget  = length (nub 
    ((intersect (extractNotes cleanGuess) (extractNotes cleanTarget))))

-- Helper function for scoreCalc, takes a string of pitches and returns the 
-- notes.
extractNotes :: [String] -> [Char]
extractNotes []     = [] 
extractNotes (x:xs) = [x!!0] ++ (extractNotes xs)  

-- Helper function for scoreCalc, takes a string of pitches and 
-- returns the octaves.
extractOctaves :: [String] -> [Char]
extractOctaves []     = [] 
extractOctaves (x:xs) = [x!!1] ++ (extractOctaves xs)  

-- Takes guess and target chord then removes correct pitches from the guess 
-- chord, first argument being the guess and second argument being the target. 
removeCorrectPitches :: [String] -> [String] -> [String]
removeCorrectPitches (x:xs) [] = (x:xs) 
removeCorrectPitches (x:xs) y = [z | z<-(x:xs), isPresent z y == False]

-- Helper function for remoevCorrectPitches.
-- Checks if a particular pitch is present in a chord. 
isPresent :: String -> [String] -> Bool
isPresent x [] = False
isPresent x (y:ys) 
    | x == y    = True
    | otherwise = False ||  (isPresent x ys)

-- Calculates score for when the note is wrong but the octave is right
octaveScore :: [String] -> [String] -> Int
octaveScore x y = (length x) - length (deleteFirstsBy equality 
    (extractOctaves x) (extractOctaves y) )

-- Checks if two characters are equal. Used as a helper function in OctaveScore, 
-- as an argument for the built in function deleteFirstsBy.  
equality :: Char -> Char -> Bool 
equality x y = x==y

-- Function that looks though a chordTrackerList and finds the first entry with 
-- the average frequency we are looking for which is the key. We really only
-- use this to find the chord with the minimum frequency. 
chordTrackerSearch :: [ChordTracker] -> Double -> ChordTracker
chordTrackerSearch (x:xs) key 
    | (avgFreq x == key) = x
    | otherwise          = chordTrackerSearch xs key

-- Function that creates a list of chordtrackers from all possible targets and 
-- guesses. 
chordTrackerListCreator :: [[String]] -> [[String]]  -> [ChordTracker]
chordTrackerListCreator [] y         = [] 
chordTrackerListCreator (x:xs) y 
    -- This handles the fringe case when the remaining targets is just one chord
    -- left. Just return it with any frequency, it will be the correct guess. 
    | (length y) == 1 = [(ChordTracker x 0.0)]
    | otherwise       = [chordTrackerCreator x (removeChord x y)] ++ 
                        (chordTrackerListCreator xs y )  

-- Function that takes in a chord, possible targets and  returns a chordTracker 
-- with the chord and it's averageFrequency
chordTrackerCreator :: [String] -> [[String]] -> ChordTracker
chordTrackerCreator x (y:ys) = ChordTracker x (averageFrequency freqList 
                               (fromIntegral (sumFrequency freqList)) )
    where freqList = (frequencyList( (calcGroupedScoreFreq x 
                     (removeChord x (y:ys)) []) ) )

-- Function that looks through a chordTrackerList and creates a list of 
-- all the average frequencies. Practically speaking, it extracts all the 
-- average frequencies from our chordTracker list so that we can easily process
-- it as a list of doubles. 
avgFreqListCreator :: [ChordTracker] -> [Double] 
avgFreqListCreator [] = [] 
avgFreqListCreator (x:xs) = [avgFreq x] ++ avgFreqListCreator xs

--Function that takes in a list of frequencies and returns the sum 
sumFrequency :: [Int] -> Int
sumFrequency [] = 0
sumFrequency (x:xs) = x + (sumFrequency xs)

-- Function that takes in a list of frequencies, and returns the average number
-- of frequency. 
averageFrequency :: [Int] -> Double -> Double 
averageFrequency [] x             = 0.0 
averageFrequency (x:xs) totalFreq = (x'*x'/totalFreq) + 
                                    (averageFrequency xs totalFreq)
    where x' = fromIntegral x

-- Function that creates a list of frequencies given a ScoreTracker list. 
frequencyList :: [ScoreTracker] -> [Int] 
frequencyList []     = [] 
frequencyList (x:xs) = [frequency x] ++ (frequencyList xs) 

-- Function that takes in one chord, and the remaining possibile chords that 
-- might  be answers, then returns a list of ScoreTrackers, a data type that has
-- a score and frequency associated with the score. Practically, this function
-- groups together all possible scores that a candiate guess can return by the 
-- frequency at which they appear. 
calcGroupedScoreFreq :: [String] -> [[String]] -> [ScoreTracker] 
                        -> [ScoreTracker]
calcGroupedScoreFreq x [] scoreTrackerList = scoreTrackerList
calcGroupedScoreFreq x (y:ys) scoreTrackerList 
    -- if score is not on that list, add that score to the list then add that 
    -- target to the list of possible targets. 
    | ((checkScoreOnList (scoreCalc x y) scoreTrackerList) == False) 
    = (calcGroupedScoreFreq x ys (scoreTrackerList ++ 
      [ScoreTracker (scoreCalc x y) 1]))
    --If score is on the list, +1 to that particular score. 
    | ((checkScoreOnList (scoreCalc x y) scoreTrackerList) == True)  
    = calcGroupedScoreFreq x ys (updateScoreTrackerList (scoreCalc x y) 
      scoreTrackerList)
        where updatedList = updateScoreTrackerList (scoreCalc x y) 
                            scoreTrackerList

-- Takes in a score and a scoreTracker list then looks for the scoreTracker
-- with the same score, to then update that scoreTracker to have it's frequency
updateScoreTrackerList :: (Int, Int, Int) -> [ScoreTracker] -> [ScoreTracker]
updateScoreTrackerList scoreKey [] = []
updateScoreTrackerList scoreKey (x:xs)  
    -- If we are at the index of the score to change the frequency,
    -- change the frequency. 
    | (scoreKey == (score x))      = [plusOne x] ++ (xs)
    -- If we are not yet there, just keep going. 
    | otherwise                    = [x] ++ (updateScoreTrackerList scoreKey xs)

-- Helper function for updateScoreTrackerList, it edits the frequency on a 
-- ScoreTracker to +1
plusOne :: ScoreTracker -> ScoreTracker
plusOne x = ( ScoreTracker (score x) ((frequency x) + 1) )

-- Helper function for updateScoreTrackerList 
-- that helps check if a particular score is already included on the list. 
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


