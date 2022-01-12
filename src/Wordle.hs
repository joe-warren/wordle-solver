{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wordle where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.RWS (MonadWriter)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT), evalStateT, gets)
import Data.Foldable (Foldable (toList), maximumBy, minimumBy)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Guess = Guess {getGuessText :: Text} deriving (Ord, Eq, Show)

newtype Answer = Answer {getAnswerText :: Text} deriving (Ord, Eq, Show)

data SingleMatch = White | Yellow | Green deriving (Show)

class (MonadState (NESet Answer) m, MonadReader (NESet Guess) m) => MonadWordle m where
  tryGuess :: Guess -> m [SingleMatch]

readData :: IO (NESet Answer, NESet Guess)
readData = do
  let readSet f = maybe (fail $ "empty file: " <> f) pure . NESet.nonEmptySet . Data.Set.fromList . T.lines =<< T.readFile f
  a <- readSet "possible-answers.txt"
  g <- readSet "possible-guesses.txt"
  return (NESet.map Answer a, NESet.map Guess (a `NESet.union` g))

matches :: Answer -> Guess -> [SingleMatch]
matches (Answer a) (Guess g) = uncurry matchOne <$> T.zip a g
  where
    matchOne a' g'
      | a' == g' = Green
      | T.any (== g') a = Yellow
      | otherwise = White

answerIsPossible :: Guess -> [SingleMatch] -> Answer -> Bool
answerIsPossible (Guess g) matches (Answer a) = and $ zipWith matchOne matches $ T.zip g a
  where
    matchOne Green (g', a') = g' == a'
    matchOne Yellow (g', a') = g' /= a' && T.any (== g') a
    matchOne White (g', _) = T.all (/= g') a

scoreMatch :: SingleMatch -> Int
scoreMatch White = 0
scoreMatch Yellow = 1
scoreMatch Green = 2

scoreMatches :: [SingleMatch] -> Int
scoreMatches = sum . fmap scoreMatch

findBestGuess :: NESet Guess -> NESet Answer -> Guess
findBestGuess guesses answers =
  let answerSample = take 100 $ toList answers
      scoreGuess g =
        let scoreAnswer ans = let m = matches ans g in length $ filter (answerIsPossible g m) answerSample
         in sum $ scoreAnswer <$> answerSample
   in minimumBy (compare `on` scoreGuess) guesses

findBestGuess' :: NESet Guess -> NESet Answer -> Guess
findBestGuess' guesses answers =
  let scoreGuess g =
        let scoreAnswer ans = scoreMatches $ matches ans g
         in sum $ scoreAnswer <$> toList answers
   in maximumBy (compare `on` scoreGuess) guesses

newtype InteractiveWordleM a = InteractiveWordleM {runInteractiveWordleM :: ReaderT (NESet Guess) (StateT (NESet Answer) IO) a}
  deriving (Functor, Applicative, Monad, MonadReader (NESet Guess), MonadState (NESet Answer), MonadIO) via (ReaderT (NESet Guess) (StateT (NESet Answer) IO))

parseInput :: Text -> Maybe [SingleMatch]
parseInput = traverse parseOne . T.unpack . T.toLower
  where
    parseOne 'w' = pure White
    parseOne 'y' = pure Yellow
    parseOne 'g' = pure Green
    parseOne _ = Nothing

instance MonadWordle InteractiveWordleM where
  tryGuess (Guess g) = do
    a <- get
    liftIO $ T.putStrLn . (<> " anwsers remaining") . T.pack . show . length $ a
    when (length a < 25) $ liftIO (print a)
    liftIO $ T.putStrLn $ "Try guesssing: " <> g
    input <- liftIO T.getLine
    case parseInput input of
      Nothing -> do
        liftIO $ T.putStrLn "expect input in the form \"wygyw\""
        tryGuess (Guess g)
      Just sms -> do
        correctLength <- gets (T.length . getAnswerText . NE.head . NESet.toList)
        if length sms == correctLength
          then pure sms
          else do
            liftIO $ T.putStrLn "Incorrect length input"
            tryGuess (Guess g)

solveWordle :: MonadWordle m => m (Maybe Answer)
solveWordle = do
  candidateGuesses <- ask
  candidateAnswers <- get
  let guess = findBestGuess candidateGuesses candidateAnswers
  match <- tryGuess guess
  case NESet.filter (answerIsPossible guess match) candidateAnswers of
    NESet.IsEmpty -> pure Nothing
    NESet.IsNonEmpty ne -> case NESet.elems ne of
      ans :| [] -> pure . pure $ ans
      _ -> put ne *> solveWordle

runWordle :: IO ()
runWordle = do
  (answers, guesses) <- readData
  result <- (`evalStateT` answers) . (`runReaderT` guesses) . runInteractiveWordleM $ solveWordle
  case result of
    Nothing -> T.putStrLn "Couldn't find a solution"
    Just an -> T.putStrLn $ "Anwser: " <> getAnswerText an
