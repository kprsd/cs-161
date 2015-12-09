module Main where

import Data.Char
import System.Environment
import System.Exit
import System.IO
import Control.Monad

mUntil :: Monad m => m Bool -> m b -> m ()
mUntil control action = loop where
	loop = control >>= \terminate -> unless terminate (action >> loop)

usage :: IO ()
usage = do
	progname <- getProgName
	hPutStrLn stderr $ "usage: " ++ progname ++ " infile outfile"
	exitWith $ ExitFailure 255

hMaybeGetChar :: Handle -> IO (Maybe Char)
hMaybeGetChar h = do
    atEOF <- hIsEOF h
    g <- hGetChar h
    if atEOF then return Nothing
    else return (Just g)

mWhileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
mWhileJust p f = do
    c <- p
    case c of
            Nothing -> return ()
            Just c -> do
                   f c
                   mWhileJust p f


main :: IO ()
main = do
	args <- getArgs
	case args of
		[infile,outfile] -> do
			input <- openFile infile ReadMode
			output <- openFile outfile WriteMode
			mUntil (hIsEOF input) $ do
				inChar <- hGetChar input
				let outChar = toUpper inChar
				hPutChar output outChar
			hClose output
			hClose input
		_ -> usage
