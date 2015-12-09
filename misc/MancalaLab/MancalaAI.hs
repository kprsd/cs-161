module MancalaAI(aiNextMove, isGameOver) where

import MancalaBoard

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.

simulateGame :: (MancalaBoard -> Move) -> (MancalaBoard -> Move) -> MancalaBoard -> Int -> MancalaBoard
simulateGame = undefined

isGameOver :: MancalaBoard -> Bool
isGameOver mancala = any (==0) (map (length . (allowedMovesFor mancala)) allPlayers)

heuristicScore :: Player -> MancalaBoard -> Int
heuristicScore player mancala = (sum $ playerSide mancala player) + playerScore mancala player - (sum $ playerSide mancala otherPlayer) - playerScore mancala otherPlayer
    where otherPlayer = if player == (allPlayers !! 0) then (allPlayers !! 1) else (allPlayers !! 0)

aiNextMove :: MancalaBoard -> Move
aiNextMove mancala = lookahead mancala 5 -- slowly increase the depth

evalPosition :: Player -> MancalaBoard -> Int -> Int
evalPosition player mancala 0     = heuristicScore player mancala-- heuristic score goes here
evalPosition player mancala depth = sign * heuristicScore player (theNextMove) -- recursive call, based on depth d-1 lookahead, goes here
    where sign = if ((getCurPlayer theNextMove) == getCurPlayer mancala) then 1 else (-1)
          theNextMove = move mancala (lookahead mancala (depth - 1))

getAll :: MancalaBoard -> [(MancalaBoard, Move)]
getAll mb = [ amv | amv <- map (\mv -> (move mb mv, mv)) (allowedMoves mb) ]

lookahead :: MancalaBoard -> Int -> Move
lookahead mb depth = snd $ maximum (map (\(brd, mvmt) -> (evalPosition (getCurPlayer brd) brd depth, mvmt)) (getAll mb))