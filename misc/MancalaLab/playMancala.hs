import MancalaBoard

main = do
    putStrLn "New Game"
    let uboard = initial
    mainloop uboard

mainloop board = do
    putStrLn $ show board
    putStrLn "Make your move:"
    let isWinner = (winners board) /= []
    let win = if isWinner then (show . head) (winners board) else "No one"
    input <- getLine
    let inp = read input :: Int
    if (isAllowedMove board inp) then
        if (not . gameOver) board then
            (mainloop (move board inp))
        else
            putStrLn $ "Game Over: " ++ win ++ " wins"
    else mainloop board