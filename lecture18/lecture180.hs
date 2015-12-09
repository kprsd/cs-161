import Control.Monad.State
import Prelude hiding (head)

type HTML = State (Int, String)

render :: HTML a -> String
render mHTML = snd $ execState mHTML (0, "")

adjustDepth :: (Int -> Int) -> HTML ()
adjustDepth f = do
    (x, y) <- get
    put (f x, y)

incDepth, decDepth :: HTML ()
incDepth = adjustDepth (+1)
decDepth = adjustDepth (subtract 1)

tab :: HTML ()
tab = do
    (x, y) <- get
    put (x, "  " ++ y)

newline :: HTML ()
newline = do
    (x, y) <- get
    put (x, y ++ "\n")


doTabs :: Int -> String
doTabs 0 = ""
doTabs n = "  " ++ doTabs (n - 1)

indentedString :: String -> HTML ()
indentedString str = do
    (x, y) <- get
    put (x, y ++ (doTabs x) ++ str)
    

tag :: String -> HTML a -> HTML ()
tag t mHTML = do
    indentedString $ "<" ++ t ++ ">"
    incDepth
    newline
    mHTML
    decDepth
    newline
    indentedString $ "</" ++ t ++ ">"
    

html  = tag "html"
head  = tag "head"
title = tag "title"
body  = tag "body"
p     = tag "p"
i     = tag "i"
b     = tag "b"
h1    = tag "h1"
h2    = tag "h2"
h3    = tag "h3"
h4    = tag "h4"
ol    = tag "ol"
ul    = tag "ul"
table = tag "table"
tr    = tag "tr"
th    = tag "th"

doc' :: HTML ()
doc' =
    html $ do
        head $ do
            title $ indentedString "Hello, world!"
        newline
        body $ do
            h1 $ indentedString "Greetings"
            newline
            p $ indentedString "Hello, world!"
