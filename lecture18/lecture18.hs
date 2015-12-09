import Control.Monad.State
import Prelude hiding (head)

type HTML = State (Int, String)

render :: HTML a -> String
render = undefined

adjustDepth :: (Int -> Int) -> HTML ()
adjustDepth = undefined

incDepth, decDepth :: HTML ()
incDepth = adjustDepth (+1)
decDepth = adjustDepth (subtract 1)

tab :: HTML ()
tab = undefined

newline :: HTML ()
newline = undefined

indentedString :: String -> HTML ()
indentedString = undefined

tag :: String -> HTML a -> HTML ()
tag = undefined

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
        body $ do
            h1 $ indentedString "Greetings"
            p $ indentedString "Hello, world!"
