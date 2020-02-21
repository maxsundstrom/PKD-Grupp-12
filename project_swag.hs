import Control.Exception
import Prelude hiding(catch)



menu = do
    putStrLn "Legend Of Einar And His Gucci Cap"
    putStrLn ""
    putStrLn "Play game = play | Quit Game = Press any key"
    answer <- getLine
    if answer == "play" then game else return ()


game = do
    putStrLn ""
    putStrLn "Einar vaktar alltid orten iklädd sin gucci keps och sin orten väst som säkert är skottsäker men det är oklart"
    putStrLn "En dag när Einar är ute och vaktar ser han en kran schono"
    putStrLn "Vad ska Einar göra:"
    putStrLn ""
    putStrLn "säga åt schonon att ge einar ny ladd. Press 1"
    putStrLn "börja hetsa schonon för att få han att få psykos. Press any other key than 1"
    answer <- getLine
    if answer == "1" then ladd
    else fightschono



ladd = do 
    putStrLn ""
    putStrLn "schonon kommer fram och ger Einar ladd" 
    putStrLn ""
    putStrLn "Einar: du är katten i trakten bror, du lyser upp hela natten <3"
    putStrLn "Schono: ehm okej..."
    putStrLn "Einar går hem för att ta sin ladd"

fightschono = do
    putStrLn ""
    putStrLn "schonon tappar det totalt och tar upp en becknaväska full med tegelsten."
    putStrLn "schonon slog ihjäl Einar, RIP Einar 2002-09-05"

