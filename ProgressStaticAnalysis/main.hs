import LanguageDefinition 
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

main = show $ parse openedge "test" "FOR FIRST lol:\n MESSAGE 'herp'.\n END."
