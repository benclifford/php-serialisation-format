
-- | http://www.phpinternalsbook.com/classes_objects/serialization.html

module Data.PHP where

  import Control.Applicative
  import Control.Monad
  import Text.ParserCombinators.Parsec hiding ( (<|>) , many )

  -- | Should Eq compare PHP arrays regardless of order? Is there
  -- equality behaviour to inherit from PHP?
  data PHPData =
      PHPArray [(PHPData, PHPData)]
    | PHPString String
    deriving (Show, Eq)

  phpdata =
        (phparray <?> "PHP array")
    <|> (phpstring <?> "PHP string")

  phpstring = do
    char 's'
    char ':'
    strlen <- integer
    char ':'
    char '"'
    v <- forM [1..strlen] $ \_ -> anyChar
    char '"'
    char ';'
    return $ PHPString v

  phparray = do
    char 'a'
    char ':'
    count <- integer
    char ':'
    char '{'
    v <- forM [1..count] $ \_ -> do
      key <- phpdata
      value <- phpdata
      return (key, value)
    char '}'

    return $ PHPArray v

  integer = do
    digits <- many $ oneOf "0123456789"
    return (read digits :: Int)

  parsePHP :: String -> Either ParseError PHPData
  parsePHP s = parse (phpdata <* eof) "(unknown)" s

  testStr1 = "a:0:{}"
  testStr2 = "a:2:{s:7:\"booking\";a:1:{s:15:\"booking_comment\";s:0:\"\";}s:7:\"gateway\";s:6:\"realex\";}"
  testStr3 = "s:5:\"hello\";"

  phpTestMain = do
    test testStr1
    test testStr2
    test testStr3

  test s = do
    putStrLn s
    putStrLn "=>"
    print $ parsePHP s

