module Questions exposing (questions)

questions : List (String, String)
questions =
    [ ( "(==)", "a -> a -> Bool" )
    , ( "(/=)", "a -> a -> Bool" )
    , ( "compare", "a -> a -> Ordering" )
    , ( "(<)", "a -> a -> Bool" )
    , ( "(<=)", "a -> a -> Bool" )
    , ( "(>=)", "a -> a -> Bool" )
    , ( "(>)", "a -> a -> Bool" )
    , ( "max", "a -> a -> a" )
    , ( "min", "a -> a -> a" )
    , ( "succ", "a -> a" )
    , ( "pred", "a -> a" )
    , ( "toEnum", "Int -> a" )
    , ( "fromEnum", "a -> Int" )
    , ( "enumFrom", "a -> [a]             -- [n..]" )
    , ( "enumFromThen", "a -> a -> [a]        -- [n,n'..]" )
    , ( "enumFromTo", "a -> a -> [a]        -- [n..m]" )
    , ( "enumFromThenTo", "a -> a -> a -> [a]   -- [n,n'..m]" )
    , ( "minBound", "a" )
    , ( "maxBound", "a" )
    , ( "(+)", "a -> a -> a" )
    , ( "(-)", "a -> a -> a" )
    , ( "(*)", "a -> a -> a" )
    , ( "negate", "a -> a" )
    , ( "abs", "a -> a" )
    , ( "signum", "a -> a" )
    , ( "fromInteger", "Integer -> a" )
    , ( "toRational", "a -> Rational" )
    , ( "quot", "a -> a -> a" )
    , ( "rem", "a -> a -> a" )
    , ( "div", "a -> a -> a" )
    , ( "mod", "a -> a -> a" )
    , ( "quotRem", "a -> a -> (a,a)" )
    , ( "divMod", "a -> a -> (a,a)" )
    , ( "toInteger", "a -> Integer" )
    , ( "(/)", "a -> a -> a" )
    , ( "recip", "a -> a" )
    , ( "fromRational", "Rational -> a" )
    , ( "pi", "a" )
    , ( "exp", "a -> a" )
    , ( "log", "a -> a" )
    , ( "sqrt", "a -> a" )
    , ( "(**)", "a -> a -> a" )
    , ( "logBase", "a -> a -> a" )
    , ( "sin", "a -> a" )
    , ( "cos", "a -> a" )
    , ( "tan", "a -> a" )
    , ( "asin", "a -> a" )
    , ( "acos", "a -> a" )
    , ( "atan", "a -> a" )
    , ( "sinh", "a -> a" )
    , ( "cosh", "a -> a" )
    , ( "tanh", "a -> a" )
    , ( "asinh", "a -> a" )
    , ( "acosh", "a -> a" )
    , ( "atanh", "a -> a" )
    , ( "properFraction", "(Integral b) => a -> (b,a)" )
    , ( "truncate", "(Integral b) => a -> b" )
    , ( "round", "(Integral b) => a -> b" )
    , ( "ceiling", "(Integral b) => a -> b" )
    , ( "floor", "(Integral b) => a -> b" )
    , ( "floatRadix", "a -> Integer" )
    , ( "floatDigits", "a -> Int" )
    , ( "floatRange", "a -> (Int,Int)" )
    , ( "decodeFloat", "a -> (Integer,Int)" )
    , ( "encodeFloat", "Integer -> Int -> a" )
    , ( "exponent", "a -> Int" )
    , ( "significand", "a -> a" )
    , ( "scaleFloat", "Int -> a -> a" )
    , ( "", "a -> Bool" )
    , ( "atan2", "a -> a -> a" )
    , ( "subtract", "(Num a) => a -> a -> a" )
    , ( "even", "(Integral a) => a -> Bool" )
    , ( "odd", "(Integral a) => a -> Bool" )
    , ( "gcd", "(Integral a) => a -> a -> a" )
    , ( "lcm", "(Integral a) => a -> a -> a" )
    , ( "(^)", "(Num a, Integral b) => a -> b -> a" )
    , ( "(^^)", "(Fractional a, Integral b) => a -> b -> a" )
    , ( "fromIntegral", "(Integral a, Num b) => a -> b" )
    , ( "realToFrac", "(Real a, Fractional b) => a -> b" )
    , ( "fmap", "(a -> b) -> f a -> f b" )
    , ( "(>>=)", "m a -> (a -> m b) -> m b" )
    , ( "(>>)", "m a -> m b -> m b" )
    , ( "return", "a -> m a" )
    , ( "fail", "String -> m a" )
    , ( "sequence", "Monad m => [m a] -> m [a]" )
    , ( "sequence_", "Monad m => [m a] -> m ()" )
    , ( "mapM", "Monad m => (a -> m b) -> [a] -> m [b]" )
    , ( "mapM_", "Monad m => (a -> m b) -> [a] -> m ()" )
    , ( "(=<<)", "Monad m => (a -> m b) -> m a -> m b" )
    , ( "id", "a -> a" )
    , ( "const", "a -> b -> a" )
    , ( "(.)", "(b -> c) -> (a -> b) -> a -> c" )
    , ( "flip", "(a -> b -> c) -> b -> a -> c" )
    , ( "seq", "a -> b -> b" )
    , ( "($)", "(a -> b) -> a -> b" )
    , ( "($!)", "(a -> b) -> a -> b" )
    , ( "(&&)", "Bool -> Bool -> Bool" )
    , ( "(||)", "Bool -> Bool -> Bool" )
    , ( "not", "Bool -> Bool" )
    , ( "otherwise", "Bool" )
    , ( "enumFrom c        = map toEnum [fromEnum c .. fromEnum (maxBound", "Char)]" )
    , ( "where lastChar", "Char" )
    , ( "maybe", "b -> (a -> b) -> Maybe a -> b" )
    , ( "either", "(a -> c) -> (b -> c) -> Either a b -> c" )
    , ( "numericEnumFrom", "(Fractional a) => a -> [a]" )
    , ( "numericEnumFromThen", "(Fractional a) => a -> a -> [a]" )
    , ( "numericEnumFromTo", "(Fractional a, Ord a) => a -> a -> [a]" )
    , ( "numericEnumFromThenTo", "(Fractional a, Ord a) => a -> a -> a -> [a]" )
    , ( "fst", "(a,b) -> a" )
    , ( "snd", "(a,b) -> b" )
    , ( "curry", "((a, b) -> c) -> a -> b -> c" )
    , ( "uncurry", "(a -> b -> c) -> ((a, b) -> c)" )
    , ( "until", "(a -> Bool) -> (a -> a) -> a -> a" )
    , ( "asTypeOf", "a -> a -> a" )
    , ( "error", "String -> a" )
    , ( "undefined", "a" )
    , ( "map", "(a -> b) -> [a] -> [b]" )
    , ( "(++)", "[a] -> [a] -> [a]" )
    , ( "filter", "(a -> Bool) -> [a] -> [a]" )
    , ( "concat", "[[a]] -> [a]" )
    , ( "concatMap", "(a -> [b]) -> [a] -> [b]" )
    , ( "head", "[a] -> a" )
    , ( "tail", "[a] -> [a]" )
    , ( "last", "[a] -> a" )
    , ( "init", "[a] -> [a]" )
    , ( "null", "[a] -> Bool" )
    , ( "length", "[a] -> Int" )
    , ( "(!!)", "[a] -> Int -> a" )
    , ( "foldl", "(a -> b -> a) -> a -> [b] -> a" )
    , ( "foldl1", "(a -> a -> a) -> [a] -> a" )
    , ( "scanl", "(a -> b -> a) -> a -> [b] -> [a]" )
    , ( "scanl1", "(a -> a -> a) -> [a] -> [a]" )
    , ( "foldr", "(a -> b -> b) -> b -> [a] -> b" )
    , ( "foldr1", "(a -> a -> a) -> [a] -> a" )
    , ( "scanr", "(a -> b -> b) -> b -> [a] -> [b]" )
    , ( "scanr1", "(a -> a -> a) -> [a] -> [a]" )
    , ( "iterate", "(a -> a) -> a -> [a]" )
    , ( "repeat", "a -> [a]" )
    , ( "replicate", "Int -> a -> [a]" )
    , ( "cycle", "[a] -> [a]" )
    , ( "take", "Int -> [a] -> [a]" )
    , ( "drop", "Int -> [a] -> [a]" )
    , ( "splitAt", "Int -> [a] -> ([a],[a])" )
    , ( "takeWhile", "(a -> Bool) -> [a] -> [a]" )
    , ( "dropWhile", "(a -> Bool) -> [a] -> [a]" )
    , ( "span", "(a -> Bool) -> [a] -> ([a],[a])" )
    , ( "break", "(a -> Bool) -> [a] -> ([a],[a])" )
    , ( "lines", "String -> [String]" )
    , ( "words", "String -> [String]" )
    , ( "unlines", "[String] -> String" )
    , ( "unwords", "[String] -> String" )
    , ( "reverse", "[a] -> [a]" )
    , ( "and", "[Bool] -> Bool" )
    , ( "or", "[Bool] -> Bool" )
    , ( "any", "(a -> Bool) -> [a] -> Bool" )
    , ( "all", "(a -> Bool) -> [a] -> Bool" )
    , ( "elem", "(Eq a) => a -> [a] -> Bool" )
    , ( "notElem", "(Eq a) => a -> [a] -> Bool" )
    , ( "lookup", "(Eq a) => a -> [(a,b)] -> Maybe b" )
    , ( "sum", "(Num a) => [a] -> a" )
    , ( "product", "(Num a) => [a] -> a" )
    , ( "maximum", "(Ord a) => [a] -> a" )
    , ( "minimum", "(Ord a) => [a] -> a" )
    , ( "zip", "[a] -> [b] -> [(a,b)]" )
    , ( "zip3", "[a] -> [b] -> [c] -> [(a,b,c)]" )
    , ( "zipWith", "(a->b->c) -> [a]->[b]->[c]" )
    , ( "zipWith3", "(a->b->c->d) -> [a]->[b]->[c]->[d]" )
    , ( "unzip", "[(a,b)] -> ([a],[b])" )
    , ( "unzip3", "[(a,b,c)] -> ([a],[b],[c])" )
    , ( "readsPrec", "Int -> ReadS a" )
    , ( "readList", "ReadS [a]" )
    , ( "showsPrec", "Int -> a -> ShowS" )
    , ( "show", "a -> String" )
    , ( "showList", "[a] -> ShowS" )
    , ( "reads", "(Read a) => ReadS a" )
    , ( "shows", "(Show a) => a -> ShowS" )
    , ( "read", "(Read a) => String -> a" )
    , ( "showChar", "Char -> ShowS" )
    , ( "showString", "String -> ShowS" )
    , ( "showParen", "Bool -> ShowS -> ShowS" )
    , ( "readParen", "Bool -> ReadS a -> ReadS a" )
    , ( "lex", "ReadS String" )
    , ( "ioError", "IOError -> IO a" )
    , ( "userError", "String -> IOError" )
    , ( "catch", "IO a -> (IOError -> IO a) -> IO a" )
    , ( "putChar", "Char -> IO ()" )
    , ( "putStr", "String -> IO ()" )
    , ( "putStrLn", "String -> IO ()" )
    , ( "print", "Show a => a -> IO ()" )
    , ( "getChar", "IO Char" )
    , ( "getLine", "IO String" )
    , ( "getContents", "IO String" )
    , ( "interact", "(String -> String) -> IO ()" )
    , ( "readFile", "FilePath -> IO String" )
    , ( "writeFile", "FilePath -> String -> IO ()" )
    , ( "appendFile", "FilePath -> String -> IO ()" )
    , ( "readIO", "Read a => String -> IO a" )
    , ( "readLn", "Read a => IO a" )
    ]
