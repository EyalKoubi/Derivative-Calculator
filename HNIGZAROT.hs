
import Data.Time.Clock

-- define function that check if the first letter is a specific letter
isFirstIsSomething :: [Char] -> Char -> Bool
isFirstIsSomething [] letter = False
isFirstIsSomething (a:_) letter = a == letter

-- define function that check if the char is present digit
isDigit :: Char -> Bool
isDigit x = (x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9')

isNumberWithCnt :: [Char] -> Int -> Int -> Bool -> Bool
isNumberWithCnt [] cnt dotCnt isFirst = not (cnt == 0)
isNumberWithCnt (a:ax) cnt dotCnt isFirst | isFirst && ax == [] = isDigit a
                                          | isFirst && a == '-' = isNumberWithCnt ax 0 0 True
                                          | isFirst && a == '.' = False  
                                          | (ax == []) = isDigit a
                                          | ((not (isDigit a)) && (not (a == '.'))) || ((a == '.') && (dotCnt == 1)) = False
                                          | a == '.' = isNumberWithCnt ax (cnt+1) (dotCnt+1) False
                                          | otherwise = isNumberWithCnt ax (cnt+1) dotCnt False

-- define function that check if the string is present number
-- we check for a number. Number is define as:
-- 1. all the chars are digit except of 0-1 dots
-- 2. The dot is not in the begining or the end of the string
-- 3. There is no leading zeros
-- 4. Number can start with '-'
isNumber :: [Char] -> Bool
isNumber str = isNumberWithCnt str 0 0 True

-- define function that check if the string starts with '(' and end with ')'
checkBrackets :: [Char] -> Bool -> Bool
checkBrackets [] _ = False
checkBrackets (a:ax) isFirst | isFirst && (a == '(') = checkBrackets ax False
                             | isFirst = False
                             | (ax == []) && (a == ')') = True
                             | otherwise = checkBrackets ax False

-- define function that cut the first letter from the string
cutFirst :: [Char] -> [Char]
cutFirst [] = ""
cutFirst (_:ax) = ax

-- define function that cut the first letter from the string and the last letter from the string
cutFirstAndLast :: [Char] -> Bool -> [Char] -> [Char]
cutFirstAndLast [] _ new = new
cutFirstAndLast (a:ax) isFirst new | isFirst = cutFirstAndLast ax False new
                                   | ax == [] = new
                                   | otherwise = cutFirstAndLast ax isFirst (new ++ [a])

-- define function that check if the char is present creation sign
isCreationSign :: Char -> Bool
isCreationSign letter = (letter == '+' || letter == '-' || letter == '*' || letter == '/' || letter == '^')

-- define function that handle brackets and returns (alpha,beta,@) tuple for alpha@beta
bracketsHandler :: [Char] -> Int -> [Char] -> ([Char], [Char], Char)
bracketsHandler [] _ left = (left,"",'@')
bracketsHandler (a:ax) cnt left | isCreationSign a && cnt == 0 = (left,ax,a)
                                | a == '(' = bracketsHandler ax (cnt+1) (left ++ [a])
                                | a == ')' = bracketsHandler ax (cnt-1) (left ++ [a])
                                | otherwise = bracketsHandler ax cnt (left ++ [a])

-- define function that get str from the tuple
getFromStrTuple :: ([Char], [Char], Char) -> Int -> [Char]
getFromStrTuple (str, _, _) 1 = str
getFromStrTuple (_, str, _) 2 = str

-- define function that returns the thisd argument from tuple (alpha,beta,@)
getSign :: ([Char], [Char], Char) -> Char
getSign (_, _, sign) = sign

-- define function that makes double from string
stringToDouble :: String -> Double
stringToDouble str = read str

-- define function that makes string from double
doubleToString :: Double -> String
doubleToString num = show num

-- define function that handle 2 strings and 1 sign (handle number case)
numbersHandeller :: [Char] -> [Char] -> Char -> [Char]
numbersHandeller firstStr secondStr sign | isNumber firstStr && isNumber secondStr && sign == '+' = doubleToString (stringToDouble firstStr + stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '-' = doubleToString (stringToDouble firstStr - stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '*' = doubleToString (stringToDouble firstStr * stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '/' = doubleToString (stringToDouble firstStr / stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '^' = doubleToString (stringToDouble firstStr ** stringToDouble secondStr)
                                         | otherwise = "(" ++ firstStr ++ [sign] ++ secondStr ++ ")"

-- define function that handle 2 strings and 1 sign (handle adish case)
adishHandeller :: [Char] -> [Char] -> [Char] -> [Char]
adishHandeller firstStr secondStr adish | firstStr == adish = secondStr
                                        | secondStr == adish = firstStr

-- define function that handle + diff
plusOrMinusHandller :: [Char] -> [Char] -> Char -> [Char]
plusOrMinusHandller firstStr secondStr sign | df == "0" || dg == "0" = adishHandeller df dg "0"
                                            | otherwise = numbersHandeller df dg sign
                                                where df = diff firstStr
                                                      dg = diff secondStr

-- define function that handle * diff
multiplyHandeller :: [Char] -> [Char] -> Char -> [Char]
multiplyHandeller firstStr secondStr sign | (df == "0" || secondStr == "0") && (firstStr == "0" || dg == "0") = "0"
                                          | (df == "0" || secondStr == "0") && (firstStr == "1" || dg == "1") = adishHandeller firstStr dg "1"
                                          | df == "0" || secondStr == "0" = numbersHandeller firstStr dg '*'
                                          | (firstStr == "0" || dg == "0") && (df == "1" || secondStr == "1") = adishHandeller df secondStr "1"
                                          | firstStr == "0" || dg == "0" = numbersHandeller df secondStr '*'
                                          | (df == "1" || secondStr == "1") && (firstStr == "1" || dg == "1") = numbersHandeller partOneAdish partTwoAdish sign
                                          | df == "1" || secondStr == "1" = numbersHandeller partOneAdish partTwoReg sign
                                          | firstStr == "1" || dg == "1" = numbersHandeller partOneReg partTwoAdish sign
                                          | otherwise = numbersHandeller partOneReg partTwoReg sign
                                            where df = diff firstStr
                                                  dg = diff secondStr
                                                  partOneAdish = adishHandeller df secondStr "1"
                                                  partTwoAdish = adishHandeller firstStr dg "1"
                                                  partOneReg = numbersHandeller df secondStr '*'
                                                  partTwoReg = numbersHandeller firstStr dg '*'

-- define function that handle / diff
divisionHandeller :: [Char] -> [Char] -> [Char]
divisionHandeller firstStr secondStr | secondStr == "0" = error "Math Error"
                                     | otherwise = numbersHandeller mone mechane '/'
                                        where
                                            mone = multiplyHandeller firstStr secondStr '-'
                                            mechane = numbersHandeller secondStr "2" '^'

-- define function that check if there is kan error
isLanError :: [Char] -> Bool
isLanError str | not (isNumber str) = False
               | stringToDouble str <= 0 = True
               | otherwise = False

-- define function that handle ^ diff - g`(x)*ln(f(x))
firstPartOfExpDiff :: [Char] -> [Char] -> [Char]
firstPartOfExpDiff dgx fx | isLanError fx = error "Math Error"
                          | dgx == "0" = "0"
                          | isNumber fx && (dgx == "1" || log(stringToDouble fx) == 1) = adishHandeller dgx (doubleToString $ log(stringToDouble fx)) "1"
                          | isNumber fx = numbersHandeller dgx (doubleToString $ log(stringToDouble fx)) '*'
                          | dgx == "1" = adishHandeller dgx lanFxStr "1"
                          | otherwise = numbersHandeller dgx lanFxStr '*'
                            where lanFxStr = "ln(" ++ fx ++ ")"

-- define function that handle ^ diff - (g(x)*f'(x))/f(x)
secondPartOfExpDiff :: [Char] -> [Char] -> [Char] -> [Char]
secondPartOfExpDiff gx fx dfx | fx == "0" = error "Math Error"
                              | (gx == "1" || dfx == "1") && fx == "1" = moneAdish
                              | gx == "1" || dfx == "1" = numbersHandeller moneAdish fx '/'
                              | fx == "1" = moneReg
                              | otherwise = numbersHandeller moneReg fx '/'
                               where
                                    moneAdish = adishHandeller gx dfx "1"
                                    moneReg = numbersHandeller gx dfx '*'

-- define function that handle ^ diff
exponentHandeller :: [Char] -> [Char] -> [Char]
exponentHandeller firstStr secondStr | (firstStr == "0" && secondStr == "0") = error "Math Error"
                                     | firstStr == "0" || secondStr == "0" || firstStr == "1" = "0"
                                     | secondStr == "1" = df
                                     | otherwise = numbersHandeller start end '*'
                                        where
                                            df = diff firstStr
                                            dg = diff secondStr
                                            start = numbersHandeller firstStr secondStr '^'
                                            firstExp = firstPartOfExpDiff dg firstStr
                                            secondExp = secondPartOfExpDiff secondStr firstStr df
                                            end = numbersHandeller firstExp secondExp '+'

-- define diff function
diff :: [Char] -> [Char]
diff str | str == "sin(x)" = "cos(x)"
         | str == "cos(x)" = "(-sin(x))"
         | str == "tg(x)" = "(1 / (cos(x) ** 2))"
         | str == "arcsin(x)" = "(1 / sqrt(1 - x ** 2))"
         | str == "arccos(x)" = "((-1) / sqrt(1 - x ** 2))"
         | str == "arctg(x)" = "(1 / (1 + x ** 2))"
         | str == "exp(x)" = "exp(x)"
         | str == "ln(x)" = "(1 / x)"
         | str == "x" = "1"
         | isNumber str = "0"
         | not (checkBrackets str True) = error "Syntax Error"
         | isFirstIsSomething withOutBrackets '-' && isNumber minusDiff = "-" ++ minusDiff
         | isFirstIsSomething withOutBrackets '-' = "(-" ++ minusDiff ++ ")"
         | getSign resTuple == '+' = plusOrMinusHandller firstPart secondPart '+'
         | getSign resTuple == '-' = plusOrMinusHandller firstPart secondPart '-'
         | getSign resTuple == '*' = multiplyHandeller firstPart secondPart '+'
         | getSign resTuple == '/' = divisionHandeller firstPart secondPart
         | getSign resTuple == '^' = exponentHandeller firstPart secondPart
         | otherwise = error "Syntax Error"
            where withOutBrackets = cutFirstAndLast str True ""
                  minusDiff = diff $ cutFirst withOutBrackets
                  resTuple = bracketsHandler withOutBrackets 0 ""
                  firstPart = getFromStrTuple resTuple 1
                  secondPart = getFromStrTuple resTuple 2

-- define eval function
eval :: [Char] -> Double -> Double
eval f x | f == "sin(x)" = sin x
         | f == "cos(x)" = cos x
         | f == "tan(x)" = tan x
         | f == "arcsin(x)" = asin x
         | f == "arccos(x)" = acos x
         | f == "arctan(x)" = atan x
         | f == "exp(x)" = exp x
         | f == "ln(x)" = log x
         | f == "(1 / (cos(x) ** 2))" = (1 / (cos x ** 2))
         | f == "(1 / sqrt(1 - x ** 2))" = (1 / sqrt(1 - x ** 2))
         | f == "((-1) / sqrt(1 - x ** 2))" = ((-1) / sqrt(1 - x ** 2))
         | f == "(1 / (1 + x ** 2))" = (1 / (1 + x ** 2))
         | f == "(1 / x)" = (1 / x)
         | isNumber f = stringToDouble f
         | f == "x" = x
         | not (checkBrackets f True) = error "Syntax Error"
         | isFirstIsSomething withOutBrackets '-' = minusDiff
         | getSign resTuple == '+' = eval firstPart x + eval secondPart x
         | getSign resTuple == '-' = eval firstPart x - eval secondPart x
         | getSign resTuple == '*' = eval firstPart x * eval secondPart x
         | getSign resTuple == '/' && secondPart == "0" = 0/0
         | getSign resTuple == '/' = eval firstPart x / eval secondPart x
         | getSign resTuple == '^' && firstPart == "0" && secondPart == "0" = error "Math Error"
         | getSign resTuple == '^' = eval firstPart x ** eval secondPart x
         | otherwise = error "Syntax Error"
            where minusDiff = (-1) * (eval (cutFirst withOutBrackets) x)
                  withOutBrackets = cutFirstAndLast f True ""
                  resTuple = bracketsHandler withOutBrackets 0 ""
                  firstPart = getFromStrTuple resTuple 1
                  secondPart = getFromStrTuple resTuple 2

-- define function that create string for testing
stringForTestCreator :: Int -> Bool -> [Char] -> [Char]
stringForTestCreator cnt isFirst res | cnt == 0 = res
                                     | isFirst = stringForTestCreator (cnt-1) False "x"
                                     | otherwise = stringForTestCreator (cnt-1) False ("(" ++ res ++ "*" ++ res ++ ")")

-- main function
main :: IO ()
main = do
    tt_start <- getCurrentTime
    let res1 =  diff $ stringForTestCreator 9 True ""
    let res2 = eval res1 1
    let res3 = eval (stringForTestCreator 9 True "") 1
    print res2
    print res3
    -- print $ eval (diff "(-(((x^ln(x))*sin(x))/(arccos(x)+(5*x))))") 0.5
    tt_end <- getCurrentTime
    print (diffUTCTime tt_end tt_start)
    