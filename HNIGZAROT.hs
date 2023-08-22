
isNotEmptyStr :: [Char] -> Bool
isNotEmptyStr [] = False
isNotEmptyStr (a:ax) = True

tailOfStr :: [Char] -> Char -> Char
tailOfStr [] last = last
tailOfStr (a:ax) last = tailOfStr ax a

isNotLastIsSomething :: [Char] -> Char -> Bool
isNotLastIsSomething str letter | tailOfStr str '@' == letter = False
                                | otherwise = True

isLastIsSomething :: [Char] -> Char -> Bool
isLastIsSomething str letter | isNotLastIsSomething str letter = False
                              | otherwise = True

isNotFirstIsSomething :: [Char] -> Char -> Bool
isNotFirstIsSomething [] letter = True
isNotFirstIsSomething (a:ax) letter | a == letter = False
                                    | otherwise = True

isFirstIsSomething :: [Char] -> Char -> Bool
isFirstIsSomething str letter | isNotFirstIsSomething str letter = False
                              | otherwise = True

isLenIsNotOne :: [Char] -> Int -> Bool
isLenIsNotOne [] len | len == 1 = False
                     | otherwise = True
isLenIsNotOne (a:ax) len | len == 1 || len == 0 = isLenIsNotOne ax (len+1)
                         | otherwise = True

isNotZeroIsFirstAndItsNotZero :: [Char] -> Bool
isNotZeroIsFirstAndItsNotZero str | isLenIsNotOne str 0 && isFirstIsSomething str '0' = False
                                  | otherwise = True

isDigit :: Char -> Bool
isDigit x | x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9' = True
          | otherwise = False

isHaveJustOneOrlessPointAndOtherAreDigits :: [Char] -> Int -> Bool
isHaveJustOneOrlessPointAndOtherAreDigits [] cnt | cnt <= 1 = True
                                                 | otherwise = False
isHaveJustOneOrlessPointAndOtherAreDigits (a:ax) cnt | a == '.' && cnt == 0 = isHaveJustOneOrlessPointAndOtherAreDigits ax 1
                                               | isDigit a = isHaveJustOneOrlessPointAndOtherAreDigits ax cnt
                                               | otherwise = False

isNumber :: [Char] -> Bool
isNumber str | (isHaveJustOneOrlessPointAndOtherAreDigits str 0 && isNotFirstIsSomething str '.' && isNotZeroIsFirstAndItsNotZero str && isNotLastIsSomething str '.' && isNotEmptyStr str) || (isFirstIsSomething str '-' && isNumber (cutFirst str)) = True
             | otherwise = False

isNotNumber :: [Char] -> Bool
isNotNumber str | isNumber str = False
                | otherwise = True

isNotFirstIsSomethingOrNotLastIsSomthing :: [Char] -> Char -> Char -> Bool
isNotFirstIsSomethingOrNotLastIsSomthing str letterOne letterTwo | isFirstIsSomething str letterOne && isLastIsSomething str letterTwo = False
                                                                 | otherwise = True

cutFirst :: [Char] -> [Char]
cutFirst [] = ""
cutFirst (a:ax) = ax

cutFirstAndLast :: [Char] -> Bool -> [Char] -> [Char]
cutFirstAndLast [] isFirst new = new
cutFirstAndLast (a:ax) isFirst new | isFirst = cutFirstAndLast ax False new
                                   | ax == [] = cutFirstAndLast ax isFirst new
                                   | otherwise = cutFirstAndLast ax isFirst (new ++ [a])

isCreationSign :: Char -> Bool
isCreationSign letter | letter == '+' || letter == '-' || letter == '*' || letter == '/' || letter == '^' = True
                      | otherwise = False

bracketsHandler :: [Char] -> Bool -> Int -> [Char] -> [Char] -> Char -> ([Char], [Char], Char)
bracketsHandler [] isFirst cnt left right sign = (left,right,sign)
bracketsHandler (a:ax) isFirst cnt left right sign | isFirst && a == '(' = bracketsHandler ax False (cnt+1) (left ++ [a]) right a
                                                   | isFirst && a == ')' = bracketsHandler ax False (cnt-1) (left ++ [a]) right a
                                                   | isFirst = bracketsHandler ax False cnt (left ++ [a]) right a
                                                   | a == '(' = bracketsHandler ax False (cnt+1) (left ++ [a]) right a
                                                   | a == ')' = bracketsHandler ax False (cnt-1) (left ++ [a]) right a
                                                   | cnt == 0 && isCreationSign a = (left,ax,a)
                                                   | otherwise = bracketsHandler ax False cnt (left ++ [a]) right a

getFirstStr :: ([Char], [Char], Char) -> [Char]
getFirstStr (str, _, _) = str

getSecondStr :: ([Char], [Char], Char) -> [Char]
getSecondStr (_, str, _) = str

getSign :: ([Char], [Char], Char) -> Char
getSign (_, _, sign) = sign

stringToDouble :: String -> Double
stringToDouble str = read str

doubleToString :: Double -> String
doubleToString num = show num

numbersHandeller :: [Char] -> [Char] -> Char -> [Char]
numbersHandeller firstStr secondStr sign | isNumber firstStr && isNumber secondStr && sign == '+' = doubleToString (stringToDouble firstStr + stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '-' = doubleToString (stringToDouble firstStr - stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '*' = doubleToString (stringToDouble firstStr * stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '/' = doubleToString (stringToDouble firstStr / stringToDouble secondStr)
                                         | isNumber firstStr && isNumber secondStr && sign == '^' = doubleToString (stringToDouble firstStr ** stringToDouble secondStr)
                                         | otherwise = "(" ++ firstStr ++ [sign] ++ secondStr ++ ")"

adishHandeller :: [Char] -> [Char] -> [Char] -> [Char]
adishHandeller firstStr secondStr adish | firstStr == adish && secondStr == adish = adish
                                        | firstStr == adish = secondStr
                                        | secondStr == adish = firstStr

plusOrMinusHandller :: [Char] -> [Char] -> Char -> [Char]
plusOrMinusHandller firstStr secondStr sign | firstStr == "Syntacs Error" || secondStr == "Syntacs Error" || df == "Syntacs Error" || dg == "Syntacs Error" = "Syntacs Error"
                                            | firstStr == "Math Error" || secondStr == "Math Error" || df == "Math Error" || dg == "Math Error" = "Math Error"
                                            | df == "0" || dg == "0" = adishHandeller df dg "0"
                                            | otherwise = numbersHandeller df dg sign
                                                where df = diff firstStr
                                                      dg = diff secondStr

multiplyHandeller :: [Char] -> [Char] -> Char -> [Char]
multiplyHandeller firstStr secondStr sign | firstStr == "Syntacs Error" || secondStr == "Syntacs Error" || df == "Syntacs Error" || dg == "Syntacs Error" = "Syntacs Error"
                                          | firstStr == "Math Error" || secondStr == "Math Error" || df == "Math Error" || dg == "Math Error" = "Math Error"
                                          | (df == "0" || secondStr == "0") && (firstStr == "0" || dg == "0") = "0"
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

divisionHandeller :: [Char] -> [Char] -> [Char]
divisionHandeller firstStr secondStr | firstStr == "Syntacs Error" || secondStr == "Syntacs Error" || mone == "Syntacs Error" || mechane == "Syntacs Error" = "Syntacs Error"
                                     | secondStr == "0" || firstStr == "Math Error" || secondStr == "Math Error" || mone == "Math Error" || mechane == "Math Error" = "Math Error"
                                     | otherwise = numbersHandeller mone mechane '/'
                                        where
                                            mone = multiplyHandeller firstStr secondStr '-'
                                            mechane = numbersHandeller secondStr "2" '^'

isLanError :: [Char] -> Bool
isLanError str | isNotNumber str = False
               | stringToDouble str <= 0 = True
               | otherwise = False

firstPartOfExpDiff :: [Char] -> [Char] -> [Char]
firstPartOfExpDiff dgx fx | dgx == "Syntacs Error" || fx == "Syntacs Error" = "Syntacs Error"
                          | isLanError fx || dgx == "Math Error" || fx == "Math Error" = "Math Error"
                          | dgx == "0" = "0"
                          | isNumber fx && (dgx == "1" || (doubleToString $ log(stringToDouble fx)) == "1") = adishHandeller dgx (doubleToString $ log(stringToDouble fx)) "1"
                          | isNumber fx = numbersHandeller dgx (doubleToString $ log(stringToDouble fx)) '*'
                          | dgx == "1" = adishHandeller dgx lanFxStr "1"
                          | otherwise = numbersHandeller dgx lanFxStr '*'
                            where lanFxStr = "ln(" ++ fx ++ ")"

secondPartOfExpDiff :: [Char] -> [Char] -> [Char] -> [Char]
secondPartOfExpDiff gx fx dfx | gx == "Syntacs Error" || fx == "Syntacs Error" || dfx == "Syntacs Error" = "Syntacs Error"
                              | fx == "0" || gx == "Math Error" || fx == "Math Error" || dfx == "Math Error" = "Math Error"
                              | (gx == "1" || dfx == "1") && fx == "1" = moneAdish
                              | gx == "1" || dfx == "1" = numbersHandeller moneAdish fx '/'
                              | fx == "1" = moneReg
                              | otherwise = numbersHandeller moneReg fx '/'
                               where
                                    moneAdish = adishHandeller gx dfx "1"
                                    moneReg = numbersHandeller gx dfx '*'

exponentHandeller :: [Char] -> [Char] -> [Char]
exponentHandeller firstStr secondStr | firstStr == "Syntacs Error" || secondStr == "Syntacs Error" || df == "Syntacs Error" || dg == "Syntacs Error" = "Syntacs Error"
                                     | (firstStr == "0" && secondStr == "0") || firstStr == "Math Error" || secondStr == "Math Error" || df == "Math Error" || dg == "Math Error" = "Math Error"
                                     | firstStr == "0" = "0"
                                     | secondStr == "0" = "1"
                                     | firstStr == "1" = "1"
                                     | secondStr == "1" = firstStr
                                     | otherwise = numbersHandeller start end '*'
                                        where
                                            df = diff firstStr
                                            dg = diff secondStr
                                            start = numbersHandeller firstStr secondStr '^'
                                            firstExp = firstPartOfExpDiff dg firstStr
                                            secondExp = secondPartOfExpDiff secondStr firstStr df
                                            end = numbersHandeller firstExp secondExp '+'


diff :: [Char] -> [Char]
diff str | str == "sin(x)" = "cos(x)"
         | str == "cos(x)" = "(-sin(x))"
         | str == "tg(x)" = "(1 / (cos(x) ** 2))"
         | str == "arcsin(x)" = "(1 / sqrt(1 - x ** 2))"
         | str == "arccos(x)" = "((-1) / sqrt(1 - x ** 2))"
         | str == "arctg(x)" = "(1 / (1 + x ** 2))"
         | str == "exp(x)" = "exxp(x)"
         | str == "ln(x)" = "(1 / x)"
         | str == "x" = "1"
         | isNumber str = "0"
         | isNotFirstIsSomethingOrNotLastIsSomthing str '(' ')' = "Syntacs Error"
         | isFirstIsSomething withOutStartAndEnd '-' && minusRes == "Math Error" = "Math Error"
         | isFirstIsSomething withOutStartAndEnd '-' && minusRes == "Syntacs Error" = "Syntacs Error"
         | isFirstIsSomething withOutStartAndEnd '-' = "(-" ++ minusRes ++ ")"
         | getSign resTuple == '+' = plusOrMinusHandller firstPart secondPart '+'
         | getSign resTuple == '-' = plusOrMinusHandller firstPart secondPart '-'
         | getSign resTuple == '*' = multiplyHandeller firstPart secondPart '+'
         | getSign resTuple == '/' = divisionHandeller firstPart secondPart
         | getSign resTuple == '^' = exponentHandeller firstPart secondPart
         | otherwise = "Syntacs Error"
            where minusRes = diff $ cutFirst $ cutFirstAndLast str True ""
                  withOutStartAndEnd = cutFirstAndLast str True ""
                  resTuple = bracketsHandler withOutStartAndEnd True 0 "" "" '@'
                  firstPart = getFirstStr resTuple
                  secondPart = getSecondStr resTuple

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
         | f == "Math Error" || f == "Syntacs Error" || isNotFirstIsSomethingOrNotLastIsSomthing f '(' ')' = 0/0
         | isFirstIsSomething withOutStartAndEnd '-' = minusRes
         | getSign resTuple == '+' = eval firstPart x + eval secondPart x
         | getSign resTuple == '-' = eval firstPart x - eval secondPart x
         | getSign resTuple == '*' = eval firstPart x * eval secondPart x
         | getSign resTuple == '/' && secondPart == "0" = 0/0
         | getSign resTuple == '/' = eval firstPart x / eval secondPart x
         | getSign resTuple == '^' && firstPart == "0" && secondPart == "0" = 0/0
         | getSign resTuple == '^' = eval firstPart x ** eval secondPart x
         | otherwise = 9
            where minusRes = (-1) * (eval (cutFirst withOutStartAndEnd) x)
                  withOutStartAndEnd = cutFirstAndLast f True ""
                  resTuple = bracketsHandler withOutStartAndEnd True 0 "" "" '@'
                  firstPart = getFirstStr resTuple
                  secondPart = getSecondStr resTuple

main :: IO ()
main = do
    print $ eval (diff "(((x^ln(x))*sin(x))/(arccos(x)+(5*x)))") 0.5
