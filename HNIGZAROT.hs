
-- define function that check if the string is empty
isNotEmptyStr :: [Char] -> Bool
isNotEmptyStr [] = False
isNotEmptyStr (a:ax) = True

-- define function that return the last letter of the string
tailOfStr :: [Char] -> Char -> Char
tailOfStr [] last = last
tailOfStr (a:ax) last = tailOfStr ax a

-- define function that check if the last letter is not a specific letter
isNotLastIsSomething :: [Char] -> Char -> Bool
isNotLastIsSomething str letter = not (tailOfStr str '@' == letter)

-- define function that check if the last letter is a specific letter
isLastIsSomething :: [Char] -> Char -> Bool
isLastIsSomething str letter = not (isNotLastIsSomething str letter)

-- define function that check if the first letter is not a specific letter
isNotFirstIsSomething :: [Char] -> Char -> Bool
isNotFirstIsSomething [] letter = True
isNotFirstIsSomething (a:ax) letter = not (a == letter)

-- define function that check if the first letter is a specific letter
isFirstIsSomething :: [Char] -> Char -> Bool
isFirstIsSomething str letter = not (isNotFirstIsSomething str letter)

-- define function that check if the string length is not 1
isLenIsNotOne :: [Char] -> Int -> Bool
isLenIsNotOne [] len = not (len == 1)
isLenIsNotOne (a:ax) len | len == 0 = isLenIsNotOne ax (len+1)
                         | otherwise = True

-- define function that check if not ( (the string length is not 1) and (the first letter is a specific letter) )
-- for checking if the string is with zero-leading-char
isNotZeroIsFirstAndItsNotZero :: [Char] -> Bool
isNotZeroIsFirstAndItsNotZero str = not (isLenIsNotOne str 0 && isFirstIsSomething str '0')

-- define function that check if the char is present digit
isDigit :: Char -> Bool
isDigit x = (x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9')

-- define function that check if the string has just digits and only 1 dot
isHaveJustOneOrlessPointAndOtherAreDigits :: [Char] -> Int -> Bool
isHaveJustOneOrlessPointAndOtherAreDigits [] cnt = (cnt <= 1)
isHaveJustOneOrlessPointAndOtherAreDigits (a:ax) cnt | a == '.' && cnt == 0 = isHaveJustOneOrlessPointAndOtherAreDigits ax 1
                                                     | isDigit a = isHaveJustOneOrlessPointAndOtherAreDigits ax cnt
                                                     | otherwise = False

-- define function that check if the string is present number
-- we check for a number. Number is define as:
-- 1. all the chars are digit except of 0-1 dots
-- 2. The dot is not in the begining or the end of the string
-- 3. There is no leading zeros
-- 4. Number can start with '-'
isNumber :: [Char] -> Bool
isNumber str = (isHaveJustOneOrlessPointAndOtherAreDigits str 0 && isNotFirstIsSomething str '.' && isNotZeroIsFirstAndItsNotZero str && isNotLastIsSomething str '.' && isNotEmptyStr str) || (isFirstIsSomething str '-' && isNumber (cutFirst str))

-- define function that check if the string is not present number
isNotNumber :: [Char] -> Bool
isNotNumber str = not (isNumber str)

-- define function that check if not ( (the first letter is specific letter) and (the last letter is specific letter) )
-- for future use to see if we have starting and ending brackets (in the string)
isNotFirstIsSomethingOrNotLastIsSomthing :: [Char] -> Char -> Char -> Bool
isNotFirstIsSomethingOrNotLastIsSomthing str letterOne letterTwo = not (isFirstIsSomething str letterOne && isLastIsSomething str letterTwo)

-- define function that cut the first letter from the string
cutFirst :: [Char] -> [Char]
cutFirst [] = ""
cutFirst (a:ax) = ax

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
bracketsHandler :: [Char] -> Int -> [Char] -> [Char] -> ([Char], [Char], Char)
bracketsHandler [] _ left right = (left,right,'@')
bracketsHandler (a:ax) cnt left right | isCreationSign a && cnt == 0 = (left,ax,a)
                                      | a == '(' = bracketsHandler ax (cnt+1) (left ++ [a]) right
                                      | a == ')' = bracketsHandler ax (cnt-1) (left ++ [a]) right
                                      | otherwise = bracketsHandler ax cnt (left ++ [a]) right

-- define function that returns the first argument from tuple (alpha,beta,@)
getFirstStr :: ([Char], [Char], Char) -> [Char]
getFirstStr (str, _, _) = str

-- define function that returns the second argument from tuple (alpha,beta,@)
getSecondStr :: ([Char], [Char], Char) -> [Char]
getSecondStr (_, str, _) = str

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
plusOrMinusHandller firstStr secondStr sign | firstStr == "Syntax Error" || secondStr == "Syntax Error" || df == "Syntax Error" || dg == "Syntax Error" = "Syntax Error"
                                            | firstStr == "Math Error" || secondStr == "Math Error" || df == "Math Error" || dg == "Math Error" = "Math Error"
                                            | df == "0" || dg == "0" = adishHandeller df dg "0"
                                            | otherwise = numbersHandeller df dg sign
                                                where df = diff firstStr
                                                      dg = diff secondStr

-- define function that handle * diff
multiplyHandeller :: [Char] -> [Char] -> Char -> [Char]
multiplyHandeller firstStr secondStr sign | firstStr == "Syntax Error" || secondStr == "Syntax Error" || df == "Syntax Error" || dg == "Syntax Error" = "Syntax Error"
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

-- define function that handle / diff
divisionHandeller :: [Char] -> [Char] -> [Char]
divisionHandeller firstStr secondStr | firstStr == "Syntax Error" || secondStr == "Syntax Error" || mone == "Syntax Error" || mechane == "Syntax Error" = "Syntax Error"
                                     | secondStr == "0" || firstStr == "Math Error" || secondStr == "Math Error" || mone == "Math Error" || mechane == "Math Error" = "Math Error"
                                     | otherwise = numbersHandeller mone mechane '/'
                                        where
                                            mone = multiplyHandeller firstStr secondStr '-'
                                            mechane = numbersHandeller secondStr "2" '^'

-- define function that check if there is kan error
isLanError :: [Char] -> Bool
isLanError str | isNotNumber str = False
               | stringToDouble str <= 0 = True
               | otherwise = False

-- define function that handle ^ diff - g`(x)*ln(f(x))
firstPartOfExpDiff :: [Char] -> [Char] -> [Char]
firstPartOfExpDiff dgx fx | dgx == "Syntax Error" || fx == "Syntax Error" = "Syntax Error"
                          | isLanError fx || dgx == "Math Error" || fx == "Math Error" = "Math Error"
                          | dgx == "0" = "0"
                          | isNumber fx && (dgx == "1" || log(stringToDouble fx) == 1) = adishHandeller dgx (doubleToString $ log(stringToDouble fx)) "1"
                          | isNumber fx = numbersHandeller dgx (doubleToString $ log(stringToDouble fx)) '*'
                          | dgx == "1" = adishHandeller dgx lanFxStr "1"
                          | otherwise = numbersHandeller dgx lanFxStr '*'
                            where lanFxStr = "ln(" ++ fx ++ ")"

-- define function that handle ^ diff - (g(x)*f'(x))/f(x)
secondPartOfExpDiff :: [Char] -> [Char] -> [Char] -> [Char]
secondPartOfExpDiff gx fx dfx | gx == "Syntax Error" || fx == "Syntax Error" || dfx == "Syntax Error" = "Syntax Error"
                              | fx == "0" || gx == "Math Error" || fx == "Math Error" || dfx == "Math Error" = "Math Error"
                              | (gx == "1" || dfx == "1") && fx == "1" = moneAdish
                              | gx == "1" || dfx == "1" = numbersHandeller moneAdish fx '/'
                              | fx == "1" = moneReg
                              | otherwise = numbersHandeller moneReg fx '/'
                               where
                                    moneAdish = adishHandeller gx dfx "1"
                                    moneReg = numbersHandeller gx dfx '*'

-- define function that handle ^ diff
exponentHandeller :: [Char] -> [Char] -> [Char]
exponentHandeller firstStr secondStr | firstStr == "Syntax Error" || secondStr == "Syntax Error" || df == "Syntax Error" || dg == "Syntax Error" = "Syntax Error"
                                     | (firstStr == "0" && secondStr == "0") || firstStr == "Math Error" || secondStr == "Math Error" || df == "Math Error" || dg == "Math Error" = "Math Error"
                                     | firstStr == "0" = "0"
                                     | secondStr == "0" = "0"
                                     | firstStr == "1" = "0"
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
         | str == "exp(x)" = "exxp(x)"
         | str == "ln(x)" = "(1 / x)"
         | str == "x" = "1"
         | isNumber str = "0"
         | isNotFirstIsSomethingOrNotLastIsSomthing str '(' ')' = "Syntax Error"
         | isFirstIsSomething withOutBrackets '-' && minusDiff == "Math Error" = "Math Error"
         | isFirstIsSomething withOutBrackets '-' && minusDiff == "Syntax Error" = "Syntax Error"
         | isFirstIsSomething withOutBrackets '-' && isNumber minusDiff = "-" ++ minusDiff
         | isFirstIsSomething withOutBrackets '-' = "(-" ++ minusDiff ++ ")"
         | getSign resTuple == '+' = plusOrMinusHandller firstPart secondPart '+'
         | getSign resTuple == '-' = plusOrMinusHandller firstPart secondPart '-'
         | getSign resTuple == '*' = multiplyHandeller firstPart secondPart '+'
         | getSign resTuple == '/' = divisionHandeller firstPart secondPart
         | getSign resTuple == '^' = exponentHandeller firstPart secondPart
         | otherwise = "Syntax Error"
            where withOutBrackets = cutFirstAndLast str True ""
                  minusDiff = diff $ cutFirst withOutBrackets
                  resTuple = bracketsHandler withOutBrackets 0 "" ""
                  firstPart = getFirstStr resTuple
                  secondPart = getSecondStr resTuple

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
         | f == "Math Error" || f == "Syntax Error" || isNotFirstIsSomethingOrNotLastIsSomthing f '(' ')' = 0/0
         | isFirstIsSomething withOutBrackets '-' = minusDiff
         | getSign resTuple == '+' = eval firstPart x + eval secondPart x
         | getSign resTuple == '-' = eval firstPart x - eval secondPart x
         | getSign resTuple == '*' = eval firstPart x * eval secondPart x
         | getSign resTuple == '/' && secondPart == "0" = 0/0
         | getSign resTuple == '/' = eval firstPart x / eval secondPart x
         | getSign resTuple == '^' && firstPart == "0" && secondPart == "0" = 0/0
         | getSign resTuple == '^' = eval firstPart x ** eval secondPart x
         | otherwise = 0/0
            where minusDiff = (-1) * (eval (cutFirst withOutBrackets) x)
                  withOutBrackets = cutFirstAndLast f True ""
                  resTuple = bracketsHandler withOutBrackets 0 "" ""
                  firstPart = getFirstStr resTuple
                  secondPart = getSecondStr resTuple

-- define function that create string for testing
stringForTestCreator :: Int -> Bool -> [Char] -> [Char]
stringForTestCreator cnt isFirst res | cnt == 0 = res
                                     | isFirst = stringForTestCreator (cnt-1) False "x"
                                     | otherwise = stringForTestCreator (cnt-1) False ("(" ++ res ++ "*" ++ res ++ ")")

-- main function
main :: IO ()
main = do
    print $ eval (diff $ stringForTestCreator 9 True "") 1
    print $ eval (diff "(((x^ln(x))*sin(x))/(arccos(x)+(5*x)))") 0.5