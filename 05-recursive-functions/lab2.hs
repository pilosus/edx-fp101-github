module Lab2 where

{-
<DISCLAIMER> For this lab, we provide a template file which contains
the signatures of the functions to be implemented, as well as some
definitions that will be needed in order to answer the final
question. We suggest you make use of this template when solving the
exercises.  </DISCLAIMER>

Have you ever wondered how websites validate your credit card number
when you shop online? They don’t check a massive database of numbers,
and they don’t use magic. In fact, most credit providers rely on a
checksum formula for distinguishing valid numbers from random
collection of digits (or typing mistakes).

In this lab, you will implement a validation algorithm for credit
cards. The algorithm follows these steps:

        1. Double the value of every second digit beginning with the
           rightmost.

        2. Add the digits of the doubled values and the undoubled
           digits from the original number.

        3. Calculate the modulus of the sum divided by 10.

If the result equals 0, then the number is valid. Here is an example
of the results of each step on the number 4012888888881881.

        In order to start with the rightmost digit, we produce a
reversed list of digits. Then, we double every second digit.

Result: [1,16,8,2,8,16,8,16,8,16,8,16,2,2,0,8].

    We sum all of the digits of the resulting list above. Note that we
must again split the elements of the list into their digits (e.g. 16
becomes [1, 6]).

Result: 90.

    Finally, we calculate the modulus of 90 over 10.

Result: 0.

Since the final value is 0, we know that the above number is a valid
credit card number. If we make a mistake in typing the credit card
number and instead provide 4012888888881891, then the result of the
last step is 2, proving that the number is invalid.

First we need to find the digits of a number. Define a function

toDigits :: Integer -> [Integer]

that takes a n :: Integer where n >= 0 and returns a list of the
digits of n. More precisely, toDigits should satisfy the following
properties, for all n :: Integer where n >= 0:

    eval (toDigits n) == n
    all (\d -> d >= 0 && d < 10) (toDigits n)
    length (show n) == length (toDigits n)

where eval is defined as follows:

    eval xs = foldl (\x y -> y + (10 * x)) 0 xs

Note: toDigits n should error when n < 0.

Now we need to reverse the digits of a number. Define a function

toDigitsRev :: Integer -> [Integer]

that takes a n :: Integer where n >= 0 and returns a list of the
digits of n in reverse order. More precisely, toDigitsRev should
satisfy the following properties, for all n :: Integer where n >= 0::

    n == evalRev (toDigitsRev n)
    all (\d -> d >= 0 && d < 10) (toDigitsRev n)
    length (toDigitsRev n) == length (show n)

where evalRev is defined as follows:

    evalRev xs = foldr (\x y -> x + (10 * y)) 0 xs

Note: toDigitsRev n should error when n < 0.

Once we have the digits in the proper order, we need to double every
other one. Define the function

doubleSecond :: [Integer] -> [Integer]

that doubles every second number in the input list.

Example:  The result of doubleSecond [8, 7, 6, 5] is [8, 14, 6, 10].

The output of doubleSecond has a mix of one-digit and two-digit
numbers. Define a function

sumDigits :: [Integer] -> Integer

to calculate the sum of all individual digits, even if a number in the
list has more than 2 digits.

Example:  The result of

    sumDigits [8,14,6,10] = 8 + (1 + 4) + 6 + (1 + 0) = 20.

    sumDigits [3, 9, 4, 15, 8] = 3 + 9 + 4 + (1 + 5) + 8 = 30

Define the function

isValid :: Integer -> Bool

that tells whether any input n :: Integer where n >= 0 could be a
valid credit card number, using the algorithm outlined in the
introduction.

Hint: make use of the functions defined in the previous exercises.

When you are finished with the above exercises, please proceed to the
last page for the final, graded exercises.

-}

------------------------------------------------------------------------------------------------------------------------------
-- Lab 2: Validating Credit Card Numbers
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0
-- ===================================

-- produce a list of digits of a given number

toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits n
  | n >= 10 = toDigits (n `div` 10) ++ [n `mod` 10]
  | n < 10 && n >= 0 = [n]
  | otherwise = error "Number must be non-negative"

-- foldl (\x y -> y + (10 * x)) 0 (toDigits n) == n
-- all (\d -> d >= 0 && d < 10) (toDigits n)
-- length (show n) == length (toDigits n)

-- ===================================
-- Ex. 1
-- ===================================

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- n == foldr (\x y -> x + (10 * y)) 0 (toDigitsRev n)
-- all (\d -> d >= 0 && d < 10) (toDigitsRev n)
-- length (toDigitsRev n) == length (show n)

-- ===================================
-- Ex. 2
-- ===================================

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = [if even pos then (x * 2) else x | (x, pos) <- zip xs [1..]]

-- ===================================
-- Ex. 3
-- ===================================

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)


-- ===================================
-- Ex. 4
-- ===================================

isValid :: Integer -> Bool
isValid n = ((sumDigits (doubleSecond (toDigitsRev n))) `mod` 10) == 0


-- ===================================
-- Ex. 5
-- ===================================
    
numValid :: [Integer] -> Integer
numValid xs = sum . map (\_ -> 1) $ filter isValid xs


creditcards :: [Integer]
creditcards = [ 4716347184862961,
                4532899082537349,
                4485429517622493,
                4320635998241421,
                4929778869082405,
                5256283618614517,
                5507514403575522,
                5191806267524120,
                5396452857080331,
                5567798501168013,
                6011798764103720,
                6011970953092861,
                6011486447384806,
                6011337752144550,
                6011442159205994,
                4916188093226163,
                4916699537435624,
                4024607115319476,
                4556945538735693,
                4532818294886666,
                5349308918130507,
                5156469512589415,
                5210896944802939,
                5442782486960998,
                5385907818416901,
                6011920409800508,
                6011978316213975,
                6011221666280064,
                6011285399268094,
                6011111757787451,
                4024007106747875,
                4916148692391990,
                4916918116659358,
                4024007109091313,
                4716815014741522,
                5370975221279675,
                5586822747605880,
                5446122675080587,
                5361718970369004,
                5543878863367027,
                6011996932510178,
                6011475323876084,
                6011358905586117,
                6011672107152563,
                6011660634944997,
                4532917110736356,
                4485548499291791,
                4532098581822262,
                4018626753711468,
                4454290525773941,
                5593710059099297,
                5275213041261476,
                5244162726358685,
                5583726743957726,
                5108718020905086,
                6011887079002610,
                6011119104045333,
                6011296087222376,
                6011183539053619,
                6011067418196187,
                4532462702719400,
                4420029044272063,
                4716494048062261,
                4916853817750471,
                4327554795485824,
                5138477489321723,
                5452898762612993,
                5246310677063212,
                5211257116158320,
                5230793016257272,
                6011265295282522,
                6011034443437754,
                6011582769987164,
                6011821695998586,
                6011420220198992,
                4716625186530516,
                4485290399115271,
                4556449305907296,
                4532036228186543,
                4916950537496300,
                5188481717181072,
                5535021441100707,
                5331217916806887,
                5212754109160056,
                5580039541241472,
                6011450326200252,
                6011141461689343,
                6011886911067144,
                6011835735645726,
                6011063209139742,
                379517444387209,
                377250784667541,
                347171902952673,
                379852678889749,
                345449316207827,
                349968440887576,
                347727987370269,
                370147776002793,
                374465794689268,
                340860752032008,
                349569393937707,
                379610201376008,
                346590844560212,
                376638943222680,
                378753384029375,
                348159548355291,
                345714137642682,
                347556554119626,
                370919740116903,
                375059255910682,
                373129538038460,
                346734548488728,
                370697814213115,
                377968192654740,
                379127496780069,
                375213257576161,
                379055805946370,
                345835454524671,
                377851536227201,
                345763240913232
              ]

