module Tester where

import Test.QuickCheck


trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose (False, True)


genBool' :: Gen Bool
genBool' = elements [False, True]


genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]


genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)


genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]


