{-
Copyright (c) 2022 Joe Haw

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module ChmodTypesSpec (
    spec
) where

import Test.Hspec
import Test.QuickCheck
import ChmodTypes

instance Arbitrary Extended where
  arbitrary = do
    v <- elements [True, False]
    k <- elements [Suid, Sgid, Sticky]
    return $ Extended k v

instance Arbitrary RWX where
  arbitrary = do
    r <- elements [True, False]
    w <- elements [True, False]
    x <- elements [True, False]
    return $ RWX r w x

instance Arbitrary Permission where
  arbitrary = do
      kind <- elements [User, Group, Others]
      ext <- gen_ext'
      rwx <- gen_rwx'
      return $ (Permission User ext rwx)
    where
      gen_ext' :: Gen Extended
      gen_ext' = arbitrary
      gen_rwx' :: Gen RWX
      gen_rwx' = arbitrary

prop_rwxOr :: RWX -> RWX -> Bool 
prop_rwxOr lhs@(RWX lr lw lx) rhs@(RWX rr rw rx) =
  let
    expected = RWX
      (lr || rr)
      (lw || rw)
      (lx || rx)
    actual = (lhs <||> rhs)
  in actual == expected

prop_rwxAnd :: RWX -> RWX -> Bool 
prop_rwxAnd lhs@(RWX lr lw lx) rhs@(RWX rr rw rx) =
  let
    expected = RWX
      (lr && rr)
      (lw && rw)
      (lx && rx)
    actual = (lhs <&&> rhs)
  in actual == expected

prop_rwxInvert :: RWX -> Bool 
prop_rwxInvert rwx@(RWX r w x) =
  let
    expected = RWX
      (not r)
      (not w)
      (not x)
    actual = invert $ rwx
  in actual == expected

prop_extOr :: Extended -> Extended -> Bool 
prop_extOr lhs@(Extended k lval) rhs@(Extended _ rval) =
  let
    expected = Extended k
      (lval || rval)
    actual = (lhs <||> rhs)
  in actual == expected

prop_extAnd :: Extended -> Extended -> Bool 
prop_extAnd lhs@(Extended k lval) rhs@(Extended _ rval) =
  let
    expected = Extended k
      (lval && rval)
    actual = (lhs <&&> rhs)
  in actual == expected

prop_extInvert :: Extended -> Bool
prop_extInvert ext@(Extended k val) =
  let
    expected = (Extended k (not val))
    actual = (invert $ ext)
  in actual == expected

prop_permOr :: Permission -> Permission -> Bool
prop_permOr lhs@(Permission k lext lrwx) rhs@(Permission _ rext rrwx) = 
  let
    expected = Permission k (lext <||> rext) (lrwx <||> rrwx)
    actual = (lhs <||> rhs)
  in actual == expected

prop_permAnd :: Permission -> Permission -> Bool
prop_permAnd lhs@(Permission k lext lrwx) rhs@(Permission _ rext rrwx) = 
  let
    expected = Permission k (lext <&&> rext) (lrwx <&&> rrwx)
    actual = (lhs <&&> rhs)
  in actual == expected

prop_permInvert :: Permission -> Bool
prop_permInvert perm@(Permission k ext rwx) = 
  let
    expected = Permission k (invert ext) (invert rwx)
    actual = (invert $ perm)
  in actual == expected

spec :: Spec
spec = do
  describe "RWX Properties" $
    do it "prop_rwxOr" $ property prop_rwxOr
       it "prop_rwxAnd" $ property prop_rwxAnd
       it "prop_rwxInvert" $ property prop_rwxInvert
  describe "Extended Properties" $
    do it "prop_extOr" $ property prop_extOr
       it "prop_extAnd" $ property prop_extAnd
       it "prop_extInvert" $ property prop_extInvert
  describe "Permission Properties" $
    do it "prop_permOr" $ property prop_permOr
       it "prop_permAnd" $ property prop_permAnd
       it "prop_permInvert" $ property prop_permInvert
