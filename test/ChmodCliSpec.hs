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
module ChmodCliSpec (
    spec
) where

import Test.Hspec
import Test.QuickCheck
import ChmodTypes 
import ChmodCli
import Data.List (intercalate, partition)
import Data.Maybe (isNothing)

parse :: String -> Maybe [Update]
parse str = do
  let parse = ChmodCli.parseUpdates str
  case parse of
    Left _ ->
      Nothing
    Right updates ->
      Just updates

type UGOA = Char
type EqualPlusMinus = Char
type RWXST = Char

genTarget :: Gen UGOA
genTarget = elements "ugoa"
genMethod :: Gen EqualPlusMinus
genMethod = elements "=+-"
genRwxst :: Gen RWXST
genRwxst = elements "rwxst"
genUpdate = (,,) <$> (listOf1 $ genTarget) <*> (genMethod) <*> (listOf genRwxst)
updateToStr :: ([UGOA], EqualPlusMinus, [RWXST]) -> String
updateToStr = (\(t, m, p) -> t <> [m] <> p)

makeExpectations :: (String, Char, String) -> [Update]
makeExpectations (targets, method, perms) = do
  concat $ map mkExpectations' targets
  where
    mkExpectations' t = case t of
      'a' -> makeExpectations ("ugo", method, perms)
      _   -> [mkExpect' t method perms]
    expectedTarget' 'u' = User
    expectedTarget' 'g' = Group
    expectedTarget' 'o' = Others
    expectedMethod' '=' = Set
    expectedMethod' '+' = Add
    expectedMethod' '-' = Remove
    expectedRWX' perms = do
      let r = 'r' `elem` perms
      let w = 'w' `elem` perms
      let x = 'x' `elem` perms
      RWX r w x
    expectedExt' User perms   = (Extended Suid $ 's' `elem` perms)
    expectedExt' Group perms  = (Extended Sgid $ 's' `elem` perms)
    expectedExt' Others perms = (Extended Sticky $ 't' `elem` perms)
    mkExpect' :: Char -> Char -> String -> Update
    mkExpect' target method perms =
      let
        t = expectedTarget' target
        m = expectedMethod' method
        ext = expectedExt' t perms
        rwx = expectedRWX' perms
      in Update t m (Permission t ext rwx)

prop_parseSingleUpdate :: Property
prop_parseSingleUpdate =
  forAll genUpdate (\update -> do
    let single = updateToStr update
    let (Just actual) = parse single
    let expectations = makeExpectations update 
    actual == expectations
  )

prop_parseMultipleUpdates :: Property
prop_parseMultipleUpdates =
  let genUpdates = listOf1 $ genUpdate in
  forAll (genUpdates) (\updates -> do
    let multi = intercalate "," $ map updateToStr updates
    let (Just actual) = parse multi
    let expectations = concat $ map makeExpectations updates
    actual == expectations
  )

prop_parseLeadingComma :: Property
prop_parseLeadingComma = 
  forAll genUpdate (\update -> do
    let valid = updateToStr update
    let invalid = "," <> valid
    let actual = parse invalid
    isNothing actual
  )

prop_parseTrailingComma :: Property
prop_parseTrailingComma = 
  forAll genUpdate (\update -> do
    let valid = updateToStr update
    let invalid = valid <> ","
    let actual = parse invalid
    isNothing actual
  )

spec :: Spec
spec = do
  describe "Parsing properties" $
    do it "prop_parseSingleUpdate"    prop_parseSingleUpdate
       it "prop_parseMultipleUpdates" prop_parseMultipleUpdates
       it "prop_parseLeadingComma"    prop_parseLeadingComma
       it "prop_parseTrailingComma"   prop_parseTrailingComma
       