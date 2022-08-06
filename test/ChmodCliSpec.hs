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

type UGO = Char
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

prop_updateTarget :: UGO -> Update -> Bool
prop_updateTarget 'u' (Update User _ _) = True
prop_updateTarget 'g' (Update Group _ _) = True
prop_updateTarget 'o' (Update Others _ _) = True
prop_updateTarget _ _ = False

prop_updateMethod :: EqualPlusMinus -> Update -> Bool
prop_updateMethod '=' (Update _ Set _) = True
prop_updateMethod '+' (Update _ Add _) = True
prop_updateMethod '-' (Update _ Remove _) = True
prop_updateMethod _ _ = False

prop_permsExtFor :: Target -> [RWXST] -> Extended -> Bool
prop_permsExtFor User p (Extended Suid val) = 
  val == ('s' `elem` p)
prop_permsExtFor Group p (Extended Sgid val) = 
  val == ('s' `elem` p)
prop_permsExtFor Others p (Extended Sticky val) = 
  val == ('t' `elem` p)

prop_permsRWX :: [RWXST] -> RWX -> Bool
prop_permsRWX p (RWX r w x) =
  and [
    r == ('r' `elem` p),
    w == ('w' `elem` p),
    x == ('x' `elem` p)
  ]

prop_updatePerms :: [RWXST] -> Update -> Bool
prop_updatePerms p (Update t _ (Permission t_ ext rwx)) =
  and [
    t == t_,
    prop_permsExtFor t p ext,
    prop_permsRWX p rwx
  ]

prop_updateFor :: UGO -> EqualPlusMinus -> [RWXST] -> Update -> Bool
prop_updateFor t m p u = 
  and [
    prop_updateTarget t u,
    prop_updateMethod m u
  ]

prop_updatesFor :: [([UGOA], EqualPlusMinus, [RWXST])] -> [Update] -> Bool
prop_updatesFor ((('a' : ts), m, p) : xs) updates =
  prop_updatesFor ([("ugo" ++ ts, m, p)] ++ xs) updates
prop_updatesFor ((t : ts, m, p) : xs) (u : updates) = 
  prop_updateFor t m p u
    && prop_updatesFor ([(ts, m, p)] ++ xs) updates
prop_updatesFor (([], _, _) : xs) updates =
  prop_updatesFor xs updates

prop_updatesFor xs []
  | length(xs) > 0
  = False
prop_updatesFor [] updates
  | length(updates) > 0
  = False
prop_updatesFor [] [] = True

prop_parseSingleUpdate :: Property
prop_parseSingleUpdate =
  forAll genUpdate (\update -> do
    let single = updateToStr update
    let (Just actual) = parse single
    prop_updatesFor [update] actual
  )

prop_parseMultipleUpdates :: Property
prop_parseMultipleUpdates =
  let genUpdates = listOf1 $ genUpdate in
  forAll (genUpdates) (\updates -> do
    let multi = intercalate "," $ map updateToStr updates
    let (Just actual) = parse multi
    prop_updatesFor updates actual
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
       