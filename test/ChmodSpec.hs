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
module ChmodSpec (
    spec
) where
import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck ( choose, chooseInt, vectorOf, forAll, Gen, Property, oneof, Testable (property), elements, listOf, listOf1, shuffle )
import Chmod
import Data.Bits (shiftL, shiftR, testBit, bit, (.&.))
import ChmodTypes
import qualified Data.Map as Map

data TagMode = ModeExtended | ModeUser | ModeGroup | ModeOthers
tagModes' xs = zip xs [ModeUser, ModeGroup, ModeOthers, ModeExtended]
type Mode = Int
type ModesInt = Int

sumModes' xs = do
  sum $ map (\(mode, tag) -> 
    let shift =
          case tag of
          ModeUser -> 0
          ModeGroup -> 3
          ModeOthers -> 6
          ModeExtended -> 9
    in
    mode `shiftL` shift) xs

genMode :: Gen Mode
genMode = chooseInt (0,7)
genModes :: Int -> Gen [Mode]
genModes n = vectorOf n genMode
genTaggedModes :: Int -> Gen [(Mode, TagMode)]
genTaggedModes n = fmap tagModes' (genModes n)
genModesInt :: Int -> Gen ModesInt
genModesInt n = fmap sumModes' (genTaggedModes n)

genExt :: Target -> Gen Extended
genExt target = do
  val <- choose(True, False)
  return $ case target of
    User -> Extended Suid val
    Group -> Extended Sgid val
    Others -> Extended Sticky val

genRWX :: Gen RWX
genRWX = do
  r <- choose(True, False)
  w <- choose(True, False)
  x <- choose(True, False)
  return $ RWX r w x

genPerm :: Target -> Gen Permission
genPerm target = do
  ext <- genExt target
  rwx <- genRWX
  return $ Permission target ext rwx

genPermsMap :: Gen (Map.Map Target Permission)
genPermsMap = do
  user <- genPerm User
  group <- genPerm Group
  others <- genPerm Others
  return $ Map.fromList([
      (User, user),
      (Group, group),
      (Others, others)
    ]) 

genUpdateFor :: Target -> Method -> Gen Update
genUpdateFor t m = do
  p <- genPerm t
  return $ Update t m p

genUpdate :: Method -> Gen Update
genUpdate m = do
  t <- elements [User, Group, Others]
  genUpdateFor t m

genUpdateWith :: [Method] -> Gen Update
genUpdateWith ms = do
  m <- elements ms
  genUpdate m

genUpdatesWith :: [Method] -> Gen [Update]
genUpdatesWith ms = listOf $ genUpdateWith ms

prop_bitSet :: Bool -> Int -> Int -> Bool
prop_bitSet True value bit = (value .&. bit) /= 0
prop_bitSet False value bit = not (prop_bitSet True value bit)

relation_ReadMode :: RWX -> Int -> Bool
relation_ReadMode (RWX r _ _) mode = prop_bitSet r mode 4
relation_WriteMode :: RWX -> Int -> Bool
relation_WriteMode (RWX _ w _) mode = prop_bitSet w mode 2
relation_ExexMode :: RWX -> Int -> Bool
relation_ExexMode (RWX _ _ x) mode = prop_bitSet x mode 1

prop_rwxAndMode :: RWX -> Int -> Bool
prop_rwxAndMode rwx mode =
  and [
    relation_ReadMode rwx mode,
    relation_WriteMode rwx mode,
    relation_ExexMode rwx mode
  ]

relation_RwxMode :: Target -> RWX -> Mode -> Bool
relation_RwxMode User rwx mode   = prop_rwxAndMode rwx (mode `shiftR` 6)
relation_RwxMode Group rwx mode  = prop_rwxAndMode rwx (mode `shiftR` 3)
relation_RwxMode Others rwx mode = prop_rwxAndMode rwx (mode `shiftR` 0) 

relation_ExtMode :: Target -> Extended -> Mode -> Bool
relation_ExtMode User (Extended Suid val) mode = 
  prop_bitSet val (mode `shiftR` 9) 4
relation_ExtMode Group (Extended Sgid val) mode = 
  prop_bitSet val (mode `shiftR` 9) 2
relation_ExtMode Others (Extended Sticky val) mode = 
  prop_bitSet val (mode `shiftR` 9) 1

relation_PermMode :: Permission -> ModesInt -> Bool
relation_PermMode (Permission t ext rwx) mode = 
  and [
    relation_ExtMode t ext mode,
    relation_RwxMode t rwx mode
  ]

prop_fromModeAndBack :: Property
prop_fromModeAndBack = 
  let gen = oneof [genModesInt 4, genModesInt 3] in
  forAll (gen) (\mode ->
    mode == (toMode . fromMode) mode
  )

prop_toModeAndBack :: Property
prop_toModeAndBack = 
  forAll (genPermsMap) (\perms ->
    perms == (fromMode . toMode) perms
  )

prop_toMode :: Property
prop_toMode =
  forAll (genPermsMap) (\perms ->
    let
      mode = toMode perms
      (Just user)   = Map.lookup User perms
      (Just group)  = Map.lookup Group perms
      (Just others) = Map.lookup Others perms
    in
    and [
      relation_PermMode user mode,
      relation_PermMode group mode,
      relation_PermMode others mode
    ]
  )

prop_fromMode :: Property
prop_fromMode =
  let gen = oneof [genModesInt 4, genModesInt 3] in
  forAll gen (\mode ->
    let
      perms = fromMode mode
      (Just user)   = Map.lookup User perms
      (Just group)  = Map.lookup Group perms
      (Just others) = Map.lookup Others perms
    in
    and [
      relation_PermMode user mode,
      relation_PermMode group mode,
      relation_PermMode others mode
    ]
  )

applyUpdate :: Permission -> Update -> Permission
applyUpdate a (Update _ m b) =
  apply' m a b
  where
    apply' :: Method -> Permission -> Permission -> Permission
    apply' Set a b = b
    apply' Add a b = (a <||> b)
    apply' Remove a b = (a <&&> (invert b))

applyUpdatesFor :: Target -> Map.Map Target Permission -> [Update] -> Permission
applyUpdatesFor t perms updates = 
  let
    us = filter (\(Update t_ _ _) -> t == t_) updates
    (Just perm) = Map.lookup t perms
  in
    foldl applyUpdate perm us

prop_applyUpdates :: Property
prop_applyUpdates = 
  let 
    gen = (,) <$> genPermsMap <*> (genUpdatesWith [Set, Add, Remove])
  in
  forAll (gen) (\(perms, updates) ->
    let
      actual = applyUpdates perms updates
      (Just actual_user)    = Map.lookup User   actual
      (Just actual_group)   = Map.lookup Group  actual
      (Just actual_others)  = Map.lookup Others actual
    in
    and [
      actual_user   == applyUpdatesFor User    perms updates,
      actual_group  == applyUpdatesFor Group   perms updates,
      actual_others == applyUpdatesFor Others  perms updates  
    ]
  )  

prop_applyUpdatesLeftToRight :: Property
prop_applyUpdatesLeftToRight =
  let 
    gen = (,,)
      <$> genPermsMap
      <*> genUpdatesWith [Set, Add, Remove]
      <*> oneof [
        genUpdateFor User Set,
        genUpdateFor Group Set,
        genUpdateFor Others Set
      ]
  in
  forAll (gen) (\(perms, left, right) ->
    let
      updates = left ++ [right]
      applied = applyUpdates perms updates

      Update t _ expected = right
      (Just actual) = Map.lookup t applied
    in
      actual == expected
  )

spec :: Spec
spec = do
  describe "toMode Properties" $
    do it "prop_toModeAndBack" prop_toModeAndBack
       it "prop_toMode" prop_toMode
  describe "fromMode Properties" $
    do it "prop_fromModeAndBack" prop_fromModeAndBack
       it "prop_fromMode" prop_fromMode
  describe "applyUpdates Properties" $
    do it "prop_applyUpdates" prop_applyUpdates
       it "prop_applyUpdatesLeftToRight" prop_applyUpdatesLeftToRight