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

module Chmod (
    applyPermissionUpdates,
    toMode,
    fromMode,
) where

import ChmodTypes

import Data.List as List
import Data.Map as Map
import Data.Bits ( Bits((.|.), shiftL, (.&.), shiftR) )

applyPermissionUpdates :: Map Target Permission -> [PermissionUpdate] -> Map Target Permission
applyPermissionUpdates perms new_perms = List.foldl applyPermissionUpdate perms new_perms

applyPermissionUpdate :: Map Target Permission -> PermissionUpdate -> Map Target Permission
applyPermissionUpdate perms new_perm =
  let
    PermissionUpdate { target = target, method = method, permission = permission } = new_perm
    lhs = perms
    rhs = Map.fromList $ [(target, permission)]
    combine = combine' method
  in Map.unionWith combine lhs rhs
  where
    combine' :: Method -> Permission -> Permission -> Permission
    combine' Remove lhs rhs = lhs <&&> (invert rhs)
    combine' Add    lhs rhs = lhs <||> rhs
    combine' Set    lhs rhs = rhs

toExtMode :: Extended -> Int
toExtMode (Extended kind val) = 
  let mode = toMode' kind val
  in mode `shiftL` 9
  where 
    toMode' Suid    True = 4 
    toMode' Sgid    True = 2 
    toMode' Sticky  True = 1
    toMode' _       _    = 0

fromExtMode :: Target -> Int -> Extended
fromExtMode target mode_ =
  let
    kind = case target of 
        User    -> Suid
        Group   -> Sgid
        Others  -> Sticky
    mode  = mode_ `shiftR` 9
    val   = isset' kind mode
  in Extended kind val
  where
    isset' :: ExtendedKind -> Int -> Bool
    isset' Suid   mode  = mode .&. 4 == 4
    isset' Sgid   mode  = mode .&. 2 == 2
    isset' Sticky mode  = mode .&. 1 == 1

toRWXMode :: Target -> RWX -> Int
toRWXMode target (RWX r_ w_ x_) = 
  let
    r = toMode' Read r_
    w = toMode' Write w_
    x = toMode' Execute x_
    mode = (r + w + x)
  in shift' target mode
  where
    shift' :: Target -> Int -> Int
    shift' User   mode = mode `shiftL` 6
    shift' Group  mode = mode `shiftL` 3
    shift' Others mode = mode `shiftL` 0
    toMode' :: RWXMode -> Bool -> Int
    toMode' Read    True  = 4
    toMode' Write   True  = 2
    toMode' Execute True  = 1
    toMode' _       False = 0

fromRWXMode :: Target -> Int -> RWX
fromRWXMode target mode_ =
  let
    mode = getMode' target mode_
    r = isset' Read     mode
    w = isset' Write    mode
    x = isset' Execute  mode
  in RWX r w x
  where
    getMode' :: Target -> Int -> Int
    getMode' User   mode = mode `shiftR` 6
    getMode' Group  mode = mode `shiftR` 3
    getMode' Others mode = mode `shiftR` 0
    isset' :: RWXMode -> Int -> Bool
    isset' Read     mode  = mode .&. 4 == 4
    isset' Write    mode  = mode .&. 2 == 2
    isset' Execute  mode  = mode .&. 1 == 1

toPermissionMode :: Permission -> Int
toPermissionMode (Permission target ext rwx) = toExtMode ext + toRWXMode target rwx

fromPermissionMode :: Target -> Int -> Permission
fromPermissionMode target mode =
  let
    ext = fromExtMode target mode
    rwx = fromRWXMode target mode
  in Permission target ext rwx

toMode :: Map Target Permission -> Int
toMode perms = 
  let
    modes = Map.map toPermissionMode perms
  in List.sum(modes)

fromMode :: Int -> Map Target Permission
fromMode mode =
  let
    user   = fromPermissionMode User mode
    group  = fromPermissionMode Group mode
    others = fromPermissionMode Others mode
  in Map.fromList [(User, user), (Group, group), (Others, others)]
