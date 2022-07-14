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
    module ChmodTypes,
    applySetPermissions,
    toMode,
    toPermissions,
) where

import ChmodTypes

import Data.List as List
import Data.Map as Map
import Data.Bits ( Bits((.|.), shiftL, (.&.), shiftR) )

(<||>) :: Permission -> Permission -> Permission
(<||>) a b = Permission { r = (r a) || (r b), w = (w a) || (w b), x = (x a) || (x b) }

(<&&>) :: Permission -> Permission -> Permission
(<&&>) a b = Permission { r = (r a) && (r b), w = (w a) && (w b), x = (x a) && (x b) }

invert :: Permission -> Permission
invert perm = Permission { r = not(r perm), w = not(w perm), x = not(x perm) }

applySetPermissions :: Map Target Permission -> [SetPermission] -> Map Target Permission
applySetPermissions perms set_perms = List.foldl applySetPermission perms set_perms

applySetPermission :: Map Target Permission -> SetPermission -> Map Target Permission
applySetPermission perms set_perm =
  let
    SetPermission { targets = targets, method = method, permission = permission } = set_perm
    lhs = perms
    rhs = Map.fromList $ [(target, perm) | target <- targets, perm <- [permission]]
    combine = combine' method
  in Map.unionWith combine lhs rhs
  where
    combine' :: Method -> Permission -> Permission -> Permission
    combine' Remove lhs rhs = lhs <&&> (invert rhs)
    combine' Add    lhs rhs = lhs <||> rhs
    combine' Set    lhs rhs = rhs

toMode :: Map Target Permission -> Int
toMode perms =
  let
    modes = Map.map toMode' perms
    getMode' t = shift' t $ Map.findWithDefault 0 t modes

    user_mode   = getMode' User
    group_mode  = getMode' Group
    others_mode = getMode' Others

    mode = user_mode + group_mode + others_mode
  in mode
  where
    shift' :: Target -> Int -> Int 
    shift' User   mode  = shiftL mode 6
    shift' Group  mode  = shiftL mode 3
    shift' Others mode  = shiftL mode 0
    toMode' :: Permission -> Int
    toMode' perm =
      let
        read  = if r perm then 4 else 0
        write = if w perm then 2 else 0
        exec  = if x perm then 1 else 0
        mode = read + write + exec
      in mode

toPermissions :: Int -> Map Target Permission
toPermissions mode =
  let 
    user_mode   = getMode' User   mode
    group_mode  = getMode' Group  mode
    others_mode = getMode' Others mode

    modes = Map.fromList [
      (User, user_mode),
      (Group, group_mode),
      (Others, others_mode)]
  in Map.map toPerm' modes
  where
    getMode' :: Target -> Int -> Int
    getMode' User   mode  = shiftR mode 6 .&. 7
    getMode' Group  mode  = shiftR mode 3 .&. 7
    getMode' Others mode  = shiftR mode 0 .&. 7
    toPerm' :: Int -> Permission
    toPerm' mode =
      let
        read  = mode .&. 4 == 4
        write = mode .&. 2 == 2
        exec  = mode .&. 1 == 1
      in Permission { r = read, w = write, x = exec }