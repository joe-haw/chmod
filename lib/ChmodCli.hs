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

module ChmodCli (
    parseSetPermissions
) where

import ChmodTypes

import Text.Parsec as P
import Text.Parsec.String as P
import Data.Char (toLower)

import Data.List as List
import Data.Map as Map

parseSetPermissions :: String -> Maybe [SetPermission]
parseSetPermissions args = do
  let parser = P.sepEndBy1 parseSetPermission (P.char ',')
  case P.runParser parser () "" args of
    Left _      -> Nothing
    Right args  -> Just args

parseSetPermission = do
  targets <- P.string "a" <|> P.many (P.oneOf "ugo")
  method  <- P.oneOf "+-="
  perms   <- P.many (P.oneOf "rwx")

  let
      t = parseTargets targets
      m = parseMethod method
      p = parsePermissions perms
    in
      return SetPermission {targets = t, method = m, permission = p}

parseTargets :: String -> [Target]
parseTargets input =
  concatMap parse input
  where
    parse c = case c of
      'u' -> [User]
      'g' -> [Group]
      'o' -> [Others]
      'a' -> [User, Group, Others]

parsePermissions :: String -> Permission
parsePermissions input =
  let perm = Permission {r=False,w=False,x=False}
  in List.foldr parse perm input
  where
    parse c perm = case c of
      'r' -> perm { r = True }
      'w' -> perm { w = True }
      'x' -> perm { x = True }

parseMethod :: Char -> Method
parseMethod method = case method of
    '+' -> Add
    '-' -> Remove
    '=' -> Set