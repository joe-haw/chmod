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
    parsePermissionUpdates
) where

import ChmodTypes

import Text.Parsec as P
import Text.Parsec.String as P
import Data.Char (toLower)

import Data.List as List

parsePermissionUpdates :: String -> Either ParseError [PermissionUpdate]
parsePermissionUpdates args = do
  let parser = P.sepBy1 parsePermissionUpdate (P.char ',')
  case P.runParser parser () "" args of
    Left err      -> Left err
    Right args    -> Right $ concat args

parsePermissionUpdate = do
  targets <- P.many1 (P.oneOf "ugoa")
  method  <- P.oneOf "+-="
  perms   <- P.manyTill (P.oneOf "rwxst") (try (do {
    P.lookAhead $ P.char ',';
    return ()
  }) <|> P.eof)

  let
      ts = parseTargets targets
      m = parseMethod method
    in
      return [
        PermissionUpdate {target = t, method = m, permission = perm}
        | t <- ts, let perm = parsePermissions t perms
      ]

parseTargets :: String -> [Target]
parseTargets input =
  concatMap parse $ fmap toLower input
  where
    parse c =
      case c of
      'u' -> [User]
      'g' -> [Group]
      'o' -> [Others]
      'a' -> [User, Group, Others]

parseMethod :: Char -> Method
parseMethod method =
  case method of
  '+' -> Add
  '-' -> Remove
  '=' -> Set

parsePermissions :: Target -> String -> Permission
parsePermissions target input = do
  let
    rwx = parsePermission input
    ext = parseExtPermission target input
  makePermission target ext rwx
  
parseExtPermission :: Target -> String -> Extended
parseExtPermission target input = 
  let
    acc = case target of
      User    -> Extended Suid    False
      Group   -> Extended Sgid    False
      Others  -> Extended Sticky  False
  in List.foldr (parse' target) acc input
  where
    parse' :: Target -> Char -> Extended -> Extended
    parse' target c acc =
      case (target, c) of
      (User,    's')  -> Extended Suid    True
      (Group,   's')  -> Extended Sgid    True
      (Others,  't')  -> Extended Sticky  True
      _               -> acc

parsePermission :: String -> RWX
parsePermission input = 
  List.foldr parse' (RWX False False False) input
  where
    parse' c acc@(RWX r w x) =
      case c of
      'r' -> RWX True w x
      'w' -> RWX r True x
      'x' -> RWX r w True
      _   -> acc