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

module ChmodSystem (chmod, statMode) where

import System.Posix (CMode)
import Foreign.C (CInt, newCString)
import System.Posix.Internals (c_chmod, sizeof_stat, c_stat, st_mode)
import Foreign (allocaBytes)

chmod :: Int -> String -> IO Int
chmod mode path = do
  cstrPath <- newCString path
  code <- c_chmod cstrPath (fromIntegral mode)
  return (fromIntegral code)

statMode :: String -> IO (Int, Int)
statMode path = do
  cstrPath <- newCString path
  allocaBytes sizeof_stat $ \ptrStat -> do
    code <- c_stat cstrPath ptrStat 
    mode <- st_mode ptrStat
    return (fromIntegral code, fromIntegral mode)