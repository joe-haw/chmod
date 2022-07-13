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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import qualified System.Exit as Exit
import Control.Monad.Except

import Chmod
import ChmodCli
import ChmodSystem

data AppError = 
  InvalidArguments | InvalidPath | ChmodFailed

main = do
  let process :: ExceptT AppError IO () = do
        args <- liftIO getArgs

        if length(args) /= 2 then throwError InvalidArguments else return ()
        let [argFirst, argPath] = args
        argSetPermissions <- case (parseSetPermissions argFirst) of
              Just result -> return result
              _           -> throwError InvalidArguments

        (code, mode) <- liftIO $ statMode argPath
        throwIfNotZero code InvalidPath

        let orig_perms = toPermissions mode
        let new_mode = toMode $ applySetPermissions orig_perms argSetPermissions

        code <- liftIO $ chmod new_mode argPath
        throwIfNotZero code ChmodFailed

        return ()
  result <- runExceptT process
  case result of
    Left err -> do
      handleErr err
      Exit.exitFailure
    Right _ ->
      Exit.exitSuccess
  where
    throwIfNotZero x err = case x of 
      0 -> return ()
      _ -> throwError err
    handleErr err = case err of
      InvalidArguments -> usage
      InvalidPath -> putStrLn "Unable to access specified path"
      ChmodFailed -> putStrLn "Unable to change mode (chmod failed)"
    usage = do
      putStrLn "Usage: [ugoa][=+-][rwx],... path"
      putStrLn "Example: chmod a=,ug=rw /tmp/a.txt"