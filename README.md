# Chmod 
Chmod CLI Application and Library - written in Haskell

## CLI Usage

```bash
# Usage: [ugoa][=+-][rwxst],... path
cabal run chmod a=r,ug=rwxs,o+t /tmp/a.txt
```

## Library Usage

### Full Example 
```haskell
import Chmod
import ChmodCli
import ChmodSystem

main :: IO ()
main = do
  let path = "/tmp/a.txt"
  let (Right new_perms) = parsePermissionUpdates "u=rwx"
  
  (_, mode) <- statMode path
  let orig_perms = fromMode mode
  let new_mode = toMode $ applyPermissionUpdates orig_perms new_perms

  chmod new_mode path
  
  putStrLn "OK."
```

### chmod only

```haskell
import ChmodSystem

main :: IO ()
main = do
  let path = "/tmp/a.txt"
  chmod 0o7777 path

  putStrLn "OK."
```

## License

See LICENSE 
