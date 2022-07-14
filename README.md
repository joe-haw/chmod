# Chmod 
Chmod CLI Application and Library - written in Haskell

## App Usage

```bash
# Usage: [ugoa][=+-][rwx],... path
cabal run chmod a=r,ug=rwx /tmp/a.tx
```

## Library Usage

```haskell
import Chmod
import ChmodCli
import ChmodSystem

main :: IO ()
main = do
  let path = "/tmp/a.txt"
  let (Just set_perms) = parseSetPermissions "u=rwx"
  
  (_, mode) <- statMode path
  let orig_perms = toPermissions mode
  let new_mode = toMode $ applySetPermissions orig_perms set_perms

  chmod new_mode path
  
  putStrLn $ "OK."
```

## License

See LICENSE 
