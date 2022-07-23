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

module ChmodTypes where

data RWX = RWX Bool Bool Bool
  deriving (Show, Eq)
data RWXMode = Read | Write | Execute
data ExtendedKind = Suid | Sgid | Sticky
  deriving (Show, Eq)
data Extended = Extended ExtendedKind Bool
  deriving (Show, Eq)

data Target = User | Group | Others
  deriving (Show, Eq, Ord)
data PermissionRWX = PermissionRWX Target RWX
  deriving (Show, Eq)
data Permission = Permission Target Extended PermissionRWX
  deriving (Show, Eq)
data Method = 
  Set | Add | Remove
  deriving (Show, Eq)

data PermissionUpdate = PermissionUpdate {
  target :: Target,
  method :: Method,
  permission :: Permission
} deriving (Show, Eq)

makeExtended :: Target -> Bool -> Extended
makeExtended target val = case target of
  User    -> Extended Suid val
  Group   -> Extended Sgid val
  Others  -> Extended Sticky val

makePermission ::
  Target -> Extended -> RWX -> Permission
makePermission target ext@(Extended ext_target _) (RWX r w x)
  | (target == User   && ext_target == Suid) ||
    (target == Group  && ext_target == Sgid) ||
    (target == Others && ext_target == Sticky)
  = Permission target ext (PermissionRWX target (RWX r w x))

class LogicalOps a where
  (<||>) :: a -> a -> a
  (<&&>) :: a -> a -> a
  invert :: a -> a

instance LogicalOps Extended where
  (<||>) (Extended k val) (Extended t_ val_) = Extended k (val || val_)
  (<&&>) (Extended k val) (Extended t_ val_) = Extended k (val && val_)
  invert (Extended k val) = Extended k (not val)

instance LogicalOps RWX where
  (<||>) (RWX r w x) (RWX r_ w_ x_) = RWX (r || r_) (w || w_) (x || x_)
  (<&&>) (RWX r w x) (RWX r_ w_ x_) = RWX (r && r_) (w && w_) (x && x_)
  invert (RWX r w x) = RWX (not r) (not w) (not x)

instance LogicalOps PermissionRWX where
  (<||>) (PermissionRWX k rwx) (PermissionRWX _ rwx_) = 
    PermissionRWX k (rwx <||> rwx_)
  (<&&>) (PermissionRWX k rwx) (PermissionRWX _ rwx_) = 
    PermissionRWX k (rwx <&&> rwx_)
  invert (PermissionRWX k rwx) = 
    PermissionRWX k (invert rwx)

instance LogicalOps Permission where
  (<||>) (Permission k ext rwx) (Permission _ ext_ rwx_) = 
    Permission k (ext <||> ext_) (rwx <||> rwx_)
  (<&&>) (Permission k ext rwx) (Permission _ ext_ rwx_) = 
    Permission k (ext <&&> ext_) (rwx <&&> rwx_)
  invert (Permission k ext rwx) = 
    Permission k (invert ext) (invert rwx)
