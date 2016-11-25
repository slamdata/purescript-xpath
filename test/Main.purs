{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.String as Str
import Data.XPath as X
import Test.Assert (ASSERT, assert)

main ∷ Eff (assert ∷ ASSERT, console ∷ CONSOLE) Unit
main = do
  let
    fooBarBaz = X.nodeName "foo" <> X.nodeName "bar" <> X.nodeName "baz"
    unfolded = Str.joinWith "/" $ map X.printXPathStep $ X.toUnfoldable fooBarBaz
    printed = X.printXPath fooBarBaz

  log "Testing that unfolding returns the path in left-to-right order"
  assert (unfolded == printed)

  log "Testing that parens are used with indices"
  assert
    $ X.printXPath ((X.nodeName "x" <> X.nodeName "y" <> X.nodeName "z") `X.predicated` X.int 1)
    == "(x/y/z)[1]"

  log "Testing that parens can be avoided with indices"
  assert
    $ X.printXPath (X.nodeName "x" <> X.nodeName "y" <> X.nodeName "z" `X.predicated` X.int 1)
    == "x/y/z[1]"
