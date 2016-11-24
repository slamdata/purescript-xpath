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

module Data.XPath.Literal where

import Prelude
import Data.XPath.Types (XPath, XPathLiteral(..), XPathStep(..), xpath)

num ∷ Number → XPath
num = xpath <<< Literal <<< Number

int ∷ Int → XPath
int = xpath <<< Literal <<< Int

str ∷ String → XPath
str = xpath <<< Literal <<< String
