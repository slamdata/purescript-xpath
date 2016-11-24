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

module Data.XPath.Function where

import Prelude
import Data.XPath.Types (XPath, XPathFunction(..), XPathStep(..), xpath)
import Data.Maybe (Maybe(..))

position ∷ XPath
position = xpath (Function Position)

count ∷ XPath → XPath
count = xpath <<< Function <<< Count

toString ∷ XPath
toString = xpath (Function (ToString Nothing))

toString' ∷ Maybe XPath → XPath
toString' = xpath <<< Function <<< ToString

concat ∷ XPath → XPath → XPath
concat x y = xpath (Function (Concat x y))

startsWith ∷ XPath → XPath → XPath
startsWith x y = xpath (Function (StartsWith x y))

contains ∷ XPath → XPath → XPath
contains x y = xpath (Function (Contains x y))

substring ∷ XPath → Int → XPath
substring x start = xpath (Function (Substring x start Nothing))

substring' ∷ XPath → Int → Maybe Int → XPath
substring' x start length = xpath (Function (Substring x start length))

substringBefore ∷ XPath → XPath → XPath
substringBefore x y = xpath (Function (SubstringBefore x y))

substringAfter ∷ XPath → XPath → XPath
substringAfter x y = xpath (Function (SubstringAfter x y))

substringLength ∷ XPath
substringLength = xpath (Function (SubstringLength Nothing))

substringLength' ∷ Maybe XPath → XPath
substringLength' = xpath <<< Function <<< SubstringLength

normalizeSpace ∷ XPath
normalizeSpace = xpath (Function (NormalizeSpace Nothing))

normalizeSpace' ∷ Maybe XPath → XPath
normalizeSpace' = xpath <<< Function <<< NormalizeSpace

not ∷ XPath → XPath
not = xpath <<< Function <<< Not

tt ∷ XPath
tt = xpath (Function True)

ff ∷ XPath
ff = xpath (Function False)

sum ∷ XPath → XPath
sum = xpath <<< Function <<< Sum
