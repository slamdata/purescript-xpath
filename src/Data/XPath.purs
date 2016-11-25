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

module Data.XPath
  ( toUnfoldable
  , module Data.XPath.Annotation
  , module Data.XPath.Axis
  , module Data.XPath.Function
  , module Data.XPath.Literal
  , module Data.XPath.NodeTest
  , module Data.XPath.Operator
  , module Data.XPath.Predicate
  , module Data.XPath.Types
  ) where

import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Unfoldable (class Unfoldable)
import Data.XPath.Annotation (annotate, (<?>))
import Data.XPath.Axis (ancestor, ancestorOrSelf, attribute, child, descendant, descendantOrSelf, following, followingSibling, namespace, parent, preceding, precedingSibling, self)
import Data.XPath.Function (concat, contains, count, ff, normalizeSpace, normalizeSpace', not, position, startsWith, substring, substring', substringAfter, substringBefore, substringLength, substringLength', sum, toString, toString', tt)
import Data.XPath.Literal (int, num, str)
import Data.XPath.NodeTest (comment, node, nodeName, processingInstruction, text)
import Data.XPath.Operator (add, and, divide, equal, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, modulo, multiply, nodeSetUnion, notEqual, or, subtract, (%), (&&), (*), (+), (-), (/), (/=), (<>), (<), (<=), (==), (>), (>=), (||))
import Data.XPath.Predicate (predicated)
import Data.XPath.Types (XPath(..), XPathAxis(..), XPathFunction(..), XPathLiteral(..), XPathNodeTest(..), XPathOperator(..), XPathStep(..), printXPath, printXPathAxis, printXPathFunction, printXPathLiteral, printXPathNodeTest, printXPathOperator, printXPathStep, xpath)

toUnfoldable ∷ ∀ f. Unfoldable f ⇒ XPath → f XPathStep
toUnfoldable (XPath xp) = L.toUnfoldable (L.reverse (NEL.toUnfoldable xp))
