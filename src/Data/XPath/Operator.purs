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

module Data.XPath.Operator where

import Prelude hiding (add, (&&), (||), (+), (-), (*), (/), (==), (/=), (<>), (<), (<=), (>), (>=))
import Data.XPath.Types (XPath, XPathStep(..), XPathOperator(..), xpath)

nodeSetUnion ∷ XPath → XPath → XPath
nodeSetUnion lhs rhs = xpath (Operator NodeSetUnion lhs rhs)

infixl 12 nodeSetUnion as <>

and ∷ XPath → XPath → XPath
and lhs rhs = xpath (Operator And lhs rhs)

infixl 8 and as &&

or ∷ XPath → XPath → XPath
or lhs rhs = xpath (Operator Or lhs rhs)

infixl 7 or as ||

add ∷ XPath → XPath → XPath
add lhs rhs = xpath (Operator Add lhs rhs)

infixl 10 add as +

subtract ∷ XPath → XPath → XPath
subtract lhs rhs = xpath (Operator Subtract lhs rhs)

infixl 10 subtract as -

multiply ∷ XPath → XPath → XPath
multiply lhs rhs = xpath (Operator Multiply lhs rhs)

infixl 11 multiply as *

divide ∷ XPath → XPath → XPath
divide lhs rhs = xpath (Operator Divide lhs rhs)

infixl 11 divide as /

modulo ∷ XPath → XPath → XPath
modulo lhs rhs = xpath (Operator Modulo lhs rhs)

infixl 11 modulo as %

equal ∷ XPath → XPath → XPath
equal lhs rhs = xpath (Operator Equal lhs rhs)

infixl 9 equal as ==

notEqual ∷ XPath → XPath → XPath
notEqual lhs rhs = xpath (Operator NotEqual lhs rhs)

infixl 9 notEqual as /=

lessThan ∷ XPath → XPath → XPath
lessThan lhs rhs = xpath (Operator LessThan lhs rhs)

infixl 9 lessThan as <

lessThanOrEqual ∷ XPath → XPath → XPath
lessThanOrEqual lhs rhs = xpath (Operator LessThanOrEqual lhs rhs)

infixl 9 lessThanOrEqual as <=

greaterThan ∷ XPath → XPath → XPath
greaterThan lhs rhs = xpath (Operator GreaterThan lhs rhs)

infixl 9 greaterThan as >

greaterThanOrEqual ∷ XPath → XPath → XPath
greaterThanOrEqual lhs rhs = xpath (Operator GreaterThanOrEqual lhs rhs)

infixl 9 greaterThanOrEqual as >=
