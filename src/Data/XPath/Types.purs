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

module Data.XPath.Types where

import Prelude

import Data.Generic (class Generic)
import Data.NonEmpty ((:|))
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Foldable as F

newtype XPath = XPath (NonEmptyList XPathStep)

xpath :: XPathStep → XPath
xpath x = XPath (NonEmptyList (x :| Nil))

derive instance newtypeXPath :: Newtype XPath _
derive newtype instance eqXPath :: Eq XPath
derive newtype instance ordXPath :: Ord XPath
derive instance genericXPath :: Generic XPath

instance semigroupXPath :: Semigroup XPath where
  append (XPath a) (XPath b) = XPath (b <> a)

printXPath ∷ XPath → String
printXPath (XPath xp) = F.foldr go "" xp
  where
  go xps "" = printXPathStep xps
  go xps acc = acc <> "/" <> printXPathStep xps

data XPathStep
  = Annotation XPath String
  | Axis XPathAxis XPath
  | Literal XPathLiteral
  | NodeTest XPathNodeTest
  | Operator XPathOperator XPath XPath
  | Predicate XPath XPath
  | Function XPathFunction

derive instance eqXPathStep ∷ Eq XPathStep
derive instance ordXPathStep ∷ Ord XPathStep
derive instance genericXPathStep ∷ Generic XPathStep

printXPathStep ∷ XPathStep → String
printXPathStep =
  case _ of
    Annotation xp _ → printXPath xp
    Axis ax xp → printXPathAxis ax <> "::" <> printXPath' xp
    Function xp → printXPathFunction xp
    Literal lit → printXPathLiteral lit
    NodeTest nt → printXPathNodeTest nt
    Operator op lhs rhs → printXPath' lhs <> " " <> printXPathOperator op <> " " <> printXPath' rhs
    Predicate path pred → printXPath' path <> "[" <> printXPath pred <> "]"
  where
  printXPath' = case _ of
    xp@(XPath (NonEmptyList (_ :| Nil))) → printXPath xp
    xp → "(" <> printXPath xp <> ")"

data XPathAxis
  = Ancestor
  | AncestorOrSelf
  | Attribute
  | Child
  | Descendant
  | DescendantOrSelf
  | Following
  | FollowingSibling
  | Namespace
  | Parent
  | Preceding
  | PrecedingSibling
  | Self

derive instance eqXPathAxis ∷ Eq XPathAxis
derive instance ordXPathAxis ∷ Ord XPathAxis
derive instance genericXPathAxis ∷ Generic XPathAxis

instance showXPathAxis ∷ Show XPathAxis where
  show = case _ of
    Ancestor → "Ancestor"
    AncestorOrSelf → "AncestorOrSelf"
    Attribute → "Attribute"
    Child → "Child"
    Descendant → "Descendant"
    DescendantOrSelf → "DescendantOrSelf"
    Following → "Following"
    FollowingSibling → "FollowingSibling"
    Namespace → "Namespace"
    Parent → "Parent"
    Preceding → "Preceding"
    PrecedingSibling → "PrecedingSibling"
    Self → "Self"

printXPathAxis ∷ XPathAxis → String
printXPathAxis = case _ of
  Ancestor → "ancestor"
  AncestorOrSelf → "ancestor-or-self"
  Attribute → "attribute"
  Child → "child"
  Descendant → "descendant"
  DescendantOrSelf → "descendant-or-self"
  Following → "following"
  FollowingSibling → "following-sibling"
  Namespace → "namespace"
  Parent → "parent"
  Preceding → "preceding"
  PrecedingSibling → "preceding-sibling"
  Self → "self"

data XPathNodeTest
  = NodeName String
  | Comment
  | Text
  | ProcessingInstruction
  | Node

derive instance eqXPathNodeTest ∷ Eq XPathNodeTest
derive instance ordXPathNodeTest ∷ Ord XPathNodeTest
derive instance genericXPathNodeTest ∷ Generic XPathNodeTest

instance showXPathNodeTest ∷ Show XPathNodeTest where
  show = case _ of
    NodeName name → "(NodeName" <> show name <> ")"
    Comment → "Comment"
    Text → "Text"
    ProcessingInstruction → "ProcessingInstruction"
    Node → "Node"

printXPathNodeTest ∷ XPathNodeTest → String
printXPathNodeTest = case _ of
  NodeName name → name
  Comment → "comment()"
  Text → "text()"
  ProcessingInstruction → "processing-instruction()"
  Node → "node()"

data XPathLiteral
  = String String
  | Number Number
  | Int Int

derive instance eqXPathLiteral ∷ Eq XPathLiteral
derive instance ordXPathLiteral ∷ Ord XPathLiteral
derive instance genericXPathLiteral ∷ Generic XPathLiteral

instance showXPathLiteral ∷ Show XPathLiteral where
  show = case _ of
    String s → "(String" <> show s <> ")"
    Number n → "(Number" <> show n <> ")"
    Int i → "(Int" <> show i <> ")"

printXPathLiteral ∷ XPathLiteral → String
printXPathLiteral = case _ of
  String s → show s
  Number n → show n
  Int i → show i

data XPathOperator
  = NodeSetUnion
  | And
  | Or
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

derive instance eqXPathOperator ∷ Eq XPathOperator
derive instance ordXPathOperator ∷ Ord XPathOperator
derive instance genericXPathOperator ∷ Generic XPathOperator

instance showXPathOperator ∷ Show XPathOperator where
  show = case _ of
    NodeSetUnion → "NodeSetUnion"
    And → "And"
    Or → "Or"
    Add → "Add"
    Subtract → "Subtract"
    Multiply → "Multiply"
    Divide → "Divide"
    Modulo → "Modulo"
    Equal → "Equal"
    NotEqual → "NotEqual"
    LessThan → "LessThan"
    LessThanOrEqual → "LessThanOrEqual"
    GreaterThan → "GreaterThan"
    GreaterThanOrEqual → "GreaterThanOrEqual"

printXPathOperator ∷ XPathOperator → String
printXPathOperator = case _ of
  NodeSetUnion → "|"
  And → "and"
  Or → "or"
  Add → "+"
  Subtract → "-"
  Multiply → "*"
  Divide → "div"
  Modulo → "mod"
  Equal → "="
  NotEqual → "!="
  LessThan → "<"
  LessThanOrEqual → "<="
  GreaterThan → ">"
  GreaterThanOrEqual → ">="

data XPathFunction
  = Position
  | Count XPath
  | ToString (Maybe XPath)
  | Concat XPath XPath
  | StartsWith XPath XPath
  | Contains XPath XPath
  | Substring XPath Int (Maybe Int)
  | SubstringBefore XPath XPath
  | SubstringAfter XPath XPath
  | SubstringLength (Maybe XPath)
  | NormalizeSpace (Maybe XPath)
  | Not XPath
  | True
  | False
  | Sum XPath

derive instance eqXPathFunction ∷ Eq XPathFunction
derive instance ordXPathFunction ∷ Ord XPathFunction
derive instance genericXPathFunction ∷ Generic XPathFunction

printXPathFunction ∷ XPathFunction → String
printXPathFunction = case _ of
  Position → "position()"
  Count xp → "count(" <> printXPath xp <> ")"
  ToString mxp → "string(" <> maybe "" printXPath mxp <> ")"
  Concat xp1 xp2 → "concat(" <> printXPath xp1 <> ", " <> printXPath xp2 <> ")"
  StartsWith xp1 xp2 → "starts-with(" <> printXPath xp1 <> ", " <> printXPath xp2 <> ")"
  Contains xp1 xp2 → "contains(" <> printXPath xp1 <> ", " <> printXPath xp2 <> ")"
  Substring xp start length → "substring(" <> printXPath xp <> ", " <> show start <> "" <> maybe "" (\i → ", " <> show i) length <> ")"
  SubstringBefore xp1 xp2 → "substring-before(" <> printXPath xp1 <> ", " <> printXPath xp2 <> ")"
  SubstringAfter xp1 xp2 → "substring-after(" <> printXPath xp1 <> ", " <> printXPath xp2 <> ")"
  SubstringLength mxp → "string-length(" <> maybe "" printXPath mxp <> ")"
  NormalizeSpace mxp → "normalize-space(" <> maybe "" printXPath mxp <> ")"
  Not xp → "not(" <> printXPath xp <> ")"
  True → "true()"
  False → "false()"
  Sum xp → "sum(" <> printXPath xp <> ")"
