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

module Data.XPath.Axis where

import Prelude
import Data.XPath.Types (XPath, XPathStep(..), XPathAxis(..), xpath)

ancestor ∷ XPath → XPath
ancestor = xpath <<< Axis Ancestor

ancestorOrSelf ∷ XPath → XPath
ancestorOrSelf = xpath <<< Axis AncestorOrSelf

attribute ∷ XPath → XPath
attribute = xpath <<< Axis Attribute

child ∷ XPath → XPath
child = xpath <<< Axis Child

descendant ∷ XPath → XPath
descendant = xpath <<< Axis Descendant

descendantOrSelf ∷ XPath → XPath
descendantOrSelf = xpath <<< Axis DescendantOrSelf

following ∷ XPath → XPath
following = xpath <<< Axis Following

followingSibling ∷ XPath → XPath
followingSibling = xpath <<< Axis FollowingSibling

namespace ∷ XPath → XPath
namespace = xpath <<< Axis Namespace

parent ∷ XPath → XPath
parent = xpath <<< Axis Parent

preceding ∷ XPath → XPath
preceding = xpath <<< Axis Preceding

precedingSibling ∷ XPath → XPath
precedingSibling = xpath <<< Axis PrecedingSibling

self ∷ XPath → XPath
self = xpath <<< Axis Self
