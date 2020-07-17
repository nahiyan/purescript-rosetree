module Tree.Zipper
  ( Zipper
  , fromTree
  , fromForest
  , toTree
  , toForest
  , tree
  , label
  , children
  , firstChild
  , lastChild
  , parent
  , forward
  , backward
  , root
  , lastDescendant
  , nextSibling
  , previousSibling
  , siblingsBeforeFocus
  , siblingsAfterFocus
  , mapTree
  , replaceTree
  , removeTree
  , mapLabel
  , replaceLabel
  , append
  , prepend
  , findNext
  , findPrevious
  , findFromRoot
  ) where

import Tree as Tree
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Maybe as Maybe
import Prelude

data Zipper a
  = Zipper
    { focus :: Tree.Tree a
    , before :: Array (Tree.Tree a)
    , after :: Array (Tree.Tree a)
    , crumbs :: Array (Crumb a)
    }

type Crumb a
  = { label :: a
    , before :: Array (Tree.Tree a)
    , after :: Array (Tree.Tree a)
    }

data Triple a b c
  = Triple a b c

always :: forall a b. a -> b -> a
always a b = a

fromTree :: forall a. Tree.Tree a -> Zipper a
fromTree t = Zipper { focus: t, before: [], after: [], crumbs: [] }

fromForest :: forall a. Tree.Tree a -> Array (Tree.Tree a) -> Zipper a
fromForest t ts = Zipper { focus: t, before: [], after: ts, crumbs: [] }

toTree :: forall a. Zipper a -> Tree.Tree a
toTree = tree <<< root

toForest :: forall a. Zipper a -> Tuple.Tuple (Tree.Tree a) (Array (Tree.Tree a))
toForest input =
  let
    (Zipper { focus, after }) = root input
  in
    Tuple.Tuple focus after

tree :: forall a. Zipper a -> Tree.Tree a
tree (Zipper { focus }) = focus

forward :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
forward zipper = firstOf [ firstChild, nextSibling, nextSiblingOfAncestor ] zipper

backward :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
backward zipper = firstOf [ previousSibling >>> map lastDescendant, parent ] zipper

parent :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
parent (Zipper zipper) =
  let
    rest = zipper.crumbs # Array.tail # Maybe.fromMaybe []
  in
    case zipper.crumbs # Array.head of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just crumb ->
        Maybe.Just
          $ Zipper
              { focus: reconstruct zipper.focus zipper.before zipper.after crumb.label
              , before: crumb.before
              , after: crumb.after
              , crumbs: rest
              }

firstChild :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
firstChild (Zipper zipper) =
  let
    cs = Tree.children zipper.focus # Array.tail # Maybe.fromMaybe []
  in
    case Tree.children zipper.focus # Array.head of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just c ->
        Maybe.Just
          $ Zipper
              { focus: c
              , before: []
              , after: cs
              , crumbs:
                  [ { label: Tree.label zipper.focus
                    , before: zipper.before
                    , after: zipper.after
                    }
                  ]
                    <> zipper.crumbs
              }

lastChild :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
lastChild (Zipper zipper) =
  let
    rest = Tree.children zipper.focus # Array.reverse # Array.tail # Maybe.fromMaybe []
  in
    case Tree.children zipper.focus # Array.reverse # Array.head of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just c ->
        Zipper
          { focus: c
          , before: rest
          , after: []
          , crumbs:
              [ { label: Tree.label zipper.focus
                , before: zipper.before
                , after: zipper.after
                }
              ]
                <> zipper.crumbs
          }
          # Maybe.Just

root :: forall a. Zipper a -> Zipper a
root zipper = case parent zipper of
  Maybe.Nothing -> firstSibling zipper
  Maybe.Just z -> root z

firstSibling :: forall a. Zipper a -> Zipper a
firstSibling zipper = case previousSibling zipper of
  Maybe.Nothing -> zipper
  Maybe.Just z -> firstSibling z

lastDescendant :: forall a. Zipper a -> Zipper a
lastDescendant zipper = case lastChild zipper of
  Maybe.Nothing -> zipper
  Maybe.Just child -> lastDescendant child

nextSibling :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
nextSibling (Zipper zipper) =
  let
    rest = zipper.after # Array.tail # Maybe.fromMaybe []
  in
    case zipper.after # Array.head of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just next ->
        Maybe.Just
          $ Zipper
              { focus: next
              , before: [ zipper.focus ] <> zipper.before
              , after: rest
              , crumbs: zipper.crumbs
              }

previousSibling :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
previousSibling (Zipper zipper) =
  let
    rest = zipper.before # Array.tail # Maybe.fromMaybe []
  in
    case zipper.before # Array.head of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just previous ->
        Maybe.Just
          $ Zipper
              { focus: previous
              , before: rest
              , after: [ zipper.focus ] <> zipper.after
              , crumbs: zipper.crumbs
              }

findNext :: forall a. (a -> Boolean) -> Zipper a -> Maybe.Maybe (Zipper a)
findNext f zipper = find f forward zipper

findPrevious :: forall a. (a -> Boolean) -> Zipper a -> Maybe.Maybe (Zipper a)
findPrevious f zipper = find f backward zipper

find :: forall a. (a -> Boolean) -> (Zipper a -> Maybe.Maybe (Zipper a)) -> Zipper a -> Maybe.Maybe (Zipper a)
find predicate move zipper = case move zipper of
  Maybe.Just next ->
    if predicate (label next) then
      Maybe.Just next
    else
      find predicate move next
  Maybe.Nothing -> Maybe.Nothing

findFromRoot :: forall a. (a -> Boolean) -> Zipper a -> Maybe.Maybe (Zipper a)
findFromRoot f zipper =
  let
    r = root zipper
  in
    if f (label r) then
      Maybe.Just r
    else
      findNext f r

label :: forall a. Zipper a -> a
label zipper = Tree.label $ tree zipper

children :: forall a. Zipper a -> Array (Tree.Tree a)
children zipper = Tree.children $ tree zipper

siblingsBeforeFocus :: forall a. Zipper a -> Array (Tree.Tree a)
siblingsBeforeFocus (Zipper { before }) = Array.reverse before

siblingsAfterFocus :: forall a. Zipper a -> Array (Tree.Tree a)
siblingsAfterFocus (Zipper { after }) = after

mapTree :: forall a. (Tree.Tree a -> Tree.Tree a) -> Zipper a -> Zipper a
mapTree f (Zipper zipper) = Zipper (zipper { focus = f zipper.focus })

replaceTree :: forall a. Tree.Tree a -> Zipper a -> Zipper a
replaceTree t (Zipper zipper) = Zipper (zipper { focus = t })

removeTree :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
removeTree (Zipper { crumbs, before, after }) = case Triple (crumbs # Array.head) (before # Array.head) (after # Array.head) of
  Triple Maybe.Nothing Maybe.Nothing Maybe.Nothing -> Maybe.Nothing
  Triple (Maybe.Just crumb) _ _ ->
    let
      rest = crumbs # Array.tail # Maybe.fromMaybe []
    in
      Maybe.Just
        $ Zipper
            { focus: reconstructWithoutFocus crumb.label before after
            , before: crumb.before
            , after: crumb.after
            , crumbs: rest
            }
  Triple Maybe.Nothing (Maybe.Just b) _ ->
    let
      bs = before # Array.tail # Maybe.fromMaybe []
    in
      Maybe.Just
        $ Zipper
            { focus: b
            , before: bs
            , after: after
            , crumbs: []
            }
  Triple Maybe.Nothing Maybe.Nothing (Maybe.Just a) ->
    let
      as_ = after # Array.tail # Maybe.fromMaybe []
    in
      Maybe.Just
        $ Zipper
            { focus: a
            , before: []
            , after: as_
            , crumbs: []
            }

mapLabel :: forall a. (a -> a) -> Zipper a -> Zipper a
mapLabel f zipper = mapTree (Tree.mapLabel f) zipper

replaceLabel :: forall a. a -> Zipper a -> Zipper a
replaceLabel l zipper = mapLabel (always l) zipper

prepend :: forall a. Tree.Tree a -> Zipper a -> Zipper a
prepend t (Zipper zipper) = Zipper (zipper { before = [ t ] <> zipper.before })

append :: forall a. Tree.Tree a -> Zipper a -> Zipper a
append t (Zipper zipper) = Zipper (zipper { after = [ t ] <> zipper.after })

firstOf :: forall a b. Array (a -> Maybe.Maybe b) -> a -> Maybe.Maybe b
firstOf options v = case options # Array.head of
  Maybe.Nothing -> Maybe.Nothing
  Maybe.Just option -> case option v of
    Maybe.Just r -> Maybe.Just r
    Maybe.Nothing ->
      let
        rest = options # Array.tail # Maybe.fromMaybe []
      in
        firstOf rest v

nextSiblingOfAncestor :: forall a. Zipper a -> Maybe.Maybe (Zipper a)
nextSiblingOfAncestor zipper = case parent zipper of
  Maybe.Nothing -> Maybe.Nothing
  Maybe.Just parent_ -> case nextSibling parent_ of
    Maybe.Nothing -> nextSiblingOfAncestor parent_
    Maybe.Just s -> Maybe.Just s

reconstruct :: forall a. Tree.Tree a -> Array (Tree.Tree a) -> Array (Tree.Tree a) -> a -> Tree.Tree a
reconstruct focus before after l = Tree.tree l (Array.reverse before <> [ focus ] <> after)

reconstructWithoutFocus :: forall a. a -> Array (Tree.Tree a) -> Array (Tree.Tree a) -> Tree.Tree a
reconstructWithoutFocus l before after = Tree.tree l (Array.reverse before <> after)
