module GrowthAgent.Prelude
  ( (∘)
  , (≡)
  , (≠)
  , (∨)
  , (∧)
  , (⨁)
  , type (↝)
  , type (⨁)
  , (×)
  , type (×)
  --, (⊹)
  --, type (⊹)
  , module Prelude
  , module Control.Alt
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Monad.Error.Class
  , module Control.Monad.Except
  , module Control.Monad.Gen.Class
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.Reader
  , module Control.Monad.Rec.Class
  , module Control.Monad.Trans.Class
  , module Control.MonadPlus
  , module Control.Parallel
  , module Control.Plus
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Const
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Coproduct
  , module Data.Generic.Rep
  , module Data.Lens
  , module Data.Lens.Iso.Newtype
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Newtype
  , module Data.Set
  , module Data.Show.Generic
  , module Data.Symbol
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Variant
  , module Data.Void
  , module Debug
  , module Effect
  , module Effect.Aff
  , module Effect.Aff.Class
  , module Effect.Class
  , module Safe.Coerce
  , module Type.Row
  , module Type.Proxy
  ) where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Apply ((*>), (<*))
import Control.Bind (join, (>=>), (<=<))
import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT, except)
import Control.Monad.Gen.Class (class MonadGen)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Parallel (class Parallel, parTraverse, parTraverse_)
import Control.Plus (class Plus, empty)
import Data.Bifoldable (class Bifoldable, bitraverse_, bifor_)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bitraverse, bisequence, bifor)
import Data.Const (Const(..))
import Data.Either (Either(..), either, isLeft, isRight, fromRight, note, hush)
import Data.Eq (class Eq, class EqRecord, (==), (/=))
import Data.Foldable (class Foldable, traverse_, for_, foldMap, foldl, foldr, fold)
import Data.Functor (($>), (<$))
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', Prism, Prism', Traversal, Traversal', Optic, (.~), (?~), (?=), (^.), (^?), (%~), (^..), _Just, _Nothing, _1, _2)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Newtype (class Newtype, unwrap, ala, alaF, un)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable, traverse, sequence, for)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (Variant, case_, class Contractable, contract, expand)
import Data.Void (Void, absurd)
import Debug (spy, spyWith)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Safe.Coerce (coerce)
import Type.Row (RowApply, type (+))
import Type.Proxy (Proxy(..))

-- ctrl+k Ob
infixr 9 compose as ∘

-- ctrl+k 3= or =3
infix 4 eq as ≡

-- ctrl+k != or =!
infix 4 notEq as ≠

-- ctrl+k AN
infixr 3 conj as ∧

-- ctrl+k OR
infixr 2 disj as ∨

-- ctrl+k \/ or /\
infixr 4 type Tuple as ×

-- ctrl+k \/ or /\
infixr 1 Tuple as ×

-- Add to .vimrc or nvim/init.vim
-- " digraphs
-- digraph ~> 8605 " ↝
-- digraph O+ 10753 " ⨁
infixr 4 type NaturalTransformation as ↝

infixr 4 type Coproduct as ⨁

infixr 5 coproduct as ⨁

--infixr 4 type Either as ⊹
--
--infixr 5 either as ⊹
