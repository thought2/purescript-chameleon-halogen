module VirtualDOM.Impl.Halogen.Html
  ( HalogenHtml
  , HalogenHtmlCtx(..)
  , runHalogenHtml
  , runHalogenHtmlCtx
  ) where

import Prelude

import Data.Array.NonEmpty (init)
import Data.Bifunctor (lmap)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp(..))
import Halogen.Query.Input (Input(..))
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (class Ctx, class CtxHtml, class Html, ElemName(..), Key(..), Prop(..))
import VirtualDOM as V
import Web.Event.Event (EventType(..))
import Web.Event.Internal.Types as DOM

--------------------------------------------------------------------------------
-- HalogenHtml
--------------------------------------------------------------------------------

newtype HalogenHtml a = HalogenHtml (HTML Void a)

derive instance Functor HalogenHtml

instance Html HalogenHtml where
  elem (ElemName name) props children = HalogenHtml $
    HH.element
      (HH.ElemName name)
      (mapProp <$> props)
      (coerce children)

  elemKeyed (ElemName name) props children = HalogenHtml $
    HH.keyed
      (HH.ElemName name)
      (mapProp <$> props)
      ((\(Key key /\ html) -> key /\ runHalogenHtml html) <$> children)

  text str = HalogenHtml $ HH.text str

mapProp :: forall r a. Prop a -> IProp r a
mapProp prop = case prop of
  Attr k v -> HH.prop (HH.PropName k) v
  Event n h -> IProp $ HH.handler (EventType $ Str.toLower n) (eventToForeign >>> h >>> map Action)

eventToForeign :: DOM.Event -> Foreign
eventToForeign = unsafeCoerce

runHalogenHtml :: forall b a. HalogenHtml a -> HTML b a
runHalogenHtml (HalogenHtml html) = lmap absurd $ html

--------------------------------------------------------------------------------
-- HalogenHtmlCtx
--------------------------------------------------------------------------------

newtype HalogenHtmlCtx ctx a = HalogenHtmlCtx (ctx -> HalogenHtml a)

derive instance Functor (HalogenHtmlCtx ctx)

instance Html (HalogenHtmlCtx ctx) where
  elem elemName props children = HalogenHtmlCtx \ctx ->
    V.elem elemName props (runHalogenHtmlCtx ctx <$> children)

  elemKeyed elemName props children = HalogenHtmlCtx \ctx ->
    V.elemKeyed elemName props ((\(key /\ html) -> key /\ runHalogenHtmlCtx ctx html) <$> children)

  text str = HalogenHtmlCtx \_ -> V.text str

instance Ctx (HalogenHtmlCtx ctx) ctx where
  withCtx f = HalogenHtmlCtx \ctx -> runHalogenHtmlCtx ctx $ f ctx
  setCtx ctx html = HalogenHtmlCtx \_ -> runHalogenHtmlCtx ctx html

instance CtxHtml (HalogenHtmlCtx ctx) ctx

runHalogenHtmlCtx :: forall ctx a. ctx -> HalogenHtmlCtx ctx a -> HalogenHtml a
runHalogenHtmlCtx ctx (HalogenHtmlCtx f) = f ctx

