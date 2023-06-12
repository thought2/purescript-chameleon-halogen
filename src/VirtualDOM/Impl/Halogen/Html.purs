module VirtualDOM.Impl.Halogen.Html
  ( HalogenHtml
  , runHalogenHtml
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp(..))
import Halogen.Query.Input (Input(..))
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (class Html, ElemName(..), Key(..), Prop(..))
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