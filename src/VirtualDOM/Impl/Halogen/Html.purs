module VirtualDOM.Impl.Halogen.Html
  ( HalogenHtml
  , runHalogenHtml
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.String as Str
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML (HTML, Namespace(..))
import Halogen.HTML as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as HP
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
    (mkElemFn name)
      (HH.ElemName name)
      (mapProp <$> props)
      (coerce children)

  elemKeyed (ElemName name) props children = HalogenHtml $
    (mkElemFnKeyed name)
      (HH.ElemName name)
      (mapProp <$> props)
      ((\(Key key /\ html) -> key /\ runHalogenHtml html) <$> children)

  text str = HalogenHtml $ HH.text str

mkElemFn :: forall r w i. String -> HH.ElemName -> Array (IProp r i) -> Array (HTML w i) -> HTML w i
mkElemFn name =
  if isSvgElem name then HH.elementNS (Namespace "http://www.w3.org/2000/svg")
  else HH.element

mkElemFnKeyed :: forall r w i. String -> HH.ElemName -> Array (IProp r i) -> Array (String /\ HTML w i) -> HTML w i
mkElemFnKeyed name =
  if isSvgElem name then HH.keyedNS (Namespace "http://www.w3.org/2000/svg")
  else HH.keyed

mapProp :: forall r a. Prop a -> IProp r a
mapProp prop = case prop of
  Attr k v | isHalogenAttr k -> HP.attr (HA.AttrName k) v
  Attr k v -> HP.prop (HH.PropName k) v
  Event n h -> IProp $ HH.handler (EventType $ Str.toLower n) (eventToForeign >>> h >>> map Action)

isHalogenAttr :: String -> Boolean
isHalogenAttr str = Object.member str attribs

eventToForeign :: DOM.Event -> Foreign
eventToForeign = unsafeCoerce

runHalogenHtml :: forall b a. HalogenHtml a -> HTML b a
runHalogenHtml (HalogenHtml html) = lmap absurd $ html

attribs :: Object Unit
attribs = Object.fromFoldable $ map (\x -> x /\ unit)
  [ "attributeName"
  , "begin"
  , "class_"
  , "classes"
  , "cx"
  , "cy"
  , "d"
  , "dominantBaseline"
  , "dur"
  , "fill"
  , "fillAnim"
  , "fillOpacity"
  , "fontFamily"
  , "fontSize"
  , "fontSizeAdjust"
  , "fontStretch"
  , "fontStyle"
  , "fontVariant"
  , "fontWeight"
  , "from"
  , "to"
  , "id"
  , "markerStart"
  , "markerMid"
  , "markerEnd"
  , "markerUnits"
  , "markerWidth"
  , "markerHeight"
  , "mask"
  , "maskUnits"
  , "maskContentUnits"
  , "orient"
  , "path"
  , "points"
  , "pathLength"
  , "patternContentUnits"
  , "patternTransform"
  , "patternUnits"
  , "preserveAspectRatio"
  , "r"
  , "refX"
  , "refY"
  , "repeatCount"
  , "rx"
  , "ry"
  , "style"
  , "stroke"
  , "strokeDashArray"
  , "strokeDashOffset"
  , "strokeLineCap"
  , "strokeLineJoin"
  , "strokeMiterLimit"
  , "strokeOpacity"
  , "strokeWidth"
  , "textAnchor"
  , "transform"
  , "viewBox"
  , "width"
  , "height"
  , "x"
  , "y"
  , "x1"
  , "y1"
  , "x2"
  , "y2"
  , "href"
  , "xmlns"
  ]

isSvgElem :: String -> Boolean
isSvgElem str = Object.member str svgElems

svgElems :: Object Unit
svgElems = Object.fromFoldable $ map (\x -> x /\ unit)
  [ "element"
  , "svg"
  , "g"
  , "circle"
  , "image"
  , "ellipse"
  , "rect"
  , "path"
  , "pattern"
  , "line"
  , "polyline"
  , "polygon"
  , "text"
  , "foreignObject"
  , "defs"
  , "mask"
  , "marker"
  , "animate"
  , "animateMotion"
  , "circleNode"
  , "mpath"
  , "title"
  , "use"
  ]