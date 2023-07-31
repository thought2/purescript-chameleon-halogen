-- # purescript-chameleon-halogen
--
-- Halogen implementation of the general `Html` class from the
-- [chameleon](https://github.com/thought2/purescript-chameleon) package.
-- You can write your web views in a framework agnostic way and this package can
-- convert them to Halogen views.
--
-- ## Example

module Test.SampleReadme where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen as Halogen
import Halogen.VDom.Driver as HalogenVDOM
import Chameleon (class Html, text)
import Chameleon.HTML.Attributes as VA
import Chameleon.HTML.Elements as V
import Chameleon.HTML.Events as VE
import Chameleon.Impl.Halogen as Chameleon.Halogen
import Web.HTML (HTMLElement)

-- ### Framework agnostic view

type State = Int

data Msg
  = Increment Int
  | Decrement Int

counterUpdate :: Msg -> State -> State
counterUpdate msg state = case msg of
  Increment n -> state + n
  Decrement n -> state - n

counterView :: forall html. Html html => { count :: Int } -> html Msg
counterView props =
  V.div
    [ VA.style "border: 1px solid red"
    ]
    [ text "Counter"
    , V.div [] [ text $ show props.count ]
    , V.button [ VE.onClick (Increment 1) ]
        [ text "+" ]
    , V.button [ VE.onClick (Decrement 1) ]
        [ text "-" ]
    ]

-- ### Halogen component

app :: forall q i o m. Halogen.Component q i o m
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    Chameleon.Halogen.runHalogenHtml $ counterView { count: state }

  handleAction msg = H.modify_ $ counterUpdate msg

-- ### Mount Halogen component

foreign import elemById :: String -> Effect HTMLElement

main :: Effect Unit
main = launchAff_ do
  rootElem <- liftEffect $ elemById "root"
  _ <- HalogenVDOM.runUI app unit rootElem
  pure unit