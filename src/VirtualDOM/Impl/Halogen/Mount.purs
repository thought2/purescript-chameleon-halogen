module VirtualDOM.Impl.Halogen.Mount where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen as Halogen
import Halogen.VDom.Driver as HalogenVDOM
import VirtualDOM.Impl.Halogen.Html (HalogenHtml, runHalogenHtml)
import Web.HTML (HTMLElement)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type UI html msg sta =
  { view :: sta -> html msg
  , update :: msg -> sta -> sta
  , init :: sta
  }

--------------------------------------------------------------------------------
-- Halogen Component
--------------------------------------------------------------------------------

uiToHalogenComponent :: forall q i o m msg sta. UI HalogenHtml msg sta -> Halogen.Component q i o m
uiToHalogenComponent ui =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = ui.init

  render state =
    runHalogenHtml $ ui.view state

  handleAction msg = H.modify_ $ ui.update msg

--------------------------------------------------------------------------------
-- Mounting
--------------------------------------------------------------------------------

uiMountAtId :: forall msg sta. String -> UI HalogenHtml msg sta -> Effect Unit
uiMountAtId id ui = launchAff_ do
  rootElem <- liftEffect $ elemById id
  let comp = uiToHalogenComponent ui
  _ <- HalogenVDOM.runUI comp unit rootElem
  pure unit

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import elemById :: String -> Effect HTMLElement