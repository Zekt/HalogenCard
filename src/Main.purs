module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int
import Data.List
import Effect (Effect)
import Halogen.HTML.CSS as HC
import CSS as CSS
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data State = Number

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Increment | Decrement

cardpos :: Number -> CSS.CSS
cardpos n = do CSS.border CSS.solid (CSS.px 5.0) CSS.red
               CSS.height (CSS.px 5.0)
               CSS.width  (CSS.px 5.0)
               CSS.marginLeft (CSS.px $ 20.0 + toNumber (floor n `mod` 5) * 100.0)
               CSS.rule (CSS.Property (CSS.Key $ CSS.Plain "transition") (CSS.Value $ CSS.Plain "margin 1s"))
               CSS.position CSS.absolute

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 10.0

  render state =
    HH.div_ (
      [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
      , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
      ] <> (map (\pos -> HH.div [ HC.style $ cardpos $ (state + toNumber pos) ] [ HH.text $ "" ]) [1,2,3,4,5]) )
     -- <> (map (\_ -> (HH.div [ HC.style $ cardpos $ state ] [ HH.text $ "" ])) (1 .. 5))

  handleAction = case _ of
    Increment -> H.modify_ \s -> s + 1.0
    Decrement -> H.modify_ \s -> s - 1.0

