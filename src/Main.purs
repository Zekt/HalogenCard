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
cardpos n = let modn = toNumber (floor n `mod` 5) in
            do CSS.border CSS.solid (CSS.px 1.0) CSS.gray
               CSS.backgroundColor $ CSS.lighten (modn / 20.0) CSS.skyblue
               CSS.height (CSS.px 300.0)
               CSS.width  (CSS.px 500.0)
               CSS.marginLeft (CSS.px $ 20.0 + modn * 50.0)
               CSS.position CSS.absolute
               CSS.zIndex $ 5 - floor modn
               CSS.transform (CSS.Transformation (CSS.Value $ CSS.Plain $ "scale(" <> show (1.0 - modn * 0.1) <> ")"))
               CSS.rule (CSS.Property (CSS.Key $ CSS.Plain "transition") (CSS.Value $ CSS.Plain "all 1s"))


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

  handleAction = case _ of
    Increment -> H.modify_ \s -> s + 1.0
    Decrement -> H.modify_ \s -> s - 1.0

