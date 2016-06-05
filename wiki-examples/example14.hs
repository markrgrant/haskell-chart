import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

title = "Cash"

values :: [ (String,[Double]) ]
values =
  [ ("Jun", [20,30,40, 70, 80, 100, 120])
  , ("Jul", [20, 30, 80, 90, 100, 110, 180])
  , ("Aug", [10, 30, 60, 12, 0, 40, 80])
  , ("Sep", [30, 30, 60, -10, 20, 80, 400])
  , ("Oct", [0, 0, 30, 50, 70, 80, 100])
  ]

main = toFile def "example14.png" $ do
    layout_title .= "Sample Boxplot"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBoxes $ boxes title (addIndexes (map snd values))
