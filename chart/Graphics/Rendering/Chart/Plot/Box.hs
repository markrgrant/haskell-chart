-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Bars
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- BoxPlot Charts
--
-- The top of the box is the third quartile - the middle value between the
-- median and the highest value of the data set.  The bottom of the box is
-- the first quartile - the middle value between the median and the smallest
-- value.  
--
-- The top of the whiskers is typically located at the first standard deviation
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Box(
    PlotBox(..),
    plotBox,
    plot_box_style,
    plot_box_title,
    plot_box_width,
    plot_box_values,
) where


import Control.Lens
import Control.Monad
import Data.List(nub,sort)
import Graphics.Rendering.Chart.Geometry hiding (x0, y0)
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Data.Colour (opaque)
import Data.Colour.Names (black, white)
import Data.Default.Class
import Data.Monoid



-- | Value describing how to plot a set of boxes.
--   Note that the input data is typed [(x,[y])], ie for each x value
--   there are a number of y values from which the mean, median, and variance
--   are determined.  The size of each [y] list may vary. 
data PlotBox x y = PlotBox {
   -- | The style in which to draw each box. A fill style
   --   is required, and if a linestyle is given, the box will be
   --   outlined.
   _plot_box_style     :: (FillStyle,Maybe LineStyle),

   -- | The title of the box. This will be shown in the legend.
   _plot_box_title          :: String,

   -- | The width of each box
   _plot_box_width :: Double,

   -- | The actual points to be plotted.
   _plot_box_values          :: [ (x,[y]) ]
}


-- Define the "def" function so that we can construct new Box charts 
-- more easily
instance PlotValue y => Default (PlotBox x y) where
  def = PlotBox
    { _plot_box_style  = istyle
    , _plot_box_title  = ""
    , _plot_box_values = []
    , _plot_box_width  = 20
    }
    where
        istyle =  (solidFillStyle (opaque white), Just (solidLine 2.0 $ opaque black))


plotBox :: (Fractional y, PlotValue y) => PlotBox x y -> Plot x y
plotBox p = Plot {
        _plot_render     = renderPlotBoxes p,
        _plot_legend     = [(_plot_box_title p, renderPlotLegendBoxes (_plot_box_style p))],
        _plot_all_points = allBoxPoints p
    }


-- Partially apply this function to a box plot value and use as the result
-- as the render property of a Plot object.
renderPlotBoxes :: (Fractional y, PlotValue y) => PlotBox x y -> PointMapFn x y -> BackendProgram ()
renderPlotBoxes p pmap = forM_ vals box
  where
    box (x,ys) = do
        let y2s = [(y_lower, y_upper)]
            ofs = -(width/2) -- center the box at x
            stats        = boxStats ys
            y_lower      = box_stats_lower_quartile stats
            y_median     = box_stats_median stats
            y_upper      = box_stats_upper_quartile stats
            y_w_lower    = box_stats_lower_whisker stats
            y_w_upper    = box_stats_upper_whisker stats

        forM_ y2s $ \(y0,y1) -> 
            withFillStyle fstyle $ 
              alignFillPath (boxPath ofs x y0 y1)
              >>= fillPath

        forM_ y2s $ \(y0,y1) -> 
            whenJust mlstyle $ \lstyle -> 
               withLineStyle lstyle $ 
                 alignStrokePath (boxPath ofs x y0 y1)
                 >>= strokePath
        
        whenJust mlstyle $ \lstyle ->
            withLineStyle medianstyle $
              alignStrokePath (medianPath ofs x y_median)
              >>= strokePath

        whenJust mlstyle $ \lstyle ->
            withLineStyle lstyle $
              alignStrokePath (whiskerPath ofs x y_lower y_w_lower)
              >>= strokePath

        whenJust mlstyle $ \lstyle ->
            withLineStyle lstyle $
              alignStrokePath (whiskerPath ofs x y_upper y_w_upper)
              >>= strokePath

    -- draw a box centered at x, starting at y0 (the lower 
    -- quartile) and extending to y1 (the upper quartile)
    boxPath xos x y0 y1 = do
      let (Point x' y') = pmap' (x,y1)
      let (Point _ y0') = pmap' (x,y0)
      rectPath (Rect (Point (x'+xos) y0') (Point (x'+xos+width) y'))

    -- draw a line from the left side of the box to the right,  at the
    -- position of the median of the y values
    medianPath xos x y = do
      let (Point x' y') = pmap' (x,y)
      moveTo' (x'-xos) y' <> lineTo' (x'+xos) y'
   
    -- draw a whisker that extends from either the top or the bottom of the
    -- box and ends at the largest or smallest value in the data,
    -- respectively. 
    whiskerPath xos x y w = do
      let (Point x' y') = pmap' (x,y)
      let (Point _ w') = pmap' (x,w)
      moveTo' (x'-xos) w' <> lineTo' (x'+xos) w' <> moveTo' x' w' <> lineTo' x' y'
    
    vals             = _plot_box_values p
    width            = _plot_box_width p 
    (fstyle,mlstyle) = _plot_box_style p
    pmap'            = mapXY pmap
    medianstyle      = solidLine 4.0 $ opaque black

   

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust _        _ = return ()


-- Group all the x points into a list, and concat all the lists of y points
-- into a single list.
allBoxPoints :: (PlotValue y) => PlotBox x y -> ([x],[y])
allBoxPoints p = ( [x| (x,_) <- pts], concat [ys| (_,ys) <- pts] )
  where
    pts = _plot_box_values p


renderPlotLegendBoxes :: (FillStyle,Maybe LineStyle) -> Rect -> BackendProgram ()
renderPlotLegendBoxes (fstyle,_) r = 
  withFillStyle fstyle $ 
    fillPath (rectPath r)


data BoxStats a = BoxStats {
    box_stats_lower_quartile :: a,
    box_stats_median         :: a,
    box_stats_upper_quartile :: a, 
    box_stats_lower_whisker  :: a,
    box_stats_upper_whisker  :: a
} deriving (Show)


-- Function for computing box plot statistics needed for the rendering of
-- a box plot.  These statistics include:
--
-- The lower, middle (median), and upper quartiles.  The first quartile
-- determines the start of the box.   The third quartile specifies the end of
-- the box.  A band is placed at the position of the second quartile to
-- denote the median of the data. 
--
-- The whiskers are placed at the smallest and the largest values. 
--
boxStats :: (Ord a, Fractional a) => [a] -> BoxStats a

boxStats [] = undefined

boxStats [x] = BoxStats {
    box_stats_lower_quartile = x,
    box_stats_median         = x,
    box_stats_upper_quartile = x,
    box_stats_lower_whisker  = x,
    box_stats_upper_whisker  = x}

boxStats s = BoxStats {
    box_stats_lower_quartile = l,
    box_stats_median         = m,
    box_stats_upper_quartile = u,
    box_stats_lower_whisker  = lw,
    box_stats_upper_whisker  = uw}
    -- Note: There are faster ways to find order statistics than sorting a
    -- list and retrieving values at indexes.  But they require using an 
    -- algorithm like "select", which works best on unboxed types, which 
    -- requires that the values be unboxed and not just instances of Ord.
    where lst = sort s
          len = length s
          lw  = head lst
          uw  = last lst
          m   = findMedian lst
          mid = len `div` 2
          isEven = len `mod` 2 == 0
          l   = findMedian (take mid lst)
          u   = if isEven
                    then findMedian (drop mid lst)
                    else findMedian (drop (mid+1) lst)

findMedian x =  
  if isEven
      then let mid1 = x !! ((len `div` 2) - 1)
               mid2 = x !! (len `div` 2) 
           in  (mid1 + mid2) / 2
      else x !! (len `div` 2)
  where len = length x
        isEven = len `mod` 2 == 0


$( makeLenses ''PlotBox )
