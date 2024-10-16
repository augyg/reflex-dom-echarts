{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Widget.ECharts
  ( LineChartConfig(..)
  , Chart(..)
  , TimeLineChartConfig(..)
  , lineChart
  , timeLineChart
  , Sizing(..)
  , module X
  )
  where

import Prelude hiding ((!!))
import Reflex.Dom.Core
import ECharts as X hiding (ffor)

import Language.Javascript.JSaddle
--import JSDOM.Types (Element)
import qualified GHCJS.DOM.Types as GJS 

import Data.Time
import qualified Data.Some as Some
import Control.Monad (forM, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Reflex.Network

type XAxisData = Text


data Sizing = Pixel Int | Percent Int | Vh Int | Vw Int

instance Show Sizing where
  show = \case
    Pixel pxs -> show pxs <> "px"
    Percent pct -> show pct <> "%"
    Vh vh -> show vh <> "vh"
    Vw vw -> show vw <> "vw"


data LineChartConfig t k = LineChartConfig
  -- XXX Can be made a Dynamic
  -- and use this API to adjust size
  -- https://ecomfe.github.io/echarts-doc/public/en/api.html#echartsInstance.resize
  { _lineChartConfig_attrs :: Map Text Text -- (Sizing, Sizing)
  -- We will re-create the whole chart if the options change
  , _lineChartConfig_options :: Dynamic t ChartOptions
  , _lineChartConfig_series :: Map k
    ( Series SeriesLine
    , Dynamic t (Map Text (Data SeriesLine))
    , Dynamic t [Text]
    )
  }


data BarChartConfig t k = BarChartConfig
  { _barChartConfig_size :: (Int,Int)
  , _barChartConfig_options :: Dynamic t ChartOptions
  , _barChartConfig_series :: k 
  }

data Chart t = Chart
  { _chart_rendered :: Event t ()
  , _chart_finished :: Event t ()
  }



-- TODO: better name
initChartEvents
  :: ( PostBuild t m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => Element EventResult GhcjsDomSpace t
  -> m (Event t ECharts, Event t (), Event t ())
initChartEvents elementHtml = do
  p <- getPostBuild
  (evR, onActionR) <- newTriggerEvent
  (evF, onActionF) <- newTriggerEvent

  -- Init the chart
  chartEv <- performEvent $ ffor p $ \_ -> liftJSM $ do
    chart <- X.initECharts $ _element_raw elementHtml
    X.onRenderedAction chart (liftIO $ onActionR ())
    X.onFinishedAction chart (liftIO $ onActionF ())
    return chart

  pure (chartEv, evR, evF)

--TODO: dont take only px values
barChart
  :: ( PostBuild t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , PerformEvent t m
     , TriggerEvent t m
     , DomBuilder t m
     , MonadHold t m 
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => BarChartConfig t k
  -> m (Chart t)
barChart cfg = do
  let 
    optionsDyn = _barChartConfig_options cfg
    attrs = (_barChartConfig_size cfg) & \(w, h) ->
      "style" =: ("width" <> tshow w <> "; height:" <> tshow h <> ";")
      -- "style" =: ("width: 100 %; height: 100 %;")
  barChartEl <- fst <$> elAttr' "div" attrs blank
  (chartEv, evRendered, evFinished) <- initChartEvents barChartEl

  -- Can probably generalize to set options for chart or whatever

  -- use prerender here?
  
  void $ widgetHold blank $ ffor chartEv $ \chart -> do
    void $ networkView $ ffor optionsDyn $ \opt -> do
      optVObj <- liftJSM $ makeObject =<< toJSVal opt
      pure ()
      
  
  pure (Chart evRendered evFinished)
    
lineChart
  :: forall t m k .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadHold t m
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => LineChartConfig t k
  -> m (Chart t)
lineChart c = do
  let
    cDyn = _lineChartConfig_options c
  e <- fst <$> elAttr' "div" (_lineChartConfig_attrs c) blank

  --The initialization is done using PostBuild because the element need
  -- to be present in the DOM before calling echarts APIs
  (chartEv, evRendered, evFinished) <- initChartEvents e
  
  void $ widgetHold blank $ ffor chartEv $ \chart -> do
    void $ networkView $ ffor cDyn $ \opt -> do
      -- set first options
      optVObj <- liftJSM $ makeObject =<< toJSVal opt

      -- Convert the user specified "series" options to JSVal
      -- and modify it according to the Dynamic values
      -- (series, (series.xAxisIndex, xAxis[i].data))
      -- The (xAxisIndex, xAxis[i].data) are later used to modify the "xAxis" object
      seriesJSVals_updEv :: [(Object, Event t (Int, JSVal))] <-
        forM (Map.elems $ _lineChartConfig_series c) $ \(seriesOpts, dataMap, xAxisData) -> do
          -- series options without the data
          seriesConfig <- liftJSM (makeObject =<< toJSVal (Some.Some $ SeriesT_Line seriesOpts)) -- Some SeriesT

          let
            i = maybe 0 id (seriesOpts ^. series_xAxisIndex)

          ev <- networkView $ ffor ((,) <$> dataMap <*> xAxisData) $ \(map, xAxisD) -> liftJSM $ do
            -- The ordering of elements is determined by xs
            -- XXX default value of 0 might be wrong here
            seriesLineData <- toJSVal (fmap (\x -> Map.findWithDefault (DataInt 0) x map) xAxisD :: [Data SeriesLine])
            setProp "data" seriesLineData seriesConfig
            toJSVal (xAxisD)

          return (seriesConfig, (,) i <$> ev)

      let
        updEv = mergeList $ map snd seriesJSVals_updEv
        seriesJSVals = map fst seriesJSVals_updEv

      performEvent_ $ ffor updEv $ \xs -> liftJSM $ do
        series <- toJSVal seriesJSVals
        xAxisObj <- getProp "xAxis" optVObj >>= makeObject
        let
          f v = f' v
            `catch` \(JSException e) -> liftIO $ putStrLn
            "reflex-dom-echarts: Error in '_lineChartConfig_series' value.\
           \ The 'xAxis' for specified 'xAxisIndex' does not exist in 'ChartOptions'"
          f' (i, v) = do
              a <- (xAxisObj !! i) >>= makeObject
              setProp "data" v a
        mapM_ f xs
        xAxis <- toJSVal xAxisObj
        setProp "xAxis" xAxis optVObj
        setProp "series" series optVObj
        toJSVal optVObj >>= setOptionWithCatch chart

  return (Chart evRendered evFinished)

data TimeLineChartConfig t k = TimeLineChartConfig
  { _timeLineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart
  , _timeLineChartConfig_options :: Dynamic t ChartOptions
  , _timeLineChartConfig_appendData :: Map k
    ( Series SeriesLine
    , Int -- max number of data points
    , Event t [(UTCTime, Double)]
    )
  }

timeLineChart
  :: forall t m k .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadHold t m
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => TimeLineChartConfig t k
  -> m (Chart t)
timeLineChart c = do
  let
    cDyn = _timeLineChartConfig_options c
    attr = (_timeLineChartConfig_size c) & \(w, h) ->
      "style" =: ("width:" <> tshow w <> "px; height:" <> tshow h <> "px;")
  e <- fst <$> elAttr' "div" attr blank
  p <- getPostBuild
  (evR, onActionR) <- newTriggerEvent
  (evF, onActionF) <- newTriggerEvent

  -- Init the chart
  chartEv <- performEvent $ ffor p $ \_ -> liftJSM $ do
    chart <- X.initECharts $ _element_raw e
    -- This causes a flickr in charts
    -- dont know what is the fix
    -- X.onRenderedAction chart (liftIO $ onActionR ())
    -- X.onFinishedAction chart (liftIO $ onActionF ())
    -- Meanwhile trigger these actions on setting the options below
    return chart

  void $ widgetHold blank $ ffor chartEv $ \chart -> do
    void $ networkView $ ffor cDyn $ \opt -> do
      -- set first options
      optVObj <- liftJSM $ makeObject =<< toJSVal opt

      vs <- forM (Map.elems $ _timeLineChartConfig_appendData c) $ \(s, len, ev) -> do
        -- series object
        sVal <- liftJSM (makeObject =<< toJSVal (Some.Some $ SeriesT_Line s))

        rec
          newArr <- performEvent $ ffor (attach (current arrDyn) ev) $ \(arr, vs) -> liftJSM $ do
            let
              -- The timeline needs special data object with name and value fields
              -- the value has to be a tuple like this to render properly
              f :: (UTCTime, Double) -> Data SeriesLine
              f (t, v) = def
                & data_name ?~ utcTimeToEpoch t
                & data_value ?~ (utcTimeToEpoch t, v)
            n <- mapM toJSVal (map f vs)

            let newArrV = (drop ((length arr) + (length n) - len) arr) ++ n
            v <- toJSVal newArrV
            setProp "data" v sVal
            return $ newArrV

          arrDyn <- holdDyn [] newArr

        return (sVal, () <$ newArr)

      let
        updEv = leftmost $ map snd vs
        seriesJSVals = map fst vs

      performEvent_ $ ffor updEv $ \_ -> liftJSM $ do
        dv <- toJSVal seriesJSVals
        setProp "series" dv optVObj
        toJSVal optVObj >>= setOptionWithCatch chart
        -- This is a workaround, see the comments above
        liftIO $ onActionR ()
        liftIO $ onActionF ()

  return (Chart evR evF)

setOptionWithCatch :: ECharts -> JSVal -> JSM ()
setOptionWithCatch c o = setOptionJSVal c o
  -- When doing development with jsaddle, the exceptions thrown by echarts are not shown
  -- This is a workaround to capture and show the exceptions
  -- `catch` \(JSException e) -> (valToText e) >>= (liftIO . putStrLn . show) >> return ()

tshow :: Show a => a -> Text
tshow = T.pack . show
 
