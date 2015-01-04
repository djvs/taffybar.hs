import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.Battery
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

batColor :: Double -> (Double, Double, Double)
batColor p = if p < 1 then (r, g, 0) else (0.7, 0.7, 0.7)
  where r = if p < 0.5 then 1.0 else 1.0 - (p * 100 - 50) * 5.12/256.0
        g = if p > 0.5 then 1.0 else p * 100 * 5.12/256.0

myPagerConfig :: PagerConfig
myPagerConfig   = PagerConfig
  { activeWindow     = colorize "black" "#C0C0C0" . escape . shorten 100 . wrap " " " "
  , activeLayout     = colorize "white" "" . wrap "  " "  " . escape
  , activeWorkspace  = colorize "orange" "black" . wrap " (" ") " . escape
  , hiddenWorkspace  = colorize "#004400" "#B0B0B0" . wrap "  " "  " . escape
  --, emptyWorkspace  = escape
  , emptyWorkspace   = (\x -> "")
  , visibleWorkspace = wrap "(" ")" . escape
  , urgentWorkspace  = colorize "red" "yellow" . escape
  , widgetSep        = "⎞༊ "
  }

main = do

  let memCfg = defaultGraphConfig { graphDataColors = [(1, 1, 0, 1)], graphLabel = Just "mem" }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5) ], graphLabel = Just "cpu" }
      batCfg = defaultBatteryConfig { barPadding     = 1
                                    , barColor       = batColor
                                    , barBorderColor = (1,1,1)
                                    }

  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew myPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "KNYC") 10
      mpris = mprisNew
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      bat = batteryBarNew batCfg 5

  let cfg = defaultTaffybarConfig { barHeight = 25
                                  , barPosition = Bottom
                                  , widgetSpacing = 8
                                  , startWidgets = [ pager, note ]
                                  , endWidgets = [ tray, wea, clock, bat, mem, cpu, mpris ]
                                  }


  defaultTaffybar cfg
