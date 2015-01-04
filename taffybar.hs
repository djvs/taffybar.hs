import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.Battery
import System.Taffybar.MPRIS
import System.Taffybar.CommandRunner

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery
import qualified Graphics.UI.Gtk                      as Gtk
import System.Taffybar.Widgets.PollingLabel

-- util 
import           Control.Monad
import           System.Exit                          (ExitCode (..))
import qualified System.IO as IO
import qualified System.Process as P


-- command runner sucks so i forked it...
commandExecNew :: Double   -- ^ Polling period (in seconds).
               -> String   -- ^ Command to execute. Should be in $PATH or an absolute path
               -> [String] -- ^ Command argument. May be @[]@
               -> String   -- ^ If command fails this will be displayed.
               -> String   -- ^ Output color
               -> String   -- ^ begin wrap
               -> String   -- ^ end wrap
               -> IO Gtk.Widget
commandExecNew interval cmd args defaultOutput color f1 f2 = do
    label  <- pollingLabelNew "" interval $ runCmd cmd args defaultOutput color f1 f2
    Gtk.widgetShowAll label
    return $ Gtk.toWidget label

runCmd :: FilePath -> [String] -> String -> String -> String -> String-> IO String
runCmd cmd args defaultOutput color f1 f2 = do
  (ecode, stdout, stderr) <- P.readProcessWithExitCode cmd args ""
  unless (null stderr) $ do
    IO.hPutStrLn IO.stderr stderr
  return $ wrap f1 f2 . colorize color "" $ case ecode of
    ExitSuccess -> stdout
    ExitFailure _ -> defaultOutput


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
  , widgetSep        = colorize "white" "#000022" "≽ ☯"
  }





main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 1, 0, 1)], graphLabel = Just "m" }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5) ], graphLabel = Just "c" }
      batCfg = defaultBatteryConfig { barPadding     = 1
                                    , barColor       = batColor
                                    , barBorderColor = (1,1,1)
                                    }
      clock = textClockNew Nothing "<span fgcolor='orange' underline='double'>%a %b %_d %H:%M:%S</span>" 1
      pager = taffyPagerNew myPagerConfig
      --note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "KNYC") 10
      mpris = mprisNew
      fsmon = commandExecNew 5 "sh" ["-c", "df -h|grep \" /$\"|awk '{printf $4\"/\"$2}'"] "?" "#FFFFFF" "<span font_weight='bold' gravity='east'>" "</span>"

      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      bat = batteryBarNew batCfg 5
      cfg = defaultTaffybarConfig { barHeight = 25
                                  , barPosition = Bottom
                                  , widgetSpacing = 15
                                  , startWidgets = [ pager ]
                                  , endWidgets = [ tray, wea, clock, fsmon, bat, mem, cpu, mpris ]
                                  }


  defaultTaffybar cfg
