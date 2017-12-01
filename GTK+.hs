import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

-- xcode-select --install
-- (that command on the brew website)
-- brew install gtk+3
-- cabal install gtk3

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle := "Checkers", windowResizable := False, windowDefaultWidth  := 400, windowDefaultHeight := 450 ]
  display <- entryNew
  set display [ entryEditable := False, entryXalign := 0, entryText := "Let's play checkers. Player x make a move." ]
  grid <- gridNew                  -- (1)
  gridSetRowHomogeneous grid True  -- (2)
  let attach x y w h item = gridAttach grid item x y w h -- (3)
  attach 0 9 8 1 display
  --mkBtn ""  >>= attach 0 1 1 1   -- (5)
  mkBtn "o"  >>= attach 1 1 1 1
  --mkBtn ""  >>= attach 2 1 1 1
  mkBtn "o"  >>= attach 3 1 1 1
  --mkBtn ""  >>= attach 4 1 1 1
  mkBtn "o"    >>= attach 5 1 1 1
  --mkBtn ""  >>= attach 6 1 1 1
  mkBtn "o"  >>= attach 7 1 1 1

  mkBtn "o"   >>= attach 0 2 1 1
  --mkBtn ""  >>= attach 1 2 1 1
  mkBtn "o"   >>= attach 2 2 1 1
  --mkBtn ""   >>= attach 3 2 1 1
  mkBtn "o"   >>= attach 4 2 1 1
  --mkBtn ""   >>= attach 5 2 1 1
  mkBtn "o"   >>= attach 6 2 1 1
  --mkBtn ""   >>= attach 7 2 1 1

  --mkBtn ""   >>= attach 0 3 1 1
  mkBtn "o"   >>= attach 1 3 1 1
  --mkBtn ""   >>= attach 2 3 1 1
  mkBtn "o"   >>= attach 3 3 1 1
  --mkBtn ""   >>= attach 4 3 1 1
  mkBtn "o"   >>= attach 5 3 1 1
  --mkBtn ""   >>= attach 6 3 1 1
  mkBtn "o"   >>= attach 7 3 1 1

  mkBtn ""   >>= attach 0 4 1 1
  --mkBtn ""   >>= attach 1 4 1 1
  mkBtn ""   >>= attach 2 4 1 1
  --mkBtn ""   >>= attach 3 4 1 1
  mkBtn "" >>= attach 4 4 1 1
  --mkBtn ""   >>= attach 5 4 1 1
  mkBtn ""   >>= attach 6 4 1 1
  --mkBtn "" >>= attach 7 4 1 1

  --mkBtn ""   >>= attach 0 5 1 1
  mkBtn ""   >>= attach 1 5 1 1
  --mkBtn ""   >>= attach 2 5 1 1
  mkBtn ""   >>= attach 3 5 1 1
  --mkBtn ""   >>= attach 4 5 1 1
  mkBtn ""   >>= attach 5 5 1 1
  --mkBtn ""   >>= attach 6 5 1 1
  mkBtn ""   >>= attach 7 5 1 1

  mkBtn "x"   >>= attach 0 6 1 1
  --mkBtn ""   >>= attach 1 6 1 1
  mkBtn "x"   >>= attach 2 6 1 1
  --mkBtn ""   >>= attach 3 6 1 1
  mkBtn "x"   >>= attach 4 6 1 1
  --mkBtn ""   >>= attach 5 6 1 1
  mkBtn "x"   >>= attach 6 6 1 1
  --mkBtn ""   >>= attach 7 6 1 1

  --mkBtn ""   >>= attach 0 7 1 1
  mkBtn "x"   >>= attach 1 7 1 1
  --mkBtn ""   >>= attach 2 7 1 1
  mkBtn "x"   >>= attach 3 7 1 1
  --mkBtn ""   >>= attach 4 7 1 1
  mkBtn "x"   >>= attach 5 7 1 1
  --mkBtn ""   >>= attach 6 7 1 1
  mkBtn "x"   >>= attach 7 7 1 1

  mkBtn "x"   >>= attach 0 8 1 1
  --mkBtn ""   >>= attach 1 8 1 1
  mkBtn "x"   >>= attach 2 8 1 1
  --mkBtn ""   >>= attach 3 8 1 1
  mkBtn "x"   >>= attach 4 8 1 1
  --mkBtn ""   >>= attach 5 8 1 1
  mkBtn "x"   >>= attach 6 8 1 1
  --mkBtn ""   >>= attach 7 8 1 1

  containerAdd window grid         -- (6)
  widgetShowAll window
  window `on` objectDestroy $ mainQuit
  mainGUI

mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn


