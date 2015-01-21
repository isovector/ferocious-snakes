module Main where

import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data SnakeState = SnakeState {body :: [(Int, Int)]}

snakeSignal :: Signal SnakeState
snakeSignal = controlSignal
  where
    controlSignal = foldp newState initialState (orthogonal <~ Keyboard.arrows)
    orthogonal (x, y) | x == 0 = (x, y)
                      | otherwise = (x, 0)
    initialState = SnakeState { body = [(0,0), (0,1), (0,2), (1,2)] }
    newState :: (Int, Int) -> SnakeState -> SnakeState
    newState arrows@(dx, dy) state = state { body = newBody }
      where
        newBody | arrows == (0,0) = body state
                | otherwise = (tail $ body state) ++ [newPos]
        newPos = (x + dx, y + dy)
        (x, y) = last $ body state

stroke = outlined $ solid red
frame = stroke $ rect 400 760

block :: Int -> Int -> Form
block x y = move (fromIntegral $ 40 * x, fromIntegral $ 40 * y) $ filled white $ rect 40 40

snake :: SnakeState -> [Form]
snake state = map (\(x, y) -> block x y) $ body state

render :: SnakeState -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h $ [frame] ++ snake state

main :: IO ()
main = do
    run config $ render <~ snakeSignal ~~ Window.dimensions

  where
    config = defaultConfig { windowTitle = "Ferocious Snakes" }
