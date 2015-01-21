module Main where

import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data SnakeState = SnakeState {body :: [(Int, Int)], momentum :: (Int, Int)}

snakeSignal :: Signal SnakeState
snakeSignal = moveSignal
  where
    orthogonal (x, y) dt | x == 0    = ((0, y), dt)
                         | otherwise = ((x, 0), dt)

    initialState = SnakeState { body = [(0,0), (0,1), (0,2), (1,2)], momentum = (0, 1) }
    newState :: ((Int, Int), Double) -> SnakeState -> SnakeState
    newState ((dx, dy), dt) state = state { body = body', momentum = momentum' }
      where
        momentum' | (dx, dy) == (0, 0) = momentum state
                  | otherwise          = (dx, dy)
        body' = (tail . body $ state) ++ [next]
        next = (x + mx, y + my)
        (mx, my) = momentum state
        (x, y) = last . body $ state

    moveSignal = foldp newState initialState
        (orthogonal <~ Keyboard.arrows ~~ stepSignal)

stepSignal :: Signal Double
stepSignal = signal
  where
    signal = foldp step 0 (Time.fps 5)
    step :: Time -> Double -> Double
    step dt n = n + Time.inSeconds dt




stroke = outlined $ solid red
frame = stroke $ rect 400 760

block :: Int -> Int -> Form
block x y = move (fromIntegral $ 40 * x, fromIntegral $ 40 * y) $
    filled white $ rect 40 40

snake :: SnakeState -> [Form]
snake state = map (\(x, y) -> block x y) $ body state

render :: SnakeState -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h $ [frame] ++ snake state

main :: IO ()
main = do
    run config $ render <~ snakeSignal ~~ Window.dimensions

  where
    config = defaultConfig { windowTitle = "Ferocious Snakes" }
