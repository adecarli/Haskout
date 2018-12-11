-- | Jogo no estilo Breakout implementado em Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Window
import Game
import Blocks
import Control.Concurrent
import Control.Concurrent.STM

-- Propriedades da animação

-- | Número de frames por segundo.
fps :: Int
fps = 60

-- | 
produtorDePowerUp :: TVar Blocks -> TVar Blocks -> IO ()
produtorDePowerUp b1 b2 = do
    atomically $ do
        bl1 <- readTVar b1
        bl2 <- readTVar b2
        if (length $ (filter (\x -> typePower x /= None) bl1)) == 0 then do
            let bl1' = head bl1
            writeTVar b1 ((bl1' {typePower = FastBall}) : tail bl1)
            else return()
        if (length $ (filter (\x -> typePower x /= None) bl2)) == 0 then do
            let bl2' = head bl2
            writeTVar b2 ((bl2' {typePower = FastBall}) : tail bl2)
            else return()
    threadDelay 1000000
    produtorDePowerUp b1 b2
        

-- | Criação da janela.
main :: IO ()
main = do 
    bl1 <- atomically $ newTVar (map genBlock1 [0..39])
    bl2 <- atomically $ newTVar (map genBlock2 [0..39])

    let iState = initialState bl1 bl2
    forkIO $ produtorDePowerUp bl1 bl2
    playIO window background fps iState render handleKeys update
