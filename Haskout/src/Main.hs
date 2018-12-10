-- | Jogo no estilo Breakout implementado em Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Window
import Game

-- Propriedades da animação

-- | Número de frames por segundo.
fps :: Int
fps = 60

-- | Criação da janela.
main :: IO ()
main = play window background fps initialState render handleKeys update
