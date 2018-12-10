module Player where

import Graphics.Gloss
import Window

-- Informações do jogador

-- | Cor do jogador.
playerColor :: Color
playerColor = blue

-- | Tamanho do jogador.
playerWidth :: Float
playerWidth = 60

halfPlayerWidth :: Float
halfPlayerWidth = playerWidth / 2

playerHeight :: Float
playerHeight = 10

halfPlayerHeight :: Float
halfPlayerHeight = playerHeight / 2

-- | Posição do jogador no eixo y.
playerY :: Float
playerY = -halfHeight + 20

-- | Imagem do jogador.
mkPlayer :: Float -> Picture
mkPlayer x = translate x playerY $ color playerColor $ rectangleSolid playerWidth playerHeight

-- | Atualiza posição do jogador
movePlayer :: Float -> Float -> Float -> Float
movePlayer seconds x v
    | paddleWallCollision (x + deltaX) = x
    | otherwise = x + deltaX
    where
        deltaX = v * seconds

-- | Verifica se o jogador atingiu a parede da esquerda.
leftWallCollision :: Float -> Bool
leftWallCollision x 
    | x - halfPlayerWidth <= -halfWidth + 5 = True
    | otherwise                             = False

-- | Verifica se o jogador atingiu a parede da direita.
rightWallCollision :: Float -> Bool
rightWallCollision x
    | x + halfPlayerWidth >= halfWidth - 5 = True
    | otherwise                            = False

-- | Verifica se o jogador atingiu a parede do meio.
middleWallCollision :: Float -> Bool
middleWallCollision x
    | (x + halfPlayerWidth >= (-5)) && (x - halfPlayerWidth < (-5)) = True
    | (x - halfPlayerWidth <=  5) && (x + halfPlayerWidth > 5) = True
    | otherwise                 = False

-- | Verifica se o jogador atingiu a parede.
paddleWallCollision :: Float -> Bool
paddleWallCollision x = leftWallCollision x || rightWallCollision x || middleWallCollision x