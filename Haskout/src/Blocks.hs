module Blocks where

import Graphics.Gloss
import Ball
import Window

-- Propriedades dos blocos
-- | Blocos por fileira
blocksPerRow :: Int
blocksPerRow = 10

-- | Tamanho dos blocos.
blockSize :: (Float, Float)
blockSize = (40, 15)

bHalfWidth :: Float
bHalfWidth = (1 + fst blockSize) / 2

bHalfHeight :: Float
bHalfHeight = (1 + snd blockSize) / 2

-- | Informação dos blocos.
data BlockInfo = Block
    { blockPos :: Position  -- ^ (x, y) coordenada do bloco.
    , blockCol :: Color     -- ^ cor do bloco.
    }

-- | Lista dos blocos atuais.
type Blocks = [BlockInfo]

-- | Verifica se ainda existem blocos a serem destruídos.
hasBlocks :: Blocks -> Bool
hasBlocks blocks = not $ length blocks == 0

-- | Desenha os blocos.
drawBlocks :: Blocks -> Picture
drawBlocks bs = pictures $ [drawBlock x | x <- bs]
    where
        drawBlock (Block (x, y) col) = translate x y $ color col $ rectangleSolid w h
        (w, h)                       = blockSize

-- | Gera os blocos.
genBlock :: Int -> Position -> BlockInfo
genBlock n (px, py) = Block { blockPos = pos, blockCol = orange }
    where
        pos = (bx, by)
        bx = px + bHalfWidth + 6 + fromIntegral x * (fst blockSize + 1)
        by = py - fromIntegral y * (snd blockSize + 1)
        (y, x) = n `divMod` blocksPerRow

genBlock1 :: Int -> BlockInfo
genBlock1 n = genBlock n (-halfWidth, 150)
genBlock2 :: Int -> BlockInfo
genBlock2 n = genBlock n (0, 150)