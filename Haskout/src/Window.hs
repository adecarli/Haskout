module Window where

import Graphics.Gloss

-- Configuração da Tela

-- | Largura da janela.
width :: Int
width = 842

-- | Altura da janela.
height :: Int
height = 450

-- | Posição da janela.
offset :: Int
offset = 100

-- | Cor de fundo.
background :: Color
background = black

-- | Janela do jogo.
window :: Display
window = InWindow "Haskout" (width, height) (offset, offset)

-- | Para uso nas funções de colisão.
-- Esse valor é o extremo da tela
halfWidth :: Float
halfWidth = fromIntegral width / 2

halfHeight :: Float
halfHeight = fromIntegral height / 2

-- | Cor da borda
wallColor :: Color
wallColor = green

-- | Borda de cima.
topWall :: Picture
topWall = translate 0 halfHeight
        $ color wallColor
        $ rectangleSolid (fromIntegral width) 10

-- | Borda esquerda.
leftWall :: Picture
leftWall = translate (-halfWidth) 0
         $ color wallColor
         $ rectangleSolid 10 (fromIntegral height)

-- | Borda direita.
rightWall :: Picture
rightWall = translate halfWidth 0
          $ color wallColor
          $ rectangleSolid 10 (fromIntegral height)

-- | Borda do meio.
middleWall :: Picture
middleWall = translate 0 0
           $ color wallColor
           $ rectangleSolid 10 (fromIntegral height)

-- | Imagem das bordas.
walls :: Picture
walls = pictures [leftWall, rightWall, topWall, middleWall]

-- | Renderiza um texto na tela
renderTxt :: Color -> String -> Picture
renderTxt col msg = translate (-100) 180 $ scale 0.2 0.2 $ color col $ Text msg

-- | Mensagem atual a ser mostrada.
curMsg :: Int -> Bool -> Picture
curMsg   0  paused = pauseMsg paused
curMsg (-1) paused = lostMsg 1
curMsg (-2) paused = lostMsg 2
curMsg   1  paused = winMsg 1
curMsg   2  paused = winMsg 2

winMsg    1    = renderTxt green "Player 1 won!"
winMsg    2    = renderTxt green "Player 2 won!"
lostMsg   1    = renderTxt red   "Player 1 lost!"
lostMsg   2    = renderTxt red   "Player 2 lost!"
pauseMsg True  = renderTxt blue  "Press p to play!"
pauseMsg False = renderTxt blue  ""