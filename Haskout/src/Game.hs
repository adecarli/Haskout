module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Ball
import Blocks
import Player
import Collision

-- Informações do jogo.

-- | Estado do jogo.
data GameStatus = Game
    { ballLoc   :: Position -- ^ (x, y) coordenada da bola.
    , ballVel   :: Position -- ^ (x, y) velocidade da bola.
    , ballLoc2  :: Position
    , ballVel2  :: Position
    , playerLoc :: Float    -- ^ Posição horizontal do jogador
    , playerVel :: Float    -- ^ Velocidade do jogador.
    , playerLoc2 :: Float
    , playerVel2 :: Float
    , playerAcc :: Float    -- ^ Aceleração do jogador.
    , isPaused  :: Bool     -- ^ Indicador do status de pausa.
    , blocks    :: Blocks   -- ^ Lista de blocos na tela;
    , blocks2   :: Blocks
    , gameStat  :: Int      -- ^ Status do jogo: 0 - em jogo, 1 = vitória, -1 = derrota.
    }

-- | Estado inicial do jogo.
initialState :: GameStatus
initialState = Game
    { ballLoc   = (-halfWidth / 2, -100)
    , ballVel   = (50, -200)
    , ballLoc2  = (halfWidth /2, -100)
    , ballVel2  = (50, -200)
    , playerLoc = -halfWidth / 2
    , playerVel = 0
    , playerLoc2 = halfWidth / 2
    , playerVel2 = 0
    , playerAcc = 200
    , isPaused  = True
    , blocks    = map genBlock1 [0..59]
    , blocks2   = map genBlock2 [0..59]
    , gameStat  = 0
    }

-- | Converte o estado do jogo em uma imagem de tela.
render :: GameStatus -> Picture
render game = pictures [ballPics, walls, playerPics, blocksPic, msgPic]
    where
        ballPics   = pictures [ ball $ ballLoc game, ball $ ballLoc2 game]
        playerPics = pictures [ mkPlayer $ playerLoc game, mkPlayer $ playerLoc2 game ]
        blocksPic = pictures  [ drawBlocks $ blocks game, drawBlocks $ blocks2 game ]
        msgPic    = curMsg (gameStat game) (isPaused game) 

-- | Atualiza o estado da bola.
updateBall :: Float -> GameStatus -> GameStatus
updateBall seconds game = game { ballLoc = moveBall seconds pos v, ballLoc2 = moveBall seconds pos2 v2 }
    where pos = ballLoc game
          v   = ballVel game
          pos2 = ballLoc2 game
          v2   = ballVel2 game

-- | Atualiza o estado do jogador.
updatePlayer :: Float -> GameStatus -> GameStatus
updatePlayer seconds game = game { playerLoc = movePlayer seconds x v, playerLoc2 = movePlayer seconds x2 v2 }
    where x = playerLoc game
          v = playerVel game
          x2 = playerLoc2 game
          v2 = playerVel2 game

-- | Atualiza posição da bola de acordo com colisões nas bordas.
updateWall :: Float -> GameStatus -> GameStatus
updateWall seconds game = game { ballVel = wallBounce seconds pos v, ballVel2 = wallBounce seconds pos2 v2 }
    where pos = ballLoc game
          v   = ballVel game
          pos2 = ballLoc2 game
          v2 = ballVel2 game

-- | Atualiza posição da bola de acordo com colisões com o jogador.
updatePaddle :: Float -> GameStatus -> GameStatus
updatePaddle seconds game = game { ballVel = paddleBounce seconds bp bv pp pv, ballVel2 = paddleBounce seconds bp2 bv2 pp2 pv2 }
    where bp = ballLoc   game
          bv = ballVel   game
          pp = playerLoc game
          pv = playerVel game
          bp2 = ballLoc2 game
          bv2 = ballVel2 game
          pp2 = playerLoc2 game
          pv2 = playerVel2 game

-- | Atualiza a posição da bola de acordo com colisões nos blocos e remove blocos.
updateBlocks :: Float -> GameStatus -> GameStatus
updateBlocks seconds game = game { ballVel = ballVel', blocks = blocks', ballVel2 = ballVel2', blocks2 = blocks2' }
    where
        -- atualiza a velocidade da bola ao atingir blocos
        ballVel' = blockCollision seconds bv bp bs
        blocks'  = removeBlocks seconds bs bp bv
        bv = ballVel game
        bp = ballLoc game
        bs = blocks  game
        ballVel2' = blockCollision seconds bv2 bp2 bs2
        blocks2'  = removeBlocks seconds bs2 bp2 bv2
        bv2 = ballVel2 game
        bp2 = ballLoc2 game
        bs2 = blocks2  game

-- | Atualiza o estado do jogo.
update :: Float -> GameStatus -> GameStatus
update seconds game | isPaused game = game
                    | (not (hasBlocks (blocks game))) || (not (hasBlocks (blocks2 game))) = game { gameStat = 1 } 
                    | dropped = game { gameStat = (-1) }
                    | otherwise = collisions $ moves game
                    where
                        dropped    = y < (-halfHeight) - 5
                        y          = snd $ ballLoc game
                        collisions = updatePaddle seconds . updateBlocks seconds . updateWall seconds
                        moves      = updatePlayer seconds . updateBall seconds

-- | Responde aos eventos de teclas.
handleKeys :: Event -> GameStatus -> GameStatus
-- Tecla 'r' retorna ao estado inicial.
handleKeys (EventKey (Char 'r') Down _ _) game = initialState
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _) game = invPause game
-- Tecla 'a' move para a esquerda.
handleKeys (EventKey (Char 'a') Down _ _) game = decVel game 1
-- Soltar a tecla 'a' para o jogador.
handleKeys (EventKey (Char 'a') Up _ _) game = incVel game 1
-- Tecla 'd' move o jogador para a direita.
handleKeys (EventKey (Char 'd') Down _ _) game = incVel game 1
-- Soltar a tecla 'd' para o jogador.
handleKeys (EventKey (Char 'd') Up _ _) game = decVel game 1
-- Tecla '<-' move o jogador 2 para esquerda.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = decVel game 2
-- Soltar '<-' para o jogador 2.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = incVel game 2
-- Tecla '->' move o jogador 2 para direita.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = incVel game 2
-- Soltar '->' para o jogador 2.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = decVel game 2
-- Qualquer outra tecla é ignorada.
handleKeys _ game = game

-- | Incrementa a velocidade do jogador.
incVel :: GameStatus -> Int -> GameStatus
incVel game 1 = game { playerVel = playerVel'}
    where
        playerVel' = playerVel game + playerAcc game
incVel game 2 = game { playerVel2 = playerVel2'}
    where
        playerVel2' = playerVel2 game + playerAcc game

-- | Decrementa a velocidade do jogador.
decVel :: GameStatus -> Int -> GameStatus
decVel game 1 = game { playerVel = playerVel' }
    where
        playerVel' = playerVel game - playerAcc game
decVel game 2 = game { playerVel2 = playerVel2' }
    where
        playerVel2' = playerVel2 game - playerAcc game

-- | Inverte o estado de pausa do jogo.
invPause :: GameStatus -> GameStatus
invPause game = game { isPaused = isPaused' }
    where
        isPaused' = not $ isPaused game