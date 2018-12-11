module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Concurrent.STM
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
    , blocks    :: TVar Blocks   -- ^ Lista de blocos na tela;
    , blocks2   :: TVar Blocks
    , gameStat  :: Int      -- ^ Status do jogo: 0 - em jogo, 1 = vitória, -1 = derrota.
    
    }

-- | Estado inicial do jogo.
initialState :: TVar Blocks -> TVar Blocks -> GameStatus
initialState b1 b2 = Game
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
    , blocks    = b1
    , blocks2   = b2
    , gameStat  = 0
    }

-- | Converte o estado do jogo em uma imagem de tela.
render :: GameStatus -> IO (Picture)
render game = do
    bl1 <- atomically $ readTVar $ blocks game
    bl2 <- atomically $ readTVar $ blocks2 game
    return (pictures [ballPics, walls, playerPics, pictures[drawBlocks bl1, drawBlocks bl2], msgPic])
    where
        ballPics   = pictures [ ball $ ballLoc game, ball $ ballLoc2 game]
        playerPics = pictures [ mkPlayer $ playerLoc game, mkPlayer $ playerLoc2 game ]
        msgPic     = curMsg (gameStat game) (isPaused game) 

-- | Atualiza o estado da bola.
updateBall :: Float -> GameStatus -> IO (GameStatus)
updateBall seconds game = return $ game { ballLoc = moveBall seconds pos v, ballLoc2 = moveBall seconds pos2 v2 }
    where pos = ballLoc game
          v   = ballVel game
          pos2 = ballLoc2 game
          v2   = ballVel2 game

-- | Atualiza o estado do jogador.
updatePlayer :: Float -> GameStatus -> IO (GameStatus)
updatePlayer seconds game = return $ game { playerLoc = movePlayer seconds x v, playerLoc2 = movePlayer seconds x2 v2 }
    where x = playerLoc game
          v = playerVel game
          x2 = playerLoc2 game
          v2 = playerVel2 game

-- | Atualiza posição da bola de acordo com colisões nas bordas.
updateWall :: Float -> GameStatus -> IO (GameStatus)
updateWall seconds game = return $ game { ballVel = wallBounce seconds pos v, ballVel2 = wallBounce seconds pos2 v2 }
    where pos = ballLoc game
          v   = ballVel game
          pos2 = ballLoc2 game
          v2 = ballVel2 game

-- | Atualiza posição da bola de acordo com colisões com o jogador.
updatePaddle :: Float -> GameStatus -> IO (GameStatus)
updatePaddle seconds game = return $ game { ballVel = paddleBounce seconds bp bv pp pv, ballVel2 = paddleBounce seconds bp2 bv2 pp2 pv2 }
    where bp = ballLoc   game
          bv = ballVel   game
          pp = playerLoc game
          pv = playerVel game
          bp2 = ballLoc2 game
          bv2 = ballVel2 game
          pp2 = playerLoc2 game
          pv2 = playerVel2 game

-- | Atualiza a posição da bola de acordo com colisões nos blocos e remove blocos.
updateBlocks :: Float -> GameStatus -> IO (GameStatus)
updateBlocks seconds game = do
    ((v1x, v1y), (v2x, v2y), pow1, pow2) <- atomically $ do
        bl1 <- readTVar $ blocks game
        bl2 <- readTVar $ blocks2 game
        let (ballVel', p1) = blockCollision seconds bv bp bl1
        let (ballVel2', p2) = blockCollision seconds bv2 bp2 bl2
        writeTVar (blocks game) (removeBlocks seconds bl1 bp bv)
        writeTVar (blocks2 game) (removeBlocks seconds bl2 bp2 bv2)
        return (ballVel', ballVel2', p1, p2)
    let v2 = if pow1 == FastBall then (v2x*1.1, v2y*1.1) else (v2x, v2y)
    let v1 = if pow2 == FastBall then (v1x*1.1, v1y*1.1) else (v1x, v1y)

    return $ game { ballVel = v1, ballVel2 = v2}
        where
        -- atualiza a velocidade da bola ao atingir blocos
            bv = ballVel game
            bp = ballLoc game
            bv2 = ballVel2 game
            bp2 = ballLoc2 game

-- | 
createPowerUp :: GameStatus -> IO (GameStatus)
createPowerUp game = do
    atomically $ do
        bl1 <- readTVar $ blocks game
        bl2 <- readTVar $ blocks2 game
        if length bl1 == 0|| length bl2 == 0 then return () else do
            let bl1' = head bl1
            let bl2' = head bl2
            writeTVar (blocks game) ((bl1' {typePower = FastBall}) : tail bl1)
            writeTVar (blocks2 game) ((bl2' {typePower = FastBall}) : tail bl2)
    return $ game 
        

    

-- | Atualiza o estado do jogo.
update :: Float -> GameStatus -> IO (GameStatus)
update seconds game = do
    bl1 <- atomically $ readTVar $ blocks game
    bl2 <- atomically $ readTVar $ blocks2 game
    if isPaused game then return game else
        if (not $ hasBlocks bl1) then return $ game { gameStat = 1} else
            if (not $ hasBlocks bl2) then return $ game { gameStat = 2} else
                if dropped 1 then return $ game { gameStat = (-1) } else
                    if dropped 2 then return $ game { gameStat = (-2) } else do        
                        x1 <- updateBall seconds game
                        x2 <- updatePlayer seconds x1
                        x3 <- updateWall seconds x2
                        x4 <- updateBlocks seconds x3
                        x5 <- updatePaddle seconds x4
                        return x5
    where
        dropped 1  = y < (-halfHeight) - 5
        dropped 2  = y' < (-halfHeight) - 5
        y          = snd $ ballLoc game
        y'         = snd $ ballLoc2 game

-- | Responde aos eventos de teclas.
handleKeys :: Event -> GameStatus -> IO (GameStatus)
-- Tecla 'r' retorna ao estado inicial.
handleKeys (EventKey (Char 'r') Down _ _) game = do
    atomically $ do
        writeTVar (blocks game) (map genBlock1 [0..39])
        writeTVar (blocks2 game) (map genBlock2 [0..39])
        return ()
    return ( game 
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
        , gameStat  = 0
        } )
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _) game = return $ invPause game
-- Tecla 'a' move para a esquerda.
handleKeys (EventKey (Char 'a') Down _ _) game = return $ decVel game 1
-- Soltar a tecla 'a' para o jogador.
handleKeys (EventKey (Char 'a') Up _ _) game = return $ incVel game 1
-- Tecla 'd' move o jogador para a direita.
handleKeys (EventKey (Char 'd') Down _ _) game = return $ incVel game 1
-- Soltar a tecla 'd' para o jogador.
handleKeys (EventKey (Char 'd') Up _ _) game = return $ decVel game 1
-- Tecla '<-' move o jogador 2 para esquerda.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = return $ decVel game 2
-- Soltar '<-' para o jogador 2.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = return $ incVel game 2
-- Tecla '->' move o jogador 2 para direita.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = return $ incVel game 2
-- Soltar '->' para o jogador 2.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = return $ decVel game 2
-- Qualquer outra tecla é ignorada.
handleKeys _ game = return $ game

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