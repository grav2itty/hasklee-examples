
module Hasklee.Examples where

import Control.Lens hiding (children, transform)
import Control.Monad.State
import Csound.Base
import Csound.Patch
import qualified Data.Colour.Names as CN
import qualified Diagrams as D
import Linear
import System.Random
import Test.QuickCheck.Gen (generate)

import Hasklee
import Hasklee.Coords
import Hasklee.Generate
import Hasklee.Object.Mod
import Hasklee.Pipe
import Hasklee.Randomize


someLight :: NScene Double (Object Double)
someLight = return (dummyObj & pointLight def{lIntensity = 1, lRange = 1000})


modernHarp :: NScene Double ()
modernHarp = do
  --FRAME
  let
    t1 = rotateY (pi*0.5) $ triangleMesh 20
    t2 = rotateZ (-pi*0.3) t1
    frameMesh = reverseMesh (translateX (-10) t1) <> translateX 10 t2

    frame = meshObj frameMesh
      & doubleSided
      & colour CN.silver

  --SPEAKER
  speakerID <- newID
  sound <- csound $ patchByNameMidi "notAHarp" harpsichord
  let
    speaker = dummyObj
      & rID speakerID
      & csoundA sound

  --STRINGS
  let
    stringFun o = do
      i <- newID
      return $ o
        & meshObj . pipeMesh def{pipeRadius = 0.03}
        & rID i
        & component StringPluck
        & actionS speakerID (playNote "notAHarp" (i + 40) 20)
        -- & material "ProcString"
        & colour CN.white

  strings <- liftIO (generate (randomPiping frameMesh 0.1 50)) >>=
             traverse stringFun >>=
             randomizeR (each.allAttributes._ColourAtr.v3) (V3 0 0 0, V3 0.8 0 1)

  newObj =<< someLight
  newObj $ translateZ (-25) $ frame <> speaker <> mconcat strings


noiseInABox :: NScene Double ()
noiseInABox = do
  sound <- csound $ do
    rate <- chnGetCtrl (text "rate")
    -- some csound noise
    let s = mlp (on (rate * 0.5) (rate * 2) $ Csound.Base.tri 0.2) 0.3
            <$> Csound.Base.brown
    -- in stereo
    liftA2 (,) s s

  speakerID <- newID
  screenID <- newID

  let
    speaker = dummyObj
      & rID speakerID
      & csoundA sound

    cub = solidObj (Cube 2)
      -- subdivide cube into 27 solids
      & subdiv (pure [0, 0.1, 0.9, 1])
      & nix (1, 1, 1) .~ solidObj EmptySolid
      & nix (1, 2, 2) .~ solidObj EmptySolid
      & nix (1, 1, 2) %~ fun

    fun = rID screenID
        -- will receive mouse drag
        . component (Drag 1.0)
        -- which will make the object translate along Y axis
        . component (Translator (V3 0 1 0) 0 10 True)
        -- which will invoke the following Lua code on speaker with argument
        -- as distance dragged
        . actionS speakerID
          "if event == 'tran' then self.csound.rate = math.max(0, args[1] * 2000) end"
        . colour CN.crimson

  newObj =<< someLight
  newObj $ translateZ (-5) $ speaker <> cub


rgbPipes :: NScene Double ()
rgbPipes = do
  let
    pipeCount = 24

  --PANEL
    panel = subdiv' pipeCount 3 1
          . colour CN.black
          . meshObj
          . translateZ (-1)
          . translateX (-10)
          . rotateX (-pi * 0.3)
          $ rectangleMesh 24 4
  panel' <- children (rgbSliders 2) panel

  --PIPES
  randomPoints <- liftIO $ replicateM pipeCount $ randomRIO (V3 0 0 0, V3 50 50 (-50))
  let
    pipeShape = PipeSpline [V3 3.3 3.3 0, V3 6.6 (-6.6) 0] (V3 10 0 0)
    pipy = (pipe pipeShape |> pipeShape) `D.at` zero
    pipes = flip translate pipy <$> circleL _yz 2 pipeCount
    pipes' = zipWith f randomPoints pipes

    -- extend pipe to a random point
    f v p =
      let endVertex = _n .~ V3 0 0 (-1) $ vertex v
      in p |> pipeSpline (outbound p) endVertex

    pipesObj =  meshObj . pipeMesh def{pipeRadius = 0.15} <$> pipes'
  pipesObj' <- evalStateT (traverse (rgbObject 5) pipesObj) 1

  --LIGHTS
  let
    pouts = outbound <$> pipes'
    lighty = dummyObj & spotLight (LightOptions 200 20 30 CN.black)
    lights = (\x -> set _pos (x ^. _pos) .
               transform (lookAlong0 (negated $ x ^._n)) $ lighty) <$> pouts

  lights' <- evalStateT (traverse rgbLights lights) 1

  let
    pipeLights = translateZ (-10) $ mconcat pipesObj' <> mconcat lights'
    room = meshObj (Cube 120) & flipSided & colour CN.black

  newObj =<< someLight
  newObj $ translateZ (-20) $ room <> panel' <> pipeLights


buttonGod :: NScene Double ()
buttonGod = do
  let
    m = meshObj $ iterate f (triangleMesh 10) !! 2
    f = over faces (buttonize . closedMesh)
    -- turn each mesh face into a new object
    m1 = liftFaces id m

    buttonizeN o = do
      i <- newID
      return $ o
        & _mesh %~ buttonize . closedMesh
        & rID i
        & component Button
        & colour CN.navy
        & actionS i "self.gameObject.SetColorR(1)"

  m2 <- each buttonizeN m1

  newObj =<< someLight
  newObj $ translateZ (-15) m2


-- Local Variables:
-- reflektor-type: \:reflektor-unity
-- End:
