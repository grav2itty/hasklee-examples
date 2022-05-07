{-# LANGUAGE QuasiQuotes #-}

module Hasklee.Meshlock where

import Control.Lens hiding (children, transform)
import Control.Monad
import qualified Data.Colour.Names as CN
import Data.Default.Class
import Data.Foldable
import Linear
import Text.RawString.QQ

import Hasklee
import Hasklee.Coords
import Hasklee.Weave
import Hasklee.Mesh.Graph as MeshGraph


-- See https://github.com/grav2itty/hasklee/blob/main/docs/ObjectMods.md
-- This algorithm is not correct, as it only allows for a single-width strip to be unfolded at a time.

meshLock :: Int -> Mesh Double -> NScene Double (Object Double)
meshLock block m = do
  let
    ringShape1 = circleL _xy 0.01 5
    ringShape2 = arc (ofCircle 0.1) (-0.6) (pi + 0.6) 20 :: [V3 Double]
    ringMesh = scaleZ 2 $ pipeWeave ringShape1 (repeat 1) ringShape2

    lockRing axisID = do
      i <- newID
      return $ meshObj ringMesh
        & rID i
        & colour' CN.orange
        & component' Click
        & actionS' axisID "pLock.act(self)"
        & record' "unlock" def{dLoopType = FlipFlopLoop}
        (\x -> let r x' = x' >| time 0.3 >| over _local (rotateZ (pi*0.3))
               in r . r . r $ x)
        & actionS' i "self.go.playTween('unlock'); self.go.SetColor{0, 0.5, 0, 1}"

  ringPre <- prefabF1 lockRing def

  let
    lockAxis rIDoffset (edgeIndex, faceIndex1, faceIndex2) = do
      i <- newID
      let faceID1 = rIDoffset + faceIndex1
          faceID2 = rIDoffset + faceIndex2
          (edge, edgeNormal) = MeshGraph.edge mg edgeIndex
          t = mconcat $ onEdge edge edgeNormal 0.5 (spatial originT)
      ring <- ringPre i
      let
          bl = dummyObj
               & transform t
               & rID i
               & rIDT [faceID1, faceID2]
               & luaAwake [r|
                            self.face1 = self.go.idTable(1)
                            self.face2 = self.go.idTable(2)
                            |]
               & luaStart [r|
                            luaObjects[self.face1].connected[self.ID] = true
                            luaObjects[self.face2].connected[self.ID] = true
                            |]
      return $ attach bl ring

    doFace x = do
      i <- newID
      return $ meshObj x
        & recenter
        & reorient0
        & rID i
        & component (Drag 50)
        & doubleSided
        & actionS i [r|
                      if event == "drag" then
                        local v = args[1]
                        local pointW = self.go.transform.parent.position;
                        local axisW = self.go.transform.parent.forward;
                        self.go.transform.RotateAround(pointW, axisW, v[1] + v[2]);
                      end
                      |]
        & luaAwake [r|
                     self.connected = {}
                     self.unconnecting = false
                     self.go.Mono('Drag').enabled = false;
                     |]


    codeHolder = dummyObj
      & luaAwake [r|
                   pLock = {}

                   pLock.act = function(axis)
                     local first = luaObjects[axis.face1]
                     local second = luaObjects[axis.face2]

                     first.unconnecting = true
                     second.unconnecting = true

                     pLock.unconnect(first, axis.ID)
                     pLock.unconnect(second, axis.ID)

                     axis.go.setParent(first.go, true)

                     first.unconnecting = false
                     second.unconnecting = false
                   end

                   pLock.otherFace = function(axis, i)
                     if i == axis.face1 then
                       return axis.face2
                     else
                       return axis.face1
                     end
                   end

                   pLock.connectedCheck = function(face)
                     local conCount, conToID = 0, nil
                     for k, v in pairs(face.connected) do
                       if v == true then
                         conCount = conCount + 1
                         conToID = k
                       end
                     end
                     return conCount, conToID
                   end

                   pLock.unconnect = function(face, i)
                     face.connected[i] = false
                     if face.block then return end

                     local conCount, conToID = pLock.connectedCheck(face)
                     if conCount == 1 then
                       local conTo = luaObjects[conToID]

                       face.go.setParent(conTo.go, true)
                       face.go.Mono('Drag').enabled = true;

                       local other = luaObjects[pLock.otherFace(conTo, face.ID)]
                       if (other.unconnecting == false) then
                         conTo.go.setParent(other.go, true)
                         pLock.unconnect(other, conToID)
                       end
                     end
                   end
                   |]

    mg = MeshGraph.meshGraph m
    m' = MeshGraph.mgFaces mg
    edgeList = faceAdjList $ MeshGraph.mgPlanarGraph mg

  firstFaceID <- use nextid
  fcs <- traverse doFace >=>
         ix block (return . luaStart "self.block = true" . colour CN.navy)
         $ toList m'
  locks <- traverse (lockAxis firstFaceID) edgeList
  let mlock = attach (mconcat fcs) (mconcat locks)

  return $ codeHolder <> mlock


-- Local Variables:
-- reflektor-type: \:reflektor-unity
-- End:
