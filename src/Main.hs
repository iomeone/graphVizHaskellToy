module Main where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

-- import qualified Data.Text.Lazy as TL

-- import Language
data Term = TmVar { index :: Int, contextLength :: Int }
          | TmAbs String Term
          | TmApp Term Term
          | TmBind String -- not really a term, just a binding a free variable
            deriving (Show, Eq)

type CoreDef =  (String, Term)

type CoreAlter = (Int, [String], Term)

type CoreExpr = Term


data  VLabel = VLDirectory | VLSymlink | VLFile

type V = (String, VLabel)

data ELabel = ELHardlink
            | ELSymlink

type E = (String, String, ELabel)

type AstGraph = ([V], [E])

type GraphBuilderState = (Int, AstGraph)
newtype GraphBuilder a = GB (GraphBuilderState -> (a, GraphBuilderState))

instance Functor GraphBuilder where
    -- fmap :: (a -> b) -> GraphBuilder a -> GraphBuilder b
    fmap f b = GB (\s -> let (v, s') = build b s in (f v, s'))

instance Applicative GraphBuilder where
    -- pure :: a -> GraphBuilder a
    pure x = GB (\s -> (x, s))
    -- (<*>) :: GraphBuilder (a -> b) -> GraphBuilder a -> GraphBuilder b
    bf <*> bx = GB (\s -> let (f, s') = build bf s in build (f <$> bx) s')

instance Monad GraphBuilder where
    -- (>>=) :: GraphBuilder a -> (a -> GraphBuilder b) -> GraphBuilder b
    b >>= f = GB (\s -> let (v, s') = build b s in build (f v) s')

empty :: GraphBuilderState
empty = (0, ([], []))

build :: GraphBuilder a -> GraphBuilderState -> (a, GraphBuilderState)
build (GB f) s = f s

evalGraph :: GraphBuilder a -> GraphBuilderState -> AstGraph
evalGraph b s = let (_, (_, g)) = build b s in g

addNode_ :: String -> VLabel -> GraphBuilder String
addNode_ desc vlabel = GB (\s ->
    let (n, (vs, es)) = s
        node = (show n) ++ ": " ++ desc
        vs' = vs ++ [(node, vlabel)]
        s' = (n+1, (vs', es))
    in (node, s'))

addNode :: String -> GraphBuilder String
addNode desc  = addNode_ desc VLSymlink

addEdge_ :: String -> String -> ELabel -> GraphBuilder ()
addEdge_ from to elabel= GB (\s ->
    let (n, (vs, es)) = s
        es' = es ++ [(from, to, elabel)]
        s' = (n, (vs, es'))
    in ((), s'))

addEdge :: String -> String  -> GraphBuilder ()
addEdge from to = addEdge_ from to ELSymlink

addDef :: String -> CoreDef -> GraphBuilder ()
addDef parent (var, body) = do
    n <- addNode $ "Def " ++ var
    addEdge parent n
    addExpr n body

addAlter :: String -> CoreAlter -> GraphBuilder ()
addAlter parent (tag, params, body) = do
    n <- addNode $ "Alter <" ++ show tag ++ "> " ++ unwords params
    addEdge parent n
    addExpr n body

addExpr :: String -> CoreExpr -> GraphBuilder ()
addExpr parent expr = (
    case expr of
      TmAbs str term -> addNode $ "EVar " 
        -- EVar x          -> addNode $ "EVar " ++ x
        -- e@(ENum _)      -> addNode $ show e
        -- e@(EConstr _ _) -> addNode $ show e
        -- EAp lhs rhs     -> do n <- addNode "EAp"
        --                       addExpr n lhs
        --                       addExpr n rhs
        --                       return n
        -- ELet r ds e     -> do n <- addNode $ "ELet " ++ show r
        --                       mapM (addDef n) ds
        --                       addExpr n e
        --                       return n
        -- ECase e as      -> do n <- addNode "ECase"
        --                       addExpr n e
        --                       mapM (addAlter n) as
        --                       return n
        -- ELam ps e       -> do n <- addNode $ "ELam " ++ unwords ps
        --                       addExpr n e
        --                       return n
    ) >>= addEdge parent


type ScDefn = (String, [String], Term)
type CoreScDefn = ScDefn

addScDefn :: String -> CoreScDefn -> GraphBuilder ()
addScDefn parent (var, params, body) = do
    n <- addNode $ "ScDefn " ++ var ++ " " ++ unwords params
    addEdge parent n
    addExpr n body

-- addProgram :: CoreProgram -> GraphBuilder ()
-- addProgram scs = do
--     n <- addNode "Program"
--     mapM (addScDefn n) scs
--     return ()

astGraphParams :: G.GraphvizParams String VLabel ELabel () VLabel
astGraphParams = G.defaultParams




-- GraphVisParams vertexType vertexLabeltype edgeLabelType clusterType clusterLabelType
fileGraphParams :: G.GraphvizParams String VLabel ELabel () VLabel
fileGraphParams = G.defaultParams {
  G.globalAttributes =
    [ G.GraphAttrs
      [ G.Overlap (G.PrismOverlap Nothing)
      , G.OutputOrder G.EdgesFirst
      , G.RankDir G.FromTop
      , G.BgColor [G.toWColor G.Transparent]

      ]
    , G.NodeAttrs
       [ G.Style
         [ G.SItem G.Filled []
         ]
        -- ,G.Shape G.Circle
       ]
    ],
  G.fmtNode = \(v, vl) -> case vl of
      VLDirectory -> (G.Shape G.Circle) : ( colorAttribute $ G.RGB 0 0 255)
      VLSymlink   -> colorAttribute $ G.RGB 40 255 40
      VLFile      -> colorAttribute $ G.RGB 255 40 40,
  G.fmtEdge = \(from, to, el) -> case el of
      ELHardlink -> colorAttribute $ G.RGB 0 0 255
      ELSymlink  -> colorAttribute $ G.RGB 40 255 40
      } 
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]





-- mkDotAstGraph :: CoreProgram -> String
-- mkDotAstGraph prog = let (vs, es) = evalGraph (addProgram prog) empty
--                          dotGraph = G.graphElemsToDot astGraphParams vs es :: G.DotGraph String
--                          dotText = G.printDotGraph dotGraph :: TL.Text
--                      in TL.unpack dotText  

tgb :: GraphBuilder ()
tgb = do
  h <- addNode_ "hello" VLDirectory
  w <- addNode_ "world" VLSymlink
  addEdge_ h w ELHardlink



main :: IO ()
main = 
  let 
    (vs, es) = evalGraph tgb empty 
    dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String
  in  do
    G.addExtension (G.runGraphviz dotGraph) G.Png "graph"
    putStrLn "hello world"

