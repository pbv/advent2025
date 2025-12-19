--
-- Custom Backtracking Integer Linear Solver
--
module ILP where

import Test.QuickCheck

import           Data.Maybe (maybeToList)
import           Data.List (inits, tails, nub, (\\))

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Control.Monad (guard)

type Var    = Int
type Coeff  = Int
type Matrix = [[Coeff]]
type Solution = Map Var Int

-- linear constraints
data Constr = Constr ![(Var,Coeff)] !Coeff
  deriving Show

-- example matrices for testing
example0 :: Matrix
example0 =  [[1,1,1,0,0,0,21],
            [1,0,1,1,1,1,37],
            [0,0,0,0,1,0,18],
            [1,0,0,0,0,1,9],
            [0,1,0,1,0,1,8],
            [1,0,0,0,0,1,9]
           ]

example1, example2, example3 :: Matrix
example1 = [[0,0,0,0,1,1,3],[0,1,0,0,0,1,5],[0,0,1,1,1,0,4],[1,1,0,1,0,0,7]]
example2 = [[1,0,1,1,0,7],[0,0,0,1,1,5],[1,1,0,1,1,12],[1,1,0,0,1,7],[1,0,1,0,1,2]]
example3 = [[1,1,1,0,10],[1,0,1,1,11],[1,0,1,1,11],[1,1,0,0,5],[1,1,1,0,10],[0,0,1,0,5]]


-- variables with non-zero coefficients in a matrix row 
variables :: [Coeff] -> [Var]
variables  = map fst . terms

terms :: [Coeff] -> [(Var,Coeff)]
terms row = [(v,c) | (v,c)<-zip [1..] (init row), c/=0]

constraint :: [Coeff] -> Constr
constraint row = Constr (terms row) (last row)

-- compute the sum of terms of a linear expression
linexpr :: Solution -> [(Var,Coeff)] -> Int
linexpr env terms = sum [c*env!v | (v,c)<-terms]


-- independent variables in a list of constraints
-- constraints should be substitution order 
freevars :: [Constr] -> [Var]
freevars cs = go cs [] []
  where
    go [] indep _dep = nub indep
    go (Constr terms rhs:rest) indep dep
      | null terms = go rest indep dep
      | otherwise = let dep' = fst (head terms) : dep
                        indep' = indep ++ (map fst (tail terms) \\ dep)
                    in go rest indep' dep'  

-- solve by backwards substituition and backtracking
solve :: Matrix -> [Solution]
solve matrix = solveRec freeBounds cs Map.empty
  where cs = reverse $ map constraint $ gaussElim matrix
        allBounds = bounds (map constraint matrix)
        freeBounds = [(v, allBounds!v) | v<-freevars cs]

-- compute upper bounds for each variable
bounds :: [Constr] -> Map Var Int
bounds cs
  = Map.fromListWith min [(var,indep)
                         | Constr terms indep <- cs
                         , (var, coef) <- terms ]




solveRec :: [(Var,Int)] -> [Constr] -> Solution -> [Solution] 
solveRec [] cs env
  = [env' | env'<-maybeToList (backsubst cs env) ]
solveRec ((var,b):rest) cs env
  = [env' | val <- [0..b]
          , env' <- solveRec rest cs (Map.insert var val env)
          ]


-- attempt to obtain a solution by back substitution
-- constraints should be in substitution order 
backsubst :: [Constr] -> Solution -> Maybe Solution
backsubst [] env = Just env
backsubst (Constr terms indep:rest) env
  | null terms && indep==0 = backsubst rest env
  | null terms             = Nothing  -- no solution
  | var `Map.member` env   = do
      -- variable already defined; just check the solution
      guard (linexpr env terms == indep)
      backsubst rest env
  | otherwise = 
      let rhs = indep - linexpr env (tail terms)
          val = rhs`div`coeff
          env'= Map.insert var val env
      in
        if val<0 || rhs`mod`coeff/= 0 then
          Nothing
        else
          backsubst rest env' 
  where 
    (var,coeff) = head terms -- leading var and coefficient
          

-- reduce an integer matrix to triangular row-echelon form
gaussElim :: Matrix -> Matrix
gaussElim []   = []
gaussElim rows = elimCol (pivoting rows)

elimCol :: Matrix -> Matrix
elimCol (row@(c:_):rows)
  | c /= 0 = row :
    gaussElim' [ if c'/=0 then
                  add (mult m (tail row)) (mult m' (tail row'))
                else
                  tail row'
              | row'@(c':_) <- rows,
                -- multipliers to cancel leading coefficient 
                let m = - c' `div` gcd c c',
                let m'=   c  `div` gcd c c'
              ]
  | otherwise = row : gaussElim' (map tail rows)
elimCol ([]:rows) = gaussElim rows

gaussElim' :: Matrix -> Matrix
gaussElim' = map (0:) . gaussElim

-- move a row with non-zero leading coefficient to first row
pivoting :: Matrix -> Matrix
pivoting rows@([]:_) = rows
pivoting rows
  = case suffix of
      (row:rows') -> row : (prefix ++ rows')
      []          -> rows
  where prefix = takeWhile ((==0).head) rows
        suffix = dropWhile ((==0).head) rows

-- vector operations
mult :: Coeff -> [Coeff] -> [Coeff]
mult c = map (*c) 

add :: [Coeff] -> [Coeff] -> [Coeff]
add = zipWith (+)



