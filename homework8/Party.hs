module Party where

import Employee
import Data.Tree

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (empFun emp + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL emps1 fun1) (GL emps2 fun2)
    | fun1 >= fun2 = GL emps1 fun1
    | otherwise    = GL emps2 fun2

-- Exercise 2
treeFold :: [b] -> ([b] -> Tree a -> b) -> Tree a -> b
treeFold xs f (Node a [])   = xs `f` Node a []
treeFold xs f (Node a node) = map (treeFold xs f) node `f` Node a node

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs emp gls =
    let justEmp = GL [emp] (empFun emp)
    in  case gls of
            [] -> justEmp
            _  -> moreFun justEmp (foldr mappend mempty gls)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (glCons emp mempty, mostFunGL gls)
    where
        funGLs []           = []
        funGLs ((e,g):gls') = moreFun e g : funGLs gls'
        mostFunGL gls'      = foldr moreFun mempty (funGLs gls')

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun (fst result) (snd result)
    where
        maxFun' (Node emp [])   = (glCons emp mempty, mempty)
        maxFun' (Node emp frst) = nextLevel emp (map maxFun' frst)
        result = maxFun' tree

-- Exercise 5
main :: IO ()
main = do str <- readFile "./company.txt" 
          let gl = maxFun (read str)
              getFun (GL _ fun) = fun
              getEmps (GL emps _) = emps
              names = map empName (getEmps gl)
          putStrLn ("Total fun: " ++ show (getFun gl))
          mapM_ putStrLn names
