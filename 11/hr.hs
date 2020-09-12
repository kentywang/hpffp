data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = (+) (eval a) (eval b)

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

main :: IO ()
main = do
  print $ eval (Add (Lit 1) (Lit 9001))
  putStrLn $ printExpr a3