-- I want these language extensions for my syntactic sugaring tricks at the end
{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Main where

-- I want my own definition of lookup and I want to write my own function
-- named "print".

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- {-------------------------------------------------------------------}
-- {- The pure expression language                                    -}
-- {-------------------------------------------------------------------}

data Val = I Int | B Bool
            deriving (Eq, Show, Read)

data Expr = Const Val
      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
      | And Expr Expr | Or Expr Expr | Not Expr
      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
      | Var String
    deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
                Just x -> return x
                Nothing -> fail ("Unknown variable "++k)

-- {-- Monadic style expression evaluator,
--  -- with error handling and Reader monad instance to carry dictionary
--  --}

-- ReaderT Env m a
-- ExceptT exceptionType (inner monad), string is exceptionType and Identity is inner monad
type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )
-- ReaderT, like state but can't change the state. Hides the env inside the reader monad


-- This evaluator could be a little neater
-- Integer typed expressions
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

--evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (not2) e0 (Const (B True))
                        where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


-- {-------------------------------------------------------------------}
-- {- The statement language                                          -}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
               | Debug Statement
            deriving (Show, Read, Eq)


type Run a = StateT Env (ExceptT String IO) a -- Env is program state
runRun p =  runExceptT ( runStateT p Map.empty)

-- Op to update the env map
set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table -> ((), Map.insert s i table))
-- hashtable

-- Call Debug whenever a statement would be executed to debug the program
exec :: Statement -> Run ()
exec (Assign s v) = do st <- get -- get environment
                       Right val <- return $ runEval st (eval v)
                       set (s,val) -- Save variable name and value in hashtable


exec (Seq s0 s1) = do exec (Debug s0) >> exec (Debug s1)

exec (Print e) = do st <- get
                    Right val <- return $ runEval st (eval e)
                    liftIO $ System.print val
                    return ()

exec (If cond s0 s1) = do st <- get
                          Right (B val) <- return $ runEval st (eval cond)
                          if val then do exec (Debug s0) else do exec (Debug s1)

exec (While cond s) = do st <- get
                         Right (B val) <- return $ runEval st (eval cond)
                         if val then do exec (Debug s) >> exec (Debug (While cond s)) else return ()

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

exec (Debug (Seq s0 s1))  = exec (Seq s0 s1)  -- Don't need to display the seq statements

-- To bring up debug prompt as the statements from the input file are grouped into one big statement
exec (Debug s) = showPrompt s

-- We never actually expect to encounter one of these, the programs should run fine if we left this equation out:
exec Pass = return ()


showPrompt :: Statement -> Run () -- Terminal prompt when a statement gets executed. Waits for user input for next action
showPrompt s = do getEnvVariables s -- Show system variables at current program state
                  liftIO $ putStrLn $ "Next Statement: " ++ printStatement s
                  debugInput <- liftIO $ getLine
                  case (debugInput) of
                    "n" -> exec s
                    "" -> showPrompt s
                    "h" -> do liftIO $ putStrLn $ printInstructions
                              showPrompt s

printStatement :: Statement -> String -- Shows the current statement
printStatement (While cond _) = "While " ++ (show cond)
printStatement (If cond _ _)  = "If " ++ (show cond)
printStatement Pass = "" -- Don't need to print pass
printStatement s = show s

printInstructions :: String -- To match exec type
printInstructions = "Enter 'n' to step forward"

getEnvVariables :: Statement -> Run ()
getEnvVariables s = do env <- get
                       liftIO $ putStrLn $ "------------ Variables ------------\n"
                       liftIO $ mapM_ (putStrLn . printVariable) $ Map.toList env -- [(Name,Val)] pairs
                       liftIO $ putStrLn $ "\n-----------------------------------\n"

-- printVariables :: [(Name,Val)] -> String
-- printVariables [] = "No variables to print"
-- printVariables ((n,v):xs) = printVariable (n,v)
--                             printVariables xs

printVariable :: (Name, Val) -> String
printVariable (n,v) = "Variable: " ++ n ++ " Value: " ++ show v

-- Need to show program variables state, get them from Table

-- I feel we're hitting the point of diminishing returns here, but I
-- fancy one last example of using a monad. Something to remove the need
-- to explicitely write "Seq" inbetween each pair of statements. Recall
-- that I said that >>= could be though of as a progammable semicolon?

type Program = Writer Statement ()
-- Writer monad is (computation value, log value) pair.

-- The writer monad has an operation, "tell" which appends a piece of
-- output to an accumulated value. For this to work the type we are
-- accumulating (Statement, in this case) must be have both an appending
-- (plus-like) operation and a base (zero-like) operation. In algebra
-- something with that structure is called a Monoid:

instance Monoid Statement where
   mempty = Pass
   mappend a b = a `Seq` b

-- The idea is that the bind of the Writer monad will be used to apply
-- Seq (our semicolon-like operation) between each "statement". The zero
-- statement is needed for theoretical completeness, but it will only
-- appear in a result if we were to write something like this:

-- junk :: Program
-- junk = return ()

-- For this reason we never expect to see it in a real "compiled"
-- statement, so there's no case for it in the exec function.

run :: Program -> IO ()
run program = do result <- runExceptT $ (runStateT $ exec $ snd $ runIdentity $ (runWriterT program)) Map.empty
                 case result of
                      Right ( (), env ) -> return ()
                      Left exn -> System.print ("Uncaught exception: "++exn)


-- prog10 :: Program
-- prog10 =  Assign "foo" (Const (I 1))  `Seq`
--           Print (Var "foo")

stringToProgram :: String -> Program
stringToProgram s = writer ((), statement)
                    where statement = mconcat statements' -- concatenate list of statements into single big statement
                          statements' = map (\line -> read line :: Statement) (lines s)
                          -- lambda function converts each string to Statement type
                          -- lines converts string to [string]

-- Need to read in file with input, convert string to program with single bit Statement.
-- Writer monad 'binds' each Statement with ;, like seq operator
main = do putStrLn "Enter 'n' to step forward, 'h' for help\n"
          fileContents <- readFile "input.txt"
          run (stringToProgram fileContents) -- file string to a executable program
