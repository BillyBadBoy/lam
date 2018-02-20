module Repl where

import Terms
import Env
import Parse
import Format

import Control.Exception
import Control.Monad

import System.Exit

repl :: Γ -> IO ()
repl e = putStrLn "Enter cmd:" >> getLine >>= handleCmd e >>= repl

handleCmd :: Γ -> String -> IO Γ
handleCmd env cmd = case split cmd of
    ("clearEnv",    _) -> handleClearEnv
    ("readDefs",    s) -> handleReadDefs    env s
    ("loadDefs",    s) -> handleLoadDefs    env s
    ("intDef",      s) -> handleIntDef      env s
    ("eval"    ,    s) -> handleEval        env s
    ("evalAll" ,    s) -> handleEvalAll     env s
    ("evalInt" ,    s) -> handleEvalInt     env s
    ("evalIntAll" , s) -> handleEvalIntAll  env s
    ("evalBool" ,   s) -> handleEvalBool    env s
    ("evalBoolAll", s) -> handleEvalBoolAll env s
    ("quit"    ,    s) -> handleQuit
    _                  -> unrecognizedCmd  env

unrecognizedCmd :: Γ -> IO Γ
unrecognizedCmd env = do
    putStrLn "> !!! Unrecognized command !!!"
    return env

handleQuit :: IO Γ
handleQuit = exitSuccess

handleEval :: Γ -> String -> IO Γ
handleEval env s = evalLast env s >> return env

handleEvalAll :: Γ -> String -> IO Γ
handleEvalAll env s = do
    evalFully env s
    putStrLn "> end."
    return env

handleEvalInt :: Γ -> String -> IO Γ
handleEvalInt env s = evalLast env s >>= cast asInt >> return env

handleEvalIntAll :: Γ -> String -> IO Γ
handleEvalIntAll env s = evalFully env s >>= cast asInt >> return env

handleEvalBool :: Γ -> String -> IO Γ
handleEvalBool env s = evalLast env s >>= cast asBool >> return env

handleEvalBoolAll :: Γ -> String -> IO Γ
handleEvalBoolAll env s = evalFully env s >>= cast asBool >> return env

cast :: (Show t) => (Λ -> Maybe t) -> Maybe Λ -> IO ()
cast asT λ = case λ >>= asT of
        (Just x) -> do
            putStrLn $ "which is recognized as: " ++ show x
            putStrLn "> end."
            return ()
        Nothing  -> do
            putStrLn "which could not be cast!"
            putStrLn "> end."
            return ()

handleClearEnv :: IO Γ
handleClearEnv = do
    putStrLn "> Environment cleared of all definitions."
    return Nil

handleReadDefs :: Γ -> String -> IO Γ
handleReadDefs env defs =
  case parse envP defs of
      ((e,""):_) -> do
          putStrLn "> Defintions added to environment."
          return $ e +: env
      _ -> do
          putStrLn "> !!! Could not parse input !!!"
          return env

handleIntDef :: Γ -> String -> IO Γ
handleIntDef env def = do
    putStrLn $ "> Defintion added to environment: \n" ++
               "let " ++ s ++ " = " ++ show λ ++ ";"
    return $ Γ s λ Nil Nil +: env
  where
    λ = Λ' "f" $ Λ' "x" $ foldr A' (V' "x") (replicate n (V' "f"))
    n = read def
    s = 'N':show n

handleLoadDefs :: Γ -> String -> IO Γ
handleLoadDefs env file = do
    result <- try $ readFile file :: IO (Either IOException String)
    case result of
        Left err -> do
            putStrLn "> !!! Could not read file !!!"
            return env
        Right defs -> handleReadDefs env defs

split :: String -> (String, String)
split cmd = (action, param) where
    action = if null $ words cmd then "?" else head $ words cmd
    param  = if null $ words cmd then "" else unwords $ tail $ words cmd

evalFully :: Γ -> String -> IO (Maybe Λ)
evalFully env s = case parse formulaP s of
    [] -> do
        putStrLn "> !!! Could not parse expression !!!"
        return Nothing
    ((f,_):_) -> case deref env f of
        Right λ -> do
            putStrLn "> Expression evaluates to:"
            showReduxes 0 $ βnf λ
        Left s -> do
            putStrLn $ "> !!! " ++ s ++ " !!!"
            return Nothing
    where
        showReduxes n [λ] = do
            putStrLn $ "step " ++ show n ++ ":"
            print λ
            return $ Just λ
        showReduxes n (λ:λs) | n > 9999 = do
            putStrLn "giving up after 10000 steps "
            return Nothing
                             | otherwise =  do
            putStrLn $ "step " ++ show n ++ ":"
            print λ
            showReduxes (n + 1) λs

evalLast :: Γ -> String -> IO (Maybe Λ)
evalLast env s = case parse formulaP s of
    [] -> do
        putStrLn "> !!! Could not parse expression !!!"
        return Nothing
    ((f,_):_) -> case deref env f of
        Right λ -> do
            putStrLn "> Expression evaluates to:"
            let λ' = last $ βnf λ
            print λ'
            return $ Just λ'
        Left s -> do
            putStrLn $ "> !!! " ++ s ++ " !!!"
            return Nothing
