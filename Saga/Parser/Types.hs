
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}



data Expr a where
  EVar :: ByteString -> Expr ByteString
  ENum :: Int -> Expr Int
  EStr :: ByteString -> Expr ByteString
  EBool :: Bool -> Expr Bool
  EArrow :: Expr a -> Expr b -> Expr (a -> b)
  EApp :: Expr (a -> b) -> Expr a -> Expr b
  ELet :: ByteString -> Expr a -> Expr b -> Expr b

deriving instance Show (Expr a)