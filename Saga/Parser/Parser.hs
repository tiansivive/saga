{-# OPTIONS_GHC -w #-}
module Saga.Parser.Parser where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.AST.Syntax as AST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,114) ([49152,21507,16,64,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,21507,16,960,4180,16384,0,0,64,0,16384,0,0,0,1024,0,0,1,0,128,0,8192,0,0,512,0,2048,0,960,4180,0,0,0,0,0,49152,21507,16,0,0,0,0,0,960,4180,49152,21507,16,0,0,0,0,0,0,512,0,0,0,64,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSaga","identifier","assign","pairs","record","listElements","array","tuple","literal","args","lambda","expr","id","number","string","boolean","let","in","where","with","if","then","else","match","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'='","'|'","'.'","'\\\\'","%eof"]
        bit_start = st Prelude.* 40
        bit_end = (st Prelude.+ 1) Prelude.* 40
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..39]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (15) = happyShift action_2
action_0 (16) = happyShift action_11
action_0 (17) = happyShift action_12
action_0 (18) = happyShift action_13
action_0 (27) = happyShift action_14
action_0 (29) = happyShift action_15
action_0 (31) = happyShift action_16
action_0 (37) = happyShift action_17
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (13) = happyGoto action_9
action_0 (14) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (36) = happyShift action_25
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_21

action_5 _ = happyReduce_17

action_6 _ = happyReduce_16

action_7 _ = happyReduce_15

action_8 _ = happyReduce_22

action_9 _ = happyReduce_23

action_10 (40) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_12

action_12 _ = happyReduce_13

action_13 _ = happyReduce_14

action_14 (15) = happyShift action_2
action_14 (16) = happyShift action_11
action_14 (17) = happyShift action_12
action_14 (18) = happyShift action_13
action_14 (27) = happyShift action_14
action_14 (29) = happyShift action_15
action_14 (31) = happyShift action_16
action_14 (37) = happyShift action_17
action_14 (4) = happyGoto action_3
action_14 (5) = happyGoto action_4
action_14 (7) = happyGoto action_5
action_14 (8) = happyGoto action_24
action_14 (9) = happyGoto action_6
action_14 (10) = happyGoto action_7
action_14 (11) = happyGoto action_8
action_14 (13) = happyGoto action_9
action_14 (14) = happyGoto action_23
action_14 _ = happyReduce_7

action_15 (15) = happyShift action_2
action_15 (16) = happyShift action_11
action_15 (17) = happyShift action_12
action_15 (18) = happyShift action_13
action_15 (27) = happyShift action_14
action_15 (29) = happyShift action_15
action_15 (31) = happyShift action_16
action_15 (37) = happyShift action_17
action_15 (4) = happyGoto action_3
action_15 (5) = happyGoto action_4
action_15 (7) = happyGoto action_5
action_15 (8) = happyGoto action_22
action_15 (9) = happyGoto action_6
action_15 (10) = happyGoto action_7
action_15 (11) = happyGoto action_8
action_15 (13) = happyGoto action_9
action_15 (14) = happyGoto action_23
action_15 _ = happyReduce_7

action_16 (15) = happyShift action_2
action_16 (4) = happyGoto action_20
action_16 (6) = happyGoto action_21
action_16 _ = happyReduce_3

action_17 (15) = happyShift action_2
action_17 (4) = happyGoto action_18
action_17 (12) = happyGoto action_19
action_17 _ = happyReduce_18

action_18 (15) = happyShift action_2
action_18 (4) = happyGoto action_18
action_18 (12) = happyGoto action_33
action_18 _ = happyReduce_18

action_19 (35) = happyShift action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (33) = happyShift action_31
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (32) = happyShift action_30
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (30) = happyShift action_29
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (34) = happyShift action_28
action_23 _ = happyReduce_8

action_24 (28) = happyShift action_27
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (15) = happyShift action_2
action_25 (16) = happyShift action_11
action_25 (17) = happyShift action_12
action_25 (18) = happyShift action_13
action_25 (27) = happyShift action_14
action_25 (29) = happyShift action_15
action_25 (31) = happyShift action_16
action_25 (37) = happyShift action_17
action_25 (4) = happyGoto action_3
action_25 (5) = happyGoto action_4
action_25 (7) = happyGoto action_5
action_25 (9) = happyGoto action_6
action_25 (10) = happyGoto action_7
action_25 (11) = happyGoto action_8
action_25 (13) = happyGoto action_9
action_25 (14) = happyGoto action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_2

action_27 _ = happyReduce_11

action_28 (15) = happyShift action_2
action_28 (16) = happyShift action_11
action_28 (17) = happyShift action_12
action_28 (18) = happyShift action_13
action_28 (27) = happyShift action_14
action_28 (29) = happyShift action_15
action_28 (31) = happyShift action_16
action_28 (37) = happyShift action_17
action_28 (4) = happyGoto action_3
action_28 (5) = happyGoto action_4
action_28 (7) = happyGoto action_5
action_28 (8) = happyGoto action_36
action_28 (9) = happyGoto action_6
action_28 (10) = happyGoto action_7
action_28 (11) = happyGoto action_8
action_28 (13) = happyGoto action_9
action_28 (14) = happyGoto action_23
action_28 _ = happyReduce_7

action_29 _ = happyReduce_10

action_30 _ = happyReduce_6

action_31 (15) = happyShift action_2
action_31 (16) = happyShift action_11
action_31 (17) = happyShift action_12
action_31 (18) = happyShift action_13
action_31 (27) = happyShift action_14
action_31 (29) = happyShift action_15
action_31 (31) = happyShift action_16
action_31 (37) = happyShift action_17
action_31 (4) = happyGoto action_3
action_31 (5) = happyGoto action_4
action_31 (7) = happyGoto action_5
action_31 (9) = happyGoto action_6
action_31 (10) = happyGoto action_7
action_31 (11) = happyGoto action_8
action_31 (13) = happyGoto action_9
action_31 (14) = happyGoto action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_2
action_32 (16) = happyShift action_11
action_32 (17) = happyShift action_12
action_32 (18) = happyShift action_13
action_32 (27) = happyShift action_14
action_32 (29) = happyShift action_15
action_32 (31) = happyShift action_16
action_32 (37) = happyShift action_17
action_32 (4) = happyGoto action_3
action_32 (5) = happyGoto action_4
action_32 (7) = happyGoto action_5
action_32 (9) = happyGoto action_6
action_32 (10) = happyGoto action_7
action_32 (11) = happyGoto action_8
action_32 (13) = happyGoto action_9
action_32 (14) = happyGoto action_34
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_19

action_34 _ = happyReduce_20

action_35 (34) = happyShift action_37
action_35 _ = happyReduce_5

action_36 _ = happyReduce_9

action_37 (15) = happyShift action_2
action_37 (4) = happyGoto action_20
action_37 (6) = happyGoto action_38
action_37 _ = happyReduce_3

action_38 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (unTok happy_var_1 (\range (T.Id name) -> AST.Name range name)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (AST.Assignment (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  6 happyReduction_3
happyReduction_3  =  HappyAbsSyn6
		 ([]
	)

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (AST.LRecord (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (AST.LArray (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (AST.LTuple (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (unTok happy_var_1 (\range (T.Number int) -> AST.LInt range int)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (unTok happy_var_1 (\range (T.String string) -> AST.LString range string)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (unTok happy_var_1 (\range (T.Boolean boolean) -> AST.LBool range boolean)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  12 happyReduction_18
happyReduction_18  =  HappyAbsSyn12
		 ([]
	)

happyReduce_19 = happySpecReduce_2  12 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (AST.Lambda (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn14
		 (AST.Assign happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 (AST.Lit happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 40 40 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 15;
	L.RangedToken (T.Number _) _ -> cont 16;
	L.RangedToken (T.String _) _ -> cont 17;
	L.RangedToken (T.Boolean _) _ -> cont 18;
	L.RangedToken T.Let _ -> cont 19;
	L.RangedToken T.In _ -> cont 20;
	L.RangedToken T.Where _ -> cont 21;
	L.RangedToken T.With _ -> cont 22;
	L.RangedToken T.If _ -> cont 23;
	L.RangedToken T.Then _ -> cont 24;
	L.RangedToken T.Else _ -> cont 25;
	L.RangedToken T.Match _ -> cont 26;
	L.RangedToken T.LParen _ -> cont 27;
	L.RangedToken T.RParen _ -> cont 28;
	L.RangedToken T.LBrack _ -> cont 29;
	L.RangedToken T.RBrack _ -> cont 30;
	L.RangedToken T.LCurly _ -> cont 31;
	L.RangedToken T.RCurly _ -> cont 32;
	L.RangedToken T.Colon _ -> cont 33;
	L.RangedToken T.Comma _ -> cont 34;
	L.RangedToken T.Arrow _ -> cont 35;
	L.RangedToken T.Equals _ -> cont 36;
	L.RangedToken T.Pipe _ -> cont 37;
	L.RangedToken T.Dot _ -> cont 38;
	L.RangedToken T.BackSlash _ -> cont 39;
	_ -> happyError' (tk, [])
	})

happyError_ explist 40 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen = (>>=)
happyReturn :: () => a -> L.Alex a
happyReturn = (pure)
happyThen1 :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> L.Alex a
happyReturn1 = happyReturn
happyError' :: () => ((L.RangedToken), [Prelude.String]) -> L.Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseSaga = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok range) contructor = contructor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2



parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)



runSaga :: String -> Either String (AST.Expr L.Range)
runSaga input = L.runAlex (BS.pack input) parseSaga
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
