{-# OPTIONS_GHC -w #-}
module Saga.Parser.ParserHM
    ( runSagaExpr
    -- , runSagaScript
    , runSagaType
    -- , runSagaKind
    -- , runSagaDec
    , parseSagaExpr
    , parseSagaType
    -- , parseSagaKind
    -- , parseSagaDec
    ) where

import Data.Char (isLower)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe  (Maybe (..), fromJust)
import Data.Monoid (First (..))
import Data.List (last, head)

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.Parser.ParsingInfo as P
import           Saga.Parser.ParsingInfo ((<->))

import qualified Saga.AST.TypeSystem.Types as Types
import qualified Saga.AST.TypeSystem.Kinds as Kinds

import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import qualified Saga.AST.Scripts as Scripts
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
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
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,181) ([0,60,256,2048,2048,0,30,0,1024,1024,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1920,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,0,4,32,32,30720,0,2,16,16,0,0,0,0,0,512,0,0,1024,0,0,0,0,4,0,0,16384,0,0,0,1984,0,32768,0,0,32,0,0,64,0,0,0,16384,32,0,120,0,4096,4096,0,124,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,61440,0,0,32,32,0,0,0,0,0,0,0,0,0,0,7680,32768,0,4,4,0,0,0,0,0,1920,8192,0,1,1,0,0,0,0,0,0,8192,0,0,0,0,0,0,32,0,120,512,4096,4096,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","%start_parseSagaType","identifier","params","args","fnApplication","controlFlow","term","atom","assignment","expr","type","typeAtom","typeArgs","typeExpr","typeAnnotation","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 79
        bit_end = (st Prelude.+ 1) Prelude.* 79
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..78]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (19) = happyShift action_3
action_0 (20) = happyShift action_19
action_0 (21) = happyShift action_20
action_0 (22) = happyShift action_21
action_0 (41) = happyShift action_22
action_0 (60) = happyShift action_23
action_0 (76) = happyShift action_24
action_0 (5) = happyGoto action_13
action_0 (8) = happyGoto action_14
action_0 (9) = happyGoto action_15
action_0 (10) = happyGoto action_16
action_0 (11) = happyGoto action_17
action_0 (13) = happyGoto action_18
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_3
action_1 (20) = happyShift action_8
action_1 (21) = happyShift action_9
action_1 (22) = happyShift action_10
action_1 (60) = happyShift action_11
action_1 (76) = happyShift action_12
action_1 (5) = happyGoto action_4
action_1 (14) = happyGoto action_5
action_1 (15) = happyGoto action_6
action_1 (17) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (19) = happyReduce_24
action_4 (20) = happyReduce_24
action_4 (21) = happyReduce_24
action_4 (22) = happyReduce_24
action_4 (23) = happyReduce_24
action_4 (60) = happyReduce_24
action_4 (61) = happyReduce_24
action_4 (68) = happyReduce_24
action_4 (79) = happyReduce_24
action_4 _ = happyReduce_24

action_5 _ = happyReduce_23

action_6 (61) = happyReduce_31
action_6 (68) = happyReduce_31
action_6 (79) = happyReduce_31
action_6 (16) = happyGoto action_32
action_6 _ = happyReduce_26

action_7 (68) = happyShift action_31
action_7 (79) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_20

action_9 _ = happyReduce_22

action_10 _ = happyReduce_21

action_11 (19) = happyShift action_3
action_11 (20) = happyShift action_8
action_11 (21) = happyShift action_9
action_11 (22) = happyShift action_10
action_11 (60) = happyShift action_11
action_11 (76) = happyShift action_12
action_11 (5) = happyGoto action_4
action_11 (14) = happyGoto action_5
action_11 (15) = happyGoto action_6
action_11 (17) = happyGoto action_30
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyGoto action_29
action_12 _ = happyReduce_3

action_13 _ = happyReduce_12

action_14 _ = happyReduce_17

action_15 _ = happyReduce_16

action_16 _ = happyReduce_13

action_17 (42) = happyReduce_19
action_17 (43) = happyReduce_19
action_17 (61) = happyReduce_19
action_17 (79) = happyReduce_19
action_17 (7) = happyGoto action_28
action_17 _ = happyReduce_5

action_18 (79) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_9

action_20 _ = happyReduce_11

action_21 _ = happyReduce_10

action_22 (19) = happyShift action_3
action_22 (20) = happyShift action_19
action_22 (21) = happyShift action_20
action_22 (22) = happyShift action_21
action_22 (41) = happyShift action_22
action_22 (60) = happyShift action_23
action_22 (76) = happyShift action_24
action_22 (5) = happyGoto action_13
action_22 (8) = happyGoto action_14
action_22 (9) = happyGoto action_15
action_22 (10) = happyGoto action_16
action_22 (11) = happyGoto action_17
action_22 (13) = happyGoto action_27
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (19) = happyShift action_3
action_23 (20) = happyShift action_19
action_23 (21) = happyShift action_20
action_23 (22) = happyShift action_21
action_23 (41) = happyShift action_22
action_23 (60) = happyShift action_23
action_23 (76) = happyShift action_24
action_23 (5) = happyGoto action_13
action_23 (8) = happyGoto action_14
action_23 (9) = happyGoto action_15
action_23 (10) = happyGoto action_16
action_23 (11) = happyGoto action_17
action_23 (13) = happyGoto action_26
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyGoto action_25
action_24 _ = happyReduce_3

action_25 (19) = happyShift action_3
action_25 (68) = happyShift action_43
action_25 (5) = happyGoto action_37
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (61) = happyShift action_42
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (42) = happyShift action_41
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (19) = happyShift action_3
action_28 (20) = happyShift action_19
action_28 (21) = happyShift action_20
action_28 (22) = happyShift action_21
action_28 (23) = happyShift action_40
action_28 (60) = happyShift action_23
action_28 (5) = happyGoto action_13
action_28 (10) = happyGoto action_16
action_28 (11) = happyGoto action_39
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (19) = happyShift action_3
action_29 (68) = happyShift action_38
action_29 (5) = happyGoto action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (61) = happyShift action_36
action_30 (68) = happyShift action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (19) = happyShift action_3
action_31 (20) = happyShift action_8
action_31 (21) = happyShift action_9
action_31 (22) = happyShift action_10
action_31 (60) = happyShift action_11
action_31 (76) = happyShift action_12
action_31 (5) = happyGoto action_4
action_31 (14) = happyGoto action_5
action_31 (15) = happyGoto action_6
action_31 (17) = happyGoto action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (19) = happyShift action_3
action_32 (20) = happyShift action_8
action_32 (21) = happyShift action_9
action_32 (22) = happyShift action_10
action_32 (23) = happyShift action_34
action_32 (60) = happyShift action_11
action_32 (5) = happyGoto action_4
action_32 (14) = happyGoto action_5
action_32 (15) = happyGoto action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_27

action_34 _ = happyReduce_30

action_35 (68) = happyShift action_31
action_35 _ = happyReduce_28

action_36 _ = happyReduce_25

action_37 _ = happyReduce_4

action_38 (19) = happyShift action_3
action_38 (20) = happyShift action_8
action_38 (21) = happyShift action_9
action_38 (22) = happyShift action_10
action_38 (60) = happyShift action_11
action_38 (76) = happyShift action_12
action_38 (5) = happyGoto action_4
action_38 (14) = happyGoto action_5
action_38 (15) = happyGoto action_6
action_38 (17) = happyGoto action_46
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_6

action_40 _ = happyReduce_7

action_41 (19) = happyShift action_3
action_41 (20) = happyShift action_19
action_41 (21) = happyShift action_20
action_41 (22) = happyShift action_21
action_41 (41) = happyShift action_22
action_41 (60) = happyShift action_23
action_41 (76) = happyShift action_24
action_41 (5) = happyGoto action_13
action_41 (8) = happyGoto action_14
action_41 (9) = happyGoto action_15
action_41 (10) = happyGoto action_16
action_41 (11) = happyGoto action_17
action_41 (13) = happyGoto action_45
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_14

action_43 (19) = happyShift action_3
action_43 (20) = happyShift action_19
action_43 (21) = happyShift action_20
action_43 (22) = happyShift action_21
action_43 (41) = happyShift action_22
action_43 (60) = happyShift action_23
action_43 (76) = happyShift action_24
action_43 (5) = happyGoto action_13
action_43 (8) = happyGoto action_14
action_43 (9) = happyGoto action_15
action_43 (10) = happyGoto action_16
action_43 (11) = happyGoto action_17
action_43 (13) = happyGoto action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_18

action_45 (43) = happyShift action_47
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (68) = happyShift action_31
action_46 _ = happyReduce_29

action_47 (19) = happyShift action_3
action_47 (20) = happyShift action_19
action_47 (21) = happyShift action_20
action_47 (22) = happyShift action_21
action_47 (41) = happyShift action_22
action_47 (60) = happyShift action_23
action_47 (76) = happyShift action_24
action_47 (5) = happyGoto action_13
action_47 (8) = happyGoto action_14
action_47 (9) = happyGoto action_15
action_47 (10) = happyGoto action_16
action_47 (11) = happyGoto action_17
action_47 (13) = happyGoto action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_8

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (P.identifier happy_var_1 HM.Identifier
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  6 happyReduction_3
happyReduction_3  =  HappyAbsSyn6
		 ([]
	)

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 9 happyReduction_8
happyReduction_8 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  10 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (P.number HM.LInt happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (P.string HM.LString happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (P.term happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 (P.assignment happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.number (HM.TLiteral . HM.LInt) happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.boolean (HM.TLiteral . HM.LBool) happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.string (HM.TLiteral . HM.LString) happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (P.tyExpr happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn15
		 (P.tyIdentifier happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  16 happyReduction_26
happyReduction_26  =  HappyAbsSyn16
		 ([]
	)

happyReduce_27 = happySpecReduce_2  16 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 17 happyReduction_29
happyReduction_29 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  18 happyReduction_32
happyReduction_32  =  HappyAbsSyn18
		 (Nothing
	)

happyReduce_33 = happySpecReduce_2  18 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Just happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 79 79 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 19;
	L.RangedToken (T.Number _) _ -> cont 20;
	L.RangedToken (T.String _) _ -> cont 21;
	L.RangedToken (T.Boolean _) _ -> cont 22;
	L.RangedToken (T.Operator "!") _ -> cont 23;
	L.RangedToken (T.Operator "+") _ -> cont 24;
	L.RangedToken (T.Operator "-") _ -> cont 25;
	L.RangedToken (T.Operator "*") _ -> cont 26;
	L.RangedToken (T.Operator "/") _ -> cont 27;
	L.RangedToken (T.Operator "==") _ -> cont 28;
	L.RangedToken (T.Operator "!=") _ -> cont 29;
	L.RangedToken (T.Operator "<") _ -> cont 30;
	L.RangedToken (T.Operator "<=") _ -> cont 31;
	L.RangedToken (T.Operator ">") _ -> cont 32;
	L.RangedToken (T.Operator ">=") _ -> cont 33;
	L.RangedToken (T.Operator "||") _ -> cont 34;
	L.RangedToken (T.Operator "&&") _ -> cont 35;
	L.RangedToken (T.Operator _) _ -> cont 36;
	L.RangedToken T.Let _ -> cont 37;
	L.RangedToken T.In _ -> cont 38;
	L.RangedToken T.Where _ -> cont 39;
	L.RangedToken T.With _ -> cont 40;
	L.RangedToken T.If _ -> cont 41;
	L.RangedToken T.Then _ -> cont 42;
	L.RangedToken T.Else _ -> cont 43;
	L.RangedToken T.Match _ -> cont 44;
	L.RangedToken T.Return _ -> cont 45;
	L.RangedToken T.Data _ -> cont 46;
	L.RangedToken T.Type _ -> cont 47;
	L.RangedToken T.Alias _ -> cont 48;
	L.RangedToken T.Kind _ -> cont 49;
	L.RangedToken T.Forall _ -> cont 50;
	L.RangedToken T.Exists _ -> cont 51;
	L.RangedToken T.Proof _ -> cont 52;
	L.RangedToken T.Infer _ -> cont 53;
	L.RangedToken T.Protocol _ -> cont 54;
	L.RangedToken T.Interface _ -> cont 55;
	L.RangedToken T.Instance _ -> cont 56;
	L.RangedToken T.Implements _ -> cont 57;
	L.RangedToken T.Module _ -> cont 58;
	L.RangedToken T.Import _ -> cont 59;
	L.RangedToken T.LParen _ -> cont 60;
	L.RangedToken T.RParen _ -> cont 61;
	L.RangedToken T.LBrack _ -> cont 62;
	L.RangedToken T.RBrack _ -> cont 63;
	L.RangedToken T.LCurly _ -> cont 64;
	L.RangedToken T.RCurly _ -> cont 65;
	L.RangedToken T.Colon _ -> cont 66;
	L.RangedToken T.Comma _ -> cont 67;
	L.RangedToken T.Arrow _ -> cont 68;
	L.RangedToken T.BackArrow _ -> cont 69;
	L.RangedToken T.FatArrow _ -> cont 70;
	L.RangedToken T.PipeArrow _ -> cont 71;
	L.RangedToken T.Equals _ -> cont 72;
	L.RangedToken T.Pipe _ -> cont 73;
	L.RangedToken T.Dot _ -> cont 74;
	L.RangedToken T.Section _ -> cont 75;
	L.RangedToken T.BackSlash _ -> cont 76;
	L.RangedToken T.Newline _ -> cont 77;
	L.RangedToken T.EOF _ -> cont 78;
	_ -> happyError' (tk, [])
	})

happyError_ explist 79 tk = happyError' (tk, explist)
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
happyError' tk = (\(tokens, _) -> P.parseError tokens) tk
parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- binaryOp :: HM.Expr -> L.RangedToken -> HM.Expr -> HM.Expr
-- binaryOp expr1 op expr2 = HM.FnApp (info expr1 <-> info expr2) (unTok op (\range (T.Operator char) -> HM.Identifier (HM.Name range (BS.unpack char)))) [expr1, expr2]





-- runSagaScript :: String -> Either String HM.Script
-- runSagaScript input = input `run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData HM.TypeExpr)
runSagaType input = input `P.run` parseSagaType

-- runSagaKind :: String -> Either String (Kinds.Kind L.Range)
-- runSagaKind input = input `run` parseSagaKind

-- runSagaDec :: String -> Either String HM.Declaration 
-- runSagaDec input = input `run` parseSagaDec
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
