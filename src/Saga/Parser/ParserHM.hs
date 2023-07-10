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

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26
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
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,382) ([0,15360,0,1,136,8,0,30,0,17408,1024,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,480,0,16384,16388,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1920,8192,0,17,1,49152,3,16,2176,128,0,32,0,0,0,0,0,0,0,0,0,0,8,0,0,16,0,0,0,0,512,0,0,0,0,32768,0,0,57344,1,0,260,0,0,240,64,0,0,0,960,4096,32768,32776,0,57344,1,8,1088,64,0,240,1024,8192,8194,0,30720,0,2,272,16,0,124,0,34816,0,0,512,0,0,1024,0,0,0,0,32768,0,0,0,0,0,32,0,0,0,0,49408,0,0,480,0,16384,16388,0,61440,1,0,544,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,4,0,0,0,0,0,0,0,960,0,32768,32776,0,0,0,0,0,0,0,240,0,8192,8194,0,0,0,0,0,0,0,60,0,34816,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,0,0,24,0,0,0,0,120,512,4096,4097,0,0,0,0,16,0,0,0,0,0,0,0,3840,16384,0,34,2,0,0,0,0,0,0,960,4096,32768,32776,0,57344,1,8,1088,64,0,7680,0,0,0,0,0,15,0,2048,0,0,1920,0,0,4,0,0,0,0,0,0,0,480,256,0,0,0,0,0,0,256,0,0,0,0,49152,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,30,128,17408,1024,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,60,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","%start_parseSagaType","identifier","pairs","record","tupleElems","tuple","params","args","fnApplication","controlFlow","term","atom","assignment","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","typeExpr","typeAnnotation","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 87
        bit_end = (st Prelude.+ 1) Prelude.* 87
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..86]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (27) = happyShift action_3
action_0 (28) = happyShift action_24
action_0 (29) = happyShift action_25
action_0 (30) = happyShift action_26
action_0 (49) = happyShift action_27
action_0 (68) = happyShift action_28
action_0 (72) = happyShift action_29
action_0 (84) = happyShift action_30
action_0 (5) = happyGoto action_16
action_0 (7) = happyGoto action_17
action_0 (9) = happyGoto action_18
action_0 (12) = happyGoto action_19
action_0 (13) = happyGoto action_20
action_0 (14) = happyGoto action_21
action_0 (15) = happyGoto action_22
action_0 (17) = happyGoto action_23
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (27) = happyShift action_3
action_1 (28) = happyShift action_10
action_1 (29) = happyShift action_11
action_1 (30) = happyShift action_12
action_1 (68) = happyShift action_13
action_1 (72) = happyShift action_14
action_1 (84) = happyShift action_15
action_1 (5) = happyGoto action_4
action_1 (19) = happyGoto action_5
action_1 (21) = happyGoto action_6
action_1 (22) = happyGoto action_7
action_1 (23) = happyGoto action_8
action_1 (25) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (27) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (27) = happyReduce_46
action_4 (28) = happyReduce_46
action_4 (29) = happyReduce_46
action_4 (30) = happyReduce_46
action_4 (31) = happyReduce_46
action_4 (68) = happyReduce_46
action_4 (69) = happyReduce_46
action_4 (72) = happyReduce_46
action_4 (73) = happyReduce_46
action_4 (75) = happyReduce_46
action_4 (76) = happyReduce_46
action_4 (87) = happyReduce_46
action_4 _ = happyReduce_46

action_5 _ = happyReduce_44

action_6 _ = happyReduce_43

action_7 _ = happyReduce_45

action_8 (69) = happyReduce_53
action_8 (73) = happyReduce_53
action_8 (75) = happyReduce_53
action_8 (76) = happyReduce_53
action_8 (87) = happyReduce_53
action_8 (24) = happyGoto action_46
action_8 _ = happyReduce_48

action_9 (76) = happyShift action_45
action_9 (87) = happyAccept
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_40

action_11 _ = happyReduce_42

action_12 _ = happyReduce_41

action_13 (27) = happyShift action_3
action_13 (28) = happyShift action_10
action_13 (29) = happyShift action_11
action_13 (30) = happyShift action_12
action_13 (68) = happyShift action_13
action_13 (72) = happyShift action_14
action_13 (84) = happyShift action_15
action_13 (5) = happyGoto action_4
action_13 (19) = happyGoto action_5
action_13 (21) = happyGoto action_6
action_13 (22) = happyGoto action_7
action_13 (23) = happyGoto action_8
action_13 (25) = happyGoto action_44
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (27) = happyShift action_3
action_14 (5) = happyGoto action_42
action_14 (18) = happyGoto action_43
action_14 _ = happyReduce_33

action_15 (10) = happyGoto action_41
action_15 _ = happyReduce_10

action_16 _ = happyReduce_19

action_17 _ = happyReduce_22

action_18 _ = happyReduce_21

action_19 _ = happyReduce_26

action_20 _ = happyReduce_25

action_21 _ = happyReduce_20

action_22 (32) = happyReduce_28
action_22 (33) = happyReduce_28
action_22 (34) = happyReduce_28
action_22 (35) = happyReduce_28
action_22 (50) = happyReduce_28
action_22 (51) = happyReduce_28
action_22 (69) = happyReduce_28
action_22 (73) = happyReduce_28
action_22 (75) = happyReduce_28
action_22 (87) = happyReduce_28
action_22 (11) = happyGoto action_40
action_22 _ = happyReduce_12

action_23 (32) = happyShift action_36
action_23 (33) = happyShift action_37
action_23 (34) = happyShift action_38
action_23 (35) = happyShift action_39
action_23 (87) = happyAccept
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_16

action_25 _ = happyReduce_18

action_26 _ = happyReduce_17

action_27 (27) = happyShift action_3
action_27 (28) = happyShift action_24
action_27 (29) = happyShift action_25
action_27 (30) = happyShift action_26
action_27 (49) = happyShift action_27
action_27 (68) = happyShift action_28
action_27 (72) = happyShift action_29
action_27 (84) = happyShift action_30
action_27 (5) = happyGoto action_16
action_27 (7) = happyGoto action_17
action_27 (9) = happyGoto action_18
action_27 (12) = happyGoto action_19
action_27 (13) = happyGoto action_20
action_27 (14) = happyGoto action_21
action_27 (15) = happyGoto action_22
action_27 (17) = happyGoto action_35
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (27) = happyShift action_3
action_28 (28) = happyShift action_24
action_28 (29) = happyShift action_25
action_28 (30) = happyShift action_26
action_28 (49) = happyShift action_27
action_28 (68) = happyShift action_28
action_28 (72) = happyShift action_29
action_28 (84) = happyShift action_30
action_28 (5) = happyGoto action_16
action_28 (7) = happyGoto action_17
action_28 (9) = happyGoto action_18
action_28 (12) = happyGoto action_19
action_28 (13) = happyGoto action_20
action_28 (14) = happyGoto action_21
action_28 (15) = happyGoto action_22
action_28 (17) = happyGoto action_34
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (27) = happyShift action_3
action_29 (5) = happyGoto action_32
action_29 (6) = happyGoto action_33
action_29 _ = happyReduce_3

action_30 (10) = happyGoto action_31
action_30 _ = happyReduce_10

action_31 (27) = happyShift action_3
action_31 (76) = happyShift action_69
action_31 (5) = happyGoto action_55
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (74) = happyShift action_68
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (73) = happyShift action_67
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (32) = happyShift action_36
action_34 (33) = happyShift action_37
action_34 (34) = happyShift action_38
action_34 (35) = happyShift action_39
action_34 (69) = happyShift action_65
action_34 (75) = happyShift action_66
action_34 (8) = happyGoto action_64
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (32) = happyShift action_36
action_35 (33) = happyShift action_37
action_35 (34) = happyShift action_38
action_35 (35) = happyShift action_39
action_35 (50) = happyShift action_63
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (27) = happyShift action_3
action_36 (28) = happyShift action_24
action_36 (29) = happyShift action_25
action_36 (30) = happyShift action_26
action_36 (49) = happyShift action_27
action_36 (68) = happyShift action_28
action_36 (72) = happyShift action_29
action_36 (84) = happyShift action_30
action_36 (5) = happyGoto action_16
action_36 (7) = happyGoto action_17
action_36 (9) = happyGoto action_18
action_36 (12) = happyGoto action_19
action_36 (13) = happyGoto action_20
action_36 (14) = happyGoto action_21
action_36 (15) = happyGoto action_22
action_36 (17) = happyGoto action_62
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (27) = happyShift action_3
action_37 (28) = happyShift action_24
action_37 (29) = happyShift action_25
action_37 (30) = happyShift action_26
action_37 (49) = happyShift action_27
action_37 (68) = happyShift action_28
action_37 (72) = happyShift action_29
action_37 (84) = happyShift action_30
action_37 (5) = happyGoto action_16
action_37 (7) = happyGoto action_17
action_37 (9) = happyGoto action_18
action_37 (12) = happyGoto action_19
action_37 (13) = happyGoto action_20
action_37 (14) = happyGoto action_21
action_37 (15) = happyGoto action_22
action_37 (17) = happyGoto action_61
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (27) = happyShift action_3
action_38 (28) = happyShift action_24
action_38 (29) = happyShift action_25
action_38 (30) = happyShift action_26
action_38 (49) = happyShift action_27
action_38 (68) = happyShift action_28
action_38 (72) = happyShift action_29
action_38 (84) = happyShift action_30
action_38 (5) = happyGoto action_16
action_38 (7) = happyGoto action_17
action_38 (9) = happyGoto action_18
action_38 (12) = happyGoto action_19
action_38 (13) = happyGoto action_20
action_38 (14) = happyGoto action_21
action_38 (15) = happyGoto action_22
action_38 (17) = happyGoto action_60
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (27) = happyShift action_3
action_39 (28) = happyShift action_24
action_39 (29) = happyShift action_25
action_39 (30) = happyShift action_26
action_39 (49) = happyShift action_27
action_39 (68) = happyShift action_28
action_39 (72) = happyShift action_29
action_39 (84) = happyShift action_30
action_39 (5) = happyGoto action_16
action_39 (7) = happyGoto action_17
action_39 (9) = happyGoto action_18
action_39 (12) = happyGoto action_19
action_39 (13) = happyGoto action_20
action_39 (14) = happyGoto action_21
action_39 (15) = happyGoto action_22
action_39 (17) = happyGoto action_59
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (27) = happyShift action_3
action_40 (28) = happyShift action_24
action_40 (29) = happyShift action_25
action_40 (30) = happyShift action_26
action_40 (31) = happyShift action_58
action_40 (68) = happyShift action_28
action_40 (72) = happyShift action_29
action_40 (5) = happyGoto action_16
action_40 (7) = happyGoto action_17
action_40 (9) = happyGoto action_18
action_40 (14) = happyGoto action_21
action_40 (15) = happyGoto action_57
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (27) = happyShift action_3
action_41 (76) = happyShift action_56
action_41 (5) = happyGoto action_55
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (74) = happyShift action_54
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (73) = happyShift action_53
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (69) = happyShift action_51
action_44 (75) = happyShift action_52
action_44 (76) = happyShift action_45
action_44 (20) = happyGoto action_50
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (27) = happyShift action_3
action_45 (28) = happyShift action_10
action_45 (29) = happyShift action_11
action_45 (30) = happyShift action_12
action_45 (68) = happyShift action_13
action_45 (72) = happyShift action_14
action_45 (84) = happyShift action_15
action_45 (5) = happyGoto action_4
action_45 (19) = happyGoto action_5
action_45 (21) = happyGoto action_6
action_45 (22) = happyGoto action_7
action_45 (23) = happyGoto action_8
action_45 (25) = happyGoto action_49
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (27) = happyShift action_3
action_46 (28) = happyShift action_10
action_46 (29) = happyShift action_11
action_46 (30) = happyShift action_12
action_46 (31) = happyShift action_48
action_46 (68) = happyShift action_13
action_46 (72) = happyShift action_14
action_46 (5) = happyGoto action_4
action_46 (19) = happyGoto action_5
action_46 (21) = happyGoto action_6
action_46 (22) = happyGoto action_7
action_46 (23) = happyGoto action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_49

action_48 _ = happyReduce_52

action_49 (76) = happyShift action_45
action_49 _ = happyReduce_50

action_50 (69) = happyShift action_78
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_47

action_52 (27) = happyShift action_3
action_52 (28) = happyShift action_10
action_52 (29) = happyShift action_11
action_52 (30) = happyShift action_12
action_52 (68) = happyShift action_13
action_52 (72) = happyShift action_14
action_52 (84) = happyShift action_15
action_52 (5) = happyGoto action_4
action_52 (19) = happyGoto action_5
action_52 (21) = happyGoto action_6
action_52 (22) = happyGoto action_7
action_52 (23) = happyGoto action_8
action_52 (25) = happyGoto action_77
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_36

action_54 (27) = happyShift action_3
action_54 (28) = happyShift action_10
action_54 (29) = happyShift action_11
action_54 (30) = happyShift action_12
action_54 (68) = happyShift action_13
action_54 (72) = happyShift action_14
action_54 (84) = happyShift action_15
action_54 (5) = happyGoto action_4
action_54 (19) = happyGoto action_5
action_54 (21) = happyGoto action_6
action_54 (22) = happyGoto action_7
action_54 (23) = happyGoto action_8
action_54 (25) = happyGoto action_76
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_11

action_56 (27) = happyShift action_3
action_56 (28) = happyShift action_10
action_56 (29) = happyShift action_11
action_56 (30) = happyShift action_12
action_56 (68) = happyShift action_13
action_56 (72) = happyShift action_14
action_56 (84) = happyShift action_15
action_56 (5) = happyGoto action_4
action_56 (19) = happyGoto action_5
action_56 (21) = happyGoto action_6
action_56 (22) = happyGoto action_7
action_56 (23) = happyGoto action_8
action_56 (25) = happyGoto action_75
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_13

action_58 _ = happyReduce_14

action_59 _ = happyReduce_32

action_60 _ = happyReduce_31

action_61 (34) = happyShift action_38
action_61 (35) = happyShift action_39
action_61 _ = happyReduce_30

action_62 (34) = happyShift action_38
action_62 (35) = happyShift action_39
action_62 _ = happyReduce_29

action_63 (27) = happyShift action_3
action_63 (28) = happyShift action_24
action_63 (29) = happyShift action_25
action_63 (30) = happyShift action_26
action_63 (49) = happyShift action_27
action_63 (68) = happyShift action_28
action_63 (72) = happyShift action_29
action_63 (84) = happyShift action_30
action_63 (5) = happyGoto action_16
action_63 (7) = happyGoto action_17
action_63 (9) = happyGoto action_18
action_63 (12) = happyGoto action_19
action_63 (13) = happyGoto action_20
action_63 (14) = happyGoto action_21
action_63 (15) = happyGoto action_22
action_63 (17) = happyGoto action_74
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (69) = happyShift action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_23

action_66 (27) = happyShift action_3
action_66 (28) = happyShift action_24
action_66 (29) = happyShift action_25
action_66 (30) = happyShift action_26
action_66 (49) = happyShift action_27
action_66 (68) = happyShift action_28
action_66 (72) = happyShift action_29
action_66 (84) = happyShift action_30
action_66 (5) = happyGoto action_16
action_66 (7) = happyGoto action_17
action_66 (9) = happyGoto action_18
action_66 (12) = happyGoto action_19
action_66 (13) = happyGoto action_20
action_66 (14) = happyGoto action_21
action_66 (15) = happyGoto action_22
action_66 (17) = happyGoto action_72
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_6

action_68 (27) = happyShift action_3
action_68 (28) = happyShift action_24
action_68 (29) = happyShift action_25
action_68 (30) = happyShift action_26
action_68 (49) = happyShift action_27
action_68 (68) = happyShift action_28
action_68 (72) = happyShift action_29
action_68 (84) = happyShift action_30
action_68 (5) = happyGoto action_16
action_68 (7) = happyGoto action_17
action_68 (9) = happyGoto action_18
action_68 (12) = happyGoto action_19
action_68 (13) = happyGoto action_20
action_68 (14) = happyGoto action_21
action_68 (15) = happyGoto action_22
action_68 (17) = happyGoto action_71
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (27) = happyShift action_3
action_69 (28) = happyShift action_24
action_69 (29) = happyShift action_25
action_69 (30) = happyShift action_26
action_69 (49) = happyShift action_27
action_69 (68) = happyShift action_28
action_69 (72) = happyShift action_29
action_69 (84) = happyShift action_30
action_69 (5) = happyGoto action_16
action_69 (7) = happyGoto action_17
action_69 (9) = happyGoto action_18
action_69 (12) = happyGoto action_19
action_69 (13) = happyGoto action_20
action_69 (14) = happyGoto action_21
action_69 (15) = happyGoto action_22
action_69 (17) = happyGoto action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (32) = happyShift action_36
action_70 (33) = happyShift action_37
action_70 (34) = happyShift action_38
action_70 (35) = happyShift action_39
action_70 _ = happyReduce_27

action_71 (32) = happyShift action_36
action_71 (33) = happyShift action_37
action_71 (34) = happyShift action_38
action_71 (35) = happyShift action_39
action_71 (75) = happyShift action_83
action_71 _ = happyReduce_5

action_72 (32) = happyShift action_36
action_72 (33) = happyShift action_37
action_72 (34) = happyShift action_38
action_72 (35) = happyShift action_39
action_72 (75) = happyShift action_66
action_72 (8) = happyGoto action_82
action_72 _ = happyReduce_7

action_73 _ = happyReduce_9

action_74 (32) = happyShift action_36
action_74 (33) = happyShift action_37
action_74 (34) = happyShift action_38
action_74 (35) = happyShift action_39
action_74 (51) = happyShift action_81
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (76) = happyShift action_45
action_75 _ = happyReduce_51

action_76 (75) = happyShift action_80
action_76 (76) = happyShift action_45
action_76 _ = happyReduce_35

action_77 (75) = happyShift action_52
action_77 (76) = happyShift action_45
action_77 (20) = happyGoto action_79
action_77 _ = happyReduce_37

action_78 _ = happyReduce_39

action_79 _ = happyReduce_38

action_80 (27) = happyShift action_3
action_80 (5) = happyGoto action_42
action_80 (18) = happyGoto action_86
action_80 _ = happyReduce_33

action_81 (27) = happyShift action_3
action_81 (28) = happyShift action_24
action_81 (29) = happyShift action_25
action_81 (30) = happyShift action_26
action_81 (49) = happyShift action_27
action_81 (68) = happyShift action_28
action_81 (72) = happyShift action_29
action_81 (84) = happyShift action_30
action_81 (5) = happyGoto action_16
action_81 (7) = happyGoto action_17
action_81 (9) = happyGoto action_18
action_81 (12) = happyGoto action_19
action_81 (13) = happyGoto action_20
action_81 (14) = happyGoto action_21
action_81 (15) = happyGoto action_22
action_81 (17) = happyGoto action_85
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_8

action_83 (27) = happyShift action_3
action_83 (5) = happyGoto action_32
action_83 (6) = happyGoto action_84
action_83 _ = happyReduce_3

action_84 _ = happyReduce_4

action_85 (32) = happyShift action_36
action_85 (33) = happyShift action_37
action_85 (34) = happyShift action_38
action_85 (35) = happyShift action_39
action_85 _ = happyReduce_15

action_86 _ = happyReduce_34

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

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (P.record happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ([happy_var_2]
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (P.tuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  11 happyReduction_12
happyReduction_12  =  HappyAbsSyn11
		 ([]
	)

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 13 happyReduction_15
happyReduction_15 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.number HM.LInt happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (P.string HM.LString happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (P.term happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  16 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (P.assignment happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 17 happyReduction_27
happyReduction_27 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 (HappyAbsSyn17  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  18 happyReduction_33
happyReduction_33  =  HappyAbsSyn18
		 ([]
	)

happyReduce_34 = happyReduce 5 18 happyReduction_34
happyReduction_34 ((HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn18
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  20 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn20
		 ([happy_var_2]
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 21 happyReduction_39
happyReduction_39 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  22 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  22 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  22 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  23 happyReduction_45
happyReduction_45 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  23 happyReduction_46
happyReduction_46 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn23
		 (P.tyIdentifier happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  23 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  24 happyReduction_48
happyReduction_48  =  HappyAbsSyn24
		 ([]
	)

happyReduce_49 = happySpecReduce_2  24 happyReduction_49
happyReduction_49 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happyReduce 4 25 happyReduction_51
happyReduction_51 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  25 happyReduction_52
happyReduction_52 (HappyTerminal happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  25 happyReduction_53
happyReduction_53 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  26 happyReduction_54
happyReduction_54  =  HappyAbsSyn26
		 (Nothing
	)

happyReduce_55 = happySpecReduce_2  26 happyReduction_55
happyReduction_55 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Just happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 87 87 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 27;
	L.RangedToken (T.Number _) _ -> cont 28;
	L.RangedToken (T.String _) _ -> cont 29;
	L.RangedToken (T.Boolean _) _ -> cont 30;
	L.RangedToken (T.Operator "!") _ -> cont 31;
	L.RangedToken (T.Operator "+") _ -> cont 32;
	L.RangedToken (T.Operator "-") _ -> cont 33;
	L.RangedToken (T.Operator "*") _ -> cont 34;
	L.RangedToken (T.Operator "/") _ -> cont 35;
	L.RangedToken (T.Operator "==") _ -> cont 36;
	L.RangedToken (T.Operator "!=") _ -> cont 37;
	L.RangedToken (T.Operator "<") _ -> cont 38;
	L.RangedToken (T.Operator "<=") _ -> cont 39;
	L.RangedToken (T.Operator ">") _ -> cont 40;
	L.RangedToken (T.Operator ">=") _ -> cont 41;
	L.RangedToken (T.Operator "||") _ -> cont 42;
	L.RangedToken (T.Operator "&&") _ -> cont 43;
	L.RangedToken (T.Operator _) _ -> cont 44;
	L.RangedToken T.Let _ -> cont 45;
	L.RangedToken T.In _ -> cont 46;
	L.RangedToken T.Where _ -> cont 47;
	L.RangedToken T.With _ -> cont 48;
	L.RangedToken T.If _ -> cont 49;
	L.RangedToken T.Then _ -> cont 50;
	L.RangedToken T.Else _ -> cont 51;
	L.RangedToken T.Match _ -> cont 52;
	L.RangedToken T.Return _ -> cont 53;
	L.RangedToken T.Data _ -> cont 54;
	L.RangedToken T.Type _ -> cont 55;
	L.RangedToken T.Alias _ -> cont 56;
	L.RangedToken T.Kind _ -> cont 57;
	L.RangedToken T.Forall _ -> cont 58;
	L.RangedToken T.Exists _ -> cont 59;
	L.RangedToken T.Proof _ -> cont 60;
	L.RangedToken T.Infer _ -> cont 61;
	L.RangedToken T.Protocol _ -> cont 62;
	L.RangedToken T.Interface _ -> cont 63;
	L.RangedToken T.Instance _ -> cont 64;
	L.RangedToken T.Implements _ -> cont 65;
	L.RangedToken T.Module _ -> cont 66;
	L.RangedToken T.Import _ -> cont 67;
	L.RangedToken T.LParen _ -> cont 68;
	L.RangedToken T.RParen _ -> cont 69;
	L.RangedToken T.LBrack _ -> cont 70;
	L.RangedToken T.RBrack _ -> cont 71;
	L.RangedToken T.LCurly _ -> cont 72;
	L.RangedToken T.RCurly _ -> cont 73;
	L.RangedToken T.Colon _ -> cont 74;
	L.RangedToken T.Comma _ -> cont 75;
	L.RangedToken T.Arrow _ -> cont 76;
	L.RangedToken T.BackArrow _ -> cont 77;
	L.RangedToken T.FatArrow _ -> cont 78;
	L.RangedToken T.PipeArrow _ -> cont 79;
	L.RangedToken T.Equals _ -> cont 80;
	L.RangedToken T.Pipe _ -> cont 81;
	L.RangedToken T.Dot _ -> cont 82;
	L.RangedToken T.Section _ -> cont 83;
	L.RangedToken T.BackSlash _ -> cont 84;
	L.RangedToken T.Newline _ -> cont 85;
	L.RangedToken T.EOF _ -> cont 86;
	_ -> happyError' (tk, [])
	})

happyError_ explist 87 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
