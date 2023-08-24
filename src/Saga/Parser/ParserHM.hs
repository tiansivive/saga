{-# OPTIONS_GHC -w #-}
module Saga.Parser.ParserHM
    ( runSagaExpr
    , runSagaScript
    , runSagaType
    , runSagaKind
    , runSagaDec
    , parseSagaExpr
    , parseSagaType
    , parseSagaKind
    , parseSagaDec
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

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
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
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1381) ([0,0,0,0,49184,0,0,0,0,0,60,9216,40960,20482,0,0,0,120,0,16384,32772,0,0,0,16,0,32768,0,0,0,0,0,512,12,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,1024,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,16384,0,0,512,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,0,0,272,32,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64512,255,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,0,18,336,40,0,0,15360,0,36,672,80,0,0,30720,0,72,1344,160,0,0,61440,0,144,2688,320,0,0,8192,0,0,0,0,0,0,49152,3,0,10752,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49184,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,128,0,0,0,0,0,0,64,0,0,0,49152,4095,0,4096,8,0,0,32768,8191,0,8256,16,0,0,0,16383,0,0,48,0,0,0,32766,32,0,64,0,0,57344,1,288,5376,640,0,0,49152,3,576,10752,1280,0,0,32768,7,1152,21504,2560,0,0,0,15,2304,43008,5120,0,0,0,30,4608,20480,10241,0,0,0,60,9216,40960,20482,0,0,0,120,18432,16384,40965,0,0,0,240,36864,32768,16394,1,0,0,480,8192,1,32789,2,0,0,960,16384,2,42,5,0,0,1920,32768,4,84,10,0,0,3840,0,9,168,20,0,0,7680,0,18,336,40,0,0,15360,0,36,672,80,0,0,2048,0,0,0,0,0,0,61440,1,0,2688,0,0,0,8192,0,0,0,8,0,0,0,0,0,32768,0,0,0,0,0,0,32768,0,0,0,0,0,0,4096,24,0,0,0,30,0,4096,8193,0,0,0,124,0,8192,2,0,0,0,120,0,16384,32772,0,0,0,0,0,0,257,0,0,0,32,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,0,512,0,0,0,0,0,0,0,16,0,0,15360,0,0,546,64,0,0,0,0,0,0,8,0,0,4096,0,0,128,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,1920,0,0,68,8,0,0,0,0,0,0,0,0,0,7680,0,0,272,32,0,0,0,0,0,0,0,0,0,30720,0,0,1088,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,0,0,0,0,0,0,480,0,0,0,0,0,0,960,0,0,0,0,0,0,1920,0,0,0,0,0,0,3840,0,0,0,0,0,0,7680,0,0,0,0,0,0,15360,0,0,0,0,0,0,30720,0,0,0,0,0,0,61440,0,0,0,0,0,0,57344,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,0,0,0,0,24,0,0,0,0,0,57344,1,288,5376,640,0,0,0,0,0,0,128,0,0,32768,7,0,21504,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,60,9216,40960,20482,0,0,0,120,18432,16384,40965,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,960,16384,2,42,5,0,0,1920,32768,4,84,10,0,0,57344,2047,0,0,4,0,0,49152,4095,0,4096,8,0,0,0,0,0,0,0,0,0,0,16383,0,16384,32,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,1,0,0,0,8,0,0,0,0,0,0,240,0,32768,10,0,0,0,64512,33023,0,32768,0,0,0,0,0,0,1024,0,0,0,0,0,0,3072,0,0,0,0,0,0,6144,0,0,0,0,0,0,0,0,0,0,15360,0,0,544,64,0,0,0,0,0,32768,0,0,0,4096,0,0,0,0,0,0,0,0,8,0,2,0,0,16384,0,0,0,0,0,0,0,0,0,0,128,0,0,0,15,2304,43008,5120,0,0,0,0,0,0,4,0,0,0,4,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,2048,0,16384,0,0,0,0,4096,0,1024,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,7680,0,18,336,40,0,0,0,0,0,16384,0,0,0,0,0,0,16384,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,1,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,30,4608,20480,10241,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,1023,0,0,2,0,0,0,0,0,16,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,49152,3,576,10752,1280,0,0,0,65520,3,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,0,0,8,0,0,0,0,0,0,240,0,32768,8,1,0,0,0,0,8192,28672,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,3840,0,0,136,16,0,0,49152,36863,0,0,8,0,0,1024,0,0,0,0,0,0,0,0,0,32768,0,0,0,4096,0,0,0,0,0,0,57344,1,0,4352,512,0,0,49152,3,0,8704,1024,0,0,32768,7,0,17408,2048,0,0,0,15,0,34816,4096,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,64512,255,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,1,0,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,8,0,0,0,0,0,0,240,36864,32768,16394,1,0,0,64512,255,0,32768,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","listElements","list","tupleElems","tuple","params","args","fnApplication","controlFlow","patListElems","patTupleElems","patRecordKeys","patData","pattern","term","atom","cases","matchExpr","binding","bindings","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","tagged","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 113
        bit_end = (st Prelude.+ 1) Prelude.* 113
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..112]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (70) = happyShift action_8
action_0 (79) = happyShift action_9
action_0 (80) = happyShift action_10
action_0 (47) = happyGoto action_47
action_0 (48) = happyGoto action_48
action_0 (49) = happyGoto action_49
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (50) = happyShift action_6
action_1 (51) = happyShift action_37
action_1 (52) = happyShift action_38
action_1 (53) = happyShift action_39
action_1 (74) = happyShift action_40
action_1 (77) = happyShift action_41
action_1 (93) = happyShift action_42
action_1 (95) = happyShift action_43
action_1 (97) = happyShift action_44
action_1 (108) = happyShift action_45
action_1 (110) = happyShift action_46
action_1 (8) = happyGoto action_27
action_1 (10) = happyGoto action_28
action_1 (12) = happyGoto action_29
action_1 (14) = happyGoto action_30
action_1 (17) = happyGoto action_31
action_1 (18) = happyGoto action_32
action_1 (24) = happyGoto action_33
action_1 (25) = happyGoto action_34
action_1 (27) = happyGoto action_35
action_1 (30) = happyGoto action_36
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (50) = happyShift action_6
action_2 (51) = happyShift action_21
action_2 (52) = happyShift action_22
action_2 (53) = happyShift action_23
action_2 (93) = happyShift action_24
action_2 (97) = happyShift action_25
action_2 (110) = happyShift action_26
action_2 (8) = happyGoto action_14
action_2 (32) = happyGoto action_15
action_2 (34) = happyGoto action_16
action_2 (35) = happyGoto action_17
action_2 (36) = happyGoto action_18
action_2 (40) = happyGoto action_19
action_2 (41) = happyGoto action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (50) = happyShift action_6
action_3 (93) = happyShift action_13
action_3 (8) = happyGoto action_11
action_3 (43) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (70) = happyShift action_8
action_4 (79) = happyShift action_9
action_4 (80) = happyShift action_10
action_4 (47) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (50) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (113) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (50) = happyShift action_6
action_8 (8) = happyGoto action_87
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (50) = happyShift action_6
action_9 (8) = happyGoto action_86
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (50) = happyShift action_6
action_10 (8) = happyGoto action_85
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_110

action_12 (102) = happyShift action_84
action_12 (113) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (50) = happyShift action_6
action_13 (93) = happyShift action_13
action_13 (8) = happyGoto action_11
action_13 (43) = happyGoto action_83
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (50) = happyReduce_88
action_14 (51) = happyReduce_88
action_14 (52) = happyReduce_88
action_14 (53) = happyReduce_88
action_14 (54) = happyReduce_88
action_14 (70) = happyReduce_88
action_14 (72) = happyReduce_88
action_14 (79) = happyReduce_88
action_14 (80) = happyReduce_88
action_14 (93) = happyReduce_88
action_14 (94) = happyReduce_88
action_14 (97) = happyReduce_88
action_14 (98) = happyReduce_88
action_14 (99) = happyShift action_82
action_14 (100) = happyReduce_88
action_14 (101) = happyReduce_88
action_14 (102) = happyReduce_88
action_14 (106) = happyReduce_88
action_14 (107) = happyReduce_88
action_14 (109) = happyReduce_88
action_14 (113) = happyReduce_88
action_14 _ = happyReduce_88

action_15 _ = happyReduce_86

action_16 _ = happyReduce_85

action_17 _ = happyReduce_87

action_18 (70) = happyReduce_99
action_18 (72) = happyReduce_99
action_18 (79) = happyReduce_99
action_18 (80) = happyReduce_99
action_18 (94) = happyReduce_99
action_18 (98) = happyReduce_99
action_18 (100) = happyReduce_99
action_18 (101) = happyReduce_99
action_18 (102) = happyReduce_99
action_18 (106) = happyReduce_99
action_18 (107) = happyReduce_99
action_18 (109) = happyReduce_99
action_18 (113) = happyReduce_99
action_18 (37) = happyGoto action_81
action_18 _ = happyReduce_90

action_19 _ = happyReduce_100

action_20 (102) = happyShift action_80
action_20 (113) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_82

action_22 _ = happyReduce_84

action_23 _ = happyReduce_83

action_24 (50) = happyShift action_6
action_24 (51) = happyShift action_21
action_24 (52) = happyShift action_22
action_24 (53) = happyShift action_23
action_24 (93) = happyShift action_24
action_24 (97) = happyShift action_25
action_24 (110) = happyShift action_26
action_24 (8) = happyGoto action_14
action_24 (32) = happyGoto action_15
action_24 (34) = happyGoto action_16
action_24 (35) = happyGoto action_17
action_24 (36) = happyGoto action_18
action_24 (40) = happyGoto action_19
action_24 (41) = happyGoto action_79
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (50) = happyShift action_6
action_25 (8) = happyGoto action_77
action_25 (31) = happyGoto action_78
action_25 _ = happyReduce_75

action_26 (15) = happyGoto action_76
action_26 _ = happyReduce_17

action_27 _ = happyReduce_42

action_28 _ = happyReduce_46

action_29 _ = happyReduce_45

action_30 _ = happyReduce_44

action_31 _ = happyReduce_56

action_32 _ = happyReduce_54

action_33 _ = happyReduce_43

action_34 (55) = happyReduce_58
action_34 (56) = happyReduce_58
action_34 (57) = happyReduce_58
action_34 (58) = happyReduce_58
action_34 (59) = happyReduce_58
action_34 (60) = happyReduce_58
action_34 (61) = happyReduce_58
action_34 (62) = happyReduce_58
action_34 (63) = happyReduce_58
action_34 (64) = happyReduce_58
action_34 (65) = happyReduce_58
action_34 (66) = happyReduce_58
action_34 (67) = happyReduce_58
action_34 (68) = happyReduce_58
action_34 (70) = happyReduce_58
action_34 (72) = happyReduce_58
action_34 (75) = happyReduce_58
action_34 (76) = happyReduce_58
action_34 (79) = happyReduce_58
action_34 (80) = happyReduce_58
action_34 (94) = happyReduce_58
action_34 (96) = happyReduce_58
action_34 (98) = happyReduce_58
action_34 (101) = happyReduce_58
action_34 (107) = happyReduce_58
action_34 (108) = happyReduce_58
action_34 (113) = happyReduce_58
action_34 (16) = happyGoto action_75
action_34 _ = happyReduce_19

action_35 _ = happyReduce_55

action_36 (55) = happyShift action_60
action_36 (56) = happyShift action_61
action_36 (57) = happyShift action_62
action_36 (58) = happyShift action_63
action_36 (59) = happyShift action_64
action_36 (60) = happyShift action_65
action_36 (61) = happyShift action_66
action_36 (62) = happyShift action_67
action_36 (63) = happyShift action_68
action_36 (64) = happyShift action_69
action_36 (65) = happyShift action_70
action_36 (66) = happyShift action_71
action_36 (67) = happyShift action_72
action_36 (68) = happyShift action_73
action_36 (108) = happyShift action_74
action_36 (113) = happyAccept
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_39

action_38 _ = happyReduce_40

action_39 _ = happyReduce_41

action_40 (50) = happyShift action_6
action_40 (51) = happyShift action_37
action_40 (52) = happyShift action_38
action_40 (53) = happyShift action_39
action_40 (74) = happyShift action_40
action_40 (77) = happyShift action_41
action_40 (93) = happyShift action_42
action_40 (95) = happyShift action_43
action_40 (97) = happyShift action_44
action_40 (108) = happyShift action_45
action_40 (110) = happyShift action_46
action_40 (8) = happyGoto action_27
action_40 (10) = happyGoto action_28
action_40 (12) = happyGoto action_29
action_40 (14) = happyGoto action_30
action_40 (17) = happyGoto action_31
action_40 (18) = happyGoto action_32
action_40 (24) = happyGoto action_33
action_40 (25) = happyGoto action_34
action_40 (27) = happyGoto action_35
action_40 (30) = happyGoto action_59
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (50) = happyShift action_6
action_41 (51) = happyShift action_37
action_41 (52) = happyShift action_38
action_41 (53) = happyShift action_39
action_41 (74) = happyShift action_40
action_41 (77) = happyShift action_41
action_41 (93) = happyShift action_42
action_41 (95) = happyShift action_43
action_41 (97) = happyShift action_44
action_41 (108) = happyShift action_45
action_41 (110) = happyShift action_46
action_41 (8) = happyGoto action_27
action_41 (10) = happyGoto action_28
action_41 (12) = happyGoto action_29
action_41 (14) = happyGoto action_30
action_41 (17) = happyGoto action_31
action_41 (18) = happyGoto action_32
action_41 (24) = happyGoto action_33
action_41 (25) = happyGoto action_34
action_41 (27) = happyGoto action_35
action_41 (30) = happyGoto action_58
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (50) = happyShift action_6
action_42 (51) = happyShift action_37
action_42 (52) = happyShift action_38
action_42 (53) = happyShift action_39
action_42 (74) = happyShift action_40
action_42 (77) = happyShift action_41
action_42 (93) = happyShift action_42
action_42 (95) = happyShift action_43
action_42 (97) = happyShift action_44
action_42 (108) = happyShift action_45
action_42 (110) = happyShift action_46
action_42 (8) = happyGoto action_27
action_42 (10) = happyGoto action_28
action_42 (12) = happyGoto action_29
action_42 (14) = happyGoto action_30
action_42 (17) = happyGoto action_31
action_42 (18) = happyGoto action_32
action_42 (24) = happyGoto action_33
action_42 (25) = happyGoto action_34
action_42 (27) = happyGoto action_35
action_42 (30) = happyGoto action_57
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (50) = happyShift action_6
action_43 (51) = happyShift action_37
action_43 (52) = happyShift action_38
action_43 (53) = happyShift action_39
action_43 (74) = happyShift action_40
action_43 (77) = happyShift action_41
action_43 (93) = happyShift action_42
action_43 (95) = happyShift action_43
action_43 (97) = happyShift action_44
action_43 (108) = happyShift action_45
action_43 (110) = happyShift action_46
action_43 (8) = happyGoto action_27
action_43 (10) = happyGoto action_28
action_43 (11) = happyGoto action_55
action_43 (12) = happyGoto action_29
action_43 (14) = happyGoto action_30
action_43 (17) = happyGoto action_31
action_43 (18) = happyGoto action_32
action_43 (24) = happyGoto action_33
action_43 (25) = happyGoto action_34
action_43 (27) = happyGoto action_35
action_43 (30) = happyGoto action_56
action_43 _ = happyReduce_10

action_44 (50) = happyShift action_6
action_44 (8) = happyGoto action_53
action_44 (9) = happyGoto action_54
action_44 _ = happyReduce_6

action_45 (50) = happyShift action_6
action_45 (51) = happyShift action_37
action_45 (52) = happyShift action_38
action_45 (53) = happyShift action_39
action_45 (93) = happyShift action_42
action_45 (95) = happyShift action_43
action_45 (97) = happyShift action_44
action_45 (8) = happyGoto action_27
action_45 (10) = happyGoto action_28
action_45 (12) = happyGoto action_29
action_45 (14) = happyGoto action_30
action_45 (24) = happyGoto action_33
action_45 (25) = happyGoto action_52
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (15) = happyGoto action_51
action_46 _ = happyReduce_17

action_47 _ = happyReduce_122

action_48 (70) = happyShift action_8
action_48 (79) = happyShift action_9
action_48 (80) = happyShift action_10
action_48 (47) = happyGoto action_50
action_48 _ = happyReduce_124

action_49 (113) = happyAccept
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_123

action_51 (50) = happyShift action_6
action_51 (102) = happyShift action_134
action_51 (8) = happyGoto action_105
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_59

action_53 (99) = happyShift action_133
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (98) = happyShift action_132
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (96) = happyShift action_131
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (55) = happyShift action_60
action_56 (56) = happyShift action_61
action_56 (57) = happyShift action_62
action_56 (58) = happyShift action_63
action_56 (59) = happyShift action_64
action_56 (60) = happyShift action_65
action_56 (61) = happyShift action_66
action_56 (62) = happyShift action_67
action_56 (63) = happyShift action_68
action_56 (64) = happyShift action_69
action_56 (65) = happyShift action_70
action_56 (66) = happyShift action_71
action_56 (67) = happyShift action_72
action_56 (68) = happyShift action_73
action_56 (101) = happyShift action_130
action_56 (108) = happyShift action_74
action_56 _ = happyReduce_11

action_57 (55) = happyShift action_60
action_57 (56) = happyShift action_61
action_57 (57) = happyShift action_62
action_57 (58) = happyShift action_63
action_57 (59) = happyShift action_64
action_57 (60) = happyShift action_65
action_57 (61) = happyShift action_66
action_57 (62) = happyShift action_67
action_57 (63) = happyShift action_68
action_57 (64) = happyShift action_69
action_57 (65) = happyShift action_70
action_57 (66) = happyShift action_71
action_57 (67) = happyShift action_72
action_57 (68) = happyShift action_73
action_57 (94) = happyShift action_128
action_57 (101) = happyShift action_129
action_57 (108) = happyShift action_74
action_57 (13) = happyGoto action_127
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (55) = happyShift action_60
action_58 (56) = happyShift action_61
action_58 (57) = happyShift action_62
action_58 (58) = happyShift action_63
action_58 (59) = happyShift action_64
action_58 (60) = happyShift action_65
action_58 (61) = happyShift action_66
action_58 (62) = happyShift action_67
action_58 (63) = happyShift action_68
action_58 (64) = happyShift action_69
action_58 (65) = happyShift action_70
action_58 (66) = happyShift action_71
action_58 (67) = happyShift action_72
action_58 (68) = happyShift action_73
action_58 (107) = happyShift action_126
action_58 (108) = happyShift action_74
action_58 (26) = happyGoto action_125
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (55) = happyShift action_60
action_59 (56) = happyShift action_61
action_59 (57) = happyShift action_62
action_59 (58) = happyShift action_63
action_59 (59) = happyShift action_64
action_59 (60) = happyShift action_65
action_59 (61) = happyShift action_66
action_59 (62) = happyShift action_67
action_59 (63) = happyShift action_68
action_59 (64) = happyShift action_69
action_59 (65) = happyShift action_70
action_59 (66) = happyShift action_71
action_59 (67) = happyShift action_72
action_59 (68) = happyShift action_73
action_59 (75) = happyShift action_124
action_59 (108) = happyShift action_74
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (50) = happyShift action_6
action_60 (51) = happyShift action_37
action_60 (52) = happyShift action_38
action_60 (53) = happyShift action_39
action_60 (74) = happyShift action_40
action_60 (77) = happyShift action_41
action_60 (93) = happyShift action_42
action_60 (95) = happyShift action_43
action_60 (97) = happyShift action_44
action_60 (108) = happyShift action_45
action_60 (110) = happyShift action_46
action_60 (8) = happyGoto action_27
action_60 (10) = happyGoto action_28
action_60 (12) = happyGoto action_29
action_60 (14) = happyGoto action_30
action_60 (17) = happyGoto action_31
action_60 (18) = happyGoto action_32
action_60 (24) = happyGoto action_33
action_60 (25) = happyGoto action_34
action_60 (27) = happyGoto action_35
action_60 (30) = happyGoto action_123
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (50) = happyShift action_6
action_61 (51) = happyShift action_37
action_61 (52) = happyShift action_38
action_61 (53) = happyShift action_39
action_61 (74) = happyShift action_40
action_61 (77) = happyShift action_41
action_61 (93) = happyShift action_42
action_61 (95) = happyShift action_43
action_61 (97) = happyShift action_44
action_61 (108) = happyShift action_45
action_61 (110) = happyShift action_46
action_61 (8) = happyGoto action_27
action_61 (10) = happyGoto action_28
action_61 (12) = happyGoto action_29
action_61 (14) = happyGoto action_30
action_61 (17) = happyGoto action_31
action_61 (18) = happyGoto action_32
action_61 (24) = happyGoto action_33
action_61 (25) = happyGoto action_34
action_61 (27) = happyGoto action_35
action_61 (30) = happyGoto action_122
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (50) = happyShift action_6
action_62 (51) = happyShift action_37
action_62 (52) = happyShift action_38
action_62 (53) = happyShift action_39
action_62 (74) = happyShift action_40
action_62 (77) = happyShift action_41
action_62 (93) = happyShift action_42
action_62 (95) = happyShift action_43
action_62 (97) = happyShift action_44
action_62 (108) = happyShift action_45
action_62 (110) = happyShift action_46
action_62 (8) = happyGoto action_27
action_62 (10) = happyGoto action_28
action_62 (12) = happyGoto action_29
action_62 (14) = happyGoto action_30
action_62 (17) = happyGoto action_31
action_62 (18) = happyGoto action_32
action_62 (24) = happyGoto action_33
action_62 (25) = happyGoto action_34
action_62 (27) = happyGoto action_35
action_62 (30) = happyGoto action_121
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (50) = happyShift action_6
action_63 (51) = happyShift action_37
action_63 (52) = happyShift action_38
action_63 (53) = happyShift action_39
action_63 (74) = happyShift action_40
action_63 (77) = happyShift action_41
action_63 (93) = happyShift action_42
action_63 (95) = happyShift action_43
action_63 (97) = happyShift action_44
action_63 (108) = happyShift action_45
action_63 (110) = happyShift action_46
action_63 (8) = happyGoto action_27
action_63 (10) = happyGoto action_28
action_63 (12) = happyGoto action_29
action_63 (14) = happyGoto action_30
action_63 (17) = happyGoto action_31
action_63 (18) = happyGoto action_32
action_63 (24) = happyGoto action_33
action_63 (25) = happyGoto action_34
action_63 (27) = happyGoto action_35
action_63 (30) = happyGoto action_120
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (50) = happyShift action_6
action_64 (51) = happyShift action_37
action_64 (52) = happyShift action_38
action_64 (53) = happyShift action_39
action_64 (74) = happyShift action_40
action_64 (77) = happyShift action_41
action_64 (93) = happyShift action_42
action_64 (95) = happyShift action_43
action_64 (97) = happyShift action_44
action_64 (108) = happyShift action_45
action_64 (110) = happyShift action_46
action_64 (8) = happyGoto action_27
action_64 (10) = happyGoto action_28
action_64 (12) = happyGoto action_29
action_64 (14) = happyGoto action_30
action_64 (17) = happyGoto action_31
action_64 (18) = happyGoto action_32
action_64 (24) = happyGoto action_33
action_64 (25) = happyGoto action_34
action_64 (27) = happyGoto action_35
action_64 (30) = happyGoto action_119
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (50) = happyShift action_6
action_65 (51) = happyShift action_37
action_65 (52) = happyShift action_38
action_65 (53) = happyShift action_39
action_65 (74) = happyShift action_40
action_65 (77) = happyShift action_41
action_65 (93) = happyShift action_42
action_65 (95) = happyShift action_43
action_65 (97) = happyShift action_44
action_65 (108) = happyShift action_45
action_65 (110) = happyShift action_46
action_65 (8) = happyGoto action_27
action_65 (10) = happyGoto action_28
action_65 (12) = happyGoto action_29
action_65 (14) = happyGoto action_30
action_65 (17) = happyGoto action_31
action_65 (18) = happyGoto action_32
action_65 (24) = happyGoto action_33
action_65 (25) = happyGoto action_34
action_65 (27) = happyGoto action_35
action_65 (30) = happyGoto action_118
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (50) = happyShift action_6
action_66 (51) = happyShift action_37
action_66 (52) = happyShift action_38
action_66 (53) = happyShift action_39
action_66 (74) = happyShift action_40
action_66 (77) = happyShift action_41
action_66 (93) = happyShift action_42
action_66 (95) = happyShift action_43
action_66 (97) = happyShift action_44
action_66 (108) = happyShift action_45
action_66 (110) = happyShift action_46
action_66 (8) = happyGoto action_27
action_66 (10) = happyGoto action_28
action_66 (12) = happyGoto action_29
action_66 (14) = happyGoto action_30
action_66 (17) = happyGoto action_31
action_66 (18) = happyGoto action_32
action_66 (24) = happyGoto action_33
action_66 (25) = happyGoto action_34
action_66 (27) = happyGoto action_35
action_66 (30) = happyGoto action_117
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (50) = happyShift action_6
action_67 (51) = happyShift action_37
action_67 (52) = happyShift action_38
action_67 (53) = happyShift action_39
action_67 (74) = happyShift action_40
action_67 (77) = happyShift action_41
action_67 (93) = happyShift action_42
action_67 (95) = happyShift action_43
action_67 (97) = happyShift action_44
action_67 (108) = happyShift action_45
action_67 (110) = happyShift action_46
action_67 (8) = happyGoto action_27
action_67 (10) = happyGoto action_28
action_67 (12) = happyGoto action_29
action_67 (14) = happyGoto action_30
action_67 (17) = happyGoto action_31
action_67 (18) = happyGoto action_32
action_67 (24) = happyGoto action_33
action_67 (25) = happyGoto action_34
action_67 (27) = happyGoto action_35
action_67 (30) = happyGoto action_116
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (50) = happyShift action_6
action_68 (51) = happyShift action_37
action_68 (52) = happyShift action_38
action_68 (53) = happyShift action_39
action_68 (74) = happyShift action_40
action_68 (77) = happyShift action_41
action_68 (93) = happyShift action_42
action_68 (95) = happyShift action_43
action_68 (97) = happyShift action_44
action_68 (108) = happyShift action_45
action_68 (110) = happyShift action_46
action_68 (8) = happyGoto action_27
action_68 (10) = happyGoto action_28
action_68 (12) = happyGoto action_29
action_68 (14) = happyGoto action_30
action_68 (17) = happyGoto action_31
action_68 (18) = happyGoto action_32
action_68 (24) = happyGoto action_33
action_68 (25) = happyGoto action_34
action_68 (27) = happyGoto action_35
action_68 (30) = happyGoto action_115
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (50) = happyShift action_6
action_69 (51) = happyShift action_37
action_69 (52) = happyShift action_38
action_69 (53) = happyShift action_39
action_69 (74) = happyShift action_40
action_69 (77) = happyShift action_41
action_69 (93) = happyShift action_42
action_69 (95) = happyShift action_43
action_69 (97) = happyShift action_44
action_69 (108) = happyShift action_45
action_69 (110) = happyShift action_46
action_69 (8) = happyGoto action_27
action_69 (10) = happyGoto action_28
action_69 (12) = happyGoto action_29
action_69 (14) = happyGoto action_30
action_69 (17) = happyGoto action_31
action_69 (18) = happyGoto action_32
action_69 (24) = happyGoto action_33
action_69 (25) = happyGoto action_34
action_69 (27) = happyGoto action_35
action_69 (30) = happyGoto action_114
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (50) = happyShift action_6
action_70 (51) = happyShift action_37
action_70 (52) = happyShift action_38
action_70 (53) = happyShift action_39
action_70 (74) = happyShift action_40
action_70 (77) = happyShift action_41
action_70 (93) = happyShift action_42
action_70 (95) = happyShift action_43
action_70 (97) = happyShift action_44
action_70 (108) = happyShift action_45
action_70 (110) = happyShift action_46
action_70 (8) = happyGoto action_27
action_70 (10) = happyGoto action_28
action_70 (12) = happyGoto action_29
action_70 (14) = happyGoto action_30
action_70 (17) = happyGoto action_31
action_70 (18) = happyGoto action_32
action_70 (24) = happyGoto action_33
action_70 (25) = happyGoto action_34
action_70 (27) = happyGoto action_35
action_70 (30) = happyGoto action_113
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (50) = happyShift action_6
action_71 (51) = happyShift action_37
action_71 (52) = happyShift action_38
action_71 (53) = happyShift action_39
action_71 (74) = happyShift action_40
action_71 (77) = happyShift action_41
action_71 (93) = happyShift action_42
action_71 (95) = happyShift action_43
action_71 (97) = happyShift action_44
action_71 (108) = happyShift action_45
action_71 (110) = happyShift action_46
action_71 (8) = happyGoto action_27
action_71 (10) = happyGoto action_28
action_71 (12) = happyGoto action_29
action_71 (14) = happyGoto action_30
action_71 (17) = happyGoto action_31
action_71 (18) = happyGoto action_32
action_71 (24) = happyGoto action_33
action_71 (25) = happyGoto action_34
action_71 (27) = happyGoto action_35
action_71 (30) = happyGoto action_112
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (50) = happyShift action_6
action_72 (51) = happyShift action_37
action_72 (52) = happyShift action_38
action_72 (53) = happyShift action_39
action_72 (74) = happyShift action_40
action_72 (77) = happyShift action_41
action_72 (93) = happyShift action_42
action_72 (95) = happyShift action_43
action_72 (97) = happyShift action_44
action_72 (108) = happyShift action_45
action_72 (110) = happyShift action_46
action_72 (8) = happyGoto action_27
action_72 (10) = happyGoto action_28
action_72 (12) = happyGoto action_29
action_72 (14) = happyGoto action_30
action_72 (17) = happyGoto action_31
action_72 (18) = happyGoto action_32
action_72 (24) = happyGoto action_33
action_72 (25) = happyGoto action_34
action_72 (27) = happyGoto action_35
action_72 (30) = happyGoto action_111
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (50) = happyShift action_6
action_73 (51) = happyShift action_37
action_73 (52) = happyShift action_38
action_73 (53) = happyShift action_39
action_73 (74) = happyShift action_40
action_73 (77) = happyShift action_41
action_73 (93) = happyShift action_42
action_73 (95) = happyShift action_43
action_73 (97) = happyShift action_44
action_73 (108) = happyShift action_45
action_73 (110) = happyShift action_46
action_73 (8) = happyGoto action_27
action_73 (10) = happyGoto action_28
action_73 (12) = happyGoto action_29
action_73 (14) = happyGoto action_30
action_73 (17) = happyGoto action_31
action_73 (18) = happyGoto action_32
action_73 (24) = happyGoto action_33
action_73 (25) = happyGoto action_34
action_73 (27) = happyGoto action_35
action_73 (30) = happyGoto action_110
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (50) = happyShift action_6
action_74 (8) = happyGoto action_109
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (50) = happyShift action_6
action_75 (51) = happyShift action_37
action_75 (52) = happyShift action_38
action_75 (53) = happyShift action_39
action_75 (54) = happyShift action_108
action_75 (93) = happyShift action_42
action_75 (95) = happyShift action_43
action_75 (97) = happyShift action_44
action_75 (8) = happyGoto action_27
action_75 (10) = happyGoto action_28
action_75 (12) = happyGoto action_29
action_75 (14) = happyGoto action_30
action_75 (24) = happyGoto action_33
action_75 (25) = happyGoto action_107
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (50) = happyShift action_6
action_76 (104) = happyShift action_106
action_76 (8) = happyGoto action_105
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (99) = happyShift action_104
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (98) = happyShift action_103
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (94) = happyShift action_101
action_79 (101) = happyShift action_102
action_79 (102) = happyShift action_80
action_79 (33) = happyGoto action_100
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (50) = happyShift action_6
action_80 (51) = happyShift action_21
action_80 (52) = happyShift action_22
action_80 (53) = happyShift action_23
action_80 (93) = happyShift action_24
action_80 (97) = happyShift action_25
action_80 (110) = happyShift action_26
action_80 (8) = happyGoto action_14
action_80 (32) = happyGoto action_15
action_80 (34) = happyGoto action_16
action_80 (35) = happyGoto action_17
action_80 (36) = happyGoto action_18
action_80 (40) = happyGoto action_19
action_80 (41) = happyGoto action_99
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (50) = happyShift action_6
action_81 (51) = happyShift action_21
action_81 (52) = happyShift action_22
action_81 (53) = happyShift action_23
action_81 (54) = happyShift action_98
action_81 (93) = happyShift action_24
action_81 (97) = happyShift action_25
action_81 (8) = happyGoto action_96
action_81 (32) = happyGoto action_15
action_81 (34) = happyGoto action_16
action_81 (35) = happyGoto action_17
action_81 (36) = happyGoto action_97
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (50) = happyShift action_6
action_82 (51) = happyShift action_21
action_82 (52) = happyShift action_22
action_82 (53) = happyShift action_23
action_82 (93) = happyShift action_24
action_82 (97) = happyShift action_25
action_82 (110) = happyShift action_26
action_82 (8) = happyGoto action_14
action_82 (32) = happyGoto action_15
action_82 (34) = happyGoto action_16
action_82 (35) = happyGoto action_17
action_82 (36) = happyGoto action_18
action_82 (40) = happyGoto action_19
action_82 (41) = happyGoto action_95
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (94) = happyShift action_94
action_83 (102) = happyShift action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (50) = happyShift action_6
action_84 (93) = happyShift action_13
action_84 (8) = happyGoto action_11
action_84 (43) = happyGoto action_93
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (109) = happyShift action_91
action_85 (44) = happyGoto action_92
action_85 _ = happyReduce_111

action_86 (109) = happyShift action_91
action_86 (44) = happyGoto action_90
action_86 _ = happyReduce_111

action_87 (99) = happyShift action_89
action_87 (42) = happyGoto action_88
action_87 _ = happyReduce_104

action_88 (109) = happyShift action_91
action_88 (44) = happyGoto action_158
action_88 _ = happyReduce_111

action_89 (50) = happyShift action_6
action_89 (51) = happyShift action_21
action_89 (52) = happyShift action_22
action_89 (53) = happyShift action_23
action_89 (89) = happyShift action_157
action_89 (93) = happyShift action_24
action_89 (97) = happyShift action_25
action_89 (110) = happyShift action_26
action_89 (8) = happyGoto action_14
action_89 (32) = happyGoto action_15
action_89 (34) = happyGoto action_16
action_89 (35) = happyGoto action_17
action_89 (36) = happyGoto action_18
action_89 (40) = happyGoto action_19
action_89 (41) = happyGoto action_156
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (106) = happyShift action_155
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (50) = happyShift action_6
action_91 (93) = happyShift action_13
action_91 (8) = happyGoto action_11
action_91 (43) = happyGoto action_154
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (106) = happyShift action_153
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_108

action_94 _ = happyReduce_109

action_95 (70) = happyReduce_98
action_95 (72) = happyReduce_98
action_95 (79) = happyReduce_98
action_95 (80) = happyReduce_98
action_95 (94) = happyReduce_98
action_95 (98) = happyReduce_98
action_95 (100) = happyReduce_98
action_95 (101) = happyReduce_98
action_95 (102) = happyShift action_80
action_95 (106) = happyReduce_98
action_95 (107) = happyReduce_98
action_95 (109) = happyReduce_98
action_95 (113) = happyReduce_98
action_95 _ = happyReduce_98

action_96 (50) = happyReduce_88
action_96 (51) = happyReduce_88
action_96 (52) = happyReduce_88
action_96 (53) = happyReduce_88
action_96 (54) = happyReduce_88
action_96 (93) = happyReduce_88
action_96 (97) = happyReduce_88
action_96 _ = happyReduce_88

action_97 _ = happyReduce_91

action_98 _ = happyReduce_103

action_99 (70) = happyReduce_101
action_99 (72) = happyReduce_101
action_99 (79) = happyReduce_101
action_99 (80) = happyReduce_101
action_99 (94) = happyReduce_101
action_99 (98) = happyReduce_101
action_99 (100) = happyReduce_101
action_99 (101) = happyReduce_101
action_99 (102) = happyShift action_80
action_99 (106) = happyReduce_101
action_99 (107) = happyReduce_101
action_99 (109) = happyReduce_101
action_99 (113) = happyReduce_101
action_99 _ = happyReduce_101

action_100 (94) = happyShift action_152
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_89

action_102 (50) = happyShift action_6
action_102 (51) = happyShift action_21
action_102 (52) = happyShift action_22
action_102 (53) = happyShift action_23
action_102 (93) = happyShift action_24
action_102 (97) = happyShift action_25
action_102 (110) = happyShift action_26
action_102 (8) = happyGoto action_14
action_102 (32) = happyGoto action_15
action_102 (34) = happyGoto action_16
action_102 (35) = happyGoto action_17
action_102 (36) = happyGoto action_18
action_102 (40) = happyGoto action_19
action_102 (41) = happyGoto action_151
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_78

action_104 (50) = happyShift action_6
action_104 (51) = happyShift action_21
action_104 (52) = happyShift action_22
action_104 (53) = happyShift action_23
action_104 (93) = happyShift action_24
action_104 (97) = happyShift action_25
action_104 (110) = happyShift action_26
action_104 (8) = happyGoto action_14
action_104 (32) = happyGoto action_15
action_104 (34) = happyGoto action_16
action_104 (35) = happyGoto action_17
action_104 (36) = happyGoto action_18
action_104 (40) = happyGoto action_19
action_104 (41) = happyGoto action_150
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_18

action_106 (50) = happyShift action_6
action_106 (51) = happyShift action_21
action_106 (52) = happyShift action_22
action_106 (53) = happyShift action_23
action_106 (93) = happyShift action_24
action_106 (97) = happyShift action_25
action_106 (110) = happyShift action_26
action_106 (8) = happyGoto action_14
action_106 (32) = happyGoto action_15
action_106 (34) = happyGoto action_16
action_106 (35) = happyGoto action_17
action_106 (36) = happyGoto action_18
action_106 (40) = happyGoto action_19
action_106 (41) = happyGoto action_149
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_20

action_108 _ = happyReduce_21

action_109 _ = happyReduce_60

action_110 (55) = happyShift action_60
action_110 (56) = happyShift action_61
action_110 (57) = happyShift action_62
action_110 (58) = happyShift action_63
action_110 (59) = happyFail []
action_110 (60) = happyFail []
action_110 (61) = happyFail []
action_110 (62) = happyFail []
action_110 (63) = happyFail []
action_110 (64) = happyFail []
action_110 (65) = happyFail []
action_110 (66) = happyFail []
action_110 (67) = happyFail []
action_110 (68) = happyFail []
action_110 _ = happyReduce_74

action_111 (55) = happyShift action_60
action_111 (56) = happyShift action_61
action_111 (57) = happyShift action_62
action_111 (58) = happyShift action_63
action_111 (59) = happyFail []
action_111 (60) = happyFail []
action_111 (61) = happyFail []
action_111 (62) = happyFail []
action_111 (63) = happyFail []
action_111 (64) = happyFail []
action_111 (65) = happyFail []
action_111 (66) = happyFail []
action_111 (67) = happyFail []
action_111 (68) = happyFail []
action_111 _ = happyReduce_73

action_112 (55) = happyShift action_60
action_112 (56) = happyShift action_61
action_112 (57) = happyShift action_62
action_112 (58) = happyShift action_63
action_112 (59) = happyFail []
action_112 (60) = happyFail []
action_112 (61) = happyFail []
action_112 (62) = happyFail []
action_112 (63) = happyFail []
action_112 (64) = happyFail []
action_112 (65) = happyFail []
action_112 (66) = happyFail []
action_112 (67) = happyFail []
action_112 (68) = happyFail []
action_112 _ = happyReduce_66

action_113 (55) = happyShift action_60
action_113 (56) = happyShift action_61
action_113 (57) = happyShift action_62
action_113 (58) = happyShift action_63
action_113 (59) = happyFail []
action_113 (60) = happyFail []
action_113 (61) = happyFail []
action_113 (62) = happyFail []
action_113 (63) = happyFail []
action_113 (64) = happyFail []
action_113 (65) = happyFail []
action_113 (66) = happyFail []
action_113 (67) = happyFail []
action_113 (68) = happyFail []
action_113 _ = happyReduce_65

action_114 (55) = happyShift action_60
action_114 (56) = happyShift action_61
action_114 (57) = happyShift action_62
action_114 (58) = happyShift action_63
action_114 (59) = happyFail []
action_114 (60) = happyFail []
action_114 (61) = happyFail []
action_114 (62) = happyFail []
action_114 (63) = happyFail []
action_114 (64) = happyFail []
action_114 (65) = happyFail []
action_114 (66) = happyFail []
action_114 (67) = happyFail []
action_114 (68) = happyFail []
action_114 _ = happyReduce_72

action_115 (55) = happyShift action_60
action_115 (56) = happyShift action_61
action_115 (57) = happyShift action_62
action_115 (58) = happyShift action_63
action_115 (59) = happyFail []
action_115 (60) = happyFail []
action_115 (61) = happyFail []
action_115 (62) = happyFail []
action_115 (63) = happyFail []
action_115 (64) = happyFail []
action_115 (65) = happyFail []
action_115 (66) = happyFail []
action_115 (67) = happyFail []
action_115 (68) = happyFail []
action_115 _ = happyReduce_70

action_116 (55) = happyShift action_60
action_116 (56) = happyShift action_61
action_116 (57) = happyShift action_62
action_116 (58) = happyShift action_63
action_116 (59) = happyFail []
action_116 (60) = happyFail []
action_116 (61) = happyFail []
action_116 (62) = happyFail []
action_116 (63) = happyFail []
action_116 (64) = happyFail []
action_116 (65) = happyFail []
action_116 (66) = happyFail []
action_116 (67) = happyFail []
action_116 (68) = happyFail []
action_116 _ = happyReduce_71

action_117 (55) = happyShift action_60
action_117 (56) = happyShift action_61
action_117 (57) = happyShift action_62
action_117 (58) = happyShift action_63
action_117 (59) = happyFail []
action_117 (60) = happyFail []
action_117 (61) = happyFail []
action_117 (62) = happyFail []
action_117 (63) = happyFail []
action_117 (64) = happyFail []
action_117 (65) = happyFail []
action_117 (66) = happyFail []
action_117 (67) = happyFail []
action_117 (68) = happyFail []
action_117 _ = happyReduce_69

action_118 (55) = happyShift action_60
action_118 (56) = happyShift action_61
action_118 (57) = happyShift action_62
action_118 (58) = happyShift action_63
action_118 (59) = happyFail []
action_118 (60) = happyFail []
action_118 (61) = happyFail []
action_118 (62) = happyFail []
action_118 (63) = happyFail []
action_118 (64) = happyFail []
action_118 (65) = happyFail []
action_118 (66) = happyFail []
action_118 (67) = happyFail []
action_118 (68) = happyFail []
action_118 _ = happyReduce_68

action_119 (55) = happyShift action_60
action_119 (56) = happyShift action_61
action_119 (57) = happyShift action_62
action_119 (58) = happyShift action_63
action_119 (59) = happyFail []
action_119 (60) = happyFail []
action_119 (61) = happyFail []
action_119 (62) = happyFail []
action_119 (63) = happyFail []
action_119 (64) = happyFail []
action_119 (65) = happyFail []
action_119 (66) = happyFail []
action_119 (67) = happyFail []
action_119 (68) = happyFail []
action_119 _ = happyReduce_67

action_120 _ = happyReduce_64

action_121 _ = happyReduce_63

action_122 (57) = happyShift action_62
action_122 (58) = happyShift action_63
action_122 _ = happyReduce_62

action_123 (57) = happyShift action_62
action_123 (58) = happyShift action_63
action_123 _ = happyReduce_61

action_124 (50) = happyShift action_6
action_124 (51) = happyShift action_37
action_124 (52) = happyShift action_38
action_124 (53) = happyShift action_39
action_124 (74) = happyShift action_40
action_124 (77) = happyShift action_41
action_124 (93) = happyShift action_42
action_124 (95) = happyShift action_43
action_124 (97) = happyShift action_44
action_124 (108) = happyShift action_45
action_124 (110) = happyShift action_46
action_124 (8) = happyGoto action_27
action_124 (10) = happyGoto action_28
action_124 (12) = happyGoto action_29
action_124 (14) = happyGoto action_30
action_124 (17) = happyGoto action_31
action_124 (18) = happyGoto action_32
action_124 (24) = happyGoto action_33
action_124 (25) = happyGoto action_34
action_124 (27) = happyGoto action_35
action_124 (30) = happyGoto action_148
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (55) = happyReduce_50
action_125 (56) = happyReduce_50
action_125 (57) = happyReduce_50
action_125 (58) = happyReduce_50
action_125 (59) = happyReduce_50
action_125 (60) = happyReduce_50
action_125 (61) = happyReduce_50
action_125 (62) = happyReduce_50
action_125 (63) = happyReduce_50
action_125 (64) = happyReduce_50
action_125 (65) = happyReduce_50
action_125 (66) = happyReduce_50
action_125 (67) = happyReduce_50
action_125 (68) = happyReduce_50
action_125 (70) = happyReduce_50
action_125 (72) = happyReduce_50
action_125 (75) = happyReduce_50
action_125 (76) = happyReduce_50
action_125 (79) = happyReduce_50
action_125 (80) = happyReduce_50
action_125 (94) = happyReduce_50
action_125 (96) = happyReduce_50
action_125 (98) = happyReduce_50
action_125 (101) = happyReduce_50
action_125 (107) = happyShift action_147
action_125 (108) = happyReduce_50
action_125 (113) = happyReduce_50
action_125 _ = happyReduce_50

action_126 (50) = happyShift action_6
action_126 (51) = happyShift action_37
action_126 (52) = happyShift action_38
action_126 (53) = happyShift action_39
action_126 (93) = happyShift action_144
action_126 (95) = happyShift action_145
action_126 (97) = happyShift action_146
action_126 (8) = happyGoto action_140
action_126 (22) = happyGoto action_141
action_126 (23) = happyGoto action_142
action_126 (24) = happyGoto action_143
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (94) = happyShift action_139
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_47

action_129 (50) = happyShift action_6
action_129 (51) = happyShift action_37
action_129 (52) = happyShift action_38
action_129 (53) = happyShift action_39
action_129 (74) = happyShift action_40
action_129 (77) = happyShift action_41
action_129 (93) = happyShift action_42
action_129 (95) = happyShift action_43
action_129 (97) = happyShift action_44
action_129 (108) = happyShift action_45
action_129 (110) = happyShift action_46
action_129 (8) = happyGoto action_27
action_129 (10) = happyGoto action_28
action_129 (12) = happyGoto action_29
action_129 (14) = happyGoto action_30
action_129 (17) = happyGoto action_31
action_129 (18) = happyGoto action_32
action_129 (24) = happyGoto action_33
action_129 (25) = happyGoto action_34
action_129 (27) = happyGoto action_35
action_129 (30) = happyGoto action_138
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (50) = happyShift action_6
action_130 (51) = happyShift action_37
action_130 (52) = happyShift action_38
action_130 (53) = happyShift action_39
action_130 (74) = happyShift action_40
action_130 (77) = happyShift action_41
action_130 (93) = happyShift action_42
action_130 (95) = happyShift action_43
action_130 (97) = happyShift action_44
action_130 (108) = happyShift action_45
action_130 (110) = happyShift action_46
action_130 (8) = happyGoto action_27
action_130 (10) = happyGoto action_28
action_130 (11) = happyGoto action_137
action_130 (12) = happyGoto action_29
action_130 (14) = happyGoto action_30
action_130 (17) = happyGoto action_31
action_130 (18) = happyGoto action_32
action_130 (24) = happyGoto action_33
action_130 (25) = happyGoto action_34
action_130 (27) = happyGoto action_35
action_130 (30) = happyGoto action_56
action_130 _ = happyReduce_10

action_131 _ = happyReduce_13

action_132 _ = happyReduce_9

action_133 (50) = happyShift action_6
action_133 (51) = happyShift action_37
action_133 (52) = happyShift action_38
action_133 (53) = happyShift action_39
action_133 (74) = happyShift action_40
action_133 (77) = happyShift action_41
action_133 (93) = happyShift action_42
action_133 (95) = happyShift action_43
action_133 (97) = happyShift action_44
action_133 (108) = happyShift action_45
action_133 (110) = happyShift action_46
action_133 (8) = happyGoto action_27
action_133 (10) = happyGoto action_28
action_133 (12) = happyGoto action_29
action_133 (14) = happyGoto action_30
action_133 (17) = happyGoto action_31
action_133 (18) = happyGoto action_32
action_133 (24) = happyGoto action_33
action_133 (25) = happyGoto action_34
action_133 (27) = happyGoto action_35
action_133 (30) = happyGoto action_136
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (50) = happyShift action_6
action_134 (51) = happyShift action_37
action_134 (52) = happyShift action_38
action_134 (53) = happyShift action_39
action_134 (74) = happyShift action_40
action_134 (77) = happyShift action_41
action_134 (93) = happyShift action_42
action_134 (95) = happyShift action_43
action_134 (97) = happyShift action_44
action_134 (108) = happyShift action_45
action_134 (110) = happyShift action_46
action_134 (8) = happyGoto action_27
action_134 (10) = happyGoto action_28
action_134 (12) = happyGoto action_29
action_134 (14) = happyGoto action_30
action_134 (17) = happyGoto action_31
action_134 (18) = happyGoto action_32
action_134 (24) = happyGoto action_33
action_134 (25) = happyGoto action_34
action_134 (27) = happyGoto action_35
action_134 (30) = happyGoto action_135
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (55) = happyShift action_60
action_135 (56) = happyShift action_61
action_135 (57) = happyShift action_62
action_135 (58) = happyShift action_63
action_135 (59) = happyShift action_64
action_135 (60) = happyShift action_65
action_135 (61) = happyShift action_66
action_135 (62) = happyShift action_67
action_135 (63) = happyShift action_68
action_135 (64) = happyShift action_69
action_135 (65) = happyShift action_70
action_135 (66) = happyShift action_71
action_135 (67) = happyShift action_72
action_135 (68) = happyShift action_73
action_135 (108) = happyShift action_74
action_135 _ = happyReduce_57

action_136 (55) = happyShift action_60
action_136 (56) = happyShift action_61
action_136 (57) = happyShift action_62
action_136 (58) = happyShift action_63
action_136 (59) = happyShift action_64
action_136 (60) = happyShift action_65
action_136 (61) = happyShift action_66
action_136 (62) = happyShift action_67
action_136 (63) = happyShift action_68
action_136 (64) = happyShift action_69
action_136 (65) = happyShift action_70
action_136 (66) = happyShift action_71
action_136 (67) = happyShift action_72
action_136 (68) = happyShift action_73
action_136 (101) = happyShift action_180
action_136 (108) = happyShift action_74
action_136 _ = happyReduce_8

action_137 _ = happyReduce_12

action_138 (55) = happyShift action_60
action_138 (56) = happyShift action_61
action_138 (57) = happyShift action_62
action_138 (58) = happyShift action_63
action_138 (59) = happyShift action_64
action_138 (60) = happyShift action_65
action_138 (61) = happyShift action_66
action_138 (62) = happyShift action_67
action_138 (63) = happyShift action_68
action_138 (64) = happyShift action_69
action_138 (65) = happyShift action_70
action_138 (66) = happyShift action_71
action_138 (67) = happyShift action_72
action_138 (68) = happyShift action_73
action_138 (101) = happyShift action_129
action_138 (108) = happyShift action_74
action_138 (13) = happyGoto action_179
action_138 _ = happyReduce_14

action_139 _ = happyReduce_16

action_140 (99) = happyShift action_178
action_140 _ = happyReduce_32

action_141 (50) = happyShift action_6
action_141 (8) = happyGoto action_177
action_141 _ = happyReduce_38

action_142 (102) = happyShift action_176
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_33

action_144 (50) = happyShift action_6
action_144 (8) = happyGoto action_175
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (50) = happyShift action_6
action_145 (96) = happyShift action_174
action_145 (8) = happyGoto action_172
action_145 (19) = happyGoto action_173
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (50) = happyShift action_6
action_146 (8) = happyGoto action_170
action_146 (21) = happyGoto action_171
action_146 _ = happyReduce_27

action_147 (50) = happyShift action_6
action_147 (51) = happyShift action_37
action_147 (52) = happyShift action_38
action_147 (53) = happyShift action_39
action_147 (93) = happyShift action_144
action_147 (95) = happyShift action_145
action_147 (97) = happyShift action_146
action_147 (8) = happyGoto action_140
action_147 (22) = happyGoto action_141
action_147 (23) = happyGoto action_169
action_147 (24) = happyGoto action_143
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (55) = happyShift action_60
action_148 (56) = happyShift action_61
action_148 (57) = happyShift action_62
action_148 (58) = happyShift action_63
action_148 (59) = happyShift action_64
action_148 (60) = happyShift action_65
action_148 (61) = happyShift action_66
action_148 (62) = happyShift action_67
action_148 (63) = happyShift action_68
action_148 (64) = happyShift action_69
action_148 (65) = happyShift action_70
action_148 (66) = happyShift action_71
action_148 (67) = happyShift action_72
action_148 (68) = happyShift action_73
action_148 (76) = happyShift action_168
action_148 (108) = happyShift action_74
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (70) = happyReduce_102
action_149 (72) = happyReduce_102
action_149 (79) = happyReduce_102
action_149 (80) = happyReduce_102
action_149 (94) = happyReduce_102
action_149 (98) = happyReduce_102
action_149 (100) = happyReduce_102
action_149 (101) = happyReduce_102
action_149 (102) = happyShift action_80
action_149 (106) = happyReduce_102
action_149 (107) = happyReduce_102
action_149 (109) = happyReduce_102
action_149 (113) = happyReduce_102
action_149 _ = happyReduce_102

action_150 (101) = happyShift action_167
action_150 (102) = happyShift action_80
action_150 _ = happyReduce_77

action_151 (101) = happyShift action_102
action_151 (102) = happyShift action_80
action_151 (33) = happyGoto action_166
action_151 _ = happyReduce_79

action_152 _ = happyReduce_81

action_153 (50) = happyShift action_6
action_153 (51) = happyShift action_21
action_153 (52) = happyShift action_22
action_153 (53) = happyShift action_23
action_153 (93) = happyShift action_24
action_153 (97) = happyShift action_25
action_153 (110) = happyShift action_26
action_153 (8) = happyGoto action_14
action_153 (32) = happyGoto action_15
action_153 (34) = happyGoto action_16
action_153 (35) = happyGoto action_17
action_153 (36) = happyGoto action_18
action_153 (40) = happyGoto action_19
action_153 (41) = happyGoto action_165
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (102) = happyShift action_84
action_154 _ = happyReduce_112

action_155 (50) = happyShift action_6
action_155 (8) = happyGoto action_162
action_155 (45) = happyGoto action_163
action_155 (46) = happyGoto action_164
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (72) = happyShift action_161
action_156 (102) = happyShift action_80
action_156 _ = happyReduce_105

action_157 (50) = happyShift action_6
action_157 (8) = happyGoto action_160
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (106) = happyShift action_159
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (50) = happyShift action_6
action_159 (51) = happyShift action_37
action_159 (52) = happyShift action_38
action_159 (53) = happyShift action_39
action_159 (74) = happyShift action_40
action_159 (77) = happyShift action_41
action_159 (93) = happyShift action_42
action_159 (95) = happyShift action_43
action_159 (97) = happyShift action_44
action_159 (108) = happyShift action_45
action_159 (110) = happyShift action_46
action_159 (8) = happyGoto action_27
action_159 (10) = happyGoto action_28
action_159 (12) = happyGoto action_29
action_159 (14) = happyGoto action_30
action_159 (17) = happyGoto action_31
action_159 (18) = happyGoto action_32
action_159 (24) = happyGoto action_33
action_159 (25) = happyGoto action_34
action_159 (27) = happyGoto action_35
action_159 (30) = happyGoto action_200
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (99) = happyShift action_199
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (50) = happyShift action_6
action_161 (8) = happyGoto action_196
action_161 (38) = happyGoto action_197
action_161 (39) = happyGoto action_198
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (99) = happyShift action_195
action_162 _ = happyFail (happyExpListPerState 162)

action_163 _ = happyReduce_114

action_164 (72) = happyShift action_193
action_164 (107) = happyShift action_194
action_164 _ = happyReduce_118

action_165 (72) = happyShift action_192
action_165 (102) = happyShift action_80
action_165 _ = happyReduce_120

action_166 _ = happyReduce_80

action_167 (50) = happyShift action_6
action_167 (8) = happyGoto action_77
action_167 (31) = happyGoto action_191
action_167 _ = happyReduce_75

action_168 (50) = happyShift action_6
action_168 (51) = happyShift action_37
action_168 (52) = happyShift action_38
action_168 (53) = happyShift action_39
action_168 (74) = happyShift action_40
action_168 (77) = happyShift action_41
action_168 (93) = happyShift action_42
action_168 (95) = happyShift action_43
action_168 (97) = happyShift action_44
action_168 (108) = happyShift action_45
action_168 (110) = happyShift action_46
action_168 (8) = happyGoto action_27
action_168 (10) = happyGoto action_28
action_168 (12) = happyGoto action_29
action_168 (14) = happyGoto action_30
action_168 (17) = happyGoto action_31
action_168 (18) = happyGoto action_32
action_168 (24) = happyGoto action_33
action_168 (25) = happyGoto action_34
action_168 (27) = happyGoto action_35
action_168 (30) = happyGoto action_190
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (102) = happyShift action_189
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (101) = happyShift action_188
action_170 _ = happyReduce_28

action_171 (98) = happyShift action_187
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (101) = happyShift action_186
action_172 _ = happyReduce_23

action_173 (96) = happyShift action_185
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_34

action_175 (101) = happyShift action_184
action_175 (20) = happyGoto action_183
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (50) = happyShift action_6
action_176 (51) = happyShift action_37
action_176 (52) = happyShift action_38
action_176 (53) = happyShift action_39
action_176 (74) = happyShift action_40
action_176 (77) = happyShift action_41
action_176 (93) = happyShift action_42
action_176 (95) = happyShift action_43
action_176 (97) = happyShift action_44
action_176 (108) = happyShift action_45
action_176 (110) = happyShift action_46
action_176 (8) = happyGoto action_27
action_176 (10) = happyGoto action_28
action_176 (12) = happyGoto action_29
action_176 (14) = happyGoto action_30
action_176 (17) = happyGoto action_31
action_176 (18) = happyGoto action_32
action_176 (24) = happyGoto action_33
action_176 (25) = happyGoto action_34
action_176 (27) = happyGoto action_35
action_176 (30) = happyGoto action_182
action_176 _ = happyFail (happyExpListPerState 176)

action_177 _ = happyReduce_31

action_178 _ = happyReduce_30

action_179 _ = happyReduce_15

action_180 (50) = happyShift action_6
action_180 (8) = happyGoto action_53
action_180 (9) = happyGoto action_181
action_180 _ = happyReduce_6

action_181 _ = happyReduce_7

action_182 (55) = happyShift action_60
action_182 (56) = happyShift action_61
action_182 (57) = happyShift action_62
action_182 (58) = happyShift action_63
action_182 (59) = happyShift action_64
action_182 (60) = happyShift action_65
action_182 (61) = happyShift action_66
action_182 (62) = happyShift action_67
action_182 (63) = happyShift action_68
action_182 (64) = happyShift action_69
action_182 (65) = happyShift action_70
action_182 (66) = happyShift action_71
action_182 (67) = happyShift action_72
action_182 (68) = happyShift action_73
action_182 (108) = happyShift action_74
action_182 _ = happyReduce_48

action_183 (94) = happyShift action_216
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (50) = happyShift action_6
action_184 (8) = happyGoto action_215
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_35

action_186 (50) = happyShift action_6
action_186 (8) = happyGoto action_172
action_186 (19) = happyGoto action_214
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_37

action_188 (50) = happyShift action_6
action_188 (8) = happyGoto action_170
action_188 (21) = happyGoto action_213
action_188 _ = happyReduce_27

action_189 (50) = happyShift action_6
action_189 (51) = happyShift action_37
action_189 (52) = happyShift action_38
action_189 (53) = happyShift action_39
action_189 (74) = happyShift action_40
action_189 (77) = happyShift action_41
action_189 (93) = happyShift action_42
action_189 (95) = happyShift action_43
action_189 (97) = happyShift action_44
action_189 (108) = happyShift action_45
action_189 (110) = happyShift action_46
action_189 (8) = happyGoto action_27
action_189 (10) = happyGoto action_28
action_189 (12) = happyGoto action_29
action_189 (14) = happyGoto action_30
action_189 (17) = happyGoto action_31
action_189 (18) = happyGoto action_32
action_189 (24) = happyGoto action_33
action_189 (25) = happyGoto action_34
action_189 (27) = happyGoto action_35
action_189 (30) = happyGoto action_212
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (55) = happyShift action_60
action_190 (56) = happyShift action_61
action_190 (57) = happyShift action_62
action_190 (58) = happyShift action_63
action_190 (59) = happyShift action_64
action_190 (60) = happyShift action_65
action_190 (61) = happyShift action_66
action_190 (62) = happyShift action_67
action_190 (63) = happyShift action_68
action_190 (64) = happyShift action_69
action_190 (65) = happyShift action_70
action_190 (66) = happyShift action_71
action_190 (67) = happyShift action_72
action_190 (68) = happyShift action_73
action_190 (108) = happyShift action_74
action_190 _ = happyReduce_22

action_191 _ = happyReduce_76

action_192 (50) = happyShift action_6
action_192 (8) = happyGoto action_196
action_192 (38) = happyGoto action_197
action_192 (39) = happyGoto action_211
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (50) = happyShift action_6
action_193 (8) = happyGoto action_196
action_193 (38) = happyGoto action_197
action_193 (39) = happyGoto action_210
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (50) = happyShift action_6
action_194 (8) = happyGoto action_162
action_194 (45) = happyGoto action_209
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (50) = happyShift action_6
action_195 (51) = happyShift action_21
action_195 (52) = happyShift action_22
action_195 (53) = happyShift action_23
action_195 (93) = happyShift action_24
action_195 (97) = happyShift action_25
action_195 (110) = happyShift action_26
action_195 (8) = happyGoto action_14
action_195 (32) = happyGoto action_15
action_195 (34) = happyGoto action_16
action_195 (35) = happyGoto action_17
action_195 (36) = happyGoto action_18
action_195 (40) = happyGoto action_19
action_195 (41) = happyGoto action_208
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (90) = happyShift action_204
action_196 (105) = happyShift action_205
action_196 (106) = happyShift action_206
action_196 (107) = happyShift action_207
action_196 _ = happyFail (happyExpListPerState 196)

action_197 _ = happyReduce_96

action_198 (100) = happyShift action_203
action_198 _ = happyReduce_106

action_199 (50) = happyShift action_6
action_199 (51) = happyShift action_21
action_199 (52) = happyShift action_22
action_199 (53) = happyShift action_23
action_199 (93) = happyShift action_24
action_199 (97) = happyShift action_25
action_199 (110) = happyShift action_26
action_199 (8) = happyGoto action_14
action_199 (32) = happyGoto action_15
action_199 (34) = happyGoto action_16
action_199 (35) = happyGoto action_17
action_199 (36) = happyGoto action_18
action_199 (40) = happyGoto action_19
action_199 (41) = happyGoto action_202
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (55) = happyShift action_60
action_200 (56) = happyShift action_61
action_200 (57) = happyShift action_62
action_200 (58) = happyShift action_63
action_200 (59) = happyShift action_64
action_200 (60) = happyShift action_65
action_200 (61) = happyShift action_66
action_200 (62) = happyShift action_67
action_200 (63) = happyShift action_68
action_200 (64) = happyShift action_69
action_200 (65) = happyShift action_70
action_200 (66) = happyShift action_71
action_200 (67) = happyShift action_72
action_200 (68) = happyShift action_73
action_200 (72) = happyShift action_201
action_200 (108) = happyShift action_74
action_200 _ = happyReduce_116

action_201 (50) = happyShift action_6
action_201 (8) = happyGoto action_223
action_201 (28) = happyGoto action_224
action_201 (29) = happyGoto action_225
action_201 _ = happyFail (happyExpListPerState 201)

action_202 (102) = happyShift action_80
action_202 _ = happyReduce_107

action_203 (50) = happyShift action_6
action_203 (8) = happyGoto action_196
action_203 (38) = happyGoto action_222
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (50) = happyShift action_6
action_204 (51) = happyShift action_21
action_204 (52) = happyShift action_22
action_204 (53) = happyShift action_23
action_204 (93) = happyShift action_24
action_204 (97) = happyShift action_25
action_204 (110) = happyShift action_26
action_204 (8) = happyGoto action_14
action_204 (32) = happyGoto action_15
action_204 (34) = happyGoto action_16
action_204 (35) = happyGoto action_17
action_204 (36) = happyGoto action_18
action_204 (40) = happyGoto action_19
action_204 (41) = happyGoto action_221
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (50) = happyShift action_6
action_205 (51) = happyShift action_21
action_205 (52) = happyShift action_22
action_205 (53) = happyShift action_23
action_205 (93) = happyShift action_24
action_205 (97) = happyShift action_25
action_205 (110) = happyShift action_26
action_205 (8) = happyGoto action_14
action_205 (32) = happyGoto action_15
action_205 (34) = happyGoto action_16
action_205 (35) = happyGoto action_17
action_205 (36) = happyGoto action_18
action_205 (40) = happyGoto action_19
action_205 (41) = happyGoto action_220
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (50) = happyShift action_6
action_206 (51) = happyShift action_21
action_206 (52) = happyShift action_22
action_206 (53) = happyShift action_23
action_206 (93) = happyShift action_24
action_206 (97) = happyShift action_25
action_206 (110) = happyShift action_26
action_206 (8) = happyGoto action_14
action_206 (32) = happyGoto action_15
action_206 (34) = happyGoto action_16
action_206 (35) = happyGoto action_17
action_206 (36) = happyGoto action_18
action_206 (40) = happyGoto action_19
action_206 (41) = happyGoto action_219
action_206 _ = happyFail (happyExpListPerState 206)

action_207 (50) = happyShift action_6
action_207 (51) = happyShift action_21
action_207 (52) = happyShift action_22
action_207 (53) = happyShift action_23
action_207 (93) = happyShift action_24
action_207 (97) = happyShift action_25
action_207 (110) = happyShift action_26
action_207 (8) = happyGoto action_14
action_207 (32) = happyGoto action_15
action_207 (34) = happyGoto action_16
action_207 (35) = happyGoto action_17
action_207 (36) = happyGoto action_18
action_207 (40) = happyGoto action_19
action_207 (41) = happyGoto action_218
action_207 _ = happyFail (happyExpListPerState 207)

action_208 (102) = happyShift action_80
action_208 _ = happyReduce_113

action_209 _ = happyReduce_115

action_210 (100) = happyShift action_203
action_210 _ = happyReduce_119

action_211 (100) = happyShift action_203
action_211 _ = happyReduce_121

action_212 (55) = happyShift action_60
action_212 (56) = happyShift action_61
action_212 (57) = happyShift action_62
action_212 (58) = happyShift action_63
action_212 (59) = happyShift action_64
action_212 (60) = happyShift action_65
action_212 (61) = happyShift action_66
action_212 (62) = happyShift action_67
action_212 (63) = happyShift action_68
action_212 (64) = happyShift action_69
action_212 (65) = happyShift action_70
action_212 (66) = happyShift action_71
action_212 (67) = happyShift action_72
action_212 (68) = happyShift action_73
action_212 (108) = happyShift action_74
action_212 _ = happyReduce_49

action_213 _ = happyReduce_29

action_214 _ = happyReduce_24

action_215 (101) = happyShift action_184
action_215 (20) = happyGoto action_217
action_215 _ = happyReduce_25

action_216 _ = happyReduce_36

action_217 _ = happyReduce_26

action_218 (102) = happyShift action_80
action_218 _ = happyReduce_95

action_219 (102) = happyShift action_80
action_219 _ = happyReduce_92

action_220 (102) = happyShift action_80
action_220 _ = happyReduce_94

action_221 (102) = happyShift action_80
action_221 _ = happyReduce_93

action_222 _ = happyReduce_97

action_223 (106) = happyShift action_227
action_223 _ = happyFail (happyExpListPerState 223)

action_224 _ = happyReduce_52

action_225 (101) = happyShift action_226
action_225 _ = happyReduce_117

action_226 (50) = happyShift action_6
action_226 (8) = happyGoto action_223
action_226 (28) = happyGoto action_229
action_226 _ = happyFail (happyExpListPerState 226)

action_227 (50) = happyShift action_6
action_227 (51) = happyShift action_37
action_227 (52) = happyShift action_38
action_227 (53) = happyShift action_39
action_227 (74) = happyShift action_40
action_227 (77) = happyShift action_41
action_227 (93) = happyShift action_42
action_227 (95) = happyShift action_43
action_227 (97) = happyShift action_44
action_227 (108) = happyShift action_45
action_227 (110) = happyShift action_46
action_227 (8) = happyGoto action_27
action_227 (10) = happyGoto action_28
action_227 (12) = happyGoto action_29
action_227 (14) = happyGoto action_30
action_227 (17) = happyGoto action_31
action_227 (18) = happyGoto action_32
action_227 (24) = happyGoto action_33
action_227 (25) = happyGoto action_34
action_227 (27) = happyGoto action_35
action_227 (30) = happyGoto action_228
action_227 _ = happyFail (happyExpListPerState 227)

action_228 (55) = happyShift action_60
action_228 (56) = happyShift action_61
action_228 (57) = happyShift action_62
action_228 (58) = happyShift action_63
action_228 (59) = happyShift action_64
action_228 (60) = happyShift action_65
action_228 (61) = happyShift action_66
action_228 (62) = happyShift action_67
action_228 (63) = happyShift action_68
action_228 (64) = happyShift action_69
action_228 (65) = happyShift action_70
action_228 (66) = happyShift action_71
action_228 (67) = happyShift action_72
action_228 (68) = happyShift action_73
action_228 (108) = happyShift action_74
action_228 _ = happyReduce_51

action_229 _ = happyReduce_53

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (P.identifier happy_var_1 HM.Identifier
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  9 happyReduction_6
happyReduction_6  =  HappyAbsSyn9
		 ([]
	)

happyReduce_7 = happyReduce 5 9 happyReduction_7
happyReduction_7 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (P.record happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  11 happyReduction_10
happyReduction_10  =  HappyAbsSyn11
		 ([]
	)

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (P.list happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  13 happyReduction_14
happyReduction_14 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn13
		 ([happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 14 happyReduction_16
happyReduction_16 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (P.tuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_0  15 happyReduction_17
happyReduction_17  =  HappyAbsSyn15
		 ([]
	)

happyReduce_18 = happySpecReduce_2  15 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  16 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 ([]
	)

happyReduce_20 = happySpecReduce_2  16 happyReduction_20
happyReduction_20 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  17 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn17
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 18 happyReduction_22
happyReduction_22 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  19 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  19 happyReduction_24
happyReduction_24 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  20 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn20
		 ([happy_var_2]
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  20 happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  21 happyReduction_27
happyReduction_27  =  HappyAbsSyn21
		 ([]
	)

happyReduce_28 = happySpecReduce_1  21 happyReduction_28
happyReduction_28 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  21 happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  22 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  22 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  23 happyReduction_32
happyReduction_32 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn23
		 (P.pattern $ P.Var happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (P.pattern $ P.Term $ fmap HM.Term happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  23 happyReduction_34
happyReduction_34 _
	_
	 =  HappyAbsSyn23
		 (P.pattern $ P.List []
	)

happyReduce_35 = happySpecReduce_3  23 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (P.pattern $ P.List happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 23 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (P.pattern $ P.Tuple (happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_3  23 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (P.pattern $ P.Record happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 (P.pattern $ P.Tagged happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (P.number HM.LInt happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (P.string HM.LString happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  25 happyReduction_42
happyReduction_42 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 (P.term happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 26 happyReduction_48
happyReduction_48 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 ([P.matchCase happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 5 26 happyReduction_49
happyReduction_49 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (happy_var_1 ++ [P.matchCase happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  27 happyReduction_50
happyReduction_50 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  28 happyReduction_51
happyReduction_51 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  29 happyReduction_52
happyReduction_52 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  29 happyReduction_53
happyReduction_53 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  30 happyReduction_54
happyReduction_54 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  30 happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  30 happyReduction_56
happyReduction_56 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 30 happyReduction_57
happyReduction_57 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_1  30 happyReduction_58
happyReduction_58 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  30 happyReduction_59
happyReduction_59 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (P.dotLambda happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  30 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  30 happyReduction_63
happyReduction_63 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  30 happyReduction_64
happyReduction_64 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  30 happyReduction_65
happyReduction_65 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  30 happyReduction_66
happyReduction_66 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  30 happyReduction_67
happyReduction_67 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  30 happyReduction_68
happyReduction_68 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  30 happyReduction_69
happyReduction_69 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  30 happyReduction_70
happyReduction_70 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  30 happyReduction_71
happyReduction_71 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  30 happyReduction_72
happyReduction_72 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  30 happyReduction_73
happyReduction_73 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  30 happyReduction_74
happyReduction_74 (HappyAbsSyn30  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  31 happyReduction_75
happyReduction_75  =  HappyAbsSyn31
		 ([]
	)

happyReduce_76 = happyReduce 5 31 happyReduction_76
happyReduction_76 ((HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_77 = happySpecReduce_3  31 happyReduction_77
happyReduction_77 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn31
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  32 happyReduction_78
happyReduction_78 (HappyTerminal happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  33 happyReduction_79
happyReduction_79 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn33
		 ([happy_var_2]
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  33 happyReduction_80
happyReduction_80 (HappyAbsSyn33  happy_var_3)
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2 : happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happyReduce 4 34 happyReduction_81
happyReduction_81 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_1  35 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  35 happyReduction_83
happyReduction_83 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  35 happyReduction_84
happyReduction_84 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  35 happyReduction_85
happyReduction_85 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  35 happyReduction_86
happyReduction_86 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  36 happyReduction_87
happyReduction_87 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  36 happyReduction_88
happyReduction_88 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyIdentifier happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  36 happyReduction_89
happyReduction_89 (HappyTerminal happy_var_3)
	(HappyAbsSyn41  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_0  37 happyReduction_90
happyReduction_90  =  HappyAbsSyn37
		 ([]
	)

happyReduce_91 = happySpecReduce_2  37 happyReduction_91
happyReduction_91 (HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  38 happyReduction_92
happyReduction_92 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn38
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  38 happyReduction_93
happyReduction_93 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn38
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  38 happyReduction_94
happyReduction_94 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn38
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  38 happyReduction_95
happyReduction_95 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn38
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  39 happyReduction_96
happyReduction_96 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn39
		 ([happy_var_1]
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  39 happyReduction_97
happyReduction_97 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  40 happyReduction_98
happyReduction_98 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn40
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  41 happyReduction_99
happyReduction_99 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  41 happyReduction_100
happyReduction_100 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  41 happyReduction_101
happyReduction_101 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happyReduce 4 41 happyReduction_102
happyReduction_102 ((HappyAbsSyn41  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_103 = happySpecReduce_3  41 happyReduction_103
happyReduction_103 (HappyTerminal happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn41
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_0  42 happyReduction_104
happyReduction_104  =  HappyAbsSyn42
		 (Nothing
	)

happyReduce_105 = happySpecReduce_2  42 happyReduction_105
happyReduction_105 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (Just happy_var_2
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happyReduce 4 42 happyReduction_106
happyReduction_106 ((HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_107 = happyReduce 5 42 happyReduction_107
happyReduction_107 ((HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_3  43 happyReduction_108
happyReduction_108 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  43 happyReduction_109
happyReduction_109 _
	(HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (happy_var_2
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  43 happyReduction_110
happyReduction_110 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn43
		 (P.kindId happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_0  44 happyReduction_111
happyReduction_111  =  HappyAbsSyn44
		 (Nothing
	)

happyReduce_112 = happySpecReduce_2  44 happyReduction_112
happyReduction_112 (HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (Just happy_var_2
	)
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  45 happyReduction_113
happyReduction_113 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn45
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  46 happyReduction_114
happyReduction_114 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn46
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  46 happyReduction_115
happyReduction_115 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happyReduce 6 47 happyReduction_116
happyReduction_116 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_4) `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_117 = happyReduce 8 47 happyReduction_117
happyReduction_117 ((HappyAbsSyn29  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_4) `HappyStk`
	(HappyAbsSyn42  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_118 = happyReduce 5 47 happyReduction_118
happyReduction_118 ((HappyAbsSyn46  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_119 = happyReduce 7 47 happyReduction_119
happyReduction_119 ((HappyAbsSyn39  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_120 = happyReduce 5 47 happyReduction_120
happyReduction_120 ((HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_121 = happyReduce 7 47 happyReduction_121
happyReduction_121 ((HappyAbsSyn39  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_122 = happySpecReduce_1  48 happyReduction_122
happyReduction_122 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_2  48 happyReduction_123
happyReduction_123 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_123 _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  49 happyReduction_124
happyReduction_124 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn49
		 (P.script happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 113 113 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 50;
	L.RangedToken (T.Number _) _ -> cont 51;
	L.RangedToken (T.String _) _ -> cont 52;
	L.RangedToken (T.Boolean _) _ -> cont 53;
	L.RangedToken (T.Operator "!") _ -> cont 54;
	L.RangedToken (T.Operator "+") _ -> cont 55;
	L.RangedToken (T.Operator "-") _ -> cont 56;
	L.RangedToken (T.Operator "*") _ -> cont 57;
	L.RangedToken (T.Operator "/") _ -> cont 58;
	L.RangedToken (T.Operator "==") _ -> cont 59;
	L.RangedToken (T.Operator "!=") _ -> cont 60;
	L.RangedToken (T.Operator "<") _ -> cont 61;
	L.RangedToken (T.Operator "<=") _ -> cont 62;
	L.RangedToken (T.Operator ">") _ -> cont 63;
	L.RangedToken (T.Operator ">=") _ -> cont 64;
	L.RangedToken (T.Operator "||") _ -> cont 65;
	L.RangedToken (T.Operator "&&") _ -> cont 66;
	L.RangedToken (T.Operator "|>") _ -> cont 67;
	L.RangedToken (T.Operator "<|") _ -> cont 68;
	L.RangedToken (T.Operator _) _ -> cont 69;
	L.RangedToken T.Let _ -> cont 70;
	L.RangedToken T.In _ -> cont 71;
	L.RangedToken T.Where _ -> cont 72;
	L.RangedToken T.With _ -> cont 73;
	L.RangedToken T.If _ -> cont 74;
	L.RangedToken T.Then _ -> cont 75;
	L.RangedToken T.Else _ -> cont 76;
	L.RangedToken T.Match _ -> cont 77;
	L.RangedToken T.Return _ -> cont 78;
	L.RangedToken T.Data _ -> cont 79;
	L.RangedToken T.Type _ -> cont 80;
	L.RangedToken T.Alias _ -> cont 81;
	L.RangedToken T.Kind _ -> cont 82;
	L.RangedToken T.Forall _ -> cont 83;
	L.RangedToken T.Exists _ -> cont 84;
	L.RangedToken T.Proof _ -> cont 85;
	L.RangedToken T.Infer _ -> cont 86;
	L.RangedToken T.Protocol _ -> cont 87;
	L.RangedToken T.Interface _ -> cont 88;
	L.RangedToken T.Instance _ -> cont 89;
	L.RangedToken T.Implements _ -> cont 90;
	L.RangedToken T.Module _ -> cont 91;
	L.RangedToken T.Import _ -> cont 92;
	L.RangedToken T.LParen _ -> cont 93;
	L.RangedToken T.RParen _ -> cont 94;
	L.RangedToken T.LBrack _ -> cont 95;
	L.RangedToken T.RBrack _ -> cont 96;
	L.RangedToken T.LCurly _ -> cont 97;
	L.RangedToken T.RCurly _ -> cont 98;
	L.RangedToken T.Colon _ -> cont 99;
	L.RangedToken T.SemiColon _ -> cont 100;
	L.RangedToken T.Comma _ -> cont 101;
	L.RangedToken T.Arrow _ -> cont 102;
	L.RangedToken T.BackArrow _ -> cont 103;
	L.RangedToken T.FatArrow _ -> cont 104;
	L.RangedToken T.PipeArrow _ -> cont 105;
	L.RangedToken T.Equals _ -> cont 106;
	L.RangedToken T.Pipe _ -> cont 107;
	L.RangedToken T.Dot _ -> cont 108;
	L.RangedToken T.Section _ -> cont 109;
	L.RangedToken T.BackSlash _ -> cont 110;
	L.RangedToken T.Newline _ -> cont 111;
	L.RangedToken T.EOF _ -> cont 112;
	_ -> happyError' (tk, [])
	})

happyError_ explist 113 tk = happyError' (tk, explist)
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
parseSagaScript = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn49 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn47 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


runSagaScript :: String -> Either String (P.ParsedData P.Script)
runSagaScript input = input `P.run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData HM.TypeExpr)
runSagaType input = input `P.run` parseSagaType

runSagaKind :: String -> Either String (P.ParsedData HM.Kind)
runSagaKind input = input `P.run` parseSagaKind

runSagaDec :: String -> Either String (P.ParsedData P.Declaration)
runSagaDec input = input `P.run` parseSagaDec
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
