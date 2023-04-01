{-# OPTIONS_GHC -w #-}
module Saga.Parser.Parser  
    ( runSagaScript
    , runSagaExpr 
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe 
import Data.Monoid (First (..))
import Data.List (last)

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.AST.Syntax as AST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35
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
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,865) ([0,0,0,16384,0,0,0,120,768,4117,0,0,8,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,120,0,21,0,0,52992,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,8960,4117,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,2048,0,0,0,128,0,0,0,0,0,64,0,0,8,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,8,0,0,0,0,0,0,256,0,0,0,0,576,0,0,0,0,32,0,0,0,0,32,0,0,53112,8975,4117,0,0,120,768,4117,0,0,0,0,8,0,0,52992,15,128,0,0,52992,15,130,0,0,52992,1039,0,0,0,0,0,0,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,120,768,4117,0,0,0,0,0,0,0,128,0,0,0,0,120,0,21,0,0,120,768,4117,0,0,120,768,4117,0,0,3840,0,0,0,0,52992,15,0,0,0,0,0,0,0,0,0,0,0,0,0,52992,3,0,0,0,52992,11,0,0,0,3840,0,0,0,0,3840,0,0,0,0,3840,0,0,0,0,3840,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3072,0,0,0,0,3072,0,0,0,0,120,768,4117,0,0,0,0,2,0,0,0,0,0,0,0,120,768,4117,0,0,120,768,4117,0,0,0,0,0,0,0,52992,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,768,4117,0,0,120,768,4117,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,512,0,0,120,512,17,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,512,17,0,0,120,512,17,0,0,8,0,0,0,0,120,768,4117,0,0,0,0,0,0,0,0,0,0,0,0,52992,15,0,0,0,52992,15,128,0,0,0,0,0,0,0,52992,15,128,0,0,0,0,0,0,0,52992,2063,0,0,0,120,768,4117,0,0,0,0,0,0,0,8,0,0,0,0,52992,15,0,0,0,0,0,64,0,0,0,0,32,0,0,0,0,130,0,0,0,1024,0,0,0,120,512,17,0,0,0,0,2,0,0,0,0,0,0,0,120,512,17,0,0,0,0,0,0,0,120,512,17,0,0,0,0,64,0,0,0,0,0,0,0,52992,15,0,0,0,0,0,128,0,0,0,0,128,0,0,0,0,0,0,0,0,2048,0,0,0,120,512,17,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","identifier","path","pairs","record","listElements","list","tupleElems","tuple","args","lambda","params","fnApplication","controlFlow","clause","block","term","atom","expr","tpairs","trecord","ttupleElems","ttuple","type","typeExpr","typeAnnotation","dec","declarations","moduleDef","importMod","imports","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","module","import","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'='","'|'","'.'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 80
        bit_end = (st Prelude.+ 1) Prelude.* 80
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..79]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (63) = happyShift action_26
action_0 (32) = happyGoto action_24
action_0 (35) = happyGoto action_25
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (36) = happyShift action_3
action_1 (37) = happyShift action_15
action_1 (38) = happyShift action_16
action_1 (39) = happyShift action_17
action_1 (57) = happyShift action_18
action_1 (58) = happyShift action_19
action_1 (65) = happyShift action_20
action_1 (67) = happyShift action_21
action_1 (69) = happyShift action_22
action_1 (77) = happyShift action_23
action_1 (5) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (12) = happyGoto action_7
action_1 (14) = happyGoto action_8
action_1 (16) = happyGoto action_9
action_1 (17) = happyGoto action_10
action_1 (18) = happyGoto action_11
action_1 (20) = happyGoto action_12
action_1 (21) = happyGoto action_13
action_1 (22) = happyGoto action_14
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (36) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (74) = happyShift action_59
action_4 _ = happyReduce_32

action_5 _ = happyReduce_31

action_6 _ = happyReduce_30

action_7 _ = happyReduce_29

action_8 _ = happyReduce_38

action_9 _ = happyReduce_37

action_10 _ = happyReduce_36

action_11 (55) = happyShift action_58
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_33

action_13 (36) = happyShift action_3
action_13 (37) = happyShift action_15
action_13 (38) = happyShift action_16
action_13 (39) = happyShift action_17
action_13 (41) = happyReduce_40
action_13 (42) = happyReduce_40
action_13 (43) = happyReduce_40
action_13 (44) = happyReduce_40
action_13 (47) = happyReduce_40
action_13 (48) = happyReduce_40
action_13 (49) = happyReduce_40
action_13 (50) = happyReduce_40
action_13 (51) = happyReduce_40
action_13 (52) = happyReduce_40
action_13 (55) = happyReduce_40
action_13 (57) = happyReduce_40
action_13 (58) = happyReduce_40
action_13 (59) = happyReduce_40
action_13 (60) = happyReduce_40
action_13 (62) = happyReduce_40
action_13 (64) = happyReduce_40
action_13 (65) = happyShift action_20
action_13 (66) = happyReduce_40
action_13 (67) = happyShift action_21
action_13 (68) = happyReduce_40
action_13 (69) = happyShift action_22
action_13 (70) = happyReduce_40
action_13 (72) = happyReduce_40
action_13 (77) = happyReduce_40
action_13 (80) = happyReduce_40
action_13 (5) = happyGoto action_55
action_13 (8) = happyGoto action_5
action_13 (10) = happyGoto action_6
action_13 (12) = happyGoto action_7
action_13 (15) = happyGoto action_56
action_13 (20) = happyGoto action_12
action_13 (21) = happyGoto action_57
action_13 _ = happyReduce_19

action_14 (41) = happyShift action_45
action_14 (42) = happyShift action_46
action_14 (43) = happyShift action_47
action_14 (44) = happyShift action_48
action_14 (47) = happyShift action_49
action_14 (48) = happyShift action_50
action_14 (49) = happyShift action_51
action_14 (50) = happyShift action_52
action_14 (51) = happyShift action_53
action_14 (52) = happyShift action_54
action_14 (80) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_26

action_16 _ = happyReduce_27

action_17 _ = happyReduce_28

action_18 (36) = happyShift action_3
action_18 (5) = happyGoto action_29
action_18 (29) = happyGoto action_30
action_18 (30) = happyGoto action_31
action_18 (31) = happyGoto action_44
action_18 _ = happyReduce_71

action_19 (36) = happyShift action_3
action_19 (37) = happyShift action_15
action_19 (38) = happyShift action_16
action_19 (39) = happyShift action_17
action_19 (57) = happyShift action_18
action_19 (58) = happyShift action_19
action_19 (65) = happyShift action_20
action_19 (67) = happyShift action_21
action_19 (69) = happyShift action_22
action_19 (77) = happyShift action_23
action_19 (5) = happyGoto action_4
action_19 (8) = happyGoto action_5
action_19 (10) = happyGoto action_6
action_19 (12) = happyGoto action_7
action_19 (14) = happyGoto action_8
action_19 (16) = happyGoto action_9
action_19 (17) = happyGoto action_10
action_19 (18) = happyGoto action_11
action_19 (20) = happyGoto action_12
action_19 (21) = happyGoto action_13
action_19 (22) = happyGoto action_43
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (36) = happyShift action_3
action_20 (37) = happyShift action_15
action_20 (38) = happyShift action_16
action_20 (39) = happyShift action_17
action_20 (57) = happyShift action_18
action_20 (58) = happyShift action_19
action_20 (65) = happyShift action_20
action_20 (67) = happyShift action_21
action_20 (69) = happyShift action_22
action_20 (77) = happyShift action_23
action_20 (5) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 (10) = happyGoto action_6
action_20 (12) = happyGoto action_7
action_20 (14) = happyGoto action_8
action_20 (16) = happyGoto action_9
action_20 (17) = happyGoto action_10
action_20 (18) = happyGoto action_11
action_20 (20) = happyGoto action_12
action_20 (21) = happyGoto action_13
action_20 (22) = happyGoto action_42
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (36) = happyShift action_3
action_21 (37) = happyShift action_15
action_21 (38) = happyShift action_16
action_21 (39) = happyShift action_17
action_21 (57) = happyShift action_18
action_21 (58) = happyShift action_19
action_21 (65) = happyShift action_20
action_21 (67) = happyShift action_21
action_21 (69) = happyShift action_22
action_21 (77) = happyShift action_23
action_21 (5) = happyGoto action_4
action_21 (8) = happyGoto action_5
action_21 (9) = happyGoto action_40
action_21 (10) = happyGoto action_6
action_21 (12) = happyGoto action_7
action_21 (14) = happyGoto action_8
action_21 (16) = happyGoto action_9
action_21 (17) = happyGoto action_10
action_21 (18) = happyGoto action_11
action_21 (20) = happyGoto action_12
action_21 (21) = happyGoto action_13
action_21 (22) = happyGoto action_41
action_21 _ = happyReduce_9

action_22 (36) = happyShift action_3
action_22 (37) = happyShift action_15
action_22 (38) = happyShift action_16
action_22 (39) = happyShift action_17
action_22 (57) = happyShift action_18
action_22 (58) = happyShift action_19
action_22 (62) = happyShift action_39
action_22 (65) = happyShift action_20
action_22 (67) = happyShift action_21
action_22 (69) = happyShift action_22
action_22 (77) = happyShift action_23
action_22 (5) = happyGoto action_35
action_22 (7) = happyGoto action_36
action_22 (8) = happyGoto action_5
action_22 (10) = happyGoto action_6
action_22 (12) = happyGoto action_7
action_22 (14) = happyGoto action_8
action_22 (16) = happyGoto action_9
action_22 (17) = happyGoto action_10
action_22 (18) = happyGoto action_11
action_22 (19) = happyGoto action_37
action_22 (20) = happyGoto action_12
action_22 (21) = happyGoto action_13
action_22 (22) = happyGoto action_38
action_22 _ = happyReduce_5

action_23 (36) = happyShift action_3
action_23 (5) = happyGoto action_33
action_23 (13) = happyGoto action_34
action_23 _ = happyReduce_16

action_24 (36) = happyShift action_3
action_24 (5) = happyGoto action_29
action_24 (29) = happyGoto action_30
action_24 (30) = happyGoto action_31
action_24 (31) = happyGoto action_32
action_24 _ = happyReduce_71

action_25 (80) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (36) = happyShift action_3
action_26 (5) = happyGoto action_27
action_26 (6) = happyGoto action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (76) = happyShift action_94
action_27 _ = happyReduce_3

action_28 (56) = happyShift action_93
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (71) = happyShift action_92
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (36) = happyShift action_3
action_30 (5) = happyGoto action_91
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (36) = happyShift action_3
action_31 (5) = happyGoto action_29
action_31 (29) = happyGoto action_30
action_31 (30) = happyGoto action_31
action_31 (31) = happyGoto action_90
action_31 _ = happyReduce_71

action_32 (64) = happyShift action_89
action_32 (33) = happyGoto action_87
action_32 (34) = happyGoto action_88
action_32 _ = happyReduce_75

action_33 (36) = happyShift action_3
action_33 (5) = happyGoto action_33
action_33 (13) = happyGoto action_86
action_33 _ = happyReduce_16

action_34 (73) = happyShift action_85
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (71) = happyShift action_84
action_35 (74) = happyShift action_59
action_35 _ = happyReduce_32

action_36 (70) = happyShift action_83
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (70) = happyShift action_82
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (36) = happyShift action_3
action_38 (37) = happyShift action_15
action_38 (38) = happyShift action_16
action_38 (39) = happyShift action_17
action_38 (41) = happyShift action_45
action_38 (42) = happyShift action_46
action_38 (43) = happyShift action_47
action_38 (44) = happyShift action_48
action_38 (47) = happyShift action_49
action_38 (48) = happyShift action_50
action_38 (49) = happyShift action_51
action_38 (50) = happyShift action_52
action_38 (51) = happyShift action_53
action_38 (52) = happyShift action_54
action_38 (57) = happyShift action_18
action_38 (58) = happyShift action_19
action_38 (62) = happyShift action_39
action_38 (65) = happyShift action_20
action_38 (67) = happyShift action_21
action_38 (69) = happyShift action_22
action_38 (77) = happyShift action_23
action_38 (5) = happyGoto action_4
action_38 (8) = happyGoto action_5
action_38 (10) = happyGoto action_6
action_38 (12) = happyGoto action_7
action_38 (14) = happyGoto action_8
action_38 (16) = happyGoto action_9
action_38 (17) = happyGoto action_10
action_38 (18) = happyGoto action_11
action_38 (19) = happyGoto action_81
action_38 (20) = happyGoto action_12
action_38 (21) = happyGoto action_13
action_38 (22) = happyGoto action_38
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (36) = happyShift action_3
action_39 (37) = happyShift action_15
action_39 (38) = happyShift action_16
action_39 (39) = happyShift action_17
action_39 (57) = happyShift action_18
action_39 (58) = happyShift action_19
action_39 (65) = happyShift action_20
action_39 (67) = happyShift action_21
action_39 (69) = happyShift action_22
action_39 (77) = happyShift action_23
action_39 (5) = happyGoto action_4
action_39 (8) = happyGoto action_5
action_39 (10) = happyGoto action_6
action_39 (12) = happyGoto action_7
action_39 (14) = happyGoto action_8
action_39 (16) = happyGoto action_9
action_39 (17) = happyGoto action_10
action_39 (18) = happyGoto action_11
action_39 (20) = happyGoto action_12
action_39 (21) = happyGoto action_13
action_39 (22) = happyGoto action_80
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (68) = happyShift action_79
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (41) = happyShift action_45
action_41 (42) = happyShift action_46
action_41 (43) = happyShift action_47
action_41 (44) = happyShift action_48
action_41 (47) = happyShift action_49
action_41 (48) = happyShift action_50
action_41 (49) = happyShift action_51
action_41 (50) = happyShift action_52
action_41 (51) = happyShift action_53
action_41 (52) = happyShift action_54
action_41 (72) = happyShift action_78
action_41 _ = happyReduce_10

action_42 (41) = happyShift action_45
action_42 (42) = happyShift action_46
action_42 (43) = happyShift action_47
action_42 (44) = happyShift action_48
action_42 (47) = happyShift action_49
action_42 (48) = happyShift action_50
action_42 (49) = happyShift action_51
action_42 (50) = happyShift action_52
action_42 (51) = happyShift action_53
action_42 (52) = happyShift action_54
action_42 (66) = happyShift action_76
action_42 (72) = happyShift action_77
action_42 (11) = happyGoto action_75
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (41) = happyShift action_45
action_43 (42) = happyShift action_46
action_43 (43) = happyShift action_47
action_43 (44) = happyShift action_48
action_43 (47) = happyShift action_49
action_43 (48) = happyShift action_50
action_43 (49) = happyShift action_51
action_43 (50) = happyShift action_52
action_43 (51) = happyShift action_53
action_43 (52) = happyShift action_54
action_43 (59) = happyShift action_74
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_23

action_45 (36) = happyShift action_3
action_45 (37) = happyShift action_15
action_45 (38) = happyShift action_16
action_45 (39) = happyShift action_17
action_45 (57) = happyShift action_18
action_45 (58) = happyShift action_19
action_45 (65) = happyShift action_20
action_45 (67) = happyShift action_21
action_45 (69) = happyShift action_22
action_45 (77) = happyShift action_23
action_45 (5) = happyGoto action_4
action_45 (8) = happyGoto action_5
action_45 (10) = happyGoto action_6
action_45 (12) = happyGoto action_7
action_45 (14) = happyGoto action_8
action_45 (16) = happyGoto action_9
action_45 (17) = happyGoto action_10
action_45 (18) = happyGoto action_11
action_45 (20) = happyGoto action_12
action_45 (21) = happyGoto action_13
action_45 (22) = happyGoto action_73
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (36) = happyShift action_3
action_46 (37) = happyShift action_15
action_46 (38) = happyShift action_16
action_46 (39) = happyShift action_17
action_46 (57) = happyShift action_18
action_46 (58) = happyShift action_19
action_46 (65) = happyShift action_20
action_46 (67) = happyShift action_21
action_46 (69) = happyShift action_22
action_46 (77) = happyShift action_23
action_46 (5) = happyGoto action_4
action_46 (8) = happyGoto action_5
action_46 (10) = happyGoto action_6
action_46 (12) = happyGoto action_7
action_46 (14) = happyGoto action_8
action_46 (16) = happyGoto action_9
action_46 (17) = happyGoto action_10
action_46 (18) = happyGoto action_11
action_46 (20) = happyGoto action_12
action_46 (21) = happyGoto action_13
action_46 (22) = happyGoto action_72
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (36) = happyShift action_3
action_47 (37) = happyShift action_15
action_47 (38) = happyShift action_16
action_47 (39) = happyShift action_17
action_47 (57) = happyShift action_18
action_47 (58) = happyShift action_19
action_47 (65) = happyShift action_20
action_47 (67) = happyShift action_21
action_47 (69) = happyShift action_22
action_47 (77) = happyShift action_23
action_47 (5) = happyGoto action_4
action_47 (8) = happyGoto action_5
action_47 (10) = happyGoto action_6
action_47 (12) = happyGoto action_7
action_47 (14) = happyGoto action_8
action_47 (16) = happyGoto action_9
action_47 (17) = happyGoto action_10
action_47 (18) = happyGoto action_11
action_47 (20) = happyGoto action_12
action_47 (21) = happyGoto action_13
action_47 (22) = happyGoto action_71
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (36) = happyShift action_3
action_48 (37) = happyShift action_15
action_48 (38) = happyShift action_16
action_48 (39) = happyShift action_17
action_48 (57) = happyShift action_18
action_48 (58) = happyShift action_19
action_48 (65) = happyShift action_20
action_48 (67) = happyShift action_21
action_48 (69) = happyShift action_22
action_48 (77) = happyShift action_23
action_48 (5) = happyGoto action_4
action_48 (8) = happyGoto action_5
action_48 (10) = happyGoto action_6
action_48 (12) = happyGoto action_7
action_48 (14) = happyGoto action_8
action_48 (16) = happyGoto action_9
action_48 (17) = happyGoto action_10
action_48 (18) = happyGoto action_11
action_48 (20) = happyGoto action_12
action_48 (21) = happyGoto action_13
action_48 (22) = happyGoto action_70
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (36) = happyShift action_3
action_49 (37) = happyShift action_15
action_49 (38) = happyShift action_16
action_49 (39) = happyShift action_17
action_49 (57) = happyShift action_18
action_49 (58) = happyShift action_19
action_49 (65) = happyShift action_20
action_49 (67) = happyShift action_21
action_49 (69) = happyShift action_22
action_49 (77) = happyShift action_23
action_49 (5) = happyGoto action_4
action_49 (8) = happyGoto action_5
action_49 (10) = happyGoto action_6
action_49 (12) = happyGoto action_7
action_49 (14) = happyGoto action_8
action_49 (16) = happyGoto action_9
action_49 (17) = happyGoto action_10
action_49 (18) = happyGoto action_11
action_49 (20) = happyGoto action_12
action_49 (21) = happyGoto action_13
action_49 (22) = happyGoto action_69
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (36) = happyShift action_3
action_50 (37) = happyShift action_15
action_50 (38) = happyShift action_16
action_50 (39) = happyShift action_17
action_50 (57) = happyShift action_18
action_50 (58) = happyShift action_19
action_50 (65) = happyShift action_20
action_50 (67) = happyShift action_21
action_50 (69) = happyShift action_22
action_50 (77) = happyShift action_23
action_50 (5) = happyGoto action_4
action_50 (8) = happyGoto action_5
action_50 (10) = happyGoto action_6
action_50 (12) = happyGoto action_7
action_50 (14) = happyGoto action_8
action_50 (16) = happyGoto action_9
action_50 (17) = happyGoto action_10
action_50 (18) = happyGoto action_11
action_50 (20) = happyGoto action_12
action_50 (21) = happyGoto action_13
action_50 (22) = happyGoto action_68
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (36) = happyShift action_3
action_51 (37) = happyShift action_15
action_51 (38) = happyShift action_16
action_51 (39) = happyShift action_17
action_51 (57) = happyShift action_18
action_51 (58) = happyShift action_19
action_51 (65) = happyShift action_20
action_51 (67) = happyShift action_21
action_51 (69) = happyShift action_22
action_51 (77) = happyShift action_23
action_51 (5) = happyGoto action_4
action_51 (8) = happyGoto action_5
action_51 (10) = happyGoto action_6
action_51 (12) = happyGoto action_7
action_51 (14) = happyGoto action_8
action_51 (16) = happyGoto action_9
action_51 (17) = happyGoto action_10
action_51 (18) = happyGoto action_11
action_51 (20) = happyGoto action_12
action_51 (21) = happyGoto action_13
action_51 (22) = happyGoto action_67
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (36) = happyShift action_3
action_52 (37) = happyShift action_15
action_52 (38) = happyShift action_16
action_52 (39) = happyShift action_17
action_52 (57) = happyShift action_18
action_52 (58) = happyShift action_19
action_52 (65) = happyShift action_20
action_52 (67) = happyShift action_21
action_52 (69) = happyShift action_22
action_52 (77) = happyShift action_23
action_52 (5) = happyGoto action_4
action_52 (8) = happyGoto action_5
action_52 (10) = happyGoto action_6
action_52 (12) = happyGoto action_7
action_52 (14) = happyGoto action_8
action_52 (16) = happyGoto action_9
action_52 (17) = happyGoto action_10
action_52 (18) = happyGoto action_11
action_52 (20) = happyGoto action_12
action_52 (21) = happyGoto action_13
action_52 (22) = happyGoto action_66
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (36) = happyShift action_3
action_53 (37) = happyShift action_15
action_53 (38) = happyShift action_16
action_53 (39) = happyShift action_17
action_53 (57) = happyShift action_18
action_53 (58) = happyShift action_19
action_53 (65) = happyShift action_20
action_53 (67) = happyShift action_21
action_53 (69) = happyShift action_22
action_53 (77) = happyShift action_23
action_53 (5) = happyGoto action_4
action_53 (8) = happyGoto action_5
action_53 (10) = happyGoto action_6
action_53 (12) = happyGoto action_7
action_53 (14) = happyGoto action_8
action_53 (16) = happyGoto action_9
action_53 (17) = happyGoto action_10
action_53 (18) = happyGoto action_11
action_53 (20) = happyGoto action_12
action_53 (21) = happyGoto action_13
action_53 (22) = happyGoto action_65
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (36) = happyShift action_3
action_54 (37) = happyShift action_15
action_54 (38) = happyShift action_16
action_54 (39) = happyShift action_17
action_54 (57) = happyShift action_18
action_54 (58) = happyShift action_19
action_54 (65) = happyShift action_20
action_54 (67) = happyShift action_21
action_54 (69) = happyShift action_22
action_54 (77) = happyShift action_23
action_54 (5) = happyGoto action_4
action_54 (8) = happyGoto action_5
action_54 (10) = happyGoto action_6
action_54 (12) = happyGoto action_7
action_54 (14) = happyGoto action_8
action_54 (16) = happyGoto action_9
action_54 (17) = happyGoto action_10
action_54 (18) = happyGoto action_11
action_54 (20) = happyGoto action_12
action_54 (21) = happyGoto action_13
action_54 (22) = happyGoto action_64
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_32

action_56 (40) = happyShift action_63
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (36) = happyShift action_3
action_57 (37) = happyShift action_15
action_57 (38) = happyShift action_16
action_57 (39) = happyShift action_17
action_57 (65) = happyShift action_20
action_57 (67) = happyShift action_21
action_57 (69) = happyShift action_22
action_57 (5) = happyGoto action_55
action_57 (8) = happyGoto action_5
action_57 (10) = happyGoto action_6
action_57 (12) = happyGoto action_7
action_57 (15) = happyGoto action_62
action_57 (20) = happyGoto action_12
action_57 (21) = happyGoto action_57
action_57 _ = happyReduce_19

action_58 (36) = happyShift action_3
action_58 (37) = happyShift action_15
action_58 (38) = happyShift action_16
action_58 (39) = happyShift action_17
action_58 (57) = happyShift action_18
action_58 (58) = happyShift action_19
action_58 (65) = happyShift action_20
action_58 (67) = happyShift action_21
action_58 (69) = happyShift action_22
action_58 (77) = happyShift action_23
action_58 (5) = happyGoto action_4
action_58 (8) = happyGoto action_5
action_58 (10) = happyGoto action_6
action_58 (12) = happyGoto action_7
action_58 (14) = happyGoto action_8
action_58 (16) = happyGoto action_9
action_58 (17) = happyGoto action_10
action_58 (18) = happyGoto action_11
action_58 (20) = happyGoto action_12
action_58 (21) = happyGoto action_13
action_58 (22) = happyGoto action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (36) = happyShift action_3
action_59 (37) = happyShift action_15
action_59 (38) = happyShift action_16
action_59 (39) = happyShift action_17
action_59 (57) = happyShift action_18
action_59 (58) = happyShift action_19
action_59 (65) = happyShift action_20
action_59 (67) = happyShift action_21
action_59 (69) = happyShift action_22
action_59 (77) = happyShift action_23
action_59 (5) = happyGoto action_4
action_59 (8) = happyGoto action_5
action_59 (10) = happyGoto action_6
action_59 (12) = happyGoto action_7
action_59 (14) = happyGoto action_8
action_59 (16) = happyGoto action_9
action_59 (17) = happyGoto action_10
action_59 (18) = happyGoto action_11
action_59 (20) = happyGoto action_12
action_59 (21) = happyGoto action_13
action_59 (22) = happyGoto action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (41) = happyShift action_45
action_60 (42) = happyShift action_46
action_60 (43) = happyShift action_47
action_60 (44) = happyShift action_48
action_60 (47) = happyFail []
action_60 (48) = happyFail []
action_60 (49) = happyFail []
action_60 (50) = happyFail []
action_60 _ = happyReduce_41

action_61 (41) = happyShift action_45
action_61 (42) = happyShift action_46
action_61 (43) = happyShift action_47
action_61 (44) = happyShift action_48
action_61 (47) = happyShift action_49
action_61 (48) = happyShift action_50
action_61 (49) = happyShift action_51
action_61 (50) = happyShift action_52
action_61 (51) = happyShift action_53
action_61 (52) = happyShift action_54
action_61 _ = happyReduce_39

action_62 _ = happyReduce_20

action_63 _ = happyReduce_21

action_64 (41) = happyShift action_45
action_64 (42) = happyShift action_46
action_64 (43) = happyShift action_47
action_64 (44) = happyShift action_48
action_64 (47) = happyShift action_49
action_64 (48) = happyShift action_50
action_64 (49) = happyShift action_51
action_64 (50) = happyShift action_52
action_64 _ = happyReduce_51

action_65 (41) = happyShift action_45
action_65 (42) = happyShift action_46
action_65 (43) = happyShift action_47
action_65 (44) = happyShift action_48
action_65 (47) = happyShift action_49
action_65 (48) = happyShift action_50
action_65 (49) = happyShift action_51
action_65 (50) = happyShift action_52
action_65 (52) = happyShift action_54
action_65 _ = happyReduce_50

action_66 (41) = happyShift action_45
action_66 (42) = happyShift action_46
action_66 (43) = happyShift action_47
action_66 (44) = happyShift action_48
action_66 (47) = happyFail []
action_66 (48) = happyFail []
action_66 (49) = happyFail []
action_66 (50) = happyFail []
action_66 _ = happyReduce_49

action_67 (41) = happyShift action_45
action_67 (42) = happyShift action_46
action_67 (43) = happyShift action_47
action_67 (44) = happyShift action_48
action_67 (47) = happyFail []
action_67 (48) = happyFail []
action_67 (49) = happyFail []
action_67 (50) = happyFail []
action_67 _ = happyReduce_48

action_68 (41) = happyShift action_45
action_68 (42) = happyShift action_46
action_68 (43) = happyShift action_47
action_68 (44) = happyShift action_48
action_68 (47) = happyFail []
action_68 (48) = happyFail []
action_68 (49) = happyFail []
action_68 (50) = happyFail []
action_68 _ = happyReduce_47

action_69 (41) = happyShift action_45
action_69 (42) = happyShift action_46
action_69 (43) = happyShift action_47
action_69 (44) = happyShift action_48
action_69 (47) = happyFail []
action_69 (48) = happyFail []
action_69 (49) = happyFail []
action_69 (50) = happyFail []
action_69 _ = happyReduce_46

action_70 _ = happyReduce_45

action_71 _ = happyReduce_44

action_72 (43) = happyShift action_47
action_72 (44) = happyShift action_48
action_72 _ = happyReduce_43

action_73 (43) = happyShift action_47
action_73 (44) = happyShift action_48
action_73 _ = happyReduce_42

action_74 (36) = happyShift action_3
action_74 (37) = happyShift action_15
action_74 (38) = happyShift action_16
action_74 (39) = happyShift action_17
action_74 (57) = happyShift action_18
action_74 (58) = happyShift action_19
action_74 (65) = happyShift action_20
action_74 (67) = happyShift action_21
action_74 (69) = happyShift action_22
action_74 (77) = happyShift action_23
action_74 (5) = happyGoto action_4
action_74 (8) = happyGoto action_5
action_74 (10) = happyGoto action_6
action_74 (12) = happyGoto action_7
action_74 (14) = happyGoto action_8
action_74 (16) = happyGoto action_9
action_74 (17) = happyGoto action_10
action_74 (18) = happyGoto action_11
action_74 (20) = happyGoto action_12
action_74 (21) = happyGoto action_13
action_74 (22) = happyGoto action_115
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (66) = happyShift action_114
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_35

action_77 (36) = happyShift action_3
action_77 (37) = happyShift action_15
action_77 (38) = happyShift action_16
action_77 (39) = happyShift action_17
action_77 (57) = happyShift action_18
action_77 (58) = happyShift action_19
action_77 (65) = happyShift action_20
action_77 (67) = happyShift action_21
action_77 (69) = happyShift action_22
action_77 (77) = happyShift action_23
action_77 (5) = happyGoto action_4
action_77 (8) = happyGoto action_5
action_77 (10) = happyGoto action_6
action_77 (12) = happyGoto action_7
action_77 (14) = happyGoto action_8
action_77 (16) = happyGoto action_9
action_77 (17) = happyGoto action_10
action_77 (18) = happyGoto action_11
action_77 (20) = happyGoto action_12
action_77 (21) = happyGoto action_13
action_77 (22) = happyGoto action_113
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (36) = happyShift action_3
action_78 (37) = happyShift action_15
action_78 (38) = happyShift action_16
action_78 (39) = happyShift action_17
action_78 (57) = happyShift action_18
action_78 (58) = happyShift action_19
action_78 (65) = happyShift action_20
action_78 (67) = happyShift action_21
action_78 (69) = happyShift action_22
action_78 (77) = happyShift action_23
action_78 (5) = happyGoto action_4
action_78 (8) = happyGoto action_5
action_78 (9) = happyGoto action_112
action_78 (10) = happyGoto action_6
action_78 (12) = happyGoto action_7
action_78 (14) = happyGoto action_8
action_78 (16) = happyGoto action_9
action_78 (17) = happyGoto action_10
action_78 (18) = happyGoto action_11
action_78 (20) = happyGoto action_12
action_78 (21) = happyGoto action_13
action_78 (22) = happyGoto action_41
action_78 _ = happyReduce_9

action_79 _ = happyReduce_12

action_80 (41) = happyShift action_45
action_80 (42) = happyShift action_46
action_80 (43) = happyShift action_47
action_80 (44) = happyShift action_48
action_80 (47) = happyShift action_49
action_80 (48) = happyShift action_50
action_80 (49) = happyShift action_51
action_80 (50) = happyShift action_52
action_80 (51) = happyShift action_53
action_80 (52) = happyShift action_54
action_80 _ = happyReduce_24

action_81 _ = happyReduce_25

action_82 _ = happyReduce_34

action_83 _ = happyReduce_8

action_84 (36) = happyShift action_3
action_84 (37) = happyShift action_15
action_84 (38) = happyShift action_16
action_84 (39) = happyShift action_17
action_84 (57) = happyShift action_18
action_84 (58) = happyShift action_19
action_84 (65) = happyShift action_20
action_84 (67) = happyShift action_21
action_84 (69) = happyShift action_22
action_84 (77) = happyShift action_23
action_84 (5) = happyGoto action_4
action_84 (8) = happyGoto action_5
action_84 (10) = happyGoto action_6
action_84 (12) = happyGoto action_7
action_84 (14) = happyGoto action_8
action_84 (16) = happyGoto action_9
action_84 (17) = happyGoto action_10
action_84 (18) = happyGoto action_11
action_84 (20) = happyGoto action_12
action_84 (21) = happyGoto action_13
action_84 (22) = happyGoto action_111
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (36) = happyShift action_3
action_85 (37) = happyShift action_15
action_85 (38) = happyShift action_16
action_85 (39) = happyShift action_17
action_85 (57) = happyShift action_18
action_85 (58) = happyShift action_19
action_85 (65) = happyShift action_20
action_85 (67) = happyShift action_21
action_85 (69) = happyShift action_22
action_85 (77) = happyShift action_23
action_85 (5) = happyGoto action_4
action_85 (8) = happyGoto action_5
action_85 (10) = happyGoto action_6
action_85 (12) = happyGoto action_7
action_85 (14) = happyGoto action_8
action_85 (16) = happyGoto action_9
action_85 (17) = happyGoto action_10
action_85 (18) = happyGoto action_11
action_85 (20) = happyGoto action_12
action_85 (21) = happyGoto action_13
action_85 (22) = happyGoto action_110
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_17

action_87 (64) = happyShift action_89
action_87 (33) = happyGoto action_87
action_87 (34) = happyGoto action_109
action_87 _ = happyReduce_75

action_88 _ = happyReduce_77

action_89 (36) = happyShift action_3
action_89 (5) = happyGoto action_27
action_89 (6) = happyGoto action_108
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_72

action_91 (74) = happyShift action_107
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (36) = happyShift action_3
action_92 (37) = happyShift action_101
action_92 (38) = happyShift action_102
action_92 (39) = happyShift action_103
action_92 (58) = happyShift action_104
action_92 (65) = happyShift action_105
action_92 (69) = happyShift action_106
action_92 (5) = happyGoto action_96
action_92 (24) = happyGoto action_97
action_92 (26) = happyGoto action_98
action_92 (27) = happyGoto action_99
action_92 (28) = happyGoto action_100
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_73

action_94 (36) = happyShift action_3
action_94 (5) = happyGoto action_27
action_94 (6) = happyGoto action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_4

action_96 _ = happyReduce_64

action_97 _ = happyReduce_63

action_98 _ = happyReduce_62

action_99 _ = happyReduce_65

action_100 _ = happyReduce_69

action_101 _ = happyReduce_59

action_102 _ = happyReduce_60

action_103 _ = happyReduce_61

action_104 (36) = happyShift action_3
action_104 (37) = happyShift action_101
action_104 (38) = happyShift action_102
action_104 (39) = happyShift action_103
action_104 (58) = happyShift action_104
action_104 (65) = happyShift action_105
action_104 (69) = happyShift action_106
action_104 (5) = happyGoto action_96
action_104 (24) = happyGoto action_97
action_104 (26) = happyGoto action_98
action_104 (27) = happyGoto action_99
action_104 (28) = happyGoto action_123
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (36) = happyShift action_3
action_105 (37) = happyShift action_101
action_105 (38) = happyShift action_102
action_105 (39) = happyShift action_103
action_105 (58) = happyShift action_104
action_105 (65) = happyShift action_105
action_105 (69) = happyShift action_106
action_105 (5) = happyGoto action_96
action_105 (24) = happyGoto action_97
action_105 (26) = happyGoto action_98
action_105 (27) = happyGoto action_99
action_105 (28) = happyGoto action_122
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (36) = happyShift action_3
action_106 (5) = happyGoto action_120
action_106 (23) = happyGoto action_121
action_106 _ = happyReduce_52

action_107 (36) = happyShift action_3
action_107 (37) = happyShift action_15
action_107 (38) = happyShift action_16
action_107 (39) = happyShift action_17
action_107 (57) = happyShift action_18
action_107 (58) = happyShift action_19
action_107 (65) = happyShift action_20
action_107 (67) = happyShift action_21
action_107 (69) = happyShift action_22
action_107 (77) = happyShift action_23
action_107 (5) = happyGoto action_4
action_107 (8) = happyGoto action_5
action_107 (10) = happyGoto action_6
action_107 (12) = happyGoto action_7
action_107 (14) = happyGoto action_8
action_107 (16) = happyGoto action_9
action_107 (17) = happyGoto action_10
action_107 (18) = happyGoto action_11
action_107 (20) = happyGoto action_12
action_107 (21) = happyGoto action_13
action_107 (22) = happyGoto action_119
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_74

action_109 _ = happyReduce_76

action_110 (41) = happyShift action_45
action_110 (42) = happyShift action_46
action_110 (43) = happyShift action_47
action_110 (44) = happyShift action_48
action_110 (47) = happyShift action_49
action_110 (48) = happyShift action_50
action_110 (49) = happyShift action_51
action_110 (50) = happyShift action_52
action_110 (51) = happyShift action_53
action_110 (52) = happyShift action_54
action_110 _ = happyReduce_18

action_111 (41) = happyShift action_45
action_111 (42) = happyShift action_46
action_111 (43) = happyShift action_47
action_111 (44) = happyShift action_48
action_111 (47) = happyShift action_49
action_111 (48) = happyShift action_50
action_111 (49) = happyShift action_51
action_111 (50) = happyShift action_52
action_111 (51) = happyShift action_53
action_111 (52) = happyShift action_54
action_111 (72) = happyShift action_118
action_111 _ = happyReduce_7

action_112 _ = happyReduce_11

action_113 (41) = happyShift action_45
action_113 (42) = happyShift action_46
action_113 (43) = happyShift action_47
action_113 (44) = happyShift action_48
action_113 (47) = happyShift action_49
action_113 (48) = happyShift action_50
action_113 (49) = happyShift action_51
action_113 (50) = happyShift action_52
action_113 (51) = happyShift action_53
action_113 (52) = happyShift action_54
action_113 (72) = happyShift action_77
action_113 (11) = happyGoto action_117
action_113 _ = happyReduce_13

action_114 _ = happyReduce_15

action_115 (41) = happyShift action_45
action_115 (42) = happyShift action_46
action_115 (43) = happyShift action_47
action_115 (44) = happyShift action_48
action_115 (47) = happyShift action_49
action_115 (48) = happyShift action_50
action_115 (49) = happyShift action_51
action_115 (50) = happyShift action_52
action_115 (51) = happyShift action_53
action_115 (52) = happyShift action_54
action_115 (60) = happyShift action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (36) = happyShift action_3
action_116 (37) = happyShift action_15
action_116 (38) = happyShift action_16
action_116 (39) = happyShift action_17
action_116 (57) = happyShift action_18
action_116 (58) = happyShift action_19
action_116 (65) = happyShift action_20
action_116 (67) = happyShift action_21
action_116 (69) = happyShift action_22
action_116 (77) = happyShift action_23
action_116 (5) = happyGoto action_4
action_116 (8) = happyGoto action_5
action_116 (10) = happyGoto action_6
action_116 (12) = happyGoto action_7
action_116 (14) = happyGoto action_8
action_116 (16) = happyGoto action_9
action_116 (17) = happyGoto action_10
action_116 (18) = happyGoto action_11
action_116 (20) = happyGoto action_12
action_116 (21) = happyGoto action_13
action_116 (22) = happyGoto action_132
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_14

action_118 (36) = happyShift action_3
action_118 (5) = happyGoto action_130
action_118 (7) = happyGoto action_131
action_118 _ = happyReduce_5

action_119 (41) = happyShift action_45
action_119 (42) = happyShift action_46
action_119 (43) = happyShift action_47
action_119 (44) = happyShift action_48
action_119 (47) = happyShift action_49
action_119 (48) = happyShift action_50
action_119 (49) = happyShift action_51
action_119 (50) = happyShift action_52
action_119 (51) = happyShift action_53
action_119 (52) = happyShift action_54
action_119 _ = happyReduce_70

action_120 (71) = happyShift action_129
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (70) = happyShift action_128
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (66) = happyShift action_126
action_122 (72) = happyShift action_127
action_122 (25) = happyGoto action_125
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (59) = happyShift action_124
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (36) = happyShift action_3
action_124 (37) = happyShift action_101
action_124 (38) = happyShift action_102
action_124 (39) = happyShift action_103
action_124 (58) = happyShift action_104
action_124 (65) = happyShift action_105
action_124 (69) = happyShift action_106
action_124 (5) = happyGoto action_96
action_124 (24) = happyGoto action_97
action_124 (26) = happyGoto action_98
action_124 (27) = happyGoto action_99
action_124 (28) = happyGoto action_136
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (66) = happyShift action_135
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_67

action_127 (36) = happyShift action_3
action_127 (37) = happyShift action_101
action_127 (38) = happyShift action_102
action_127 (39) = happyShift action_103
action_127 (58) = happyShift action_104
action_127 (65) = happyShift action_105
action_127 (69) = happyShift action_106
action_127 (5) = happyGoto action_96
action_127 (24) = happyGoto action_97
action_127 (26) = happyGoto action_98
action_127 (27) = happyGoto action_99
action_127 (28) = happyGoto action_134
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_55

action_129 (36) = happyShift action_3
action_129 (37) = happyShift action_101
action_129 (38) = happyShift action_102
action_129 (39) = happyShift action_103
action_129 (58) = happyShift action_104
action_129 (65) = happyShift action_105
action_129 (69) = happyShift action_106
action_129 (5) = happyGoto action_96
action_129 (24) = happyGoto action_97
action_129 (26) = happyGoto action_98
action_129 (27) = happyGoto action_99
action_129 (28) = happyGoto action_133
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (71) = happyShift action_84
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_6

action_132 (41) = happyShift action_45
action_132 (42) = happyShift action_46
action_132 (43) = happyShift action_47
action_132 (44) = happyShift action_48
action_132 (47) = happyShift action_49
action_132 (48) = happyShift action_50
action_132 (49) = happyShift action_51
action_132 (50) = happyShift action_52
action_132 (51) = happyShift action_53
action_132 (52) = happyShift action_54
action_132 _ = happyReduce_22

action_133 (72) = happyShift action_139
action_133 _ = happyReduce_54

action_134 (72) = happyShift action_127
action_134 (25) = happyGoto action_138
action_134 _ = happyReduce_56

action_135 _ = happyReduce_58

action_136 (60) = happyShift action_137
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (36) = happyShift action_3
action_137 (37) = happyShift action_101
action_137 (38) = happyShift action_102
action_137 (39) = happyShift action_103
action_137 (58) = happyShift action_104
action_137 (65) = happyShift action_105
action_137 (69) = happyShift action_106
action_137 (5) = happyGoto action_96
action_137 (24) = happyGoto action_97
action_137 (26) = happyGoto action_98
action_137 (27) = happyGoto action_99
action_137 (28) = happyGoto action_141
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_57

action_139 (36) = happyShift action_3
action_139 (5) = happyGoto action_120
action_139 (23) = happyGoto action_140
action_139 _ = happyReduce_52

action_140 _ = happyReduce_53

action_141 _ = happyReduce_66

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (unTok happy_var_1 (\range (T.Id name) -> AST.Name range (BS.unpack name))
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (AST.LRecord (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (AST.LList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ([happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 12 happyReduction_15
happyReduction_15 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AST.LTuple (L.rtRange happy_var_1 <-> L.rtRange happy_var_4) (happy_var_2:happy_var_3)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  13 happyReduction_16
happyReduction_16  =  HappyAbsSyn13
		 ([]
	)

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 14 happyReduction_18
happyReduction_18 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AST.Lambda (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  15 happyReduction_19
happyReduction_19  =  HappyAbsSyn15
		 ([]
	)

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.FnApp (info happy_var_1 <-> L.rtRange happy_var_3) happy_var_1 happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 17 happyReduction_22
happyReduction_22 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AST.IfElse (L.rtRange happy_var_1 <-> info happy_var_6) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  18 happyReduction_23
happyReduction_23 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  19 happyReduction_24
happyReduction_24 (HappyAbsSyn22  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 ([AST.Return (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2]
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  19 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  20 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (unTok happy_var_1 (\range (T.Number int) -> AST.LInt range int)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  20 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (unTok happy_var_1 (\range (T.String string) -> AST.LString range string)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  20 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (unTok happy_var_1 (\range (T.Boolean boolean) -> AST.LBool range boolean)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  21 happyReduction_32
happyReduction_32 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn21
		 (AST.Identifier happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 (AST.Term happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_3)
	(HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (AST.Block (L.rtRange  happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (AST.Parens (L.rtRange  happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  22 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn22
		 (AST.Clause ((info $ head happy_var_1) <-> (info $ last happy_var_1)) happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  22 happyReduction_41
happyReduction_41 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn22
		 (AST.Assign happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  22 happyReduction_43
happyReduction_43 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  22 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  22 happyReduction_45
happyReduction_45 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22 happyReduction_48
happyReduction_48 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  22 happyReduction_49
happyReduction_49 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  22 happyReduction_50
happyReduction_50 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  22 happyReduction_51
happyReduction_51 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  23 happyReduction_52
happyReduction_52  =  HappyAbsSyn23
		 ([]
	)

happyReduce_53 = happyReduce 5 23 happyReduction_53
happyReduction_53 ((HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  23 happyReduction_54
happyReduction_54 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn23
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  24 happyReduction_55
happyReduction_55 (HappyTerminal happy_var_3)
	(HappyAbsSyn23  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (AST.TRecord (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  25 happyReduction_56
happyReduction_56 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 ([happy_var_2]
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  25 happyReduction_57
happyReduction_57 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2 : happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 4 26 happyReduction_58
happyReduction_58 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (AST.TTuple (L.rtRange happy_var_1 <-> L.rtRange happy_var_4) (happy_var_2:happy_var_3)
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (unTok happy_var_1 (\range (T.Number int) -> AST.TLiteral $ AST.LInt range int)
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (unTok happy_var_1 (\range (T.String string) -> AST.TLiteral $ AST.LString range string)
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (unTok happy_var_1 (\range (T.Boolean boolean) -> AST.TLiteral $ AST.LBool range boolean)
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  27 happyReduction_63
happyReduction_63 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  27 happyReduction_64
happyReduction_64 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn27
		 (AST.TIdentifier happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  28 happyReduction_65
happyReduction_65 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (AST.Type happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happyReduce 6 28 happyReduction_66
happyReduction_66 ((HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (AST.TConditional (L.rtRange happy_var_1 <-> info happy_var_6) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_3  28 happyReduction_67
happyReduction_67 (HappyTerminal happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (AST.TParens (L.rtRange  happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  29 happyReduction_68
happyReduction_68  =  HappyAbsSyn29
		 (None
	)

happyReduce_69 = happySpecReduce_3  29 happyReduction_69
happyReduction_69 (HappyAbsSyn28  happy_var_3)
	_
	_
	 =  HappyAbsSyn29
		 (Just happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happyReduce 4 30 happyReduction_70
happyReduction_70 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (declaration happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_0  31 happyReduction_71
happyReduction_71  =  HappyAbsSyn31
		 ([]
	)

happyReduce_72 = happySpecReduce_2  31 happyReduction_72
happyReduction_72 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  32 happyReduction_73
happyReduction_73 (HappyTerminal happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (AST.Mod (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) ( map (\(AST.Name _ name) -> name) happy_var_2 )
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  33 happyReduction_74
happyReduction_74 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 (AST.Import (L.rtRange happy_var_1 <-> (info $ last happy_var_2)) ( map (\(AST.Name _ name) -> name) happy_var_2 )
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  34 happyReduction_75
happyReduction_75  =  HappyAbsSyn34
		 ([]
	)

happyReduce_76 = happySpecReduce_2  34 happyReduction_76
happyReduction_76 (HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 : happy_var_2
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  35 happyReduction_77
happyReduction_77 (HappyAbsSyn34  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn35
		 (AST.Script (info happy_var_1 <-> (info $ last happy_var_2)) happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 80 80 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 36;
	L.RangedToken (T.Number _) _ -> cont 37;
	L.RangedToken (T.String _) _ -> cont 38;
	L.RangedToken (T.Boolean _) _ -> cont 39;
	L.RangedToken (T.Operator "!") _ -> cont 40;
	L.RangedToken (T.Operator "+") _ -> cont 41;
	L.RangedToken (T.Operator "-") _ -> cont 42;
	L.RangedToken (T.Operator "*") _ -> cont 43;
	L.RangedToken (T.Operator "/") _ -> cont 44;
	L.RangedToken (T.Operator "==") _ -> cont 45;
	L.RangedToken (T.Operator "!=") _ -> cont 46;
	L.RangedToken (T.Operator "<") _ -> cont 47;
	L.RangedToken (T.Operator "<=") _ -> cont 48;
	L.RangedToken (T.Operator ">") _ -> cont 49;
	L.RangedToken (T.Operator ">=") _ -> cont 50;
	L.RangedToken (T.Operator "||") _ -> cont 51;
	L.RangedToken (T.Operator "&&") _ -> cont 52;
	L.RangedToken (T.Operator _) _ -> cont 53;
	L.RangedToken T.Let _ -> cont 54;
	L.RangedToken T.In _ -> cont 55;
	L.RangedToken T.Where _ -> cont 56;
	L.RangedToken T.With _ -> cont 57;
	L.RangedToken T.If _ -> cont 58;
	L.RangedToken T.Then _ -> cont 59;
	L.RangedToken T.Else _ -> cont 60;
	L.RangedToken T.Match _ -> cont 61;
	L.RangedToken T.Return _ -> cont 62;
	L.RangedToken T.Module _ -> cont 63;
	L.RangedToken T.Import _ -> cont 64;
	L.RangedToken T.LParen _ -> cont 65;
	L.RangedToken T.RParen _ -> cont 66;
	L.RangedToken T.LBrack _ -> cont 67;
	L.RangedToken T.RBrack _ -> cont 68;
	L.RangedToken T.LCurly _ -> cont 69;
	L.RangedToken T.RCurly _ -> cont 70;
	L.RangedToken T.Colon _ -> cont 71;
	L.RangedToken T.Comma _ -> cont 72;
	L.RangedToken T.Arrow _ -> cont 73;
	L.RangedToken T.Equals _ -> cont 74;
	L.RangedToken T.Pipe _ -> cont 75;
	L.RangedToken T.Dot _ -> cont 76;
	L.RangedToken T.BackSlash _ -> cont 77;
	L.RangedToken T.Newline _ -> cont 78;
	L.RangedToken T.EOF _ -> cont 79;
	_ -> happyError' (tk, [])
	})

happyError_ explist 80 tk = happyError' (tk, explist)
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
parseSagaScript = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


declaration :: AST.Name L.Range -> AST.Expr L.Range -> Maybe (AST.Type L.Range) -> AST.Declaration L.Range
declaration id expr tyAnn = AST.Define (info id <-> info expr) id expr tyAnn
  

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok range) contructor = contructor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure


binaryOp :: AST.Expr L.Range -> L.RangedToken -> AST.Expr L.Range -> AST.Expr L.Range 
binaryOp expr1 op expr2 = AST.FnApp (info expr1 <-> info expr2) (unTok op (\range (T.Operator char) -> AST.Identifier (AST.Name range (BS.unpack char)))) [expr1, expr2]


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



runSagaScript :: String -> Either String (AST.Script L.Range)
runSagaScript input = L.runAlex (BS.pack input) parseSagaScript

runSagaExpr :: String -> Either String (AST.Expr L.Range)
runSagaExpr input = L.runAlex (BS.pack input) parseSagaExpr
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
