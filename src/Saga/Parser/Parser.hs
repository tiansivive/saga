{-# OPTIONS_GHC -w #-}
module Saga.Parser.Parser  
    ( runSagaScript
    , runSagaExpr 
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
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

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
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
	| HappyAbsSyn19 (Exp L.Range)
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,297) ([0,0,8192,0,0,57344,16577,1029,0,4096,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,672,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,15360,43032,128,0,7680,21516,64,0,3840,10758,32,0,128,0,0,0,64,0,0,0,32,0,0,0,0,0,0,0,8,0,0,0,0,16384,0,0,512,0,0,0,0,1024,0,32768,0,0,0,0,16384,0,0,8192,0,0,0,0,0,32,0,0,0,4,0,0,0,1,0,0,8192,0,0,0,0,1,0,0,33280,0,0,0,2,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,6204,32936,0,0,0,0,0,0,1551,8234,0,32768,775,4117,0,0,0,1,0,0,0,0,0,61440,41056,514,0,30720,20528,257,0,0,0,0,0,0,0,0,0,3840,10758,32,0,1920,5379,16,0,0,0,0,0,0,32,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,2,0,0,0,0,0,0,8,0,0,0,0,0,0,49632,1344,4,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","identifier","path","definition","type","typeAnnotation","pairs","record","listElements","list","tupleElems","tuple","args","lambda","params","fnApplication","controlFlow","clause","block","term","atom","expr","declarations","moduleDef","importMod","imports","script","id","number","string","boolean","'!'","op","let","in","where","with","if","then","else","match","return","module","import","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'='","'|'","'.'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (46) = happyShift action_27
action_0 (27) = happyGoto action_25
action_0 (30) = happyGoto action_26
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (31) = happyShift action_3
action_1 (32) = happyShift action_16
action_1 (33) = happyShift action_17
action_1 (34) = happyShift action_18
action_1 (40) = happyShift action_19
action_1 (41) = happyShift action_20
action_1 (48) = happyShift action_21
action_1 (50) = happyShift action_22
action_1 (52) = happyShift action_23
action_1 (60) = happyShift action_24
action_1 (5) = happyGoto action_4
action_1 (7) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 (13) = happyGoto action_7
action_1 (15) = happyGoto action_8
action_1 (17) = happyGoto action_9
action_1 (19) = happyGoto action_10
action_1 (20) = happyGoto action_11
action_1 (21) = happyGoto action_12
action_1 (23) = happyGoto action_13
action_1 (24) = happyGoto action_14
action_1 (25) = happyGoto action_15
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (31) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (57) = happyShift action_44
action_4 _ = happyReduce_38

action_5 _ = happyReduce_41

action_6 _ = happyReduce_37

action_7 _ = happyReduce_36

action_8 _ = happyReduce_35

action_9 _ = happyReduce_44

action_10 (31) = happyShift action_3
action_10 (32) = happyShift action_16
action_10 (33) = happyShift action_17
action_10 (34) = happyShift action_18
action_10 (48) = happyShift action_21
action_10 (50) = happyShift action_22
action_10 (52) = happyShift action_23
action_10 (5) = happyGoto action_42
action_10 (11) = happyGoto action_6
action_10 (13) = happyGoto action_7
action_10 (15) = happyGoto action_8
action_10 (23) = happyGoto action_13
action_10 (24) = happyGoto action_43
action_10 _ = happyReduce_42

action_11 _ = happyReduce_43

action_12 _ = happyReduce_45

action_13 _ = happyReduce_39

action_14 _ = happyReduce_26

action_15 (63) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_32

action_17 _ = happyReduce_33

action_18 _ = happyReduce_34

action_19 (31) = happyShift action_3
action_19 (5) = happyGoto action_30
action_19 (7) = happyGoto action_31
action_19 (26) = happyGoto action_41
action_19 _ = happyReduce_46

action_20 (31) = happyShift action_3
action_20 (32) = happyShift action_16
action_20 (33) = happyShift action_17
action_20 (34) = happyShift action_18
action_20 (40) = happyShift action_19
action_20 (41) = happyShift action_20
action_20 (48) = happyShift action_21
action_20 (50) = happyShift action_22
action_20 (52) = happyShift action_23
action_20 (60) = happyShift action_24
action_20 (5) = happyGoto action_4
action_20 (7) = happyGoto action_5
action_20 (11) = happyGoto action_6
action_20 (13) = happyGoto action_7
action_20 (15) = happyGoto action_8
action_20 (17) = happyGoto action_9
action_20 (19) = happyGoto action_10
action_20 (20) = happyGoto action_11
action_20 (21) = happyGoto action_12
action_20 (23) = happyGoto action_13
action_20 (24) = happyGoto action_14
action_20 (25) = happyGoto action_40
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (31) = happyShift action_3
action_21 (32) = happyShift action_16
action_21 (33) = happyShift action_17
action_21 (34) = happyShift action_18
action_21 (40) = happyShift action_19
action_21 (41) = happyShift action_20
action_21 (48) = happyShift action_21
action_21 (50) = happyShift action_22
action_21 (52) = happyShift action_23
action_21 (60) = happyShift action_24
action_21 (5) = happyGoto action_4
action_21 (7) = happyGoto action_5
action_21 (11) = happyGoto action_6
action_21 (13) = happyGoto action_7
action_21 (15) = happyGoto action_8
action_21 (17) = happyGoto action_9
action_21 (19) = happyGoto action_10
action_21 (20) = happyGoto action_11
action_21 (21) = happyGoto action_12
action_21 (23) = happyGoto action_13
action_21 (24) = happyGoto action_14
action_21 (25) = happyGoto action_39
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (31) = happyShift action_3
action_22 (32) = happyShift action_16
action_22 (33) = happyShift action_17
action_22 (34) = happyShift action_18
action_22 (40) = happyShift action_19
action_22 (41) = happyShift action_20
action_22 (48) = happyShift action_21
action_22 (50) = happyShift action_22
action_22 (52) = happyShift action_23
action_22 (60) = happyShift action_24
action_22 (5) = happyGoto action_4
action_22 (7) = happyGoto action_5
action_22 (11) = happyGoto action_6
action_22 (12) = happyGoto action_37
action_22 (13) = happyGoto action_7
action_22 (15) = happyGoto action_8
action_22 (17) = happyGoto action_9
action_22 (19) = happyGoto action_10
action_22 (20) = happyGoto action_11
action_22 (21) = happyGoto action_12
action_22 (23) = happyGoto action_13
action_22 (24) = happyGoto action_14
action_22 (25) = happyGoto action_38
action_22 _ = happyReduce_13

action_23 (31) = happyShift action_3
action_23 (5) = happyGoto action_35
action_23 (10) = happyGoto action_36
action_23 _ = happyReduce_9

action_24 (31) = happyShift action_3
action_24 (5) = happyGoto action_33
action_24 (16) = happyGoto action_34
action_24 _ = happyReduce_20

action_25 (31) = happyShift action_3
action_25 (5) = happyGoto action_30
action_25 (7) = happyGoto action_31
action_25 (26) = happyGoto action_32
action_25 _ = happyReduce_46

action_26 (63) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (31) = happyShift action_3
action_27 (5) = happyGoto action_28
action_27 (6) = happyGoto action_29
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (59) = happyShift action_62
action_28 _ = happyReduce_3

action_29 (39) = happyShift action_61
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (57) = happyShift action_44
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_3
action_31 (5) = happyGoto action_30
action_31 (7) = happyGoto action_31
action_31 (26) = happyGoto action_60
action_31 _ = happyReduce_46

action_32 (47) = happyShift action_59
action_32 (28) = happyGoto action_57
action_32 (29) = happyGoto action_58
action_32 _ = happyReduce_50

action_33 (31) = happyShift action_3
action_33 (5) = happyGoto action_33
action_33 (16) = happyGoto action_56
action_33 _ = happyReduce_20

action_34 (56) = happyShift action_55
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (54) = happyShift action_54
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (53) = happyShift action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (51) = happyShift action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (55) = happyShift action_51
action_38 _ = happyReduce_14

action_39 (49) = happyShift action_49
action_39 (55) = happyShift action_50
action_39 (14) = happyGoto action_48
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (42) = happyShift action_47
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (38) = happyShift action_46
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_38

action_43 _ = happyReduce_25

action_44 (31) = happyShift action_3
action_44 (32) = happyShift action_16
action_44 (33) = happyShift action_17
action_44 (34) = happyShift action_18
action_44 (40) = happyShift action_19
action_44 (41) = happyShift action_20
action_44 (48) = happyShift action_21
action_44 (50) = happyShift action_22
action_44 (52) = happyShift action_23
action_44 (60) = happyShift action_24
action_44 (5) = happyGoto action_4
action_44 (7) = happyGoto action_5
action_44 (11) = happyGoto action_6
action_44 (13) = happyGoto action_7
action_44 (15) = happyGoto action_8
action_44 (17) = happyGoto action_9
action_44 (19) = happyGoto action_10
action_44 (20) = happyGoto action_11
action_44 (21) = happyGoto action_12
action_44 (23) = happyGoto action_13
action_44 (24) = happyGoto action_14
action_44 (25) = happyGoto action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_5

action_46 (31) = happyShift action_3
action_46 (32) = happyShift action_16
action_46 (33) = happyShift action_17
action_46 (34) = happyShift action_18
action_46 (40) = happyShift action_19
action_46 (41) = happyShift action_20
action_46 (48) = happyShift action_21
action_46 (50) = happyShift action_22
action_46 (52) = happyShift action_23
action_46 (60) = happyShift action_24
action_46 (5) = happyGoto action_4
action_46 (7) = happyGoto action_5
action_46 (11) = happyGoto action_6
action_46 (13) = happyGoto action_7
action_46 (15) = happyGoto action_8
action_46 (17) = happyGoto action_9
action_46 (19) = happyGoto action_10
action_46 (20) = happyGoto action_11
action_46 (21) = happyGoto action_12
action_46 (23) = happyGoto action_13
action_46 (24) = happyGoto action_14
action_46 (25) = happyGoto action_72
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (31) = happyShift action_3
action_47 (32) = happyShift action_16
action_47 (33) = happyShift action_17
action_47 (34) = happyShift action_18
action_47 (40) = happyShift action_19
action_47 (41) = happyShift action_20
action_47 (48) = happyShift action_21
action_47 (50) = happyShift action_22
action_47 (52) = happyShift action_23
action_47 (60) = happyShift action_24
action_47 (5) = happyGoto action_4
action_47 (7) = happyGoto action_5
action_47 (11) = happyGoto action_6
action_47 (13) = happyGoto action_7
action_47 (15) = happyGoto action_8
action_47 (17) = happyGoto action_9
action_47 (19) = happyGoto action_10
action_47 (20) = happyGoto action_11
action_47 (21) = happyGoto action_12
action_47 (23) = happyGoto action_13
action_47 (24) = happyGoto action_14
action_47 (25) = happyGoto action_71
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (49) = happyShift action_70
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_40

action_50 (31) = happyShift action_3
action_50 (32) = happyShift action_16
action_50 (33) = happyShift action_17
action_50 (34) = happyShift action_18
action_50 (40) = happyShift action_19
action_50 (41) = happyShift action_20
action_50 (48) = happyShift action_21
action_50 (50) = happyShift action_22
action_50 (52) = happyShift action_23
action_50 (60) = happyShift action_24
action_50 (5) = happyGoto action_4
action_50 (7) = happyGoto action_5
action_50 (11) = happyGoto action_6
action_50 (13) = happyGoto action_7
action_50 (15) = happyGoto action_8
action_50 (17) = happyGoto action_9
action_50 (19) = happyGoto action_10
action_50 (20) = happyGoto action_11
action_50 (21) = happyGoto action_12
action_50 (23) = happyGoto action_13
action_50 (24) = happyGoto action_14
action_50 (25) = happyGoto action_69
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (31) = happyShift action_3
action_51 (32) = happyShift action_16
action_51 (33) = happyShift action_17
action_51 (34) = happyShift action_18
action_51 (40) = happyShift action_19
action_51 (41) = happyShift action_20
action_51 (48) = happyShift action_21
action_51 (50) = happyShift action_22
action_51 (52) = happyShift action_23
action_51 (60) = happyShift action_24
action_51 (5) = happyGoto action_4
action_51 (7) = happyGoto action_5
action_51 (11) = happyGoto action_6
action_51 (12) = happyGoto action_68
action_51 (13) = happyGoto action_7
action_51 (15) = happyGoto action_8
action_51 (17) = happyGoto action_9
action_51 (19) = happyGoto action_10
action_51 (20) = happyGoto action_11
action_51 (21) = happyGoto action_12
action_51 (23) = happyGoto action_13
action_51 (24) = happyGoto action_14
action_51 (25) = happyGoto action_38
action_51 _ = happyReduce_13

action_52 _ = happyReduce_16

action_53 _ = happyReduce_12

action_54 (31) = happyShift action_3
action_54 (32) = happyShift action_16
action_54 (33) = happyShift action_17
action_54 (34) = happyShift action_18
action_54 (40) = happyShift action_19
action_54 (41) = happyShift action_20
action_54 (48) = happyShift action_21
action_54 (50) = happyShift action_22
action_54 (52) = happyShift action_23
action_54 (60) = happyShift action_24
action_54 (5) = happyGoto action_4
action_54 (7) = happyGoto action_5
action_54 (11) = happyGoto action_6
action_54 (13) = happyGoto action_7
action_54 (15) = happyGoto action_8
action_54 (17) = happyGoto action_9
action_54 (19) = happyGoto action_10
action_54 (20) = happyGoto action_11
action_54 (21) = happyGoto action_12
action_54 (23) = happyGoto action_13
action_54 (24) = happyGoto action_14
action_54 (25) = happyGoto action_67
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (31) = happyShift action_3
action_55 (32) = happyShift action_16
action_55 (33) = happyShift action_17
action_55 (34) = happyShift action_18
action_55 (40) = happyShift action_19
action_55 (41) = happyShift action_20
action_55 (48) = happyShift action_21
action_55 (50) = happyShift action_22
action_55 (52) = happyShift action_23
action_55 (60) = happyShift action_24
action_55 (5) = happyGoto action_4
action_55 (7) = happyGoto action_5
action_55 (11) = happyGoto action_6
action_55 (13) = happyGoto action_7
action_55 (15) = happyGoto action_8
action_55 (17) = happyGoto action_9
action_55 (19) = happyGoto action_10
action_55 (20) = happyGoto action_11
action_55 (21) = happyGoto action_12
action_55 (23) = happyGoto action_13
action_55 (24) = happyGoto action_14
action_55 (25) = happyGoto action_66
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_21

action_57 (47) = happyShift action_59
action_57 (28) = happyGoto action_57
action_57 (29) = happyGoto action_65
action_57 _ = happyReduce_50

action_58 _ = happyReduce_52

action_59 (31) = happyShift action_3
action_59 (5) = happyGoto action_28
action_59 (6) = happyGoto action_64
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_47

action_61 _ = happyReduce_48

action_62 (31) = happyShift action_3
action_62 (5) = happyGoto action_28
action_62 (6) = happyGoto action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_4

action_64 _ = happyReduce_49

action_65 _ = happyReduce_51

action_66 _ = happyReduce_22

action_67 (55) = happyShift action_75
action_67 _ = happyReduce_11

action_68 _ = happyReduce_15

action_69 (55) = happyShift action_50
action_69 (14) = happyGoto action_74
action_69 _ = happyReduce_17

action_70 _ = happyReduce_19

action_71 (43) = happyShift action_73
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_28

action_73 (31) = happyShift action_3
action_73 (32) = happyShift action_16
action_73 (33) = happyShift action_17
action_73 (34) = happyShift action_18
action_73 (40) = happyShift action_19
action_73 (41) = happyShift action_20
action_73 (48) = happyShift action_21
action_73 (50) = happyShift action_22
action_73 (52) = happyShift action_23
action_73 (60) = happyShift action_24
action_73 (5) = happyGoto action_4
action_73 (7) = happyGoto action_5
action_73 (11) = happyGoto action_6
action_73 (13) = happyGoto action_7
action_73 (15) = happyGoto action_8
action_73 (17) = happyGoto action_9
action_73 (19) = happyGoto action_10
action_73 (20) = happyGoto action_11
action_73 (21) = happyGoto action_12
action_73 (23) = happyGoto action_13
action_73 (24) = happyGoto action_14
action_73 (25) = happyGoto action_77
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_18

action_75 (31) = happyShift action_3
action_75 (5) = happyGoto action_35
action_75 (10) = happyGoto action_76
action_75 _ = happyReduce_9

action_76 _ = happyReduce_10

action_77 _ = happyReduce_27

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

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (AST.Def (info happy_var_1 <-> info happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn8
		 (
	)

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn8
		 (
	)

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 _
	_
	_
	 =  HappyAbsSyn9
		 (
	)

happyReduce_9 = happySpecReduce_0  10 happyReduction_9
happyReduction_9  =  HappyAbsSyn10
		 ([]
	)

happyReduce_10 = happyReduce 5 10 happyReduction_10
happyReduction_10 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (AST.LRecord (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  12 happyReduction_13
happyReduction_13  =  HappyAbsSyn12
		 ([]
	)

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (AST.LList (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn14
		 ([happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  14 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 15 happyReduction_19
happyReduction_19 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.LTuple (L.rtRange happy_var_1 <-> L.rtRange happy_var_4) (happy_var_2:happy_var_3)
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0  16 happyReduction_20
happyReduction_20  =  HappyAbsSyn16
		 ([]
	)

happyReduce_21 = happySpecReduce_2  16 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 17 happyReduction_22
happyReduction_22 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AST.Lambda (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  18 happyReduction_23
happyReduction_23 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  19 happyReduction_25
happyReduction_25 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AST.FnApp (info happy_var_1 <-> info happy_var_2) happy_var_1 [happy_var_2]
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 6 20 happyReduction_27
happyReduction_27 ((HappyAbsSyn25  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (AST.Flow (L.rtRange happy_var_1 <-> info happy_var_6) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 21 happyReduction_28
happyReduction_28 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (AST.Clause (L.rtRange happy_var_1 <-> info happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  22 happyReduction_29
happyReduction_29 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  22 happyReduction_30
happyReduction_30 (HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 ([AST.Return (L.rtRange happy_var_1 <-> info happy_var_2) happy_var_2]
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  22 happyReduction_31
happyReduction_31 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  23 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (unTok happy_var_1 (\range (T.Number int) -> AST.LInt range int)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (unTok happy_var_1 (\range (T.String string) -> AST.LString range string)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  23 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (unTok happy_var_1 (\range (T.Boolean boolean) -> AST.LBool range boolean)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  23 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  23 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  24 happyReduction_38
happyReduction_38 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn24
		 (AST.Identifier happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (AST.Term happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  24 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (AST.Parens (L.rtRange  happy_var_1 <-> L.rtRange happy_var_3) happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn25
		 (AST.Declaration happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  25 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  26 happyReduction_46
happyReduction_46  =  HappyAbsSyn26
		 ([]
	)

happyReduce_47 = happySpecReduce_2  26 happyReduction_47
happyReduction_47 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 : happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  27 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (AST.DefMod (L.rtRange happy_var_1 <-> L.rtRange happy_var_3) (AST.Mod ( map (\(AST.Name _ name) -> name) happy_var_2 ))
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  28 happyReduction_49
happyReduction_49 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (AST.Import (L.rtRange happy_var_1 <-> (info $ last happy_var_2)) (AST.Mod ( map (\(AST.Name _ name) -> name) happy_var_2 ))
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  29 happyReduction_50
happyReduction_50  =  HappyAbsSyn29
		 ([]
	)

happyReduce_51 = happySpecReduce_2  29 happyReduction_51
happyReduction_51 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 : happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  30 happyReduction_52
happyReduction_52 (HappyAbsSyn29  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn30
		 (AST.Script (info happy_var_1 <-> (info $ last happy_var_2)) happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 63 63 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 31;
	L.RangedToken (T.Number _) _ -> cont 32;
	L.RangedToken (T.String _) _ -> cont 33;
	L.RangedToken (T.Boolean _) _ -> cont 34;
	L.RangedToken (T.Operator "!") _ -> cont 35;
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
	L.RangedToken T.Module _ -> cont 46;
	L.RangedToken T.Import _ -> cont 47;
	L.RangedToken T.LParen _ -> cont 48;
	L.RangedToken T.RParen _ -> cont 49;
	L.RangedToken T.LBrack _ -> cont 50;
	L.RangedToken T.RBrack _ -> cont 51;
	L.RangedToken T.LCurly _ -> cont 52;
	L.RangedToken T.RCurly _ -> cont 53;
	L.RangedToken T.Colon _ -> cont 54;
	L.RangedToken T.Comma _ -> cont 55;
	L.RangedToken T.Arrow _ -> cont 56;
	L.RangedToken T.Equals _ -> cont 57;
	L.RangedToken T.Pipe _ -> cont 58;
	L.RangedToken T.Dot _ -> cont 59;
	L.RangedToken T.BackSlash _ -> cont 60;
	L.RangedToken T.Newline _ -> cont 61;
	L.RangedToken T.EOF _ -> cont 62;
	_ -> happyError' (tk, [])
	})

happyError_ explist 63 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

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
