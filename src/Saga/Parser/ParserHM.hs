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

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,671) ([0,0,0,2048,48,0,0,0,0,15,64,8704,1280,0,0,1920,0,0,17,2,0,16384,0,0,128,0,0,0,0,128,3,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,256,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,16,0,8192,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,17408,2048,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,512,4096,10241,0,0,15360,0,1,136,20,0,0,2,0,0,0,0,0,3840,0,0,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,16,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,32,0,0,0,120,0,33024,64,0,0,15360,4096,0,8192,0,0,61440,0,4,544,80,0,0,120,512,4096,10241,0,0,15360,0,1,136,20,0,0,30,128,17408,2560,0,0,256,0,0,0,0,0,32768,15,0,4352,0,0,0,64,0,0,1024,0,0,0,0,0,4096,0,0,0,0,0,0,4,0,0,0,0,0,12320,0,0,0,60,0,34816,4096,0,0,15872,0,0,68,0,0,0,15,0,8704,1024,0,0,0,0,0,514,0,0,16384,0,0,128,0,0,0,0,0,0,16384,0,0,0,0,0,0,32,0,0,0,0,0,4,0,0,0,0,0,0,8,0,0,30,0,17472,2048,0,0,0,0,0,16384,0,0,32768,0,0,256,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,61440,0,0,544,64,0,0,0,0,0,0,0,0,15360,0,0,136,16,0,0,0,0,0,0,0,0,3840,0,0,34,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,0,0,0,3,0,0,0,0,0,15,64,8704,1280,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,480,2048,16384,40964,0,0,0,0,0,0,0,0,0,120,512,4096,10241,0,0,15360,0,1,136,20,0,0,960,0,0,512,0,0,57344,1,0,512,1,0,0,240,0,0,129,0,0,0,0,0,0,0,0,0,60,32,0,32,0,0,0,0,0,64,0,0,0,0,0,12288,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,15,0,8704,1024,0,0,0,0,0,512,0,0,16384,0,0,0,0,0,0,0,512,0,128,0,0,4096,0,0,0,0,0,0,0,0,0,512,0,0,15360,0,1,136,20,0,0,0,0,0,1,0,0,256,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,2,0,16,0,0,0,256,0,64,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,7680,32768,0,68,10,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,15360,0,0,8192,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,1024,0,0,0,0,0,0,2,0,0,0,0,0,3840,0,0,34,4,0,0,0,0,32,112,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,240,0,8192,16386,0,0,0,32783,0,0,8,0,0,4,0,0,0,0,0,0,0,0,2048,0,0,0,1,0,0,0,0,0,1920,0,0,17,2,0,49152,3,0,2176,256,0,0,480,0,16384,32772,0,0,61440,0,0,544,64,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,256,0,0,0,0,0,0,2,0,0,0,0,0,256,0,0,0,0,0,32768,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,128,0,0,0,0,0,49152,3,16,2176,320,0,0,15360,0,0,8192,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","tupleElems","tuple","params","args","fnApplication","controlFlow","term","atom","assignment","binding","bindings","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","tagged","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 103
        bit_end = (st Prelude.+ 1) Prelude.* 103
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..102]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (60) = happyShift action_8
action_0 (69) = happyShift action_9
action_0 (70) = happyShift action_10
action_0 (39) = happyGoto action_43
action_0 (40) = happyGoto action_44
action_0 (41) = happyGoto action_45
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (42) = happyShift action_6
action_1 (43) = happyShift action_35
action_1 (44) = happyShift action_36
action_1 (45) = happyShift action_37
action_1 (64) = happyShift action_38
action_1 (83) = happyShift action_39
action_1 (87) = happyShift action_40
action_1 (98) = happyShift action_41
action_1 (100) = happyShift action_42
action_1 (8) = happyGoto action_27
action_1 (10) = happyGoto action_28
action_1 (12) = happyGoto action_29
action_1 (15) = happyGoto action_30
action_1 (16) = happyGoto action_31
action_1 (17) = happyGoto action_32
action_1 (18) = happyGoto action_33
action_1 (22) = happyGoto action_34
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (42) = happyShift action_6
action_2 (43) = happyShift action_21
action_2 (44) = happyShift action_22
action_2 (45) = happyShift action_23
action_2 (83) = happyShift action_24
action_2 (87) = happyShift action_25
action_2 (100) = happyShift action_26
action_2 (8) = happyGoto action_14
action_2 (24) = happyGoto action_15
action_2 (26) = happyGoto action_16
action_2 (27) = happyGoto action_17
action_2 (28) = happyGoto action_18
action_2 (32) = happyGoto action_19
action_2 (33) = happyGoto action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (42) = happyShift action_6
action_3 (83) = happyShift action_13
action_3 (8) = happyGoto action_11
action_3 (35) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (60) = happyShift action_8
action_4 (69) = happyShift action_9
action_4 (70) = happyShift action_10
action_4 (39) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (42) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (103) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (42) = happyShift action_6
action_8 (8) = happyGoto action_70
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (42) = happyShift action_6
action_9 (8) = happyGoto action_69
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (42) = happyShift action_6
action_10 (8) = happyGoto action_68
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_76

action_12 (92) = happyShift action_67
action_12 (103) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (42) = happyShift action_6
action_13 (83) = happyShift action_13
action_13 (8) = happyGoto action_11
action_13 (35) = happyGoto action_66
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (42) = happyReduce_54
action_14 (43) = happyReduce_54
action_14 (44) = happyReduce_54
action_14 (45) = happyReduce_54
action_14 (46) = happyReduce_54
action_14 (60) = happyReduce_54
action_14 (62) = happyReduce_54
action_14 (69) = happyReduce_54
action_14 (70) = happyReduce_54
action_14 (83) = happyReduce_54
action_14 (84) = happyReduce_54
action_14 (87) = happyReduce_54
action_14 (88) = happyReduce_54
action_14 (89) = happyShift action_65
action_14 (90) = happyReduce_54
action_14 (91) = happyReduce_54
action_14 (92) = happyReduce_54
action_14 (96) = happyReduce_54
action_14 (97) = happyReduce_54
action_14 (99) = happyReduce_54
action_14 (103) = happyReduce_54
action_14 _ = happyReduce_54

action_15 _ = happyReduce_52

action_16 _ = happyReduce_51

action_17 _ = happyReduce_53

action_18 (60) = happyReduce_65
action_18 (62) = happyReduce_65
action_18 (69) = happyReduce_65
action_18 (70) = happyReduce_65
action_18 (84) = happyReduce_65
action_18 (88) = happyReduce_65
action_18 (90) = happyReduce_65
action_18 (91) = happyReduce_65
action_18 (92) = happyReduce_65
action_18 (96) = happyReduce_65
action_18 (97) = happyReduce_65
action_18 (99) = happyReduce_65
action_18 (103) = happyReduce_65
action_18 (29) = happyGoto action_64
action_18 _ = happyReduce_56

action_19 _ = happyReduce_66

action_20 (92) = happyShift action_63
action_20 (103) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_48

action_22 _ = happyReduce_50

action_23 _ = happyReduce_49

action_24 (42) = happyShift action_6
action_24 (43) = happyShift action_21
action_24 (44) = happyShift action_22
action_24 (45) = happyShift action_23
action_24 (83) = happyShift action_24
action_24 (87) = happyShift action_25
action_24 (100) = happyShift action_26
action_24 (8) = happyGoto action_14
action_24 (24) = happyGoto action_15
action_24 (26) = happyGoto action_16
action_24 (27) = happyGoto action_17
action_24 (28) = happyGoto action_18
action_24 (32) = happyGoto action_19
action_24 (33) = happyGoto action_62
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (42) = happyShift action_6
action_25 (8) = happyGoto action_60
action_25 (23) = happyGoto action_61
action_25 _ = happyReduce_41

action_26 (13) = happyGoto action_59
action_26 _ = happyReduce_13

action_27 _ = happyReduce_22

action_28 _ = happyReduce_25

action_29 _ = happyReduce_24

action_30 _ = happyReduce_32

action_31 _ = happyReduce_31

action_32 _ = happyReduce_23

action_33 (47) = happyReduce_34
action_33 (48) = happyReduce_34
action_33 (49) = happyReduce_34
action_33 (50) = happyReduce_34
action_33 (60) = happyReduce_34
action_33 (62) = happyReduce_34
action_33 (65) = happyReduce_34
action_33 (66) = happyReduce_34
action_33 (69) = happyReduce_34
action_33 (70) = happyReduce_34
action_33 (84) = happyReduce_34
action_33 (88) = happyReduce_34
action_33 (91) = happyReduce_34
action_33 (98) = happyReduce_34
action_33 (103) = happyReduce_34
action_33 (14) = happyGoto action_58
action_33 _ = happyReduce_15

action_34 (47) = happyShift action_53
action_34 (48) = happyShift action_54
action_34 (49) = happyShift action_55
action_34 (50) = happyShift action_56
action_34 (98) = happyShift action_57
action_34 (103) = happyAccept
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_19

action_36 _ = happyReduce_21

action_37 _ = happyReduce_20

action_38 (42) = happyShift action_6
action_38 (43) = happyShift action_35
action_38 (44) = happyShift action_36
action_38 (45) = happyShift action_37
action_38 (64) = happyShift action_38
action_38 (83) = happyShift action_39
action_38 (87) = happyShift action_40
action_38 (98) = happyShift action_41
action_38 (100) = happyShift action_42
action_38 (8) = happyGoto action_27
action_38 (10) = happyGoto action_28
action_38 (12) = happyGoto action_29
action_38 (15) = happyGoto action_30
action_38 (16) = happyGoto action_31
action_38 (17) = happyGoto action_32
action_38 (18) = happyGoto action_33
action_38 (22) = happyGoto action_52
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (42) = happyShift action_6
action_39 (43) = happyShift action_35
action_39 (44) = happyShift action_36
action_39 (45) = happyShift action_37
action_39 (64) = happyShift action_38
action_39 (83) = happyShift action_39
action_39 (87) = happyShift action_40
action_39 (98) = happyShift action_41
action_39 (100) = happyShift action_42
action_39 (8) = happyGoto action_27
action_39 (10) = happyGoto action_28
action_39 (12) = happyGoto action_29
action_39 (15) = happyGoto action_30
action_39 (16) = happyGoto action_31
action_39 (17) = happyGoto action_32
action_39 (18) = happyGoto action_33
action_39 (22) = happyGoto action_51
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (42) = happyShift action_6
action_40 (8) = happyGoto action_49
action_40 (9) = happyGoto action_50
action_40 _ = happyReduce_6

action_41 (42) = happyShift action_6
action_41 (43) = happyShift action_35
action_41 (44) = happyShift action_36
action_41 (45) = happyShift action_37
action_41 (83) = happyShift action_39
action_41 (87) = happyShift action_40
action_41 (8) = happyGoto action_27
action_41 (10) = happyGoto action_28
action_41 (12) = happyGoto action_29
action_41 (17) = happyGoto action_32
action_41 (18) = happyGoto action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (13) = happyGoto action_47
action_42 _ = happyReduce_13

action_43 _ = happyReduce_88

action_44 (60) = happyShift action_8
action_44 (69) = happyShift action_9
action_44 (70) = happyShift action_10
action_44 (39) = happyGoto action_46
action_44 _ = happyReduce_90

action_45 (103) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_89

action_47 (42) = happyShift action_6
action_47 (92) = happyShift action_103
action_47 (8) = happyGoto action_88
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_35

action_49 (89) = happyShift action_102
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (88) = happyShift action_101
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (47) = happyShift action_53
action_51 (48) = happyShift action_54
action_51 (49) = happyShift action_55
action_51 (50) = happyShift action_56
action_51 (84) = happyShift action_99
action_51 (91) = happyShift action_100
action_51 (98) = happyShift action_57
action_51 (11) = happyGoto action_98
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (47) = happyShift action_53
action_52 (48) = happyShift action_54
action_52 (49) = happyShift action_55
action_52 (50) = happyShift action_56
action_52 (65) = happyShift action_97
action_52 (98) = happyShift action_57
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (42) = happyShift action_6
action_53 (43) = happyShift action_35
action_53 (44) = happyShift action_36
action_53 (45) = happyShift action_37
action_53 (64) = happyShift action_38
action_53 (83) = happyShift action_39
action_53 (87) = happyShift action_40
action_53 (98) = happyShift action_41
action_53 (100) = happyShift action_42
action_53 (8) = happyGoto action_27
action_53 (10) = happyGoto action_28
action_53 (12) = happyGoto action_29
action_53 (15) = happyGoto action_30
action_53 (16) = happyGoto action_31
action_53 (17) = happyGoto action_32
action_53 (18) = happyGoto action_33
action_53 (22) = happyGoto action_96
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (42) = happyShift action_6
action_54 (43) = happyShift action_35
action_54 (44) = happyShift action_36
action_54 (45) = happyShift action_37
action_54 (64) = happyShift action_38
action_54 (83) = happyShift action_39
action_54 (87) = happyShift action_40
action_54 (98) = happyShift action_41
action_54 (100) = happyShift action_42
action_54 (8) = happyGoto action_27
action_54 (10) = happyGoto action_28
action_54 (12) = happyGoto action_29
action_54 (15) = happyGoto action_30
action_54 (16) = happyGoto action_31
action_54 (17) = happyGoto action_32
action_54 (18) = happyGoto action_33
action_54 (22) = happyGoto action_95
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (42) = happyShift action_6
action_55 (43) = happyShift action_35
action_55 (44) = happyShift action_36
action_55 (45) = happyShift action_37
action_55 (64) = happyShift action_38
action_55 (83) = happyShift action_39
action_55 (87) = happyShift action_40
action_55 (98) = happyShift action_41
action_55 (100) = happyShift action_42
action_55 (8) = happyGoto action_27
action_55 (10) = happyGoto action_28
action_55 (12) = happyGoto action_29
action_55 (15) = happyGoto action_30
action_55 (16) = happyGoto action_31
action_55 (17) = happyGoto action_32
action_55 (18) = happyGoto action_33
action_55 (22) = happyGoto action_94
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (42) = happyShift action_6
action_56 (43) = happyShift action_35
action_56 (44) = happyShift action_36
action_56 (45) = happyShift action_37
action_56 (64) = happyShift action_38
action_56 (83) = happyShift action_39
action_56 (87) = happyShift action_40
action_56 (98) = happyShift action_41
action_56 (100) = happyShift action_42
action_56 (8) = happyGoto action_27
action_56 (10) = happyGoto action_28
action_56 (12) = happyGoto action_29
action_56 (15) = happyGoto action_30
action_56 (16) = happyGoto action_31
action_56 (17) = happyGoto action_32
action_56 (18) = happyGoto action_33
action_56 (22) = happyGoto action_93
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (42) = happyShift action_6
action_57 (8) = happyGoto action_92
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (42) = happyShift action_6
action_58 (43) = happyShift action_35
action_58 (44) = happyShift action_36
action_58 (45) = happyShift action_37
action_58 (46) = happyShift action_91
action_58 (83) = happyShift action_39
action_58 (87) = happyShift action_40
action_58 (8) = happyGoto action_27
action_58 (10) = happyGoto action_28
action_58 (12) = happyGoto action_29
action_58 (17) = happyGoto action_32
action_58 (18) = happyGoto action_90
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (42) = happyShift action_6
action_59 (94) = happyShift action_89
action_59 (8) = happyGoto action_88
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (89) = happyShift action_87
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (88) = happyShift action_86
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (84) = happyShift action_84
action_62 (91) = happyShift action_85
action_62 (92) = happyShift action_63
action_62 (25) = happyGoto action_83
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (42) = happyShift action_6
action_63 (43) = happyShift action_21
action_63 (44) = happyShift action_22
action_63 (45) = happyShift action_23
action_63 (83) = happyShift action_24
action_63 (87) = happyShift action_25
action_63 (100) = happyShift action_26
action_63 (8) = happyGoto action_14
action_63 (24) = happyGoto action_15
action_63 (26) = happyGoto action_16
action_63 (27) = happyGoto action_17
action_63 (28) = happyGoto action_18
action_63 (32) = happyGoto action_19
action_63 (33) = happyGoto action_82
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (42) = happyShift action_6
action_64 (43) = happyShift action_21
action_64 (44) = happyShift action_22
action_64 (45) = happyShift action_23
action_64 (46) = happyShift action_81
action_64 (83) = happyShift action_24
action_64 (87) = happyShift action_25
action_64 (8) = happyGoto action_79
action_64 (24) = happyGoto action_15
action_64 (26) = happyGoto action_16
action_64 (27) = happyGoto action_17
action_64 (28) = happyGoto action_80
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (42) = happyShift action_6
action_65 (43) = happyShift action_21
action_65 (44) = happyShift action_22
action_65 (45) = happyShift action_23
action_65 (83) = happyShift action_24
action_65 (87) = happyShift action_25
action_65 (100) = happyShift action_26
action_65 (8) = happyGoto action_14
action_65 (24) = happyGoto action_15
action_65 (26) = happyGoto action_16
action_65 (27) = happyGoto action_17
action_65 (28) = happyGoto action_18
action_65 (32) = happyGoto action_19
action_65 (33) = happyGoto action_78
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (84) = happyShift action_77
action_66 (92) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (42) = happyShift action_6
action_67 (83) = happyShift action_13
action_67 (8) = happyGoto action_11
action_67 (35) = happyGoto action_76
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (99) = happyShift action_74
action_68 (36) = happyGoto action_75
action_68 _ = happyReduce_77

action_69 (99) = happyShift action_74
action_69 (36) = happyGoto action_73
action_69 _ = happyReduce_77

action_70 (89) = happyShift action_72
action_70 (34) = happyGoto action_71
action_70 _ = happyReduce_70

action_71 (99) = happyShift action_74
action_71 (36) = happyGoto action_118
action_71 _ = happyReduce_77

action_72 (42) = happyShift action_6
action_72 (43) = happyShift action_21
action_72 (44) = happyShift action_22
action_72 (45) = happyShift action_23
action_72 (79) = happyShift action_117
action_72 (83) = happyShift action_24
action_72 (87) = happyShift action_25
action_72 (100) = happyShift action_26
action_72 (8) = happyGoto action_14
action_72 (24) = happyGoto action_15
action_72 (26) = happyGoto action_16
action_72 (27) = happyGoto action_17
action_72 (28) = happyGoto action_18
action_72 (32) = happyGoto action_19
action_72 (33) = happyGoto action_116
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (96) = happyShift action_115
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (42) = happyShift action_6
action_74 (83) = happyShift action_13
action_74 (8) = happyGoto action_11
action_74 (35) = happyGoto action_114
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (96) = happyShift action_113
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_74

action_77 _ = happyReduce_75

action_78 (60) = happyReduce_64
action_78 (62) = happyReduce_64
action_78 (69) = happyReduce_64
action_78 (70) = happyReduce_64
action_78 (84) = happyReduce_64
action_78 (88) = happyReduce_64
action_78 (90) = happyReduce_64
action_78 (91) = happyReduce_64
action_78 (92) = happyShift action_63
action_78 (96) = happyReduce_64
action_78 (97) = happyReduce_64
action_78 (99) = happyReduce_64
action_78 (103) = happyReduce_64
action_78 _ = happyReduce_64

action_79 (42) = happyReduce_54
action_79 (43) = happyReduce_54
action_79 (44) = happyReduce_54
action_79 (45) = happyReduce_54
action_79 (46) = happyReduce_54
action_79 (83) = happyReduce_54
action_79 (87) = happyReduce_54
action_79 _ = happyReduce_54

action_80 _ = happyReduce_57

action_81 _ = happyReduce_69

action_82 (60) = happyReduce_67
action_82 (62) = happyReduce_67
action_82 (69) = happyReduce_67
action_82 (70) = happyReduce_67
action_82 (84) = happyReduce_67
action_82 (88) = happyReduce_67
action_82 (90) = happyReduce_67
action_82 (91) = happyReduce_67
action_82 (92) = happyShift action_63
action_82 (96) = happyReduce_67
action_82 (97) = happyReduce_67
action_82 (99) = happyReduce_67
action_82 (103) = happyReduce_67
action_82 _ = happyReduce_67

action_83 (84) = happyShift action_112
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_55

action_85 (42) = happyShift action_6
action_85 (43) = happyShift action_21
action_85 (44) = happyShift action_22
action_85 (45) = happyShift action_23
action_85 (83) = happyShift action_24
action_85 (87) = happyShift action_25
action_85 (100) = happyShift action_26
action_85 (8) = happyGoto action_14
action_85 (24) = happyGoto action_15
action_85 (26) = happyGoto action_16
action_85 (27) = happyGoto action_17
action_85 (28) = happyGoto action_18
action_85 (32) = happyGoto action_19
action_85 (33) = happyGoto action_111
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_44

action_87 (42) = happyShift action_6
action_87 (43) = happyShift action_21
action_87 (44) = happyShift action_22
action_87 (45) = happyShift action_23
action_87 (83) = happyShift action_24
action_87 (87) = happyShift action_25
action_87 (100) = happyShift action_26
action_87 (8) = happyGoto action_14
action_87 (24) = happyGoto action_15
action_87 (26) = happyGoto action_16
action_87 (27) = happyGoto action_17
action_87 (28) = happyGoto action_18
action_87 (32) = happyGoto action_19
action_87 (33) = happyGoto action_110
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_14

action_89 (42) = happyShift action_6
action_89 (43) = happyShift action_21
action_89 (44) = happyShift action_22
action_89 (45) = happyShift action_23
action_89 (83) = happyShift action_24
action_89 (87) = happyShift action_25
action_89 (100) = happyShift action_26
action_89 (8) = happyGoto action_14
action_89 (24) = happyGoto action_15
action_89 (26) = happyGoto action_16
action_89 (27) = happyGoto action_17
action_89 (28) = happyGoto action_18
action_89 (32) = happyGoto action_19
action_89 (33) = happyGoto action_109
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_16

action_91 _ = happyReduce_17

action_92 _ = happyReduce_36

action_93 _ = happyReduce_40

action_94 _ = happyReduce_39

action_95 (49) = happyShift action_55
action_95 (50) = happyShift action_56
action_95 _ = happyReduce_38

action_96 (49) = happyShift action_55
action_96 (50) = happyShift action_56
action_96 _ = happyReduce_37

action_97 (42) = happyShift action_6
action_97 (43) = happyShift action_35
action_97 (44) = happyShift action_36
action_97 (45) = happyShift action_37
action_97 (64) = happyShift action_38
action_97 (83) = happyShift action_39
action_97 (87) = happyShift action_40
action_97 (98) = happyShift action_41
action_97 (100) = happyShift action_42
action_97 (8) = happyGoto action_27
action_97 (10) = happyGoto action_28
action_97 (12) = happyGoto action_29
action_97 (15) = happyGoto action_30
action_97 (16) = happyGoto action_31
action_97 (17) = happyGoto action_32
action_97 (18) = happyGoto action_33
action_97 (22) = happyGoto action_108
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (84) = happyShift action_107
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_26

action_100 (42) = happyShift action_6
action_100 (43) = happyShift action_35
action_100 (44) = happyShift action_36
action_100 (45) = happyShift action_37
action_100 (64) = happyShift action_38
action_100 (83) = happyShift action_39
action_100 (87) = happyShift action_40
action_100 (98) = happyShift action_41
action_100 (100) = happyShift action_42
action_100 (8) = happyGoto action_27
action_100 (10) = happyGoto action_28
action_100 (12) = happyGoto action_29
action_100 (15) = happyGoto action_30
action_100 (16) = happyGoto action_31
action_100 (17) = happyGoto action_32
action_100 (18) = happyGoto action_33
action_100 (22) = happyGoto action_106
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_9

action_102 (42) = happyShift action_6
action_102 (43) = happyShift action_35
action_102 (44) = happyShift action_36
action_102 (45) = happyShift action_37
action_102 (64) = happyShift action_38
action_102 (83) = happyShift action_39
action_102 (87) = happyShift action_40
action_102 (98) = happyShift action_41
action_102 (100) = happyShift action_42
action_102 (8) = happyGoto action_27
action_102 (10) = happyGoto action_28
action_102 (12) = happyGoto action_29
action_102 (15) = happyGoto action_30
action_102 (16) = happyGoto action_31
action_102 (17) = happyGoto action_32
action_102 (18) = happyGoto action_33
action_102 (22) = happyGoto action_105
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (42) = happyShift action_6
action_103 (43) = happyShift action_35
action_103 (44) = happyShift action_36
action_103 (45) = happyShift action_37
action_103 (64) = happyShift action_38
action_103 (83) = happyShift action_39
action_103 (87) = happyShift action_40
action_103 (98) = happyShift action_41
action_103 (100) = happyShift action_42
action_103 (8) = happyGoto action_27
action_103 (10) = happyGoto action_28
action_103 (12) = happyGoto action_29
action_103 (15) = happyGoto action_30
action_103 (16) = happyGoto action_31
action_103 (17) = happyGoto action_32
action_103 (18) = happyGoto action_33
action_103 (22) = happyGoto action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (47) = happyShift action_53
action_104 (48) = happyShift action_54
action_104 (49) = happyShift action_55
action_104 (50) = happyShift action_56
action_104 (98) = happyShift action_57
action_104 _ = happyReduce_33

action_105 (47) = happyShift action_53
action_105 (48) = happyShift action_54
action_105 (49) = happyShift action_55
action_105 (50) = happyShift action_56
action_105 (91) = happyShift action_130
action_105 (98) = happyShift action_57
action_105 _ = happyReduce_8

action_106 (47) = happyShift action_53
action_106 (48) = happyShift action_54
action_106 (49) = happyShift action_55
action_106 (50) = happyShift action_56
action_106 (91) = happyShift action_100
action_106 (98) = happyShift action_57
action_106 (11) = happyGoto action_129
action_106 _ = happyReduce_10

action_107 _ = happyReduce_12

action_108 (47) = happyShift action_53
action_108 (48) = happyShift action_54
action_108 (49) = happyShift action_55
action_108 (50) = happyShift action_56
action_108 (66) = happyShift action_128
action_108 (98) = happyShift action_57
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (60) = happyReduce_68
action_109 (62) = happyReduce_68
action_109 (69) = happyReduce_68
action_109 (70) = happyReduce_68
action_109 (84) = happyReduce_68
action_109 (88) = happyReduce_68
action_109 (90) = happyReduce_68
action_109 (91) = happyReduce_68
action_109 (92) = happyShift action_63
action_109 (96) = happyReduce_68
action_109 (97) = happyReduce_68
action_109 (99) = happyReduce_68
action_109 (103) = happyReduce_68
action_109 _ = happyReduce_68

action_110 (91) = happyShift action_127
action_110 (92) = happyShift action_63
action_110 _ = happyReduce_43

action_111 (91) = happyShift action_85
action_111 (92) = happyShift action_63
action_111 (25) = happyGoto action_126
action_111 _ = happyReduce_45

action_112 _ = happyReduce_47

action_113 (42) = happyShift action_6
action_113 (43) = happyShift action_21
action_113 (44) = happyShift action_22
action_113 (45) = happyShift action_23
action_113 (83) = happyShift action_24
action_113 (87) = happyShift action_25
action_113 (100) = happyShift action_26
action_113 (8) = happyGoto action_14
action_113 (24) = happyGoto action_15
action_113 (26) = happyGoto action_16
action_113 (27) = happyGoto action_17
action_113 (28) = happyGoto action_18
action_113 (32) = happyGoto action_19
action_113 (33) = happyGoto action_125
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (92) = happyShift action_67
action_114 _ = happyReduce_78

action_115 (42) = happyShift action_6
action_115 (8) = happyGoto action_122
action_115 (37) = happyGoto action_123
action_115 (38) = happyGoto action_124
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (62) = happyShift action_121
action_116 (92) = happyShift action_63
action_116 _ = happyReduce_71

action_117 (42) = happyShift action_6
action_117 (8) = happyGoto action_120
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (96) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (42) = happyShift action_6
action_119 (43) = happyShift action_35
action_119 (44) = happyShift action_36
action_119 (45) = happyShift action_37
action_119 (64) = happyShift action_38
action_119 (83) = happyShift action_39
action_119 (87) = happyShift action_40
action_119 (98) = happyShift action_41
action_119 (100) = happyShift action_42
action_119 (8) = happyGoto action_27
action_119 (10) = happyGoto action_28
action_119 (12) = happyGoto action_29
action_119 (15) = happyGoto action_30
action_119 (16) = happyGoto action_31
action_119 (17) = happyGoto action_32
action_119 (18) = happyGoto action_33
action_119 (22) = happyGoto action_142
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (89) = happyShift action_141
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (42) = happyShift action_6
action_121 (8) = happyGoto action_138
action_121 (30) = happyGoto action_139
action_121 (31) = happyGoto action_140
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (89) = happyShift action_137
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_80

action_124 (62) = happyShift action_135
action_124 (97) = happyShift action_136
action_124 _ = happyReduce_84

action_125 (62) = happyShift action_134
action_125 (92) = happyShift action_63
action_125 _ = happyReduce_86

action_126 _ = happyReduce_46

action_127 (42) = happyShift action_6
action_127 (8) = happyGoto action_60
action_127 (23) = happyGoto action_133
action_127 _ = happyReduce_41

action_128 (42) = happyShift action_6
action_128 (43) = happyShift action_35
action_128 (44) = happyShift action_36
action_128 (45) = happyShift action_37
action_128 (64) = happyShift action_38
action_128 (83) = happyShift action_39
action_128 (87) = happyShift action_40
action_128 (98) = happyShift action_41
action_128 (100) = happyShift action_42
action_128 (8) = happyGoto action_27
action_128 (10) = happyGoto action_28
action_128 (12) = happyGoto action_29
action_128 (15) = happyGoto action_30
action_128 (16) = happyGoto action_31
action_128 (17) = happyGoto action_32
action_128 (18) = happyGoto action_33
action_128 (22) = happyGoto action_132
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_11

action_130 (42) = happyShift action_6
action_130 (8) = happyGoto action_49
action_130 (9) = happyGoto action_131
action_130 _ = happyReduce_6

action_131 _ = happyReduce_7

action_132 (47) = happyShift action_53
action_132 (48) = happyShift action_54
action_132 (49) = happyShift action_55
action_132 (50) = happyShift action_56
action_132 (98) = happyShift action_57
action_132 _ = happyReduce_18

action_133 _ = happyReduce_42

action_134 (42) = happyShift action_6
action_134 (8) = happyGoto action_138
action_134 (30) = happyGoto action_139
action_134 (31) = happyGoto action_153
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (42) = happyShift action_6
action_135 (8) = happyGoto action_138
action_135 (30) = happyGoto action_139
action_135 (31) = happyGoto action_152
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (42) = happyShift action_6
action_136 (8) = happyGoto action_122
action_136 (37) = happyGoto action_151
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (42) = happyShift action_6
action_137 (43) = happyShift action_21
action_137 (44) = happyShift action_22
action_137 (45) = happyShift action_23
action_137 (83) = happyShift action_24
action_137 (87) = happyShift action_25
action_137 (100) = happyShift action_26
action_137 (8) = happyGoto action_14
action_137 (24) = happyGoto action_15
action_137 (26) = happyGoto action_16
action_137 (27) = happyGoto action_17
action_137 (28) = happyGoto action_18
action_137 (32) = happyGoto action_19
action_137 (33) = happyGoto action_150
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (80) = happyShift action_146
action_138 (95) = happyShift action_147
action_138 (96) = happyShift action_148
action_138 (97) = happyShift action_149
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_62

action_140 (90) = happyShift action_145
action_140 _ = happyReduce_72

action_141 (42) = happyShift action_6
action_141 (43) = happyShift action_21
action_141 (44) = happyShift action_22
action_141 (45) = happyShift action_23
action_141 (83) = happyShift action_24
action_141 (87) = happyShift action_25
action_141 (100) = happyShift action_26
action_141 (8) = happyGoto action_14
action_141 (24) = happyGoto action_15
action_141 (26) = happyGoto action_16
action_141 (27) = happyGoto action_17
action_141 (28) = happyGoto action_18
action_141 (32) = happyGoto action_19
action_141 (33) = happyGoto action_144
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (47) = happyShift action_53
action_142 (48) = happyShift action_54
action_142 (49) = happyShift action_55
action_142 (50) = happyShift action_56
action_142 (62) = happyShift action_143
action_142 (98) = happyShift action_57
action_142 _ = happyReduce_82

action_143 (42) = happyShift action_6
action_143 (8) = happyGoto action_159
action_143 (20) = happyGoto action_160
action_143 (21) = happyGoto action_161
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (92) = happyShift action_63
action_144 _ = happyReduce_73

action_145 (42) = happyShift action_6
action_145 (8) = happyGoto action_138
action_145 (30) = happyGoto action_158
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (42) = happyShift action_6
action_146 (43) = happyShift action_21
action_146 (44) = happyShift action_22
action_146 (45) = happyShift action_23
action_146 (83) = happyShift action_24
action_146 (87) = happyShift action_25
action_146 (100) = happyShift action_26
action_146 (8) = happyGoto action_14
action_146 (24) = happyGoto action_15
action_146 (26) = happyGoto action_16
action_146 (27) = happyGoto action_17
action_146 (28) = happyGoto action_18
action_146 (32) = happyGoto action_19
action_146 (33) = happyGoto action_157
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (42) = happyShift action_6
action_147 (43) = happyShift action_21
action_147 (44) = happyShift action_22
action_147 (45) = happyShift action_23
action_147 (83) = happyShift action_24
action_147 (87) = happyShift action_25
action_147 (100) = happyShift action_26
action_147 (8) = happyGoto action_14
action_147 (24) = happyGoto action_15
action_147 (26) = happyGoto action_16
action_147 (27) = happyGoto action_17
action_147 (28) = happyGoto action_18
action_147 (32) = happyGoto action_19
action_147 (33) = happyGoto action_156
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (42) = happyShift action_6
action_148 (43) = happyShift action_21
action_148 (44) = happyShift action_22
action_148 (45) = happyShift action_23
action_148 (83) = happyShift action_24
action_148 (87) = happyShift action_25
action_148 (100) = happyShift action_26
action_148 (8) = happyGoto action_14
action_148 (24) = happyGoto action_15
action_148 (26) = happyGoto action_16
action_148 (27) = happyGoto action_17
action_148 (28) = happyGoto action_18
action_148 (32) = happyGoto action_19
action_148 (33) = happyGoto action_155
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (42) = happyShift action_6
action_149 (43) = happyShift action_21
action_149 (44) = happyShift action_22
action_149 (45) = happyShift action_23
action_149 (83) = happyShift action_24
action_149 (87) = happyShift action_25
action_149 (100) = happyShift action_26
action_149 (8) = happyGoto action_14
action_149 (24) = happyGoto action_15
action_149 (26) = happyGoto action_16
action_149 (27) = happyGoto action_17
action_149 (28) = happyGoto action_18
action_149 (32) = happyGoto action_19
action_149 (33) = happyGoto action_154
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (92) = happyShift action_63
action_150 _ = happyReduce_79

action_151 _ = happyReduce_81

action_152 (90) = happyShift action_145
action_152 _ = happyReduce_85

action_153 (90) = happyShift action_145
action_153 _ = happyReduce_87

action_154 (92) = happyShift action_63
action_154 _ = happyReduce_61

action_155 (92) = happyShift action_63
action_155 _ = happyReduce_58

action_156 (92) = happyShift action_63
action_156 _ = happyReduce_60

action_157 (92) = happyShift action_63
action_157 _ = happyReduce_59

action_158 _ = happyReduce_63

action_159 (96) = happyShift action_163
action_159 _ = happyFail (happyExpListPerState 159)

action_160 _ = happyReduce_29

action_161 (91) = happyShift action_162
action_161 _ = happyReduce_83

action_162 (42) = happyShift action_6
action_162 (8) = happyGoto action_159
action_162 (20) = happyGoto action_165
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (42) = happyShift action_6
action_163 (43) = happyShift action_35
action_163 (44) = happyShift action_36
action_163 (45) = happyShift action_37
action_163 (64) = happyShift action_38
action_163 (83) = happyShift action_39
action_163 (87) = happyShift action_40
action_163 (98) = happyShift action_41
action_163 (100) = happyShift action_42
action_163 (8) = happyGoto action_27
action_163 (10) = happyGoto action_28
action_163 (12) = happyGoto action_29
action_163 (15) = happyGoto action_30
action_163 (16) = happyGoto action_31
action_163 (17) = happyGoto action_32
action_163 (18) = happyGoto action_33
action_163 (22) = happyGoto action_164
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (47) = happyShift action_53
action_164 (48) = happyShift action_54
action_164 (49) = happyShift action_55
action_164 (50) = happyShift action_56
action_164 (98) = happyShift action_57
action_164 _ = happyReduce_28

action_165 _ = happyReduce_30

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
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn22  happy_var_3)
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

happyReduce_10 = happySpecReduce_2  11 happyReduction_10
happyReduction_10 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ([happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 12 happyReduction_12
happyReduction_12 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (P.tuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  13 happyReduction_13
happyReduction_13  =  HappyAbsSyn13
		 ([]
	)

happyReduce_14 = happySpecReduce_2  13 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  14 happyReduction_15
happyReduction_15  =  HappyAbsSyn14
		 ([]
	)

happyReduce_16 = happySpecReduce_2  14 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  15 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 16 happyReduction_18
happyReduction_18 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  17 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.number HM.LInt happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  17 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  17 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.string HM.LString happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  18 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  18 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (P.term happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  18 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn19
		 (P.assignment happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  20 happyReduction_28
happyReduction_28 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn20
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  21 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  21 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  22 happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 22 happyReduction_33
happyReduction_33 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  22 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (P.dotLambda happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  22 happyReduction_36
happyReduction_36 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  22 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  22 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  22 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  23 happyReduction_41
happyReduction_41  =  HappyAbsSyn23
		 ([]
	)

happyReduce_42 = happyReduce 5 23 happyReduction_42
happyReduction_42 ((HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn23
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  24 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_3)
	(HappyAbsSyn23  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  25 happyReduction_45
happyReduction_45 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn25
		 ([happy_var_2]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2 : happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 4 26 happyReduction_47
happyReduction_47 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  27 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  27 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  27 happyReduction_51
happyReduction_51 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  27 happyReduction_52
happyReduction_52 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  28 happyReduction_53
happyReduction_53 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyIdentifier happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  28 happyReduction_55
happyReduction_55 (HappyTerminal happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_0  29 happyReduction_56
happyReduction_56  =  HappyAbsSyn29
		 ([]
	)

happyReduce_57 = happySpecReduce_2  29 happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  30 happyReduction_58
happyReduction_58 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  30 happyReduction_59
happyReduction_59 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  31 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  31 happyReduction_63
happyReduction_63 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  32 happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn32
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  33 happyReduction_65
happyReduction_65 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  33 happyReduction_66
happyReduction_66 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  33 happyReduction_67
happyReduction_67 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happyReduce 4 33 happyReduction_68
happyReduction_68 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_3  33 happyReduction_69
happyReduction_69 (HappyTerminal happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn33
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  34 happyReduction_70
happyReduction_70  =  HappyAbsSyn34
		 (Nothing
	)

happyReduce_71 = happySpecReduce_2  34 happyReduction_71
happyReduction_71 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn34
		 (Just happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happyReduce 4 34 happyReduction_72
happyReduction_72 ((HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 5 34 happyReduction_73
happyReduction_73 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_3  35 happyReduction_74
happyReduction_74 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  35 happyReduction_75
happyReduction_75 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  35 happyReduction_76
happyReduction_76 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn35
		 (P.kindId happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  36 happyReduction_77
happyReduction_77  =  HappyAbsSyn36
		 (Nothing
	)

happyReduce_78 = happySpecReduce_2  36 happyReduction_78
happyReduction_78 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn36
		 (Just happy_var_2
	)
happyReduction_78 _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  37 happyReduction_79
happyReduction_79 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  38 happyReduction_80
happyReduction_80 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  38 happyReduction_81
happyReduction_81 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happyReduce 6 39 happyReduction_82
happyReduction_82 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_4) `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 8 39 happyReduction_83
happyReduction_83 ((HappyAbsSyn21  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_4) `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 5 39 happyReduction_84
happyReduction_84 ((HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_85 = happyReduce 7 39 happyReduction_85
happyReduction_85 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_86 = happyReduce 5 39 happyReduction_86
happyReduction_86 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_87 = happyReduce 7 39 happyReduction_87
happyReduction_87 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_88 = happySpecReduce_1  40 happyReduction_88
happyReduction_88 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_2  40 happyReduction_89
happyReduction_89 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  41 happyReduction_90
happyReduction_90 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn41
		 (P.script happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 103 103 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 42;
	L.RangedToken (T.Number _) _ -> cont 43;
	L.RangedToken (T.String _) _ -> cont 44;
	L.RangedToken (T.Boolean _) _ -> cont 45;
	L.RangedToken (T.Operator "!") _ -> cont 46;
	L.RangedToken (T.Operator "+") _ -> cont 47;
	L.RangedToken (T.Operator "-") _ -> cont 48;
	L.RangedToken (T.Operator "*") _ -> cont 49;
	L.RangedToken (T.Operator "/") _ -> cont 50;
	L.RangedToken (T.Operator "==") _ -> cont 51;
	L.RangedToken (T.Operator "!=") _ -> cont 52;
	L.RangedToken (T.Operator "<") _ -> cont 53;
	L.RangedToken (T.Operator "<=") _ -> cont 54;
	L.RangedToken (T.Operator ">") _ -> cont 55;
	L.RangedToken (T.Operator ">=") _ -> cont 56;
	L.RangedToken (T.Operator "||") _ -> cont 57;
	L.RangedToken (T.Operator "&&") _ -> cont 58;
	L.RangedToken (T.Operator _) _ -> cont 59;
	L.RangedToken T.Let _ -> cont 60;
	L.RangedToken T.In _ -> cont 61;
	L.RangedToken T.Where _ -> cont 62;
	L.RangedToken T.With _ -> cont 63;
	L.RangedToken T.If _ -> cont 64;
	L.RangedToken T.Then _ -> cont 65;
	L.RangedToken T.Else _ -> cont 66;
	L.RangedToken T.Match _ -> cont 67;
	L.RangedToken T.Return _ -> cont 68;
	L.RangedToken T.Data _ -> cont 69;
	L.RangedToken T.Type _ -> cont 70;
	L.RangedToken T.Alias _ -> cont 71;
	L.RangedToken T.Kind _ -> cont 72;
	L.RangedToken T.Forall _ -> cont 73;
	L.RangedToken T.Exists _ -> cont 74;
	L.RangedToken T.Proof _ -> cont 75;
	L.RangedToken T.Infer _ -> cont 76;
	L.RangedToken T.Protocol _ -> cont 77;
	L.RangedToken T.Interface _ -> cont 78;
	L.RangedToken T.Instance _ -> cont 79;
	L.RangedToken T.Implements _ -> cont 80;
	L.RangedToken T.Module _ -> cont 81;
	L.RangedToken T.Import _ -> cont 82;
	L.RangedToken T.LParen _ -> cont 83;
	L.RangedToken T.RParen _ -> cont 84;
	L.RangedToken T.LBrack _ -> cont 85;
	L.RangedToken T.RBrack _ -> cont 86;
	L.RangedToken T.LCurly _ -> cont 87;
	L.RangedToken T.RCurly _ -> cont 88;
	L.RangedToken T.Colon _ -> cont 89;
	L.RangedToken T.SemiColon _ -> cont 90;
	L.RangedToken T.Comma _ -> cont 91;
	L.RangedToken T.Arrow _ -> cont 92;
	L.RangedToken T.BackArrow _ -> cont 93;
	L.RangedToken T.FatArrow _ -> cont 94;
	L.RangedToken T.PipeArrow _ -> cont 95;
	L.RangedToken T.Equals _ -> cont 96;
	L.RangedToken T.Pipe _ -> cont 97;
	L.RangedToken T.Dot _ -> cont 98;
	L.RangedToken T.Section _ -> cont 99;
	L.RangedToken T.BackSlash _ -> cont 100;
	L.RangedToken T.Newline _ -> cont 101;
	L.RangedToken T.EOF _ -> cont 102;
	_ -> happyError' (tk, [])
	})

happyError_ explist 103 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

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
