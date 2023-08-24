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

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1304) ([0,0,0,0,6148,0,0,0,0,49152,3,144,2688,320,0,0,61440,0,0,544,64,0,0,1024,0,0,8,0,0,0,0,1024,24,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,16384,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,64,0,32768,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,8704,1024,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,511,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,576,10752,1280,0,0,49152,3,144,2688,320,0,0,61440,0,36,672,80,0,0,15360,0,9,168,20,0,0,256,0,0,0,0,0,0,960,0,32768,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6148,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,4,0,0,0,0,0,16384,0,0,0,0,65504,1,0,258,0,0,0,32760,0,33024,64,0,0,0,8190,0,0,24,0,0,32768,2047,2,0,4,0,0,3840,16384,2,42,5,0,0,960,36864,32768,16394,1,0,0,240,9216,40960,20482,0,0,0,60,2304,43008,5120,0,0,0,15,576,10752,1280,0,0,49152,3,144,2688,320,0,0,61440,0,36,672,80,0,0,15360,0,9,168,20,0,0,3840,16384,2,42,5,0,0,960,36864,32768,16394,1,0,0,240,9216,40960,20482,0,0,0,60,2304,43008,5120,0,0,0,1,0,0,0,0,0,49152,7,0,2688,0,0,0,4096,0,0,0,1,0,0,0,0,0,512,0,0,0,0,0,0,64,0,0,0,0,0,0,385,0,0,0,240,0,8192,16386,0,0,0,124,0,34816,0,0,0,0,15,0,8704,1024,0,0,0,0,0,256,1,0,0,4096,0,0,32,0,0,0,0,0,0,0,8,0,0,0,0,0,0,2,0,0,0,0,0,32,0,0,0,0,0,0,8192,0,0,0,60,0,34944,4096,0,0,0,0,0,0,64,0,0,16384,0,0,128,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,3840,0,0,34,4,0,0,0,0,0,0,0,0,0,240,0,8192,16386,0,0,0,0,0,0,0,0,0,0,15,0,8704,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,1,0,0,0,0,0,30720,0,0,0,0,0,0,7680,0,0,0,0,0,0,1920,0,0,0,0,0,0,480,0,0,0,0,0,0,120,0,0,0,0,0,0,30,0,0,0,0,0,32768,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,0,0,0,0,0,0,1536,0,0,0,0,0,0,15,576,10752,1280,0,0,0,0,0,0,32,0,0,61440,0,0,544,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,960,36864,32768,16394,1,0,0,240,9216,40960,20482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,3,144,2688,320,0,0,61440,0,36,672,80,0,0,32768,2047,0,0,4,0,0,57344,511,0,512,1,0,0,0,0,0,0,0,0,0,65024,31,0,4128,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,16384,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,64,0,0,0,0,0,0,240,0,8192,2,0,0,0,65408,1031,0,1024,0,0,0,0,0,0,4,0,0,0,0,0,32768,1,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,3840,0,0,34,4,0,0,0,0,0,256,0,0,0,16,0,0,0,0,0,0,0,64,0,16,0,0,0,1,0,0,0,0,0,0,0,0,0,16,0,0,61440,0,36,672,80,0,0,0,0,0,512,0,0,0,256,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,64,0,512,0,0,0,0,16,0,4,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,15360,0,9,168,20,0,0,0,0,0,1024,0,0,0,0,0,0,128,0,0,0,0,0,0,4,0,0,0,0,0,0,8,0,0,0,15,576,10752,1280,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,65024,31,0,4096,0,0,0,0,0,4096,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,15360,0,9,168,20,0,0,57344,511,0,0,1,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,4,0,0,0,0,0,0,1,0,0,0,0,0,49152,3,0,2176,256,0,0,0,0,0,4,14,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,960,0,32768,8,1,0,0,65024,287,0,4096,0,0,0,4,0,0,0,0,0,0,0,0,0,4,0,0,16384,0,0,0,0,0,0,61440,0,0,544,64,0,0,15360,0,0,136,16,0,0,3840,0,0,34,4,0,0,960,0,32768,8,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,16384,0,0,0,0,8190,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,4,0,0,0,0,0,0,1,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,4,0,0,0,0,0,0,15,576,10752,1280,0,0,0,32760,0,0,64,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","listElements","list","tupleElems","tuple","params","args","fnApplication","controlFlow","patTupleElems","patRecordKeys","patData","pattern","term","atom","cases","matchExpr","binding","bindings","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","tagged","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 110
        bit_end = (st Prelude.+ 1) Prelude.* 110
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..109]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (67) = happyShift action_8
action_0 (76) = happyShift action_9
action_0 (77) = happyShift action_10
action_0 (46) = happyGoto action_47
action_0 (47) = happyGoto action_48
action_0 (48) = happyGoto action_49
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (49) = happyShift action_6
action_1 (50) = happyShift action_37
action_1 (51) = happyShift action_38
action_1 (52) = happyShift action_39
action_1 (71) = happyShift action_40
action_1 (74) = happyShift action_41
action_1 (90) = happyShift action_42
action_1 (92) = happyShift action_43
action_1 (94) = happyShift action_44
action_1 (105) = happyShift action_45
action_1 (107) = happyShift action_46
action_1 (8) = happyGoto action_27
action_1 (10) = happyGoto action_28
action_1 (12) = happyGoto action_29
action_1 (14) = happyGoto action_30
action_1 (17) = happyGoto action_31
action_1 (18) = happyGoto action_32
action_1 (23) = happyGoto action_33
action_1 (24) = happyGoto action_34
action_1 (26) = happyGoto action_35
action_1 (29) = happyGoto action_36
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (49) = happyShift action_6
action_2 (50) = happyShift action_21
action_2 (51) = happyShift action_22
action_2 (52) = happyShift action_23
action_2 (90) = happyShift action_24
action_2 (94) = happyShift action_25
action_2 (107) = happyShift action_26
action_2 (8) = happyGoto action_14
action_2 (31) = happyGoto action_15
action_2 (33) = happyGoto action_16
action_2 (34) = happyGoto action_17
action_2 (35) = happyGoto action_18
action_2 (39) = happyGoto action_19
action_2 (40) = happyGoto action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (49) = happyShift action_6
action_3 (90) = happyShift action_13
action_3 (8) = happyGoto action_11
action_3 (42) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (67) = happyShift action_8
action_4 (76) = happyShift action_9
action_4 (77) = happyShift action_10
action_4 (46) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (49) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (110) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (49) = happyShift action_6
action_8 (8) = happyGoto action_85
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (49) = happyShift action_6
action_9 (8) = happyGoto action_84
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (49) = happyShift action_6
action_10 (8) = happyGoto action_83
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_104

action_12 (99) = happyShift action_82
action_12 (110) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (49) = happyShift action_6
action_13 (90) = happyShift action_13
action_13 (8) = happyGoto action_11
action_13 (42) = happyGoto action_81
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (49) = happyReduce_82
action_14 (50) = happyReduce_82
action_14 (51) = happyReduce_82
action_14 (52) = happyReduce_82
action_14 (53) = happyReduce_82
action_14 (67) = happyReduce_82
action_14 (69) = happyReduce_82
action_14 (76) = happyReduce_82
action_14 (77) = happyReduce_82
action_14 (90) = happyReduce_82
action_14 (91) = happyReduce_82
action_14 (94) = happyReduce_82
action_14 (95) = happyReduce_82
action_14 (96) = happyShift action_80
action_14 (97) = happyReduce_82
action_14 (98) = happyReduce_82
action_14 (99) = happyReduce_82
action_14 (103) = happyReduce_82
action_14 (104) = happyReduce_82
action_14 (106) = happyReduce_82
action_14 (110) = happyReduce_82
action_14 _ = happyReduce_82

action_15 _ = happyReduce_80

action_16 _ = happyReduce_79

action_17 _ = happyReduce_81

action_18 (67) = happyReduce_93
action_18 (69) = happyReduce_93
action_18 (76) = happyReduce_93
action_18 (77) = happyReduce_93
action_18 (91) = happyReduce_93
action_18 (95) = happyReduce_93
action_18 (97) = happyReduce_93
action_18 (98) = happyReduce_93
action_18 (99) = happyReduce_93
action_18 (103) = happyReduce_93
action_18 (104) = happyReduce_93
action_18 (106) = happyReduce_93
action_18 (110) = happyReduce_93
action_18 (36) = happyGoto action_79
action_18 _ = happyReduce_84

action_19 _ = happyReduce_94

action_20 (99) = happyShift action_78
action_20 (110) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_76

action_22 _ = happyReduce_78

action_23 _ = happyReduce_77

action_24 (49) = happyShift action_6
action_24 (50) = happyShift action_21
action_24 (51) = happyShift action_22
action_24 (52) = happyShift action_23
action_24 (90) = happyShift action_24
action_24 (94) = happyShift action_25
action_24 (107) = happyShift action_26
action_24 (8) = happyGoto action_14
action_24 (31) = happyGoto action_15
action_24 (33) = happyGoto action_16
action_24 (34) = happyGoto action_17
action_24 (35) = happyGoto action_18
action_24 (39) = happyGoto action_19
action_24 (40) = happyGoto action_77
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (49) = happyShift action_6
action_25 (8) = happyGoto action_75
action_25 (30) = happyGoto action_76
action_25 _ = happyReduce_69

action_26 (15) = happyGoto action_74
action_26 _ = happyReduce_17

action_27 _ = happyReduce_38

action_28 _ = happyReduce_42

action_29 _ = happyReduce_41

action_30 _ = happyReduce_40

action_31 _ = happyReduce_52

action_32 _ = happyReduce_50

action_33 _ = happyReduce_39

action_34 (54) = happyReduce_54
action_34 (55) = happyReduce_54
action_34 (56) = happyReduce_54
action_34 (57) = happyReduce_54
action_34 (58) = happyReduce_54
action_34 (59) = happyReduce_54
action_34 (60) = happyReduce_54
action_34 (61) = happyReduce_54
action_34 (62) = happyReduce_54
action_34 (63) = happyReduce_54
action_34 (64) = happyReduce_54
action_34 (65) = happyReduce_54
action_34 (67) = happyReduce_54
action_34 (69) = happyReduce_54
action_34 (72) = happyReduce_54
action_34 (73) = happyReduce_54
action_34 (76) = happyReduce_54
action_34 (77) = happyReduce_54
action_34 (91) = happyReduce_54
action_34 (93) = happyReduce_54
action_34 (95) = happyReduce_54
action_34 (98) = happyReduce_54
action_34 (104) = happyReduce_54
action_34 (105) = happyReduce_54
action_34 (110) = happyReduce_54
action_34 (16) = happyGoto action_73
action_34 _ = happyReduce_19

action_35 _ = happyReduce_51

action_36 (54) = happyShift action_60
action_36 (55) = happyShift action_61
action_36 (56) = happyShift action_62
action_36 (57) = happyShift action_63
action_36 (58) = happyShift action_64
action_36 (59) = happyShift action_65
action_36 (60) = happyShift action_66
action_36 (61) = happyShift action_67
action_36 (62) = happyShift action_68
action_36 (63) = happyShift action_69
action_36 (64) = happyShift action_70
action_36 (65) = happyShift action_71
action_36 (105) = happyShift action_72
action_36 (110) = happyAccept
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_35

action_38 _ = happyReduce_37

action_39 _ = happyReduce_36

action_40 (49) = happyShift action_6
action_40 (50) = happyShift action_37
action_40 (51) = happyShift action_38
action_40 (52) = happyShift action_39
action_40 (71) = happyShift action_40
action_40 (74) = happyShift action_41
action_40 (90) = happyShift action_42
action_40 (92) = happyShift action_43
action_40 (94) = happyShift action_44
action_40 (105) = happyShift action_45
action_40 (107) = happyShift action_46
action_40 (8) = happyGoto action_27
action_40 (10) = happyGoto action_28
action_40 (12) = happyGoto action_29
action_40 (14) = happyGoto action_30
action_40 (17) = happyGoto action_31
action_40 (18) = happyGoto action_32
action_40 (23) = happyGoto action_33
action_40 (24) = happyGoto action_34
action_40 (26) = happyGoto action_35
action_40 (29) = happyGoto action_59
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (49) = happyShift action_6
action_41 (50) = happyShift action_37
action_41 (51) = happyShift action_38
action_41 (52) = happyShift action_39
action_41 (71) = happyShift action_40
action_41 (74) = happyShift action_41
action_41 (90) = happyShift action_42
action_41 (92) = happyShift action_43
action_41 (94) = happyShift action_44
action_41 (105) = happyShift action_45
action_41 (107) = happyShift action_46
action_41 (8) = happyGoto action_27
action_41 (10) = happyGoto action_28
action_41 (12) = happyGoto action_29
action_41 (14) = happyGoto action_30
action_41 (17) = happyGoto action_31
action_41 (18) = happyGoto action_32
action_41 (23) = happyGoto action_33
action_41 (24) = happyGoto action_34
action_41 (26) = happyGoto action_35
action_41 (29) = happyGoto action_58
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (49) = happyShift action_6
action_42 (50) = happyShift action_37
action_42 (51) = happyShift action_38
action_42 (52) = happyShift action_39
action_42 (71) = happyShift action_40
action_42 (74) = happyShift action_41
action_42 (90) = happyShift action_42
action_42 (92) = happyShift action_43
action_42 (94) = happyShift action_44
action_42 (105) = happyShift action_45
action_42 (107) = happyShift action_46
action_42 (8) = happyGoto action_27
action_42 (10) = happyGoto action_28
action_42 (12) = happyGoto action_29
action_42 (14) = happyGoto action_30
action_42 (17) = happyGoto action_31
action_42 (18) = happyGoto action_32
action_42 (23) = happyGoto action_33
action_42 (24) = happyGoto action_34
action_42 (26) = happyGoto action_35
action_42 (29) = happyGoto action_57
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (49) = happyShift action_6
action_43 (50) = happyShift action_37
action_43 (51) = happyShift action_38
action_43 (52) = happyShift action_39
action_43 (71) = happyShift action_40
action_43 (74) = happyShift action_41
action_43 (90) = happyShift action_42
action_43 (92) = happyShift action_43
action_43 (94) = happyShift action_44
action_43 (105) = happyShift action_45
action_43 (107) = happyShift action_46
action_43 (8) = happyGoto action_27
action_43 (10) = happyGoto action_28
action_43 (11) = happyGoto action_55
action_43 (12) = happyGoto action_29
action_43 (14) = happyGoto action_30
action_43 (17) = happyGoto action_31
action_43 (18) = happyGoto action_32
action_43 (23) = happyGoto action_33
action_43 (24) = happyGoto action_34
action_43 (26) = happyGoto action_35
action_43 (29) = happyGoto action_56
action_43 _ = happyReduce_10

action_44 (49) = happyShift action_6
action_44 (8) = happyGoto action_53
action_44 (9) = happyGoto action_54
action_44 _ = happyReduce_6

action_45 (49) = happyShift action_6
action_45 (50) = happyShift action_37
action_45 (51) = happyShift action_38
action_45 (52) = happyShift action_39
action_45 (90) = happyShift action_42
action_45 (92) = happyShift action_43
action_45 (94) = happyShift action_44
action_45 (8) = happyGoto action_27
action_45 (10) = happyGoto action_28
action_45 (12) = happyGoto action_29
action_45 (14) = happyGoto action_30
action_45 (23) = happyGoto action_33
action_45 (24) = happyGoto action_52
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (15) = happyGoto action_51
action_46 _ = happyReduce_17

action_47 _ = happyReduce_116

action_48 (67) = happyShift action_8
action_48 (76) = happyShift action_9
action_48 (77) = happyShift action_10
action_48 (46) = happyGoto action_50
action_48 _ = happyReduce_118

action_49 (110) = happyAccept
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_117

action_51 (49) = happyShift action_6
action_51 (99) = happyShift action_130
action_51 (8) = happyGoto action_103
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_55

action_53 (96) = happyShift action_129
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (95) = happyShift action_128
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (93) = happyShift action_127
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (54) = happyShift action_60
action_56 (55) = happyShift action_61
action_56 (56) = happyShift action_62
action_56 (57) = happyShift action_63
action_56 (58) = happyShift action_64
action_56 (59) = happyShift action_65
action_56 (60) = happyShift action_66
action_56 (61) = happyShift action_67
action_56 (62) = happyShift action_68
action_56 (63) = happyShift action_69
action_56 (64) = happyShift action_70
action_56 (65) = happyShift action_71
action_56 (98) = happyShift action_126
action_56 (105) = happyShift action_72
action_56 _ = happyReduce_11

action_57 (54) = happyShift action_60
action_57 (55) = happyShift action_61
action_57 (56) = happyShift action_62
action_57 (57) = happyShift action_63
action_57 (58) = happyShift action_64
action_57 (59) = happyShift action_65
action_57 (60) = happyShift action_66
action_57 (61) = happyShift action_67
action_57 (62) = happyShift action_68
action_57 (63) = happyShift action_69
action_57 (64) = happyShift action_70
action_57 (65) = happyShift action_71
action_57 (91) = happyShift action_124
action_57 (98) = happyShift action_125
action_57 (105) = happyShift action_72
action_57 (13) = happyGoto action_123
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (54) = happyShift action_60
action_58 (55) = happyShift action_61
action_58 (56) = happyShift action_62
action_58 (57) = happyShift action_63
action_58 (58) = happyShift action_64
action_58 (59) = happyShift action_65
action_58 (60) = happyShift action_66
action_58 (61) = happyShift action_67
action_58 (62) = happyShift action_68
action_58 (63) = happyShift action_69
action_58 (64) = happyShift action_70
action_58 (65) = happyShift action_71
action_58 (104) = happyShift action_122
action_58 (105) = happyShift action_72
action_58 (25) = happyGoto action_121
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (54) = happyShift action_60
action_59 (55) = happyShift action_61
action_59 (56) = happyShift action_62
action_59 (57) = happyShift action_63
action_59 (58) = happyShift action_64
action_59 (59) = happyShift action_65
action_59 (60) = happyShift action_66
action_59 (61) = happyShift action_67
action_59 (62) = happyShift action_68
action_59 (63) = happyShift action_69
action_59 (64) = happyShift action_70
action_59 (65) = happyShift action_71
action_59 (72) = happyShift action_120
action_59 (105) = happyShift action_72
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (49) = happyShift action_6
action_60 (50) = happyShift action_37
action_60 (51) = happyShift action_38
action_60 (52) = happyShift action_39
action_60 (71) = happyShift action_40
action_60 (74) = happyShift action_41
action_60 (90) = happyShift action_42
action_60 (92) = happyShift action_43
action_60 (94) = happyShift action_44
action_60 (105) = happyShift action_45
action_60 (107) = happyShift action_46
action_60 (8) = happyGoto action_27
action_60 (10) = happyGoto action_28
action_60 (12) = happyGoto action_29
action_60 (14) = happyGoto action_30
action_60 (17) = happyGoto action_31
action_60 (18) = happyGoto action_32
action_60 (23) = happyGoto action_33
action_60 (24) = happyGoto action_34
action_60 (26) = happyGoto action_35
action_60 (29) = happyGoto action_119
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (49) = happyShift action_6
action_61 (50) = happyShift action_37
action_61 (51) = happyShift action_38
action_61 (52) = happyShift action_39
action_61 (71) = happyShift action_40
action_61 (74) = happyShift action_41
action_61 (90) = happyShift action_42
action_61 (92) = happyShift action_43
action_61 (94) = happyShift action_44
action_61 (105) = happyShift action_45
action_61 (107) = happyShift action_46
action_61 (8) = happyGoto action_27
action_61 (10) = happyGoto action_28
action_61 (12) = happyGoto action_29
action_61 (14) = happyGoto action_30
action_61 (17) = happyGoto action_31
action_61 (18) = happyGoto action_32
action_61 (23) = happyGoto action_33
action_61 (24) = happyGoto action_34
action_61 (26) = happyGoto action_35
action_61 (29) = happyGoto action_118
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (49) = happyShift action_6
action_62 (50) = happyShift action_37
action_62 (51) = happyShift action_38
action_62 (52) = happyShift action_39
action_62 (71) = happyShift action_40
action_62 (74) = happyShift action_41
action_62 (90) = happyShift action_42
action_62 (92) = happyShift action_43
action_62 (94) = happyShift action_44
action_62 (105) = happyShift action_45
action_62 (107) = happyShift action_46
action_62 (8) = happyGoto action_27
action_62 (10) = happyGoto action_28
action_62 (12) = happyGoto action_29
action_62 (14) = happyGoto action_30
action_62 (17) = happyGoto action_31
action_62 (18) = happyGoto action_32
action_62 (23) = happyGoto action_33
action_62 (24) = happyGoto action_34
action_62 (26) = happyGoto action_35
action_62 (29) = happyGoto action_117
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (49) = happyShift action_6
action_63 (50) = happyShift action_37
action_63 (51) = happyShift action_38
action_63 (52) = happyShift action_39
action_63 (71) = happyShift action_40
action_63 (74) = happyShift action_41
action_63 (90) = happyShift action_42
action_63 (92) = happyShift action_43
action_63 (94) = happyShift action_44
action_63 (105) = happyShift action_45
action_63 (107) = happyShift action_46
action_63 (8) = happyGoto action_27
action_63 (10) = happyGoto action_28
action_63 (12) = happyGoto action_29
action_63 (14) = happyGoto action_30
action_63 (17) = happyGoto action_31
action_63 (18) = happyGoto action_32
action_63 (23) = happyGoto action_33
action_63 (24) = happyGoto action_34
action_63 (26) = happyGoto action_35
action_63 (29) = happyGoto action_116
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (49) = happyShift action_6
action_64 (50) = happyShift action_37
action_64 (51) = happyShift action_38
action_64 (52) = happyShift action_39
action_64 (71) = happyShift action_40
action_64 (74) = happyShift action_41
action_64 (90) = happyShift action_42
action_64 (92) = happyShift action_43
action_64 (94) = happyShift action_44
action_64 (105) = happyShift action_45
action_64 (107) = happyShift action_46
action_64 (8) = happyGoto action_27
action_64 (10) = happyGoto action_28
action_64 (12) = happyGoto action_29
action_64 (14) = happyGoto action_30
action_64 (17) = happyGoto action_31
action_64 (18) = happyGoto action_32
action_64 (23) = happyGoto action_33
action_64 (24) = happyGoto action_34
action_64 (26) = happyGoto action_35
action_64 (29) = happyGoto action_115
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (49) = happyShift action_6
action_65 (50) = happyShift action_37
action_65 (51) = happyShift action_38
action_65 (52) = happyShift action_39
action_65 (71) = happyShift action_40
action_65 (74) = happyShift action_41
action_65 (90) = happyShift action_42
action_65 (92) = happyShift action_43
action_65 (94) = happyShift action_44
action_65 (105) = happyShift action_45
action_65 (107) = happyShift action_46
action_65 (8) = happyGoto action_27
action_65 (10) = happyGoto action_28
action_65 (12) = happyGoto action_29
action_65 (14) = happyGoto action_30
action_65 (17) = happyGoto action_31
action_65 (18) = happyGoto action_32
action_65 (23) = happyGoto action_33
action_65 (24) = happyGoto action_34
action_65 (26) = happyGoto action_35
action_65 (29) = happyGoto action_114
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (49) = happyShift action_6
action_66 (50) = happyShift action_37
action_66 (51) = happyShift action_38
action_66 (52) = happyShift action_39
action_66 (71) = happyShift action_40
action_66 (74) = happyShift action_41
action_66 (90) = happyShift action_42
action_66 (92) = happyShift action_43
action_66 (94) = happyShift action_44
action_66 (105) = happyShift action_45
action_66 (107) = happyShift action_46
action_66 (8) = happyGoto action_27
action_66 (10) = happyGoto action_28
action_66 (12) = happyGoto action_29
action_66 (14) = happyGoto action_30
action_66 (17) = happyGoto action_31
action_66 (18) = happyGoto action_32
action_66 (23) = happyGoto action_33
action_66 (24) = happyGoto action_34
action_66 (26) = happyGoto action_35
action_66 (29) = happyGoto action_113
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (49) = happyShift action_6
action_67 (50) = happyShift action_37
action_67 (51) = happyShift action_38
action_67 (52) = happyShift action_39
action_67 (71) = happyShift action_40
action_67 (74) = happyShift action_41
action_67 (90) = happyShift action_42
action_67 (92) = happyShift action_43
action_67 (94) = happyShift action_44
action_67 (105) = happyShift action_45
action_67 (107) = happyShift action_46
action_67 (8) = happyGoto action_27
action_67 (10) = happyGoto action_28
action_67 (12) = happyGoto action_29
action_67 (14) = happyGoto action_30
action_67 (17) = happyGoto action_31
action_67 (18) = happyGoto action_32
action_67 (23) = happyGoto action_33
action_67 (24) = happyGoto action_34
action_67 (26) = happyGoto action_35
action_67 (29) = happyGoto action_112
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (49) = happyShift action_6
action_68 (50) = happyShift action_37
action_68 (51) = happyShift action_38
action_68 (52) = happyShift action_39
action_68 (71) = happyShift action_40
action_68 (74) = happyShift action_41
action_68 (90) = happyShift action_42
action_68 (92) = happyShift action_43
action_68 (94) = happyShift action_44
action_68 (105) = happyShift action_45
action_68 (107) = happyShift action_46
action_68 (8) = happyGoto action_27
action_68 (10) = happyGoto action_28
action_68 (12) = happyGoto action_29
action_68 (14) = happyGoto action_30
action_68 (17) = happyGoto action_31
action_68 (18) = happyGoto action_32
action_68 (23) = happyGoto action_33
action_68 (24) = happyGoto action_34
action_68 (26) = happyGoto action_35
action_68 (29) = happyGoto action_111
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (49) = happyShift action_6
action_69 (50) = happyShift action_37
action_69 (51) = happyShift action_38
action_69 (52) = happyShift action_39
action_69 (71) = happyShift action_40
action_69 (74) = happyShift action_41
action_69 (90) = happyShift action_42
action_69 (92) = happyShift action_43
action_69 (94) = happyShift action_44
action_69 (105) = happyShift action_45
action_69 (107) = happyShift action_46
action_69 (8) = happyGoto action_27
action_69 (10) = happyGoto action_28
action_69 (12) = happyGoto action_29
action_69 (14) = happyGoto action_30
action_69 (17) = happyGoto action_31
action_69 (18) = happyGoto action_32
action_69 (23) = happyGoto action_33
action_69 (24) = happyGoto action_34
action_69 (26) = happyGoto action_35
action_69 (29) = happyGoto action_110
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (49) = happyShift action_6
action_70 (50) = happyShift action_37
action_70 (51) = happyShift action_38
action_70 (52) = happyShift action_39
action_70 (71) = happyShift action_40
action_70 (74) = happyShift action_41
action_70 (90) = happyShift action_42
action_70 (92) = happyShift action_43
action_70 (94) = happyShift action_44
action_70 (105) = happyShift action_45
action_70 (107) = happyShift action_46
action_70 (8) = happyGoto action_27
action_70 (10) = happyGoto action_28
action_70 (12) = happyGoto action_29
action_70 (14) = happyGoto action_30
action_70 (17) = happyGoto action_31
action_70 (18) = happyGoto action_32
action_70 (23) = happyGoto action_33
action_70 (24) = happyGoto action_34
action_70 (26) = happyGoto action_35
action_70 (29) = happyGoto action_109
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (49) = happyShift action_6
action_71 (50) = happyShift action_37
action_71 (51) = happyShift action_38
action_71 (52) = happyShift action_39
action_71 (71) = happyShift action_40
action_71 (74) = happyShift action_41
action_71 (90) = happyShift action_42
action_71 (92) = happyShift action_43
action_71 (94) = happyShift action_44
action_71 (105) = happyShift action_45
action_71 (107) = happyShift action_46
action_71 (8) = happyGoto action_27
action_71 (10) = happyGoto action_28
action_71 (12) = happyGoto action_29
action_71 (14) = happyGoto action_30
action_71 (17) = happyGoto action_31
action_71 (18) = happyGoto action_32
action_71 (23) = happyGoto action_33
action_71 (24) = happyGoto action_34
action_71 (26) = happyGoto action_35
action_71 (29) = happyGoto action_108
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (49) = happyShift action_6
action_72 (8) = happyGoto action_107
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (49) = happyShift action_6
action_73 (50) = happyShift action_37
action_73 (51) = happyShift action_38
action_73 (52) = happyShift action_39
action_73 (53) = happyShift action_106
action_73 (90) = happyShift action_42
action_73 (92) = happyShift action_43
action_73 (94) = happyShift action_44
action_73 (8) = happyGoto action_27
action_73 (10) = happyGoto action_28
action_73 (12) = happyGoto action_29
action_73 (14) = happyGoto action_30
action_73 (23) = happyGoto action_33
action_73 (24) = happyGoto action_105
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (49) = happyShift action_6
action_74 (101) = happyShift action_104
action_74 (8) = happyGoto action_103
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (96) = happyShift action_102
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (95) = happyShift action_101
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (91) = happyShift action_99
action_77 (98) = happyShift action_100
action_77 (99) = happyShift action_78
action_77 (32) = happyGoto action_98
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (49) = happyShift action_6
action_78 (50) = happyShift action_21
action_78 (51) = happyShift action_22
action_78 (52) = happyShift action_23
action_78 (90) = happyShift action_24
action_78 (94) = happyShift action_25
action_78 (107) = happyShift action_26
action_78 (8) = happyGoto action_14
action_78 (31) = happyGoto action_15
action_78 (33) = happyGoto action_16
action_78 (34) = happyGoto action_17
action_78 (35) = happyGoto action_18
action_78 (39) = happyGoto action_19
action_78 (40) = happyGoto action_97
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (49) = happyShift action_6
action_79 (50) = happyShift action_21
action_79 (51) = happyShift action_22
action_79 (52) = happyShift action_23
action_79 (53) = happyShift action_96
action_79 (90) = happyShift action_24
action_79 (94) = happyShift action_25
action_79 (8) = happyGoto action_94
action_79 (31) = happyGoto action_15
action_79 (33) = happyGoto action_16
action_79 (34) = happyGoto action_17
action_79 (35) = happyGoto action_95
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (49) = happyShift action_6
action_80 (50) = happyShift action_21
action_80 (51) = happyShift action_22
action_80 (52) = happyShift action_23
action_80 (90) = happyShift action_24
action_80 (94) = happyShift action_25
action_80 (107) = happyShift action_26
action_80 (8) = happyGoto action_14
action_80 (31) = happyGoto action_15
action_80 (33) = happyGoto action_16
action_80 (34) = happyGoto action_17
action_80 (35) = happyGoto action_18
action_80 (39) = happyGoto action_19
action_80 (40) = happyGoto action_93
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (91) = happyShift action_92
action_81 (99) = happyShift action_82
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (49) = happyShift action_6
action_82 (90) = happyShift action_13
action_82 (8) = happyGoto action_11
action_82 (42) = happyGoto action_91
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (106) = happyShift action_89
action_83 (43) = happyGoto action_90
action_83 _ = happyReduce_105

action_84 (106) = happyShift action_89
action_84 (43) = happyGoto action_88
action_84 _ = happyReduce_105

action_85 (96) = happyShift action_87
action_85 (41) = happyGoto action_86
action_85 _ = happyReduce_98

action_86 (106) = happyShift action_89
action_86 (43) = happyGoto action_153
action_86 _ = happyReduce_105

action_87 (49) = happyShift action_6
action_87 (50) = happyShift action_21
action_87 (51) = happyShift action_22
action_87 (52) = happyShift action_23
action_87 (86) = happyShift action_152
action_87 (90) = happyShift action_24
action_87 (94) = happyShift action_25
action_87 (107) = happyShift action_26
action_87 (8) = happyGoto action_14
action_87 (31) = happyGoto action_15
action_87 (33) = happyGoto action_16
action_87 (34) = happyGoto action_17
action_87 (35) = happyGoto action_18
action_87 (39) = happyGoto action_19
action_87 (40) = happyGoto action_151
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (103) = happyShift action_150
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (49) = happyShift action_6
action_89 (90) = happyShift action_13
action_89 (8) = happyGoto action_11
action_89 (42) = happyGoto action_149
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (103) = happyShift action_148
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_102

action_92 _ = happyReduce_103

action_93 (67) = happyReduce_92
action_93 (69) = happyReduce_92
action_93 (76) = happyReduce_92
action_93 (77) = happyReduce_92
action_93 (91) = happyReduce_92
action_93 (95) = happyReduce_92
action_93 (97) = happyReduce_92
action_93 (98) = happyReduce_92
action_93 (99) = happyShift action_78
action_93 (103) = happyReduce_92
action_93 (104) = happyReduce_92
action_93 (106) = happyReduce_92
action_93 (110) = happyReduce_92
action_93 _ = happyReduce_92

action_94 (49) = happyReduce_82
action_94 (50) = happyReduce_82
action_94 (51) = happyReduce_82
action_94 (52) = happyReduce_82
action_94 (53) = happyReduce_82
action_94 (90) = happyReduce_82
action_94 (94) = happyReduce_82
action_94 _ = happyReduce_82

action_95 _ = happyReduce_85

action_96 _ = happyReduce_97

action_97 (67) = happyReduce_95
action_97 (69) = happyReduce_95
action_97 (76) = happyReduce_95
action_97 (77) = happyReduce_95
action_97 (91) = happyReduce_95
action_97 (95) = happyReduce_95
action_97 (97) = happyReduce_95
action_97 (98) = happyReduce_95
action_97 (99) = happyShift action_78
action_97 (103) = happyReduce_95
action_97 (104) = happyReduce_95
action_97 (106) = happyReduce_95
action_97 (110) = happyReduce_95
action_97 _ = happyReduce_95

action_98 (91) = happyShift action_147
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_83

action_100 (49) = happyShift action_6
action_100 (50) = happyShift action_21
action_100 (51) = happyShift action_22
action_100 (52) = happyShift action_23
action_100 (90) = happyShift action_24
action_100 (94) = happyShift action_25
action_100 (107) = happyShift action_26
action_100 (8) = happyGoto action_14
action_100 (31) = happyGoto action_15
action_100 (33) = happyGoto action_16
action_100 (34) = happyGoto action_17
action_100 (35) = happyGoto action_18
action_100 (39) = happyGoto action_19
action_100 (40) = happyGoto action_146
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_72

action_102 (49) = happyShift action_6
action_102 (50) = happyShift action_21
action_102 (51) = happyShift action_22
action_102 (52) = happyShift action_23
action_102 (90) = happyShift action_24
action_102 (94) = happyShift action_25
action_102 (107) = happyShift action_26
action_102 (8) = happyGoto action_14
action_102 (31) = happyGoto action_15
action_102 (33) = happyGoto action_16
action_102 (34) = happyGoto action_17
action_102 (35) = happyGoto action_18
action_102 (39) = happyGoto action_19
action_102 (40) = happyGoto action_145
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_18

action_104 (49) = happyShift action_6
action_104 (50) = happyShift action_21
action_104 (51) = happyShift action_22
action_104 (52) = happyShift action_23
action_104 (90) = happyShift action_24
action_104 (94) = happyShift action_25
action_104 (107) = happyShift action_26
action_104 (8) = happyGoto action_14
action_104 (31) = happyGoto action_15
action_104 (33) = happyGoto action_16
action_104 (34) = happyGoto action_17
action_104 (35) = happyGoto action_18
action_104 (39) = happyGoto action_19
action_104 (40) = happyGoto action_144
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_20

action_106 _ = happyReduce_21

action_107 _ = happyReduce_56

action_108 (54) = happyShift action_60
action_108 (55) = happyShift action_61
action_108 (56) = happyShift action_62
action_108 (57) = happyShift action_63
action_108 (58) = happyFail []
action_108 (59) = happyFail []
action_108 (60) = happyFail []
action_108 (61) = happyFail []
action_108 (62) = happyFail []
action_108 (63) = happyFail []
action_108 (64) = happyFail []
action_108 (65) = happyFail []
action_108 _ = happyReduce_62

action_109 (54) = happyShift action_60
action_109 (55) = happyShift action_61
action_109 (56) = happyShift action_62
action_109 (57) = happyShift action_63
action_109 (58) = happyFail []
action_109 (59) = happyFail []
action_109 (60) = happyFail []
action_109 (61) = happyFail []
action_109 (62) = happyFail []
action_109 (63) = happyFail []
action_109 (64) = happyFail []
action_109 (65) = happyFail []
action_109 _ = happyReduce_61

action_110 (54) = happyShift action_60
action_110 (55) = happyShift action_61
action_110 (56) = happyShift action_62
action_110 (57) = happyShift action_63
action_110 (58) = happyFail []
action_110 (59) = happyFail []
action_110 (60) = happyFail []
action_110 (61) = happyFail []
action_110 (62) = happyFail []
action_110 (63) = happyFail []
action_110 (64) = happyFail []
action_110 (65) = happyFail []
action_110 _ = happyReduce_68

action_111 (54) = happyShift action_60
action_111 (55) = happyShift action_61
action_111 (56) = happyShift action_62
action_111 (57) = happyShift action_63
action_111 (58) = happyFail []
action_111 (59) = happyFail []
action_111 (60) = happyFail []
action_111 (61) = happyFail []
action_111 (62) = happyFail []
action_111 (63) = happyFail []
action_111 (64) = happyFail []
action_111 (65) = happyFail []
action_111 _ = happyReduce_66

action_112 (54) = happyShift action_60
action_112 (55) = happyShift action_61
action_112 (56) = happyShift action_62
action_112 (57) = happyShift action_63
action_112 (58) = happyFail []
action_112 (59) = happyFail []
action_112 (60) = happyFail []
action_112 (61) = happyFail []
action_112 (62) = happyFail []
action_112 (63) = happyFail []
action_112 (64) = happyFail []
action_112 (65) = happyFail []
action_112 _ = happyReduce_67

action_113 (54) = happyShift action_60
action_113 (55) = happyShift action_61
action_113 (56) = happyShift action_62
action_113 (57) = happyShift action_63
action_113 (58) = happyFail []
action_113 (59) = happyFail []
action_113 (60) = happyFail []
action_113 (61) = happyFail []
action_113 (62) = happyFail []
action_113 (63) = happyFail []
action_113 (64) = happyFail []
action_113 (65) = happyFail []
action_113 _ = happyReduce_65

action_114 (54) = happyShift action_60
action_114 (55) = happyShift action_61
action_114 (56) = happyShift action_62
action_114 (57) = happyShift action_63
action_114 (58) = happyFail []
action_114 (59) = happyFail []
action_114 (60) = happyFail []
action_114 (61) = happyFail []
action_114 (62) = happyFail []
action_114 (63) = happyFail []
action_114 (64) = happyFail []
action_114 (65) = happyFail []
action_114 _ = happyReduce_64

action_115 (54) = happyShift action_60
action_115 (55) = happyShift action_61
action_115 (56) = happyShift action_62
action_115 (57) = happyShift action_63
action_115 (58) = happyFail []
action_115 (59) = happyFail []
action_115 (60) = happyFail []
action_115 (61) = happyFail []
action_115 (62) = happyFail []
action_115 (63) = happyFail []
action_115 (64) = happyFail []
action_115 (65) = happyFail []
action_115 _ = happyReduce_63

action_116 _ = happyReduce_60

action_117 _ = happyReduce_59

action_118 (56) = happyShift action_62
action_118 (57) = happyShift action_63
action_118 _ = happyReduce_58

action_119 (56) = happyShift action_62
action_119 (57) = happyShift action_63
action_119 _ = happyReduce_57

action_120 (49) = happyShift action_6
action_120 (50) = happyShift action_37
action_120 (51) = happyShift action_38
action_120 (52) = happyShift action_39
action_120 (71) = happyShift action_40
action_120 (74) = happyShift action_41
action_120 (90) = happyShift action_42
action_120 (92) = happyShift action_43
action_120 (94) = happyShift action_44
action_120 (105) = happyShift action_45
action_120 (107) = happyShift action_46
action_120 (8) = happyGoto action_27
action_120 (10) = happyGoto action_28
action_120 (12) = happyGoto action_29
action_120 (14) = happyGoto action_30
action_120 (17) = happyGoto action_31
action_120 (18) = happyGoto action_32
action_120 (23) = happyGoto action_33
action_120 (24) = happyGoto action_34
action_120 (26) = happyGoto action_35
action_120 (29) = happyGoto action_143
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (54) = happyReduce_46
action_121 (55) = happyReduce_46
action_121 (56) = happyReduce_46
action_121 (57) = happyReduce_46
action_121 (58) = happyReduce_46
action_121 (59) = happyReduce_46
action_121 (60) = happyReduce_46
action_121 (61) = happyReduce_46
action_121 (62) = happyReduce_46
action_121 (63) = happyReduce_46
action_121 (64) = happyReduce_46
action_121 (65) = happyReduce_46
action_121 (67) = happyReduce_46
action_121 (69) = happyReduce_46
action_121 (72) = happyReduce_46
action_121 (73) = happyReduce_46
action_121 (76) = happyReduce_46
action_121 (77) = happyReduce_46
action_121 (91) = happyReduce_46
action_121 (93) = happyReduce_46
action_121 (95) = happyReduce_46
action_121 (98) = happyReduce_46
action_121 (104) = happyShift action_142
action_121 (105) = happyReduce_46
action_121 (110) = happyReduce_46
action_121 _ = happyReduce_46

action_122 (49) = happyShift action_6
action_122 (50) = happyShift action_37
action_122 (51) = happyShift action_38
action_122 (52) = happyShift action_39
action_122 (90) = happyShift action_140
action_122 (94) = happyShift action_141
action_122 (8) = happyGoto action_136
action_122 (21) = happyGoto action_137
action_122 (22) = happyGoto action_138
action_122 (23) = happyGoto action_139
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (91) = happyShift action_135
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_43

action_125 (49) = happyShift action_6
action_125 (50) = happyShift action_37
action_125 (51) = happyShift action_38
action_125 (52) = happyShift action_39
action_125 (71) = happyShift action_40
action_125 (74) = happyShift action_41
action_125 (90) = happyShift action_42
action_125 (92) = happyShift action_43
action_125 (94) = happyShift action_44
action_125 (105) = happyShift action_45
action_125 (107) = happyShift action_46
action_125 (8) = happyGoto action_27
action_125 (10) = happyGoto action_28
action_125 (12) = happyGoto action_29
action_125 (14) = happyGoto action_30
action_125 (17) = happyGoto action_31
action_125 (18) = happyGoto action_32
action_125 (23) = happyGoto action_33
action_125 (24) = happyGoto action_34
action_125 (26) = happyGoto action_35
action_125 (29) = happyGoto action_134
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (49) = happyShift action_6
action_126 (50) = happyShift action_37
action_126 (51) = happyShift action_38
action_126 (52) = happyShift action_39
action_126 (71) = happyShift action_40
action_126 (74) = happyShift action_41
action_126 (90) = happyShift action_42
action_126 (92) = happyShift action_43
action_126 (94) = happyShift action_44
action_126 (105) = happyShift action_45
action_126 (107) = happyShift action_46
action_126 (8) = happyGoto action_27
action_126 (10) = happyGoto action_28
action_126 (11) = happyGoto action_133
action_126 (12) = happyGoto action_29
action_126 (14) = happyGoto action_30
action_126 (17) = happyGoto action_31
action_126 (18) = happyGoto action_32
action_126 (23) = happyGoto action_33
action_126 (24) = happyGoto action_34
action_126 (26) = happyGoto action_35
action_126 (29) = happyGoto action_56
action_126 _ = happyReduce_10

action_127 _ = happyReduce_13

action_128 _ = happyReduce_9

action_129 (49) = happyShift action_6
action_129 (50) = happyShift action_37
action_129 (51) = happyShift action_38
action_129 (52) = happyShift action_39
action_129 (71) = happyShift action_40
action_129 (74) = happyShift action_41
action_129 (90) = happyShift action_42
action_129 (92) = happyShift action_43
action_129 (94) = happyShift action_44
action_129 (105) = happyShift action_45
action_129 (107) = happyShift action_46
action_129 (8) = happyGoto action_27
action_129 (10) = happyGoto action_28
action_129 (12) = happyGoto action_29
action_129 (14) = happyGoto action_30
action_129 (17) = happyGoto action_31
action_129 (18) = happyGoto action_32
action_129 (23) = happyGoto action_33
action_129 (24) = happyGoto action_34
action_129 (26) = happyGoto action_35
action_129 (29) = happyGoto action_132
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (49) = happyShift action_6
action_130 (50) = happyShift action_37
action_130 (51) = happyShift action_38
action_130 (52) = happyShift action_39
action_130 (71) = happyShift action_40
action_130 (74) = happyShift action_41
action_130 (90) = happyShift action_42
action_130 (92) = happyShift action_43
action_130 (94) = happyShift action_44
action_130 (105) = happyShift action_45
action_130 (107) = happyShift action_46
action_130 (8) = happyGoto action_27
action_130 (10) = happyGoto action_28
action_130 (12) = happyGoto action_29
action_130 (14) = happyGoto action_30
action_130 (17) = happyGoto action_31
action_130 (18) = happyGoto action_32
action_130 (23) = happyGoto action_33
action_130 (24) = happyGoto action_34
action_130 (26) = happyGoto action_35
action_130 (29) = happyGoto action_131
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (54) = happyShift action_60
action_131 (55) = happyShift action_61
action_131 (56) = happyShift action_62
action_131 (57) = happyShift action_63
action_131 (58) = happyShift action_64
action_131 (59) = happyShift action_65
action_131 (60) = happyShift action_66
action_131 (61) = happyShift action_67
action_131 (62) = happyShift action_68
action_131 (63) = happyShift action_69
action_131 (64) = happyShift action_70
action_131 (65) = happyShift action_71
action_131 (105) = happyShift action_72
action_131 _ = happyReduce_53

action_132 (54) = happyShift action_60
action_132 (55) = happyShift action_61
action_132 (56) = happyShift action_62
action_132 (57) = happyShift action_63
action_132 (58) = happyShift action_64
action_132 (59) = happyShift action_65
action_132 (60) = happyShift action_66
action_132 (61) = happyShift action_67
action_132 (62) = happyShift action_68
action_132 (63) = happyShift action_69
action_132 (64) = happyShift action_70
action_132 (65) = happyShift action_71
action_132 (98) = happyShift action_172
action_132 (105) = happyShift action_72
action_132 _ = happyReduce_8

action_133 _ = happyReduce_12

action_134 (54) = happyShift action_60
action_134 (55) = happyShift action_61
action_134 (56) = happyShift action_62
action_134 (57) = happyShift action_63
action_134 (58) = happyShift action_64
action_134 (59) = happyShift action_65
action_134 (60) = happyShift action_66
action_134 (61) = happyShift action_67
action_134 (62) = happyShift action_68
action_134 (63) = happyShift action_69
action_134 (64) = happyShift action_70
action_134 (65) = happyShift action_71
action_134 (98) = happyShift action_125
action_134 (105) = happyShift action_72
action_134 (13) = happyGoto action_171
action_134 _ = happyReduce_14

action_135 _ = happyReduce_16

action_136 (96) = happyShift action_170
action_136 _ = happyReduce_30

action_137 (49) = happyShift action_6
action_137 (8) = happyGoto action_169
action_137 _ = happyReduce_34

action_138 (99) = happyShift action_168
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_31

action_140 (49) = happyShift action_6
action_140 (8) = happyGoto action_167
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (49) = happyShift action_6
action_141 (8) = happyGoto action_165
action_141 (20) = happyGoto action_166
action_141 _ = happyReduce_25

action_142 (49) = happyShift action_6
action_142 (50) = happyShift action_37
action_142 (51) = happyShift action_38
action_142 (52) = happyShift action_39
action_142 (90) = happyShift action_140
action_142 (94) = happyShift action_141
action_142 (8) = happyGoto action_136
action_142 (21) = happyGoto action_137
action_142 (22) = happyGoto action_164
action_142 (23) = happyGoto action_139
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (54) = happyShift action_60
action_143 (55) = happyShift action_61
action_143 (56) = happyShift action_62
action_143 (57) = happyShift action_63
action_143 (58) = happyShift action_64
action_143 (59) = happyShift action_65
action_143 (60) = happyShift action_66
action_143 (61) = happyShift action_67
action_143 (62) = happyShift action_68
action_143 (63) = happyShift action_69
action_143 (64) = happyShift action_70
action_143 (65) = happyShift action_71
action_143 (73) = happyShift action_163
action_143 (105) = happyShift action_72
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (67) = happyReduce_96
action_144 (69) = happyReduce_96
action_144 (76) = happyReduce_96
action_144 (77) = happyReduce_96
action_144 (91) = happyReduce_96
action_144 (95) = happyReduce_96
action_144 (97) = happyReduce_96
action_144 (98) = happyReduce_96
action_144 (99) = happyShift action_78
action_144 (103) = happyReduce_96
action_144 (104) = happyReduce_96
action_144 (106) = happyReduce_96
action_144 (110) = happyReduce_96
action_144 _ = happyReduce_96

action_145 (98) = happyShift action_162
action_145 (99) = happyShift action_78
action_145 _ = happyReduce_71

action_146 (98) = happyShift action_100
action_146 (99) = happyShift action_78
action_146 (32) = happyGoto action_161
action_146 _ = happyReduce_73

action_147 _ = happyReduce_75

action_148 (49) = happyShift action_6
action_148 (50) = happyShift action_21
action_148 (51) = happyShift action_22
action_148 (52) = happyShift action_23
action_148 (90) = happyShift action_24
action_148 (94) = happyShift action_25
action_148 (107) = happyShift action_26
action_148 (8) = happyGoto action_14
action_148 (31) = happyGoto action_15
action_148 (33) = happyGoto action_16
action_148 (34) = happyGoto action_17
action_148 (35) = happyGoto action_18
action_148 (39) = happyGoto action_19
action_148 (40) = happyGoto action_160
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (99) = happyShift action_82
action_149 _ = happyReduce_106

action_150 (49) = happyShift action_6
action_150 (8) = happyGoto action_157
action_150 (44) = happyGoto action_158
action_150 (45) = happyGoto action_159
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (69) = happyShift action_156
action_151 (99) = happyShift action_78
action_151 _ = happyReduce_99

action_152 (49) = happyShift action_6
action_152 (8) = happyGoto action_155
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (103) = happyShift action_154
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (49) = happyShift action_6
action_154 (50) = happyShift action_37
action_154 (51) = happyShift action_38
action_154 (52) = happyShift action_39
action_154 (71) = happyShift action_40
action_154 (74) = happyShift action_41
action_154 (90) = happyShift action_42
action_154 (92) = happyShift action_43
action_154 (94) = happyShift action_44
action_154 (105) = happyShift action_45
action_154 (107) = happyShift action_46
action_154 (8) = happyGoto action_27
action_154 (10) = happyGoto action_28
action_154 (12) = happyGoto action_29
action_154 (14) = happyGoto action_30
action_154 (17) = happyGoto action_31
action_154 (18) = happyGoto action_32
action_154 (23) = happyGoto action_33
action_154 (24) = happyGoto action_34
action_154 (26) = happyGoto action_35
action_154 (29) = happyGoto action_190
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (96) = happyShift action_189
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (49) = happyShift action_6
action_156 (8) = happyGoto action_186
action_156 (37) = happyGoto action_187
action_156 (38) = happyGoto action_188
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (96) = happyShift action_185
action_157 _ = happyFail (happyExpListPerState 157)

action_158 _ = happyReduce_108

action_159 (69) = happyShift action_183
action_159 (104) = happyShift action_184
action_159 _ = happyReduce_112

action_160 (69) = happyShift action_182
action_160 (99) = happyShift action_78
action_160 _ = happyReduce_114

action_161 _ = happyReduce_74

action_162 (49) = happyShift action_6
action_162 (8) = happyGoto action_75
action_162 (30) = happyGoto action_181
action_162 _ = happyReduce_69

action_163 (49) = happyShift action_6
action_163 (50) = happyShift action_37
action_163 (51) = happyShift action_38
action_163 (52) = happyShift action_39
action_163 (71) = happyShift action_40
action_163 (74) = happyShift action_41
action_163 (90) = happyShift action_42
action_163 (92) = happyShift action_43
action_163 (94) = happyShift action_44
action_163 (105) = happyShift action_45
action_163 (107) = happyShift action_46
action_163 (8) = happyGoto action_27
action_163 (10) = happyGoto action_28
action_163 (12) = happyGoto action_29
action_163 (14) = happyGoto action_30
action_163 (17) = happyGoto action_31
action_163 (18) = happyGoto action_32
action_163 (23) = happyGoto action_33
action_163 (24) = happyGoto action_34
action_163 (26) = happyGoto action_35
action_163 (29) = happyGoto action_180
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (99) = happyShift action_179
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (98) = happyShift action_178
action_165 _ = happyReduce_26

action_166 (95) = happyShift action_177
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (98) = happyShift action_176
action_167 (19) = happyGoto action_175
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (49) = happyShift action_6
action_168 (50) = happyShift action_37
action_168 (51) = happyShift action_38
action_168 (52) = happyShift action_39
action_168 (71) = happyShift action_40
action_168 (74) = happyShift action_41
action_168 (90) = happyShift action_42
action_168 (92) = happyShift action_43
action_168 (94) = happyShift action_44
action_168 (105) = happyShift action_45
action_168 (107) = happyShift action_46
action_168 (8) = happyGoto action_27
action_168 (10) = happyGoto action_28
action_168 (12) = happyGoto action_29
action_168 (14) = happyGoto action_30
action_168 (17) = happyGoto action_31
action_168 (18) = happyGoto action_32
action_168 (23) = happyGoto action_33
action_168 (24) = happyGoto action_34
action_168 (26) = happyGoto action_35
action_168 (29) = happyGoto action_174
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_29

action_170 _ = happyReduce_28

action_171 _ = happyReduce_15

action_172 (49) = happyShift action_6
action_172 (8) = happyGoto action_53
action_172 (9) = happyGoto action_173
action_172 _ = happyReduce_6

action_173 _ = happyReduce_7

action_174 (54) = happyShift action_60
action_174 (55) = happyShift action_61
action_174 (56) = happyShift action_62
action_174 (57) = happyShift action_63
action_174 (58) = happyShift action_64
action_174 (59) = happyShift action_65
action_174 (60) = happyShift action_66
action_174 (61) = happyShift action_67
action_174 (62) = happyShift action_68
action_174 (63) = happyShift action_69
action_174 (64) = happyShift action_70
action_174 (65) = happyShift action_71
action_174 (105) = happyShift action_72
action_174 _ = happyReduce_44

action_175 (91) = happyShift action_205
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (49) = happyShift action_6
action_176 (8) = happyGoto action_204
action_176 _ = happyFail (happyExpListPerState 176)

action_177 _ = happyReduce_33

action_178 (49) = happyShift action_6
action_178 (8) = happyGoto action_165
action_178 (20) = happyGoto action_203
action_178 _ = happyReduce_25

action_179 (49) = happyShift action_6
action_179 (50) = happyShift action_37
action_179 (51) = happyShift action_38
action_179 (52) = happyShift action_39
action_179 (71) = happyShift action_40
action_179 (74) = happyShift action_41
action_179 (90) = happyShift action_42
action_179 (92) = happyShift action_43
action_179 (94) = happyShift action_44
action_179 (105) = happyShift action_45
action_179 (107) = happyShift action_46
action_179 (8) = happyGoto action_27
action_179 (10) = happyGoto action_28
action_179 (12) = happyGoto action_29
action_179 (14) = happyGoto action_30
action_179 (17) = happyGoto action_31
action_179 (18) = happyGoto action_32
action_179 (23) = happyGoto action_33
action_179 (24) = happyGoto action_34
action_179 (26) = happyGoto action_35
action_179 (29) = happyGoto action_202
action_179 _ = happyFail (happyExpListPerState 179)

action_180 (54) = happyShift action_60
action_180 (55) = happyShift action_61
action_180 (56) = happyShift action_62
action_180 (57) = happyShift action_63
action_180 (58) = happyShift action_64
action_180 (59) = happyShift action_65
action_180 (60) = happyShift action_66
action_180 (61) = happyShift action_67
action_180 (62) = happyShift action_68
action_180 (63) = happyShift action_69
action_180 (64) = happyShift action_70
action_180 (65) = happyShift action_71
action_180 (105) = happyShift action_72
action_180 _ = happyReduce_22

action_181 _ = happyReduce_70

action_182 (49) = happyShift action_6
action_182 (8) = happyGoto action_186
action_182 (37) = happyGoto action_187
action_182 (38) = happyGoto action_201
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (49) = happyShift action_6
action_183 (8) = happyGoto action_186
action_183 (37) = happyGoto action_187
action_183 (38) = happyGoto action_200
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (49) = happyShift action_6
action_184 (8) = happyGoto action_157
action_184 (44) = happyGoto action_199
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (49) = happyShift action_6
action_185 (50) = happyShift action_21
action_185 (51) = happyShift action_22
action_185 (52) = happyShift action_23
action_185 (90) = happyShift action_24
action_185 (94) = happyShift action_25
action_185 (107) = happyShift action_26
action_185 (8) = happyGoto action_14
action_185 (31) = happyGoto action_15
action_185 (33) = happyGoto action_16
action_185 (34) = happyGoto action_17
action_185 (35) = happyGoto action_18
action_185 (39) = happyGoto action_19
action_185 (40) = happyGoto action_198
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (87) = happyShift action_194
action_186 (102) = happyShift action_195
action_186 (103) = happyShift action_196
action_186 (104) = happyShift action_197
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_90

action_188 (97) = happyShift action_193
action_188 _ = happyReduce_100

action_189 (49) = happyShift action_6
action_189 (50) = happyShift action_21
action_189 (51) = happyShift action_22
action_189 (52) = happyShift action_23
action_189 (90) = happyShift action_24
action_189 (94) = happyShift action_25
action_189 (107) = happyShift action_26
action_189 (8) = happyGoto action_14
action_189 (31) = happyGoto action_15
action_189 (33) = happyGoto action_16
action_189 (34) = happyGoto action_17
action_189 (35) = happyGoto action_18
action_189 (39) = happyGoto action_19
action_189 (40) = happyGoto action_192
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (54) = happyShift action_60
action_190 (55) = happyShift action_61
action_190 (56) = happyShift action_62
action_190 (57) = happyShift action_63
action_190 (58) = happyShift action_64
action_190 (59) = happyShift action_65
action_190 (60) = happyShift action_66
action_190 (61) = happyShift action_67
action_190 (62) = happyShift action_68
action_190 (63) = happyShift action_69
action_190 (64) = happyShift action_70
action_190 (65) = happyShift action_71
action_190 (69) = happyShift action_191
action_190 (105) = happyShift action_72
action_190 _ = happyReduce_110

action_191 (49) = happyShift action_6
action_191 (8) = happyGoto action_212
action_191 (27) = happyGoto action_213
action_191 (28) = happyGoto action_214
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (99) = happyShift action_78
action_192 _ = happyReduce_101

action_193 (49) = happyShift action_6
action_193 (8) = happyGoto action_186
action_193 (37) = happyGoto action_211
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (49) = happyShift action_6
action_194 (50) = happyShift action_21
action_194 (51) = happyShift action_22
action_194 (52) = happyShift action_23
action_194 (90) = happyShift action_24
action_194 (94) = happyShift action_25
action_194 (107) = happyShift action_26
action_194 (8) = happyGoto action_14
action_194 (31) = happyGoto action_15
action_194 (33) = happyGoto action_16
action_194 (34) = happyGoto action_17
action_194 (35) = happyGoto action_18
action_194 (39) = happyGoto action_19
action_194 (40) = happyGoto action_210
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (49) = happyShift action_6
action_195 (50) = happyShift action_21
action_195 (51) = happyShift action_22
action_195 (52) = happyShift action_23
action_195 (90) = happyShift action_24
action_195 (94) = happyShift action_25
action_195 (107) = happyShift action_26
action_195 (8) = happyGoto action_14
action_195 (31) = happyGoto action_15
action_195 (33) = happyGoto action_16
action_195 (34) = happyGoto action_17
action_195 (35) = happyGoto action_18
action_195 (39) = happyGoto action_19
action_195 (40) = happyGoto action_209
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (49) = happyShift action_6
action_196 (50) = happyShift action_21
action_196 (51) = happyShift action_22
action_196 (52) = happyShift action_23
action_196 (90) = happyShift action_24
action_196 (94) = happyShift action_25
action_196 (107) = happyShift action_26
action_196 (8) = happyGoto action_14
action_196 (31) = happyGoto action_15
action_196 (33) = happyGoto action_16
action_196 (34) = happyGoto action_17
action_196 (35) = happyGoto action_18
action_196 (39) = happyGoto action_19
action_196 (40) = happyGoto action_208
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (49) = happyShift action_6
action_197 (50) = happyShift action_21
action_197 (51) = happyShift action_22
action_197 (52) = happyShift action_23
action_197 (90) = happyShift action_24
action_197 (94) = happyShift action_25
action_197 (107) = happyShift action_26
action_197 (8) = happyGoto action_14
action_197 (31) = happyGoto action_15
action_197 (33) = happyGoto action_16
action_197 (34) = happyGoto action_17
action_197 (35) = happyGoto action_18
action_197 (39) = happyGoto action_19
action_197 (40) = happyGoto action_207
action_197 _ = happyFail (happyExpListPerState 197)

action_198 (99) = happyShift action_78
action_198 _ = happyReduce_107

action_199 _ = happyReduce_109

action_200 (97) = happyShift action_193
action_200 _ = happyReduce_113

action_201 (97) = happyShift action_193
action_201 _ = happyReduce_115

action_202 (54) = happyShift action_60
action_202 (55) = happyShift action_61
action_202 (56) = happyShift action_62
action_202 (57) = happyShift action_63
action_202 (58) = happyShift action_64
action_202 (59) = happyShift action_65
action_202 (60) = happyShift action_66
action_202 (61) = happyShift action_67
action_202 (62) = happyShift action_68
action_202 (63) = happyShift action_69
action_202 (64) = happyShift action_70
action_202 (65) = happyShift action_71
action_202 (105) = happyShift action_72
action_202 _ = happyReduce_45

action_203 _ = happyReduce_27

action_204 (98) = happyShift action_176
action_204 (19) = happyGoto action_206
action_204 _ = happyReduce_23

action_205 _ = happyReduce_32

action_206 _ = happyReduce_24

action_207 (99) = happyShift action_78
action_207 _ = happyReduce_89

action_208 (99) = happyShift action_78
action_208 _ = happyReduce_86

action_209 (99) = happyShift action_78
action_209 _ = happyReduce_88

action_210 (99) = happyShift action_78
action_210 _ = happyReduce_87

action_211 _ = happyReduce_91

action_212 (103) = happyShift action_216
action_212 _ = happyFail (happyExpListPerState 212)

action_213 _ = happyReduce_48

action_214 (98) = happyShift action_215
action_214 _ = happyReduce_111

action_215 (49) = happyShift action_6
action_215 (8) = happyGoto action_212
action_215 (27) = happyGoto action_218
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (49) = happyShift action_6
action_216 (50) = happyShift action_37
action_216 (51) = happyShift action_38
action_216 (52) = happyShift action_39
action_216 (71) = happyShift action_40
action_216 (74) = happyShift action_41
action_216 (90) = happyShift action_42
action_216 (92) = happyShift action_43
action_216 (94) = happyShift action_44
action_216 (105) = happyShift action_45
action_216 (107) = happyShift action_46
action_216 (8) = happyGoto action_27
action_216 (10) = happyGoto action_28
action_216 (12) = happyGoto action_29
action_216 (14) = happyGoto action_30
action_216 (17) = happyGoto action_31
action_216 (18) = happyGoto action_32
action_216 (23) = happyGoto action_33
action_216 (24) = happyGoto action_34
action_216 (26) = happyGoto action_35
action_216 (29) = happyGoto action_217
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (54) = happyShift action_60
action_217 (55) = happyShift action_61
action_217 (56) = happyShift action_62
action_217 (57) = happyShift action_63
action_217 (58) = happyShift action_64
action_217 (59) = happyShift action_65
action_217 (60) = happyShift action_66
action_217 (61) = happyShift action_67
action_217 (62) = happyShift action_68
action_217 (63) = happyShift action_69
action_217 (64) = happyShift action_70
action_217 (65) = happyShift action_71
action_217 (105) = happyShift action_72
action_217 _ = happyReduce_47

action_218 _ = happyReduce_49

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
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn29  happy_var_3)
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
happyReduction_11 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 _
	_
	_
	 =  HappyAbsSyn12
		 (
	)

happyReduce_14 = happySpecReduce_2  13 happyReduction_14
happyReduction_14 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn13
		 ([happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 14 happyReduction_16
happyReduction_16 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
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
happyReduction_20 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  17 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn17
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 18 happyReduction_22
happyReduction_22 ((HappyAbsSyn29  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  19 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn19
		 ([happy_var_2]
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  19 happyReduction_24
happyReduction_24 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  20 happyReduction_25
happyReduction_25  =  HappyAbsSyn20
		 ([]
	)

happyReduce_26 = happySpecReduce_1  20 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  20 happyReduction_27
happyReduction_27 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  21 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  21 happyReduction_29
happyReduction_29 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  22 happyReduction_30
happyReduction_30 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn22
		 (P.pattern $ P.Var happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (P.pattern $ P.Term $ fmap HM.Term happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 22 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (P.pattern $ P.Tuple (happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_3  22 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (P.pattern $ P.Record happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (P.pattern $ P.Tagged happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  23 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (P.number HM.LInt happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  23 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (P.string HM.LString happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  24 happyReduction_38
happyReduction_38 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (P.term happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  24 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 25 happyReduction_44
happyReduction_44 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ([P.matchCase happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 5 25 happyReduction_45
happyReduction_45 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (happy_var_1 ++ [P.matchCase happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  26 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  27 happyReduction_47
happyReduction_47 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn27
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  28 happyReduction_48
happyReduction_48 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  28 happyReduction_49
happyReduction_49 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  29 happyReduction_50
happyReduction_50 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  29 happyReduction_51
happyReduction_51 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  29 happyReduction_52
happyReduction_52 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happyReduce 4 29 happyReduction_53
happyReduction_53 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  29 happyReduction_54
happyReduction_54 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  29 happyReduction_55
happyReduction_55 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (P.dotLambda happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  29 happyReduction_56
happyReduction_56 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  29 happyReduction_57
happyReduction_57 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  29 happyReduction_58
happyReduction_58 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  29 happyReduction_59
happyReduction_59 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  29 happyReduction_60
happyReduction_60 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  29 happyReduction_61
happyReduction_61 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  29 happyReduction_62
happyReduction_62 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  29 happyReduction_63
happyReduction_63 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  29 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  29 happyReduction_65
happyReduction_65 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  29 happyReduction_66
happyReduction_66 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  29 happyReduction_67
happyReduction_67 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  29 happyReduction_68
happyReduction_68 (HappyAbsSyn29  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  30 happyReduction_69
happyReduction_69  =  HappyAbsSyn30
		 ([]
	)

happyReduce_70 = happyReduce 5 30 happyReduction_70
happyReduction_70 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_3  30 happyReduction_71
happyReduction_71 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  31 happyReduction_72
happyReduction_72 (HappyTerminal happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  32 happyReduction_73
happyReduction_73 (HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn32
		 ([happy_var_2]
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  32 happyReduction_74
happyReduction_74 (HappyAbsSyn32  happy_var_3)
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2 : happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 33 happyReduction_75
happyReduction_75 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_1  34 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  34 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  34 happyReduction_78
happyReduction_78 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  34 happyReduction_79
happyReduction_79 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  34 happyReduction_80
happyReduction_80 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  35 happyReduction_81
happyReduction_81 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  35 happyReduction_82
happyReduction_82 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn35
		 (P.tyIdentifier happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  35 happyReduction_83
happyReduction_83 (HappyTerminal happy_var_3)
	(HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  36 happyReduction_84
happyReduction_84  =  HappyAbsSyn36
		 ([]
	)

happyReduce_85 = happySpecReduce_2  36 happyReduction_85
happyReduction_85 (HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  37 happyReduction_86
happyReduction_86 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  37 happyReduction_87
happyReduction_87 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  37 happyReduction_88
happyReduction_88 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  37 happyReduction_89
happyReduction_89 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  38 happyReduction_90
happyReduction_90 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  38 happyReduction_91
happyReduction_91 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  39 happyReduction_92
happyReduction_92 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  40 happyReduction_93
happyReduction_93 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  40 happyReduction_94
happyReduction_94 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  40 happyReduction_95
happyReduction_95 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happyReduce 4 40 happyReduction_96
happyReduction_96 ((HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_97 = happySpecReduce_3  40 happyReduction_97
happyReduction_97 (HappyTerminal happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn40
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  41 happyReduction_98
happyReduction_98  =  HappyAbsSyn41
		 (Nothing
	)

happyReduce_99 = happySpecReduce_2  41 happyReduction_99
happyReduction_99 (HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (Just happy_var_2
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happyReduce 4 41 happyReduction_100
happyReduction_100 ((HappyAbsSyn38  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_101 = happyReduce 5 41 happyReduction_101
happyReduction_101 ((HappyAbsSyn40  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_3  42 happyReduction_102
happyReduction_102 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  42 happyReduction_103
happyReduction_103 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  42 happyReduction_104
happyReduction_104 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn42
		 (P.kindId happy_var_1
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  43 happyReduction_105
happyReduction_105  =  HappyAbsSyn43
		 (Nothing
	)

happyReduce_106 = happySpecReduce_2  43 happyReduction_106
happyReduction_106 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (Just happy_var_2
	)
happyReduction_106 _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  44 happyReduction_107
happyReduction_107 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn44
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  45 happyReduction_108
happyReduction_108 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn45
		 ([happy_var_1]
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  45 happyReduction_109
happyReduction_109 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happyReduce 6 46 happyReduction_110
happyReduction_110 ((HappyAbsSyn29  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn41  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_111 = happyReduce 8 46 happyReduction_111
happyReduction_111 ((HappyAbsSyn28  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn41  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_112 = happyReduce 5 46 happyReduction_112
happyReduction_112 ((HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_113 = happyReduce 7 46 happyReduction_113
happyReduction_113 ((HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_114 = happyReduce 5 46 happyReduction_114
happyReduction_114 ((HappyAbsSyn40  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_115 = happyReduce 7 46 happyReduction_115
happyReduction_115 ((HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_116 = happySpecReduce_1  47 happyReduction_116
happyReduction_116 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_2  47 happyReduction_117
happyReduction_117 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_117 _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  48 happyReduction_118
happyReduction_118 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn48
		 (P.script happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 110 110 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 49;
	L.RangedToken (T.Number _) _ -> cont 50;
	L.RangedToken (T.String _) _ -> cont 51;
	L.RangedToken (T.Boolean _) _ -> cont 52;
	L.RangedToken (T.Operator "!") _ -> cont 53;
	L.RangedToken (T.Operator "+") _ -> cont 54;
	L.RangedToken (T.Operator "-") _ -> cont 55;
	L.RangedToken (T.Operator "*") _ -> cont 56;
	L.RangedToken (T.Operator "/") _ -> cont 57;
	L.RangedToken (T.Operator "==") _ -> cont 58;
	L.RangedToken (T.Operator "!=") _ -> cont 59;
	L.RangedToken (T.Operator "<") _ -> cont 60;
	L.RangedToken (T.Operator "<=") _ -> cont 61;
	L.RangedToken (T.Operator ">") _ -> cont 62;
	L.RangedToken (T.Operator ">=") _ -> cont 63;
	L.RangedToken (T.Operator "||") _ -> cont 64;
	L.RangedToken (T.Operator "&&") _ -> cont 65;
	L.RangedToken (T.Operator _) _ -> cont 66;
	L.RangedToken T.Let _ -> cont 67;
	L.RangedToken T.In _ -> cont 68;
	L.RangedToken T.Where _ -> cont 69;
	L.RangedToken T.With _ -> cont 70;
	L.RangedToken T.If _ -> cont 71;
	L.RangedToken T.Then _ -> cont 72;
	L.RangedToken T.Else _ -> cont 73;
	L.RangedToken T.Match _ -> cont 74;
	L.RangedToken T.Return _ -> cont 75;
	L.RangedToken T.Data _ -> cont 76;
	L.RangedToken T.Type _ -> cont 77;
	L.RangedToken T.Alias _ -> cont 78;
	L.RangedToken T.Kind _ -> cont 79;
	L.RangedToken T.Forall _ -> cont 80;
	L.RangedToken T.Exists _ -> cont 81;
	L.RangedToken T.Proof _ -> cont 82;
	L.RangedToken T.Infer _ -> cont 83;
	L.RangedToken T.Protocol _ -> cont 84;
	L.RangedToken T.Interface _ -> cont 85;
	L.RangedToken T.Instance _ -> cont 86;
	L.RangedToken T.Implements _ -> cont 87;
	L.RangedToken T.Module _ -> cont 88;
	L.RangedToken T.Import _ -> cont 89;
	L.RangedToken T.LParen _ -> cont 90;
	L.RangedToken T.RParen _ -> cont 91;
	L.RangedToken T.LBrack _ -> cont 92;
	L.RangedToken T.RBrack _ -> cont 93;
	L.RangedToken T.LCurly _ -> cont 94;
	L.RangedToken T.RCurly _ -> cont 95;
	L.RangedToken T.Colon _ -> cont 96;
	L.RangedToken T.SemiColon _ -> cont 97;
	L.RangedToken T.Comma _ -> cont 98;
	L.RangedToken T.Arrow _ -> cont 99;
	L.RangedToken T.BackArrow _ -> cont 100;
	L.RangedToken T.FatArrow _ -> cont 101;
	L.RangedToken T.PipeArrow _ -> cont 102;
	L.RangedToken T.Equals _ -> cont 103;
	L.RangedToken T.Pipe _ -> cont 104;
	L.RangedToken T.Dot _ -> cont 105;
	L.RangedToken T.Section _ -> cont 106;
	L.RangedToken T.BackSlash _ -> cont 107;
	L.RangedToken T.Newline _ -> cont 108;
	L.RangedToken T.EOF _ -> cont 109;
	_ -> happyError' (tk, [])
	})

happyError_ explist 110 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn48 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn46 z -> happyReturn z; _other -> notHappyAtAll })

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
