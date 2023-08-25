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


import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.Parser.ParsingInfo as P
import           Saga.Parser.ParsingInfo ((<->))


import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51
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
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1541) ([0,0,0,0,512,12,0,0,0,0,3840,0,36,672,80,0,0,57344,1,0,17408,2304,0,0,0,4,0,32768,0,0,0,0,0,8192,192,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,1,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,16,0,0,2,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,8192,18434,0,0,0,32,0,0,0,0,0,0,15360,0,0,2176,288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65472,63,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,480,32768,4,84,10,0,0,15360,0,144,2688,320,0,0,32768,7,4608,20480,10241,0,0,0,240,16384,2,42,5,0,0,512,0,0,0,0,0,0,49152,3,0,43008,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12296,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,1,0,0,0,64512,1023,0,1024,2,0,0,32768,32767,0,33024,64,0,0,0,65520,15,0,3072,0,0,0,65024,33279,0,0,1,0,0,7680,0,72,1344,160,0,0,49152,3,2304,43008,5120,0,0,0,120,8192,1,32789,2,0,0,3840,0,36,672,80,0,0,57344,1,1152,21504,2560,0,0,0,60,36864,32768,16394,1,0,0,1920,0,18,336,40,0,0,61440,0,576,10752,1280,0,0,0,30,18432,16384,40965,0,0,0,960,0,9,168,20,0,0,30720,0,288,5376,640,0,0,0,15,9216,40960,20482,0,0,0,480,32768,4,84,10,0,0,15360,0,144,2688,320,0,0,32768,7,4608,20480,10241,0,0,0,240,16384,2,42,5,0,0,512,0,0,0,0,0,0,49152,7,0,43008,0,0,0,0,8,0,0,2048,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,1,0,0,0,0,0,0,16,0,0,0,0,0,0,12320,0,0,0,61440,0,0,8704,1152,0,0,0,30,0,16384,36868,0,0,0,1984,0,0,136,0,0,0,30720,0,0,4352,576,0,0,0,0,0,16384,64,0,0,0,32,0,0,4,0,0,0,0,0,0,0,128,0,0,0,0,0,0,4096,0,0,0,0,0,0,128,0,0,0,0,0,0,0,64,0,0,49152,3,0,34944,4608,0,0,0,0,0,0,8192,0,0,0,256,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,2048,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,240,0,0,32802,4,0,0,0,0,0,0,0,0,0,49152,3,0,34816,4608,0,0,0,0,0,0,0,0,0,0,3840,0,0,544,72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,126,0,0,0,0,0,0,4032,0,0,0,0,0,0,63488,1,0,0,0,0,0,0,63,0,0,0,0,0,0,2016,0,0,0,0,0,0,64512,0,0,0,0,0,0,32768,31,0,0,0,0,0,0,1008,0,0,0,0,0,0,32256,0,0,0,0,0,0,49152,15,0,0,0,0,0,0,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,64,0,0,0,0,0,0,3584,0,0,0,0,0,0,49152,1,0,0,0,0,0,61440,0,576,10752,1280,0,0,0,0,0,0,4096,0,0,0,960,0,0,168,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,480,32768,4,84,10,0,0,15360,0,144,2688,320,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,0,72,1344,160,0,0,49152,3,2304,43008,5120,0,0,0,65280,255,0,32768,0,0,0,57344,8191,0,8192,16,0,0,0,0,0,0,0,0,0,0,65408,127,0,16512,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,32,0,0,32,0,0,0,1024,0,0,0,0,0,0,32768,7,0,20480,1,0,0,0,65024,511,1,0,1,0,0,0,0,0,32768,0,0,0,0,0,0,0,24,0,0,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,57344,1,0,17408,2304,0,0,0,0,0,0,256,0,0,0,128,0,0,0,0,0,0,0,0,16,0,4,0,0,0,2,0,0,0,0,0,0,0,0,0,0,1,0,0,30720,0,288,5376,640,0,0,0,0,0,0,8,0,0,0,32,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,32768,0,0,0,0,0,2,32768,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,3840,0,36,672,80,0,0,0,0,0,0,8,0,0,0,0,0,0,128,0,0,0,0,0,0,0,4,0,0,0,0,0,0,2,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,15,9216,40960,20482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65528,7,0,1024,0,0,0,0,0,0,2,0,0,0,256,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,4,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,2,0,0,0,0,0,0,960,0,9,168,20,0,0,0,65535,0,0,128,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,1024,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,240,0,0,32802,4,0,0,0,0,0,8,28,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,3840,0,0,544,72,0,0,0,65532,35,0,512,0,0,0,4,0,0,0,0,0,0,0,0,0,8192,0,0,0,4096,0,0,0,0,0,0,0,30,0,16384,36868,0,0,0,960,0,0,136,18,0,0,30720,0,0,4352,576,0,0,0,15,0,8192,18434,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,512,0,0,0,49152,16383,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,2,0,0,0,0,0,0,64,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,16384,0,0,0,0,0,0,0,120,8192,1,32789,2,0,0,57344,8191,0,0,16,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","listElements","list","tupleElems","tuple","params","args","fnApplication","controlFlow","patListElems","patTupleElems","patRecordKeys","patRest","patData","pattern","term","atom","cases","matchExpr","binding","bindings","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","tagged","union","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'^'","'++'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 117
        bit_end = (st Prelude.+ 1) Prelude.* 117
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..116]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (74) = happyShift action_8
action_0 (83) = happyShift action_9
action_0 (84) = happyShift action_10
action_0 (49) = happyGoto action_49
action_0 (50) = happyGoto action_50
action_0 (51) = happyGoto action_51
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (52) = happyShift action_6
action_1 (53) = happyShift action_39
action_1 (54) = happyShift action_40
action_1 (55) = happyShift action_41
action_1 (78) = happyShift action_42
action_1 (81) = happyShift action_43
action_1 (97) = happyShift action_44
action_1 (99) = happyShift action_45
action_1 (101) = happyShift action_46
action_1 (112) = happyShift action_47
action_1 (114) = happyShift action_48
action_1 (8) = happyGoto action_29
action_1 (10) = happyGoto action_30
action_1 (12) = happyGoto action_31
action_1 (14) = happyGoto action_32
action_1 (17) = happyGoto action_33
action_1 (18) = happyGoto action_34
action_1 (25) = happyGoto action_35
action_1 (26) = happyGoto action_36
action_1 (28) = happyGoto action_37
action_1 (31) = happyGoto action_38
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (52) = happyShift action_6
action_2 (53) = happyShift action_22
action_2 (54) = happyShift action_23
action_2 (55) = happyShift action_24
action_2 (97) = happyShift action_25
action_2 (101) = happyShift action_26
action_2 (111) = happyShift action_27
action_2 (114) = happyShift action_28
action_2 (8) = happyGoto action_14
action_2 (33) = happyGoto action_15
action_2 (35) = happyGoto action_16
action_2 (36) = happyGoto action_17
action_2 (37) = happyGoto action_18
action_2 (41) = happyGoto action_19
action_2 (42) = happyGoto action_20
action_2 (43) = happyGoto action_21
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (52) = happyShift action_6
action_3 (97) = happyShift action_13
action_3 (8) = happyGoto action_11
action_3 (45) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (74) = happyShift action_8
action_4 (83) = happyShift action_9
action_4 (84) = happyShift action_10
action_4 (49) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (52) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (117) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (52) = happyShift action_6
action_8 (8) = happyGoto action_93
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (52) = happyShift action_6
action_9 (8) = happyGoto action_92
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (52) = happyShift action_6
action_10 (8) = happyGoto action_91
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_117

action_12 (106) = happyShift action_90
action_12 (117) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (52) = happyShift action_6
action_13 (97) = happyShift action_13
action_13 (8) = happyGoto action_11
action_13 (45) = happyGoto action_89
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (52) = happyReduce_92
action_14 (53) = happyReduce_92
action_14 (54) = happyReduce_92
action_14 (55) = happyReduce_92
action_14 (56) = happyReduce_92
action_14 (74) = happyReduce_92
action_14 (76) = happyReduce_92
action_14 (83) = happyReduce_92
action_14 (84) = happyReduce_92
action_14 (97) = happyReduce_92
action_14 (98) = happyReduce_92
action_14 (101) = happyReduce_92
action_14 (102) = happyReduce_92
action_14 (103) = happyShift action_88
action_14 (105) = happyReduce_92
action_14 (106) = happyReduce_92
action_14 (110) = happyReduce_92
action_14 (111) = happyReduce_92
action_14 (113) = happyReduce_92
action_14 (117) = happyReduce_92
action_14 _ = happyReduce_92

action_15 _ = happyReduce_90

action_16 _ = happyReduce_89

action_17 _ = happyReduce_91

action_18 (74) = happyReduce_105
action_18 (76) = happyReduce_105
action_18 (83) = happyReduce_105
action_18 (84) = happyReduce_105
action_18 (98) = happyReduce_105
action_18 (102) = happyReduce_105
action_18 (105) = happyReduce_105
action_18 (106) = happyReduce_105
action_18 (110) = happyReduce_105
action_18 (111) = happyReduce_105
action_18 (113) = happyReduce_105
action_18 (117) = happyReduce_105
action_18 (38) = happyGoto action_87
action_18 _ = happyReduce_94

action_19 _ = happyReduce_106

action_20 (74) = happyReduce_107
action_20 (76) = happyReduce_107
action_20 (83) = happyReduce_107
action_20 (84) = happyReduce_107
action_20 (98) = happyReduce_107
action_20 (102) = happyReduce_107
action_20 (105) = happyReduce_107
action_20 (106) = happyReduce_107
action_20 (110) = happyReduce_107
action_20 (111) = happyShift action_86
action_20 (113) = happyReduce_107
action_20 (117) = happyReduce_107
action_20 _ = happyReduce_107

action_21 (106) = happyShift action_85
action_21 (117) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_86

action_23 _ = happyReduce_88

action_24 _ = happyReduce_87

action_25 (52) = happyShift action_6
action_25 (53) = happyShift action_22
action_25 (54) = happyShift action_23
action_25 (55) = happyShift action_24
action_25 (97) = happyShift action_25
action_25 (101) = happyShift action_26
action_25 (111) = happyShift action_27
action_25 (114) = happyShift action_28
action_25 (8) = happyGoto action_14
action_25 (33) = happyGoto action_15
action_25 (35) = happyGoto action_16
action_25 (36) = happyGoto action_17
action_25 (37) = happyGoto action_18
action_25 (41) = happyGoto action_19
action_25 (42) = happyGoto action_20
action_25 (43) = happyGoto action_84
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (52) = happyShift action_6
action_26 (8) = happyGoto action_82
action_26 (32) = happyGoto action_83
action_26 _ = happyReduce_79

action_27 (52) = happyShift action_6
action_27 (53) = happyShift action_22
action_27 (54) = happyShift action_23
action_27 (55) = happyShift action_24
action_27 (97) = happyShift action_25
action_27 (101) = happyShift action_26
action_27 (111) = happyShift action_27
action_27 (114) = happyShift action_28
action_27 (8) = happyGoto action_14
action_27 (33) = happyGoto action_15
action_27 (35) = happyGoto action_16
action_27 (36) = happyGoto action_17
action_27 (37) = happyGoto action_18
action_27 (41) = happyGoto action_19
action_27 (42) = happyGoto action_20
action_27 (43) = happyGoto action_81
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (15) = happyGoto action_80
action_28 _ = happyReduce_17

action_29 _ = happyReduce_44

action_30 _ = happyReduce_48

action_31 _ = happyReduce_47

action_32 _ = happyReduce_46

action_33 _ = happyReduce_58

action_34 _ = happyReduce_56

action_35 _ = happyReduce_45

action_36 (57) = happyReduce_60
action_36 (58) = happyReduce_60
action_36 (59) = happyReduce_60
action_36 (60) = happyReduce_60
action_36 (61) = happyReduce_60
action_36 (62) = happyReduce_60
action_36 (63) = happyReduce_60
action_36 (64) = happyReduce_60
action_36 (65) = happyReduce_60
action_36 (66) = happyReduce_60
action_36 (67) = happyReduce_60
action_36 (68) = happyReduce_60
action_36 (69) = happyReduce_60
action_36 (70) = happyReduce_60
action_36 (71) = happyReduce_60
action_36 (72) = happyReduce_60
action_36 (74) = happyReduce_60
action_36 (76) = happyReduce_60
action_36 (79) = happyReduce_60
action_36 (80) = happyReduce_60
action_36 (83) = happyReduce_60
action_36 (84) = happyReduce_60
action_36 (98) = happyReduce_60
action_36 (100) = happyReduce_60
action_36 (102) = happyReduce_60
action_36 (105) = happyReduce_60
action_36 (111) = happyReduce_60
action_36 (112) = happyReduce_60
action_36 (117) = happyReduce_60
action_36 (16) = happyGoto action_79
action_36 _ = happyReduce_19

action_37 _ = happyReduce_57

action_38 (57) = happyShift action_62
action_38 (58) = happyShift action_63
action_38 (59) = happyShift action_64
action_38 (60) = happyShift action_65
action_38 (61) = happyShift action_66
action_38 (62) = happyShift action_67
action_38 (63) = happyShift action_68
action_38 (64) = happyShift action_69
action_38 (65) = happyShift action_70
action_38 (66) = happyShift action_71
action_38 (67) = happyShift action_72
action_38 (68) = happyShift action_73
action_38 (69) = happyShift action_74
action_38 (70) = happyShift action_75
action_38 (71) = happyShift action_76
action_38 (72) = happyShift action_77
action_38 (112) = happyShift action_78
action_38 (117) = happyAccept
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_41

action_40 _ = happyReduce_42

action_41 _ = happyReduce_43

action_42 (52) = happyShift action_6
action_42 (53) = happyShift action_39
action_42 (54) = happyShift action_40
action_42 (55) = happyShift action_41
action_42 (78) = happyShift action_42
action_42 (81) = happyShift action_43
action_42 (97) = happyShift action_44
action_42 (99) = happyShift action_45
action_42 (101) = happyShift action_46
action_42 (112) = happyShift action_47
action_42 (114) = happyShift action_48
action_42 (8) = happyGoto action_29
action_42 (10) = happyGoto action_30
action_42 (12) = happyGoto action_31
action_42 (14) = happyGoto action_32
action_42 (17) = happyGoto action_33
action_42 (18) = happyGoto action_34
action_42 (25) = happyGoto action_35
action_42 (26) = happyGoto action_36
action_42 (28) = happyGoto action_37
action_42 (31) = happyGoto action_61
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (52) = happyShift action_6
action_43 (53) = happyShift action_39
action_43 (54) = happyShift action_40
action_43 (55) = happyShift action_41
action_43 (78) = happyShift action_42
action_43 (81) = happyShift action_43
action_43 (97) = happyShift action_44
action_43 (99) = happyShift action_45
action_43 (101) = happyShift action_46
action_43 (112) = happyShift action_47
action_43 (114) = happyShift action_48
action_43 (8) = happyGoto action_29
action_43 (10) = happyGoto action_30
action_43 (12) = happyGoto action_31
action_43 (14) = happyGoto action_32
action_43 (17) = happyGoto action_33
action_43 (18) = happyGoto action_34
action_43 (25) = happyGoto action_35
action_43 (26) = happyGoto action_36
action_43 (28) = happyGoto action_37
action_43 (31) = happyGoto action_60
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (52) = happyShift action_6
action_44 (53) = happyShift action_39
action_44 (54) = happyShift action_40
action_44 (55) = happyShift action_41
action_44 (78) = happyShift action_42
action_44 (81) = happyShift action_43
action_44 (97) = happyShift action_44
action_44 (99) = happyShift action_45
action_44 (101) = happyShift action_46
action_44 (112) = happyShift action_47
action_44 (114) = happyShift action_48
action_44 (8) = happyGoto action_29
action_44 (10) = happyGoto action_30
action_44 (12) = happyGoto action_31
action_44 (14) = happyGoto action_32
action_44 (17) = happyGoto action_33
action_44 (18) = happyGoto action_34
action_44 (25) = happyGoto action_35
action_44 (26) = happyGoto action_36
action_44 (28) = happyGoto action_37
action_44 (31) = happyGoto action_59
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (52) = happyShift action_6
action_45 (53) = happyShift action_39
action_45 (54) = happyShift action_40
action_45 (55) = happyShift action_41
action_45 (78) = happyShift action_42
action_45 (81) = happyShift action_43
action_45 (97) = happyShift action_44
action_45 (99) = happyShift action_45
action_45 (101) = happyShift action_46
action_45 (112) = happyShift action_47
action_45 (114) = happyShift action_48
action_45 (8) = happyGoto action_29
action_45 (10) = happyGoto action_30
action_45 (11) = happyGoto action_57
action_45 (12) = happyGoto action_31
action_45 (14) = happyGoto action_32
action_45 (17) = happyGoto action_33
action_45 (18) = happyGoto action_34
action_45 (25) = happyGoto action_35
action_45 (26) = happyGoto action_36
action_45 (28) = happyGoto action_37
action_45 (31) = happyGoto action_58
action_45 _ = happyReduce_10

action_46 (52) = happyShift action_6
action_46 (8) = happyGoto action_55
action_46 (9) = happyGoto action_56
action_46 _ = happyReduce_6

action_47 (52) = happyShift action_6
action_47 (53) = happyShift action_39
action_47 (54) = happyShift action_40
action_47 (55) = happyShift action_41
action_47 (97) = happyShift action_44
action_47 (99) = happyShift action_45
action_47 (101) = happyShift action_46
action_47 (8) = happyGoto action_29
action_47 (10) = happyGoto action_30
action_47 (12) = happyGoto action_31
action_47 (14) = happyGoto action_32
action_47 (25) = happyGoto action_35
action_47 (26) = happyGoto action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (15) = happyGoto action_53
action_48 _ = happyReduce_17

action_49 _ = happyReduce_129

action_50 (74) = happyShift action_8
action_50 (83) = happyShift action_9
action_50 (84) = happyShift action_10
action_50 (49) = happyGoto action_52
action_50 _ = happyReduce_131

action_51 (117) = happyAccept
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_130

action_53 (52) = happyShift action_6
action_53 (106) = happyShift action_143
action_53 (8) = happyGoto action_112
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_61

action_55 (103) = happyShift action_142
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (102) = happyShift action_141
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (100) = happyShift action_140
action_57 _ = happyFail (happyExpListPerState 57)

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
action_58 (69) = happyShift action_74
action_58 (70) = happyShift action_75
action_58 (71) = happyShift action_76
action_58 (72) = happyShift action_77
action_58 (105) = happyShift action_139
action_58 (112) = happyShift action_78
action_58 _ = happyReduce_11

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
action_59 (69) = happyShift action_74
action_59 (70) = happyShift action_75
action_59 (71) = happyShift action_76
action_59 (72) = happyShift action_77
action_59 (98) = happyShift action_137
action_59 (105) = happyShift action_138
action_59 (112) = happyShift action_78
action_59 (13) = happyGoto action_136
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (57) = happyShift action_62
action_60 (58) = happyShift action_63
action_60 (59) = happyShift action_64
action_60 (60) = happyShift action_65
action_60 (61) = happyShift action_66
action_60 (62) = happyShift action_67
action_60 (63) = happyShift action_68
action_60 (64) = happyShift action_69
action_60 (65) = happyShift action_70
action_60 (66) = happyShift action_71
action_60 (67) = happyShift action_72
action_60 (68) = happyShift action_73
action_60 (69) = happyShift action_74
action_60 (70) = happyShift action_75
action_60 (71) = happyShift action_76
action_60 (72) = happyShift action_77
action_60 (111) = happyShift action_135
action_60 (112) = happyShift action_78
action_60 (27) = happyGoto action_134
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (57) = happyShift action_62
action_61 (58) = happyShift action_63
action_61 (59) = happyShift action_64
action_61 (60) = happyShift action_65
action_61 (61) = happyShift action_66
action_61 (62) = happyShift action_67
action_61 (63) = happyShift action_68
action_61 (64) = happyShift action_69
action_61 (65) = happyShift action_70
action_61 (66) = happyShift action_71
action_61 (67) = happyShift action_72
action_61 (68) = happyShift action_73
action_61 (69) = happyShift action_74
action_61 (70) = happyShift action_75
action_61 (71) = happyShift action_76
action_61 (72) = happyShift action_77
action_61 (79) = happyShift action_133
action_61 (112) = happyShift action_78
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (52) = happyShift action_6
action_62 (53) = happyShift action_39
action_62 (54) = happyShift action_40
action_62 (55) = happyShift action_41
action_62 (78) = happyShift action_42
action_62 (81) = happyShift action_43
action_62 (97) = happyShift action_44
action_62 (99) = happyShift action_45
action_62 (101) = happyShift action_46
action_62 (112) = happyShift action_47
action_62 (114) = happyShift action_48
action_62 (8) = happyGoto action_29
action_62 (10) = happyGoto action_30
action_62 (12) = happyGoto action_31
action_62 (14) = happyGoto action_32
action_62 (17) = happyGoto action_33
action_62 (18) = happyGoto action_34
action_62 (25) = happyGoto action_35
action_62 (26) = happyGoto action_36
action_62 (28) = happyGoto action_37
action_62 (31) = happyGoto action_132
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (52) = happyShift action_6
action_63 (53) = happyShift action_39
action_63 (54) = happyShift action_40
action_63 (55) = happyShift action_41
action_63 (78) = happyShift action_42
action_63 (81) = happyShift action_43
action_63 (97) = happyShift action_44
action_63 (99) = happyShift action_45
action_63 (101) = happyShift action_46
action_63 (112) = happyShift action_47
action_63 (114) = happyShift action_48
action_63 (8) = happyGoto action_29
action_63 (10) = happyGoto action_30
action_63 (12) = happyGoto action_31
action_63 (14) = happyGoto action_32
action_63 (17) = happyGoto action_33
action_63 (18) = happyGoto action_34
action_63 (25) = happyGoto action_35
action_63 (26) = happyGoto action_36
action_63 (28) = happyGoto action_37
action_63 (31) = happyGoto action_131
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (52) = happyShift action_6
action_64 (53) = happyShift action_39
action_64 (54) = happyShift action_40
action_64 (55) = happyShift action_41
action_64 (78) = happyShift action_42
action_64 (81) = happyShift action_43
action_64 (97) = happyShift action_44
action_64 (99) = happyShift action_45
action_64 (101) = happyShift action_46
action_64 (112) = happyShift action_47
action_64 (114) = happyShift action_48
action_64 (8) = happyGoto action_29
action_64 (10) = happyGoto action_30
action_64 (12) = happyGoto action_31
action_64 (14) = happyGoto action_32
action_64 (17) = happyGoto action_33
action_64 (18) = happyGoto action_34
action_64 (25) = happyGoto action_35
action_64 (26) = happyGoto action_36
action_64 (28) = happyGoto action_37
action_64 (31) = happyGoto action_130
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (52) = happyShift action_6
action_65 (53) = happyShift action_39
action_65 (54) = happyShift action_40
action_65 (55) = happyShift action_41
action_65 (78) = happyShift action_42
action_65 (81) = happyShift action_43
action_65 (97) = happyShift action_44
action_65 (99) = happyShift action_45
action_65 (101) = happyShift action_46
action_65 (112) = happyShift action_47
action_65 (114) = happyShift action_48
action_65 (8) = happyGoto action_29
action_65 (10) = happyGoto action_30
action_65 (12) = happyGoto action_31
action_65 (14) = happyGoto action_32
action_65 (17) = happyGoto action_33
action_65 (18) = happyGoto action_34
action_65 (25) = happyGoto action_35
action_65 (26) = happyGoto action_36
action_65 (28) = happyGoto action_37
action_65 (31) = happyGoto action_129
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (52) = happyShift action_6
action_66 (53) = happyShift action_39
action_66 (54) = happyShift action_40
action_66 (55) = happyShift action_41
action_66 (78) = happyShift action_42
action_66 (81) = happyShift action_43
action_66 (97) = happyShift action_44
action_66 (99) = happyShift action_45
action_66 (101) = happyShift action_46
action_66 (112) = happyShift action_47
action_66 (114) = happyShift action_48
action_66 (8) = happyGoto action_29
action_66 (10) = happyGoto action_30
action_66 (12) = happyGoto action_31
action_66 (14) = happyGoto action_32
action_66 (17) = happyGoto action_33
action_66 (18) = happyGoto action_34
action_66 (25) = happyGoto action_35
action_66 (26) = happyGoto action_36
action_66 (28) = happyGoto action_37
action_66 (31) = happyGoto action_128
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (52) = happyShift action_6
action_67 (53) = happyShift action_39
action_67 (54) = happyShift action_40
action_67 (55) = happyShift action_41
action_67 (78) = happyShift action_42
action_67 (81) = happyShift action_43
action_67 (97) = happyShift action_44
action_67 (99) = happyShift action_45
action_67 (101) = happyShift action_46
action_67 (112) = happyShift action_47
action_67 (114) = happyShift action_48
action_67 (8) = happyGoto action_29
action_67 (10) = happyGoto action_30
action_67 (12) = happyGoto action_31
action_67 (14) = happyGoto action_32
action_67 (17) = happyGoto action_33
action_67 (18) = happyGoto action_34
action_67 (25) = happyGoto action_35
action_67 (26) = happyGoto action_36
action_67 (28) = happyGoto action_37
action_67 (31) = happyGoto action_127
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (52) = happyShift action_6
action_68 (53) = happyShift action_39
action_68 (54) = happyShift action_40
action_68 (55) = happyShift action_41
action_68 (78) = happyShift action_42
action_68 (81) = happyShift action_43
action_68 (97) = happyShift action_44
action_68 (99) = happyShift action_45
action_68 (101) = happyShift action_46
action_68 (112) = happyShift action_47
action_68 (114) = happyShift action_48
action_68 (8) = happyGoto action_29
action_68 (10) = happyGoto action_30
action_68 (12) = happyGoto action_31
action_68 (14) = happyGoto action_32
action_68 (17) = happyGoto action_33
action_68 (18) = happyGoto action_34
action_68 (25) = happyGoto action_35
action_68 (26) = happyGoto action_36
action_68 (28) = happyGoto action_37
action_68 (31) = happyGoto action_126
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (52) = happyShift action_6
action_69 (53) = happyShift action_39
action_69 (54) = happyShift action_40
action_69 (55) = happyShift action_41
action_69 (78) = happyShift action_42
action_69 (81) = happyShift action_43
action_69 (97) = happyShift action_44
action_69 (99) = happyShift action_45
action_69 (101) = happyShift action_46
action_69 (112) = happyShift action_47
action_69 (114) = happyShift action_48
action_69 (8) = happyGoto action_29
action_69 (10) = happyGoto action_30
action_69 (12) = happyGoto action_31
action_69 (14) = happyGoto action_32
action_69 (17) = happyGoto action_33
action_69 (18) = happyGoto action_34
action_69 (25) = happyGoto action_35
action_69 (26) = happyGoto action_36
action_69 (28) = happyGoto action_37
action_69 (31) = happyGoto action_125
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (52) = happyShift action_6
action_70 (53) = happyShift action_39
action_70 (54) = happyShift action_40
action_70 (55) = happyShift action_41
action_70 (78) = happyShift action_42
action_70 (81) = happyShift action_43
action_70 (97) = happyShift action_44
action_70 (99) = happyShift action_45
action_70 (101) = happyShift action_46
action_70 (112) = happyShift action_47
action_70 (114) = happyShift action_48
action_70 (8) = happyGoto action_29
action_70 (10) = happyGoto action_30
action_70 (12) = happyGoto action_31
action_70 (14) = happyGoto action_32
action_70 (17) = happyGoto action_33
action_70 (18) = happyGoto action_34
action_70 (25) = happyGoto action_35
action_70 (26) = happyGoto action_36
action_70 (28) = happyGoto action_37
action_70 (31) = happyGoto action_124
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (52) = happyShift action_6
action_71 (53) = happyShift action_39
action_71 (54) = happyShift action_40
action_71 (55) = happyShift action_41
action_71 (78) = happyShift action_42
action_71 (81) = happyShift action_43
action_71 (97) = happyShift action_44
action_71 (99) = happyShift action_45
action_71 (101) = happyShift action_46
action_71 (112) = happyShift action_47
action_71 (114) = happyShift action_48
action_71 (8) = happyGoto action_29
action_71 (10) = happyGoto action_30
action_71 (12) = happyGoto action_31
action_71 (14) = happyGoto action_32
action_71 (17) = happyGoto action_33
action_71 (18) = happyGoto action_34
action_71 (25) = happyGoto action_35
action_71 (26) = happyGoto action_36
action_71 (28) = happyGoto action_37
action_71 (31) = happyGoto action_123
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (52) = happyShift action_6
action_72 (53) = happyShift action_39
action_72 (54) = happyShift action_40
action_72 (55) = happyShift action_41
action_72 (78) = happyShift action_42
action_72 (81) = happyShift action_43
action_72 (97) = happyShift action_44
action_72 (99) = happyShift action_45
action_72 (101) = happyShift action_46
action_72 (112) = happyShift action_47
action_72 (114) = happyShift action_48
action_72 (8) = happyGoto action_29
action_72 (10) = happyGoto action_30
action_72 (12) = happyGoto action_31
action_72 (14) = happyGoto action_32
action_72 (17) = happyGoto action_33
action_72 (18) = happyGoto action_34
action_72 (25) = happyGoto action_35
action_72 (26) = happyGoto action_36
action_72 (28) = happyGoto action_37
action_72 (31) = happyGoto action_122
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (52) = happyShift action_6
action_73 (53) = happyShift action_39
action_73 (54) = happyShift action_40
action_73 (55) = happyShift action_41
action_73 (78) = happyShift action_42
action_73 (81) = happyShift action_43
action_73 (97) = happyShift action_44
action_73 (99) = happyShift action_45
action_73 (101) = happyShift action_46
action_73 (112) = happyShift action_47
action_73 (114) = happyShift action_48
action_73 (8) = happyGoto action_29
action_73 (10) = happyGoto action_30
action_73 (12) = happyGoto action_31
action_73 (14) = happyGoto action_32
action_73 (17) = happyGoto action_33
action_73 (18) = happyGoto action_34
action_73 (25) = happyGoto action_35
action_73 (26) = happyGoto action_36
action_73 (28) = happyGoto action_37
action_73 (31) = happyGoto action_121
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (52) = happyShift action_6
action_74 (53) = happyShift action_39
action_74 (54) = happyShift action_40
action_74 (55) = happyShift action_41
action_74 (78) = happyShift action_42
action_74 (81) = happyShift action_43
action_74 (97) = happyShift action_44
action_74 (99) = happyShift action_45
action_74 (101) = happyShift action_46
action_74 (112) = happyShift action_47
action_74 (114) = happyShift action_48
action_74 (8) = happyGoto action_29
action_74 (10) = happyGoto action_30
action_74 (12) = happyGoto action_31
action_74 (14) = happyGoto action_32
action_74 (17) = happyGoto action_33
action_74 (18) = happyGoto action_34
action_74 (25) = happyGoto action_35
action_74 (26) = happyGoto action_36
action_74 (28) = happyGoto action_37
action_74 (31) = happyGoto action_120
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (52) = happyShift action_6
action_75 (53) = happyShift action_39
action_75 (54) = happyShift action_40
action_75 (55) = happyShift action_41
action_75 (78) = happyShift action_42
action_75 (81) = happyShift action_43
action_75 (97) = happyShift action_44
action_75 (99) = happyShift action_45
action_75 (101) = happyShift action_46
action_75 (112) = happyShift action_47
action_75 (114) = happyShift action_48
action_75 (8) = happyGoto action_29
action_75 (10) = happyGoto action_30
action_75 (12) = happyGoto action_31
action_75 (14) = happyGoto action_32
action_75 (17) = happyGoto action_33
action_75 (18) = happyGoto action_34
action_75 (25) = happyGoto action_35
action_75 (26) = happyGoto action_36
action_75 (28) = happyGoto action_37
action_75 (31) = happyGoto action_119
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (52) = happyShift action_6
action_76 (53) = happyShift action_39
action_76 (54) = happyShift action_40
action_76 (55) = happyShift action_41
action_76 (78) = happyShift action_42
action_76 (81) = happyShift action_43
action_76 (97) = happyShift action_44
action_76 (99) = happyShift action_45
action_76 (101) = happyShift action_46
action_76 (112) = happyShift action_47
action_76 (114) = happyShift action_48
action_76 (8) = happyGoto action_29
action_76 (10) = happyGoto action_30
action_76 (12) = happyGoto action_31
action_76 (14) = happyGoto action_32
action_76 (17) = happyGoto action_33
action_76 (18) = happyGoto action_34
action_76 (25) = happyGoto action_35
action_76 (26) = happyGoto action_36
action_76 (28) = happyGoto action_37
action_76 (31) = happyGoto action_118
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (52) = happyShift action_6
action_77 (53) = happyShift action_39
action_77 (54) = happyShift action_40
action_77 (55) = happyShift action_41
action_77 (78) = happyShift action_42
action_77 (81) = happyShift action_43
action_77 (97) = happyShift action_44
action_77 (99) = happyShift action_45
action_77 (101) = happyShift action_46
action_77 (112) = happyShift action_47
action_77 (114) = happyShift action_48
action_77 (8) = happyGoto action_29
action_77 (10) = happyGoto action_30
action_77 (12) = happyGoto action_31
action_77 (14) = happyGoto action_32
action_77 (17) = happyGoto action_33
action_77 (18) = happyGoto action_34
action_77 (25) = happyGoto action_35
action_77 (26) = happyGoto action_36
action_77 (28) = happyGoto action_37
action_77 (31) = happyGoto action_117
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (52) = happyShift action_6
action_78 (8) = happyGoto action_116
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (52) = happyShift action_6
action_79 (53) = happyShift action_39
action_79 (54) = happyShift action_40
action_79 (55) = happyShift action_41
action_79 (56) = happyShift action_115
action_79 (97) = happyShift action_44
action_79 (99) = happyShift action_45
action_79 (101) = happyShift action_46
action_79 (8) = happyGoto action_29
action_79 (10) = happyGoto action_30
action_79 (12) = happyGoto action_31
action_79 (14) = happyGoto action_32
action_79 (25) = happyGoto action_35
action_79 (26) = happyGoto action_114
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (52) = happyShift action_6
action_80 (108) = happyShift action_113
action_80 (8) = happyGoto action_112
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (106) = happyShift action_85
action_81 _ = happyReduce_103

action_82 (103) = happyShift action_111
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (102) = happyShift action_110
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (98) = happyShift action_108
action_84 (105) = happyShift action_109
action_84 (106) = happyShift action_85
action_84 (34) = happyGoto action_107
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (52) = happyShift action_6
action_85 (53) = happyShift action_22
action_85 (54) = happyShift action_23
action_85 (55) = happyShift action_24
action_85 (97) = happyShift action_25
action_85 (101) = happyShift action_26
action_85 (111) = happyShift action_27
action_85 (114) = happyShift action_28
action_85 (8) = happyGoto action_14
action_85 (33) = happyGoto action_15
action_85 (35) = happyGoto action_16
action_85 (36) = happyGoto action_17
action_85 (37) = happyGoto action_18
action_85 (41) = happyGoto action_19
action_85 (42) = happyGoto action_20
action_85 (43) = happyGoto action_106
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (52) = happyShift action_6
action_86 (53) = happyShift action_22
action_86 (54) = happyShift action_23
action_86 (55) = happyShift action_24
action_86 (97) = happyShift action_25
action_86 (101) = happyShift action_26
action_86 (111) = happyShift action_27
action_86 (114) = happyShift action_28
action_86 (8) = happyGoto action_14
action_86 (33) = happyGoto action_15
action_86 (35) = happyGoto action_16
action_86 (36) = happyGoto action_17
action_86 (37) = happyGoto action_18
action_86 (41) = happyGoto action_19
action_86 (42) = happyGoto action_20
action_86 (43) = happyGoto action_105
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (52) = happyShift action_6
action_87 (53) = happyShift action_22
action_87 (54) = happyShift action_23
action_87 (55) = happyShift action_24
action_87 (56) = happyShift action_104
action_87 (97) = happyShift action_25
action_87 (101) = happyShift action_26
action_87 (8) = happyGoto action_102
action_87 (33) = happyGoto action_15
action_87 (35) = happyGoto action_16
action_87 (36) = happyGoto action_17
action_87 (37) = happyGoto action_103
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (52) = happyShift action_6
action_88 (53) = happyShift action_22
action_88 (54) = happyShift action_23
action_88 (55) = happyShift action_24
action_88 (97) = happyShift action_25
action_88 (101) = happyShift action_26
action_88 (111) = happyShift action_27
action_88 (114) = happyShift action_28
action_88 (8) = happyGoto action_14
action_88 (33) = happyGoto action_15
action_88 (35) = happyGoto action_16
action_88 (36) = happyGoto action_17
action_88 (37) = happyGoto action_18
action_88 (41) = happyGoto action_19
action_88 (42) = happyGoto action_20
action_88 (43) = happyGoto action_101
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (98) = happyShift action_100
action_89 (106) = happyShift action_90
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (52) = happyShift action_6
action_90 (97) = happyShift action_13
action_90 (8) = happyGoto action_11
action_90 (45) = happyGoto action_99
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (113) = happyShift action_97
action_91 (46) = happyGoto action_98
action_91 _ = happyReduce_118

action_92 (113) = happyShift action_97
action_92 (46) = happyGoto action_96
action_92 _ = happyReduce_118

action_93 (103) = happyShift action_95
action_93 (44) = happyGoto action_94
action_93 _ = happyReduce_111

action_94 (113) = happyShift action_97
action_94 (46) = happyGoto action_167
action_94 _ = happyReduce_118

action_95 (52) = happyShift action_6
action_95 (53) = happyShift action_22
action_95 (54) = happyShift action_23
action_95 (55) = happyShift action_24
action_95 (93) = happyShift action_166
action_95 (97) = happyShift action_25
action_95 (101) = happyShift action_26
action_95 (111) = happyShift action_27
action_95 (114) = happyShift action_28
action_95 (8) = happyGoto action_14
action_95 (33) = happyGoto action_15
action_95 (35) = happyGoto action_16
action_95 (36) = happyGoto action_17
action_95 (37) = happyGoto action_18
action_95 (41) = happyGoto action_19
action_95 (42) = happyGoto action_20
action_95 (43) = happyGoto action_165
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (110) = happyShift action_164
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (52) = happyShift action_6
action_97 (97) = happyShift action_13
action_97 (8) = happyGoto action_11
action_97 (45) = happyGoto action_163
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (110) = happyShift action_162
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_115

action_100 _ = happyReduce_116

action_101 (74) = happyReduce_102
action_101 (76) = happyReduce_102
action_101 (83) = happyReduce_102
action_101 (84) = happyReduce_102
action_101 (98) = happyReduce_102
action_101 (102) = happyReduce_102
action_101 (105) = happyReduce_102
action_101 (106) = happyShift action_85
action_101 (110) = happyReduce_102
action_101 (111) = happyReduce_102
action_101 (113) = happyReduce_102
action_101 (117) = happyReduce_102
action_101 _ = happyReduce_102

action_102 (52) = happyReduce_92
action_102 (53) = happyReduce_92
action_102 (54) = happyReduce_92
action_102 (55) = happyReduce_92
action_102 (56) = happyReduce_92
action_102 (97) = happyReduce_92
action_102 (101) = happyReduce_92
action_102 _ = happyReduce_92

action_103 _ = happyReduce_95

action_104 _ = happyReduce_110

action_105 (106) = happyShift action_85
action_105 _ = happyReduce_104

action_106 (74) = happyReduce_108
action_106 (76) = happyReduce_108
action_106 (83) = happyReduce_108
action_106 (84) = happyReduce_108
action_106 (98) = happyReduce_108
action_106 (102) = happyReduce_108
action_106 (105) = happyReduce_108
action_106 (106) = happyShift action_85
action_106 (110) = happyReduce_108
action_106 (111) = happyReduce_108
action_106 (113) = happyReduce_108
action_106 (117) = happyReduce_108
action_106 _ = happyReduce_108

action_107 (98) = happyShift action_161
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_93

action_109 (52) = happyShift action_6
action_109 (53) = happyShift action_22
action_109 (54) = happyShift action_23
action_109 (55) = happyShift action_24
action_109 (97) = happyShift action_25
action_109 (101) = happyShift action_26
action_109 (111) = happyShift action_27
action_109 (114) = happyShift action_28
action_109 (8) = happyGoto action_14
action_109 (33) = happyGoto action_15
action_109 (35) = happyGoto action_16
action_109 (36) = happyGoto action_17
action_109 (37) = happyGoto action_18
action_109 (41) = happyGoto action_19
action_109 (42) = happyGoto action_20
action_109 (43) = happyGoto action_160
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_82

action_111 (52) = happyShift action_6
action_111 (53) = happyShift action_22
action_111 (54) = happyShift action_23
action_111 (55) = happyShift action_24
action_111 (97) = happyShift action_25
action_111 (101) = happyShift action_26
action_111 (111) = happyShift action_27
action_111 (114) = happyShift action_28
action_111 (8) = happyGoto action_14
action_111 (33) = happyGoto action_15
action_111 (35) = happyGoto action_16
action_111 (36) = happyGoto action_17
action_111 (37) = happyGoto action_18
action_111 (41) = happyGoto action_19
action_111 (42) = happyGoto action_20
action_111 (43) = happyGoto action_159
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_18

action_113 (52) = happyShift action_6
action_113 (53) = happyShift action_22
action_113 (54) = happyShift action_23
action_113 (55) = happyShift action_24
action_113 (97) = happyShift action_25
action_113 (101) = happyShift action_26
action_113 (111) = happyShift action_27
action_113 (114) = happyShift action_28
action_113 (8) = happyGoto action_14
action_113 (33) = happyGoto action_15
action_113 (35) = happyGoto action_16
action_113 (36) = happyGoto action_17
action_113 (37) = happyGoto action_18
action_113 (41) = happyGoto action_19
action_113 (42) = happyGoto action_20
action_113 (43) = happyGoto action_158
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_20

action_115 _ = happyReduce_21

action_116 _ = happyReduce_62

action_117 (57) = happyShift action_62
action_117 (58) = happyShift action_63
action_117 (59) = happyShift action_64
action_117 (60) = happyShift action_65
action_117 (61) = happyShift action_66
action_117 (62) = happyShift action_67
action_117 (63) = happyFail []
action_117 (64) = happyFail []
action_117 (65) = happyFail []
action_117 (66) = happyFail []
action_117 (67) = happyFail []
action_117 (68) = happyFail []
action_117 (69) = happyFail []
action_117 (70) = happyFail []
action_117 (71) = happyFail []
action_117 (72) = happyFail []
action_117 _ = happyReduce_77

action_118 (57) = happyShift action_62
action_118 (58) = happyShift action_63
action_118 (59) = happyShift action_64
action_118 (60) = happyShift action_65
action_118 (61) = happyShift action_66
action_118 (62) = happyShift action_67
action_118 (63) = happyFail []
action_118 (64) = happyFail []
action_118 (65) = happyFail []
action_118 (66) = happyFail []
action_118 (67) = happyFail []
action_118 (68) = happyFail []
action_118 (69) = happyFail []
action_118 (70) = happyFail []
action_118 (71) = happyFail []
action_118 (72) = happyFail []
action_118 _ = happyReduce_76

action_119 (57) = happyShift action_62
action_119 (58) = happyShift action_63
action_119 (59) = happyShift action_64
action_119 (60) = happyShift action_65
action_119 (61) = happyShift action_66
action_119 (62) = happyShift action_67
action_119 (63) = happyFail []
action_119 (64) = happyFail []
action_119 (65) = happyFail []
action_119 (66) = happyFail []
action_119 (67) = happyFail []
action_119 (68) = happyFail []
action_119 (69) = happyFail []
action_119 (70) = happyFail []
action_119 (71) = happyFail []
action_119 (72) = happyFail []
action_119 _ = happyReduce_69

action_120 (57) = happyShift action_62
action_120 (58) = happyShift action_63
action_120 (59) = happyShift action_64
action_120 (60) = happyShift action_65
action_120 (61) = happyShift action_66
action_120 (62) = happyShift action_67
action_120 (63) = happyFail []
action_120 (64) = happyFail []
action_120 (65) = happyFail []
action_120 (66) = happyFail []
action_120 (67) = happyFail []
action_120 (68) = happyFail []
action_120 (69) = happyFail []
action_120 (70) = happyFail []
action_120 (71) = happyFail []
action_120 (72) = happyFail []
action_120 _ = happyReduce_68

action_121 (57) = happyShift action_62
action_121 (58) = happyShift action_63
action_121 (59) = happyShift action_64
action_121 (60) = happyShift action_65
action_121 (61) = happyShift action_66
action_121 (62) = happyShift action_67
action_121 (63) = happyFail []
action_121 (64) = happyFail []
action_121 (65) = happyFail []
action_121 (66) = happyFail []
action_121 (67) = happyFail []
action_121 (68) = happyFail []
action_121 (69) = happyFail []
action_121 (70) = happyFail []
action_121 (71) = happyFail []
action_121 (72) = happyFail []
action_121 _ = happyReduce_75

action_122 (57) = happyShift action_62
action_122 (58) = happyShift action_63
action_122 (59) = happyShift action_64
action_122 (60) = happyShift action_65
action_122 (61) = happyShift action_66
action_122 (62) = happyShift action_67
action_122 (63) = happyFail []
action_122 (64) = happyFail []
action_122 (65) = happyFail []
action_122 (66) = happyFail []
action_122 (67) = happyFail []
action_122 (68) = happyFail []
action_122 (69) = happyFail []
action_122 (70) = happyFail []
action_122 (71) = happyFail []
action_122 (72) = happyFail []
action_122 _ = happyReduce_73

action_123 (57) = happyShift action_62
action_123 (58) = happyShift action_63
action_123 (59) = happyShift action_64
action_123 (60) = happyShift action_65
action_123 (61) = happyShift action_66
action_123 (62) = happyShift action_67
action_123 (63) = happyFail []
action_123 (64) = happyFail []
action_123 (65) = happyFail []
action_123 (66) = happyFail []
action_123 (67) = happyFail []
action_123 (68) = happyFail []
action_123 (69) = happyFail []
action_123 (70) = happyFail []
action_123 (71) = happyFail []
action_123 (72) = happyFail []
action_123 _ = happyReduce_74

action_124 (57) = happyShift action_62
action_124 (58) = happyShift action_63
action_124 (59) = happyShift action_64
action_124 (60) = happyShift action_65
action_124 (61) = happyShift action_66
action_124 (62) = happyShift action_67
action_124 (63) = happyFail []
action_124 (64) = happyFail []
action_124 (65) = happyFail []
action_124 (66) = happyFail []
action_124 (67) = happyFail []
action_124 (68) = happyFail []
action_124 (69) = happyFail []
action_124 (70) = happyFail []
action_124 (71) = happyFail []
action_124 (72) = happyFail []
action_124 _ = happyReduce_72

action_125 (57) = happyShift action_62
action_125 (58) = happyShift action_63
action_125 (59) = happyShift action_64
action_125 (60) = happyShift action_65
action_125 (61) = happyShift action_66
action_125 (62) = happyShift action_67
action_125 (63) = happyFail []
action_125 (64) = happyFail []
action_125 (65) = happyFail []
action_125 (66) = happyFail []
action_125 (67) = happyFail []
action_125 (68) = happyFail []
action_125 (69) = happyFail []
action_125 (70) = happyFail []
action_125 (71) = happyFail []
action_125 (72) = happyFail []
action_125 _ = happyReduce_71

action_126 (57) = happyShift action_62
action_126 (58) = happyShift action_63
action_126 (59) = happyShift action_64
action_126 (60) = happyShift action_65
action_126 (61) = happyShift action_66
action_126 (62) = happyShift action_67
action_126 (63) = happyFail []
action_126 (64) = happyFail []
action_126 (65) = happyFail []
action_126 (66) = happyFail []
action_126 (67) = happyFail []
action_126 (68) = happyFail []
action_126 (69) = happyFail []
action_126 (70) = happyFail []
action_126 (71) = happyFail []
action_126 (72) = happyFail []
action_126 _ = happyReduce_70

action_127 (59) = happyShift action_64
action_127 (60) = happyShift action_65
action_127 (61) = happyShift action_66
action_127 _ = happyReduce_78

action_128 _ = happyReduce_67

action_129 (61) = happyShift action_66
action_129 _ = happyReduce_66

action_130 (61) = happyShift action_66
action_130 _ = happyReduce_65

action_131 (59) = happyShift action_64
action_131 (60) = happyShift action_65
action_131 (61) = happyShift action_66
action_131 _ = happyReduce_64

action_132 (59) = happyShift action_64
action_132 (60) = happyShift action_65
action_132 (61) = happyShift action_66
action_132 _ = happyReduce_63

action_133 (52) = happyShift action_6
action_133 (53) = happyShift action_39
action_133 (54) = happyShift action_40
action_133 (55) = happyShift action_41
action_133 (78) = happyShift action_42
action_133 (81) = happyShift action_43
action_133 (97) = happyShift action_44
action_133 (99) = happyShift action_45
action_133 (101) = happyShift action_46
action_133 (112) = happyShift action_47
action_133 (114) = happyShift action_48
action_133 (8) = happyGoto action_29
action_133 (10) = happyGoto action_30
action_133 (12) = happyGoto action_31
action_133 (14) = happyGoto action_32
action_133 (17) = happyGoto action_33
action_133 (18) = happyGoto action_34
action_133 (25) = happyGoto action_35
action_133 (26) = happyGoto action_36
action_133 (28) = happyGoto action_37
action_133 (31) = happyGoto action_157
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (57) = happyReduce_52
action_134 (58) = happyReduce_52
action_134 (59) = happyReduce_52
action_134 (60) = happyReduce_52
action_134 (61) = happyReduce_52
action_134 (62) = happyReduce_52
action_134 (63) = happyReduce_52
action_134 (64) = happyReduce_52
action_134 (65) = happyReduce_52
action_134 (66) = happyReduce_52
action_134 (67) = happyReduce_52
action_134 (68) = happyReduce_52
action_134 (69) = happyReduce_52
action_134 (70) = happyReduce_52
action_134 (71) = happyReduce_52
action_134 (72) = happyReduce_52
action_134 (74) = happyReduce_52
action_134 (76) = happyReduce_52
action_134 (79) = happyReduce_52
action_134 (80) = happyReduce_52
action_134 (83) = happyReduce_52
action_134 (84) = happyReduce_52
action_134 (98) = happyReduce_52
action_134 (100) = happyReduce_52
action_134 (102) = happyReduce_52
action_134 (105) = happyReduce_52
action_134 (111) = happyShift action_156
action_134 (112) = happyReduce_52
action_134 (117) = happyReduce_52
action_134 _ = happyReduce_52

action_135 (52) = happyShift action_6
action_135 (53) = happyShift action_39
action_135 (54) = happyShift action_40
action_135 (55) = happyShift action_41
action_135 (97) = happyShift action_153
action_135 (99) = happyShift action_154
action_135 (101) = happyShift action_155
action_135 (8) = happyGoto action_149
action_135 (23) = happyGoto action_150
action_135 (24) = happyGoto action_151
action_135 (25) = happyGoto action_152
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (98) = happyShift action_148
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_49

action_138 (52) = happyShift action_6
action_138 (53) = happyShift action_39
action_138 (54) = happyShift action_40
action_138 (55) = happyShift action_41
action_138 (78) = happyShift action_42
action_138 (81) = happyShift action_43
action_138 (97) = happyShift action_44
action_138 (99) = happyShift action_45
action_138 (101) = happyShift action_46
action_138 (112) = happyShift action_47
action_138 (114) = happyShift action_48
action_138 (8) = happyGoto action_29
action_138 (10) = happyGoto action_30
action_138 (12) = happyGoto action_31
action_138 (14) = happyGoto action_32
action_138 (17) = happyGoto action_33
action_138 (18) = happyGoto action_34
action_138 (25) = happyGoto action_35
action_138 (26) = happyGoto action_36
action_138 (28) = happyGoto action_37
action_138 (31) = happyGoto action_147
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (52) = happyShift action_6
action_139 (53) = happyShift action_39
action_139 (54) = happyShift action_40
action_139 (55) = happyShift action_41
action_139 (78) = happyShift action_42
action_139 (81) = happyShift action_43
action_139 (97) = happyShift action_44
action_139 (99) = happyShift action_45
action_139 (101) = happyShift action_46
action_139 (112) = happyShift action_47
action_139 (114) = happyShift action_48
action_139 (8) = happyGoto action_29
action_139 (10) = happyGoto action_30
action_139 (11) = happyGoto action_146
action_139 (12) = happyGoto action_31
action_139 (14) = happyGoto action_32
action_139 (17) = happyGoto action_33
action_139 (18) = happyGoto action_34
action_139 (25) = happyGoto action_35
action_139 (26) = happyGoto action_36
action_139 (28) = happyGoto action_37
action_139 (31) = happyGoto action_58
action_139 _ = happyReduce_10

action_140 _ = happyReduce_13

action_141 _ = happyReduce_9

action_142 (52) = happyShift action_6
action_142 (53) = happyShift action_39
action_142 (54) = happyShift action_40
action_142 (55) = happyShift action_41
action_142 (78) = happyShift action_42
action_142 (81) = happyShift action_43
action_142 (97) = happyShift action_44
action_142 (99) = happyShift action_45
action_142 (101) = happyShift action_46
action_142 (112) = happyShift action_47
action_142 (114) = happyShift action_48
action_142 (8) = happyGoto action_29
action_142 (10) = happyGoto action_30
action_142 (12) = happyGoto action_31
action_142 (14) = happyGoto action_32
action_142 (17) = happyGoto action_33
action_142 (18) = happyGoto action_34
action_142 (25) = happyGoto action_35
action_142 (26) = happyGoto action_36
action_142 (28) = happyGoto action_37
action_142 (31) = happyGoto action_145
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (52) = happyShift action_6
action_143 (53) = happyShift action_39
action_143 (54) = happyShift action_40
action_143 (55) = happyShift action_41
action_143 (78) = happyShift action_42
action_143 (81) = happyShift action_43
action_143 (97) = happyShift action_44
action_143 (99) = happyShift action_45
action_143 (101) = happyShift action_46
action_143 (112) = happyShift action_47
action_143 (114) = happyShift action_48
action_143 (8) = happyGoto action_29
action_143 (10) = happyGoto action_30
action_143 (12) = happyGoto action_31
action_143 (14) = happyGoto action_32
action_143 (17) = happyGoto action_33
action_143 (18) = happyGoto action_34
action_143 (25) = happyGoto action_35
action_143 (26) = happyGoto action_36
action_143 (28) = happyGoto action_37
action_143 (31) = happyGoto action_144
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (57) = happyShift action_62
action_144 (58) = happyShift action_63
action_144 (59) = happyShift action_64
action_144 (60) = happyShift action_65
action_144 (61) = happyShift action_66
action_144 (62) = happyShift action_67
action_144 (63) = happyShift action_68
action_144 (64) = happyShift action_69
action_144 (65) = happyShift action_70
action_144 (66) = happyShift action_71
action_144 (67) = happyShift action_72
action_144 (68) = happyShift action_73
action_144 (69) = happyShift action_74
action_144 (70) = happyShift action_75
action_144 (71) = happyShift action_76
action_144 (72) = happyShift action_77
action_144 (112) = happyShift action_78
action_144 _ = happyReduce_59

action_145 (57) = happyShift action_62
action_145 (58) = happyShift action_63
action_145 (59) = happyShift action_64
action_145 (60) = happyShift action_65
action_145 (61) = happyShift action_66
action_145 (62) = happyShift action_67
action_145 (63) = happyShift action_68
action_145 (64) = happyShift action_69
action_145 (65) = happyShift action_70
action_145 (66) = happyShift action_71
action_145 (67) = happyShift action_72
action_145 (68) = happyShift action_73
action_145 (69) = happyShift action_74
action_145 (70) = happyShift action_75
action_145 (71) = happyShift action_76
action_145 (72) = happyShift action_77
action_145 (105) = happyShift action_189
action_145 (112) = happyShift action_78
action_145 _ = happyReduce_8

action_146 _ = happyReduce_12

action_147 (57) = happyShift action_62
action_147 (58) = happyShift action_63
action_147 (59) = happyShift action_64
action_147 (60) = happyShift action_65
action_147 (61) = happyShift action_66
action_147 (62) = happyShift action_67
action_147 (63) = happyShift action_68
action_147 (64) = happyShift action_69
action_147 (65) = happyShift action_70
action_147 (66) = happyShift action_71
action_147 (67) = happyShift action_72
action_147 (68) = happyShift action_73
action_147 (69) = happyShift action_74
action_147 (70) = happyShift action_75
action_147 (71) = happyShift action_76
action_147 (72) = happyShift action_77
action_147 (105) = happyShift action_138
action_147 (112) = happyShift action_78
action_147 (13) = happyGoto action_188
action_147 _ = happyReduce_14

action_148 _ = happyReduce_16

action_149 (103) = happyShift action_187
action_149 _ = happyReduce_34

action_150 (52) = happyShift action_6
action_150 (8) = happyGoto action_186
action_150 _ = happyReduce_40

action_151 (106) = happyShift action_185
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_35

action_153 (52) = happyShift action_6
action_153 (8) = happyGoto action_184
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (52) = happyShift action_6
action_154 (100) = happyShift action_183
action_154 (8) = happyGoto action_181
action_154 (19) = happyGoto action_182
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (52) = happyShift action_6
action_155 (8) = happyGoto action_179
action_155 (21) = happyGoto action_180
action_155 _ = happyReduce_27

action_156 (52) = happyShift action_6
action_156 (53) = happyShift action_39
action_156 (54) = happyShift action_40
action_156 (55) = happyShift action_41
action_156 (97) = happyShift action_153
action_156 (99) = happyShift action_154
action_156 (101) = happyShift action_155
action_156 (8) = happyGoto action_149
action_156 (23) = happyGoto action_150
action_156 (24) = happyGoto action_178
action_156 (25) = happyGoto action_152
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (57) = happyShift action_62
action_157 (58) = happyShift action_63
action_157 (59) = happyShift action_64
action_157 (60) = happyShift action_65
action_157 (61) = happyShift action_66
action_157 (62) = happyShift action_67
action_157 (63) = happyShift action_68
action_157 (64) = happyShift action_69
action_157 (65) = happyShift action_70
action_157 (66) = happyShift action_71
action_157 (67) = happyShift action_72
action_157 (68) = happyShift action_73
action_157 (69) = happyShift action_74
action_157 (70) = happyShift action_75
action_157 (71) = happyShift action_76
action_157 (72) = happyShift action_77
action_157 (80) = happyShift action_177
action_157 (112) = happyShift action_78
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (74) = happyReduce_109
action_158 (76) = happyReduce_109
action_158 (83) = happyReduce_109
action_158 (84) = happyReduce_109
action_158 (98) = happyReduce_109
action_158 (102) = happyReduce_109
action_158 (105) = happyReduce_109
action_158 (106) = happyShift action_85
action_158 (110) = happyReduce_109
action_158 (111) = happyReduce_109
action_158 (113) = happyReduce_109
action_158 (117) = happyReduce_109
action_158 _ = happyReduce_109

action_159 (105) = happyShift action_176
action_159 (106) = happyShift action_85
action_159 _ = happyReduce_81

action_160 (105) = happyShift action_109
action_160 (106) = happyShift action_85
action_160 (34) = happyGoto action_175
action_160 _ = happyReduce_83

action_161 _ = happyReduce_85

action_162 (52) = happyShift action_6
action_162 (53) = happyShift action_22
action_162 (54) = happyShift action_23
action_162 (55) = happyShift action_24
action_162 (97) = happyShift action_25
action_162 (101) = happyShift action_26
action_162 (111) = happyShift action_27
action_162 (114) = happyShift action_28
action_162 (8) = happyGoto action_14
action_162 (33) = happyGoto action_15
action_162 (35) = happyGoto action_16
action_162 (36) = happyGoto action_17
action_162 (37) = happyGoto action_18
action_162 (41) = happyGoto action_19
action_162 (42) = happyGoto action_20
action_162 (43) = happyGoto action_174
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (106) = happyShift action_90
action_163 _ = happyReduce_119

action_164 (52) = happyShift action_6
action_164 (8) = happyGoto action_171
action_164 (47) = happyGoto action_172
action_164 (48) = happyGoto action_173
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (76) = happyShift action_170
action_165 (106) = happyShift action_85
action_165 _ = happyReduce_112

action_166 (52) = happyShift action_6
action_166 (8) = happyGoto action_169
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (110) = happyShift action_168
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (52) = happyShift action_6
action_168 (53) = happyShift action_39
action_168 (54) = happyShift action_40
action_168 (55) = happyShift action_41
action_168 (78) = happyShift action_42
action_168 (81) = happyShift action_43
action_168 (97) = happyShift action_44
action_168 (99) = happyShift action_45
action_168 (101) = happyShift action_46
action_168 (112) = happyShift action_47
action_168 (114) = happyShift action_48
action_168 (8) = happyGoto action_29
action_168 (10) = happyGoto action_30
action_168 (12) = happyGoto action_31
action_168 (14) = happyGoto action_32
action_168 (17) = happyGoto action_33
action_168 (18) = happyGoto action_34
action_168 (25) = happyGoto action_35
action_168 (26) = happyGoto action_36
action_168 (28) = happyGoto action_37
action_168 (31) = happyGoto action_210
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (103) = happyShift action_209
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (52) = happyShift action_6
action_170 (8) = happyGoto action_206
action_170 (39) = happyGoto action_207
action_170 (40) = happyGoto action_208
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (103) = happyShift action_205
action_171 _ = happyFail (happyExpListPerState 171)

action_172 _ = happyReduce_121

action_173 (76) = happyShift action_203
action_173 (111) = happyShift action_204
action_173 _ = happyReduce_125

action_174 (76) = happyShift action_202
action_174 (106) = happyShift action_85
action_174 _ = happyReduce_127

action_175 _ = happyReduce_84

action_176 (52) = happyShift action_6
action_176 (8) = happyGoto action_82
action_176 (32) = happyGoto action_201
action_176 _ = happyReduce_79

action_177 (52) = happyShift action_6
action_177 (53) = happyShift action_39
action_177 (54) = happyShift action_40
action_177 (55) = happyShift action_41
action_177 (78) = happyShift action_42
action_177 (81) = happyShift action_43
action_177 (97) = happyShift action_44
action_177 (99) = happyShift action_45
action_177 (101) = happyShift action_46
action_177 (112) = happyShift action_47
action_177 (114) = happyShift action_48
action_177 (8) = happyGoto action_29
action_177 (10) = happyGoto action_30
action_177 (12) = happyGoto action_31
action_177 (14) = happyGoto action_32
action_177 (17) = happyGoto action_33
action_177 (18) = happyGoto action_34
action_177 (25) = happyGoto action_35
action_177 (26) = happyGoto action_36
action_177 (28) = happyGoto action_37
action_177 (31) = happyGoto action_200
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (106) = happyShift action_199
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (105) = happyShift action_198
action_179 _ = happyReduce_28

action_180 (111) = happyShift action_195
action_180 (22) = happyGoto action_197
action_180 _ = happyReduce_30

action_181 (105) = happyShift action_196
action_181 _ = happyReduce_23

action_182 (111) = happyShift action_195
action_182 (22) = happyGoto action_194
action_182 _ = happyReduce_30

action_183 _ = happyReduce_37

action_184 (105) = happyShift action_193
action_184 (20) = happyGoto action_192
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (52) = happyShift action_6
action_185 (53) = happyShift action_39
action_185 (54) = happyShift action_40
action_185 (55) = happyShift action_41
action_185 (78) = happyShift action_42
action_185 (81) = happyShift action_43
action_185 (97) = happyShift action_44
action_185 (99) = happyShift action_45
action_185 (101) = happyShift action_46
action_185 (112) = happyShift action_47
action_185 (114) = happyShift action_48
action_185 (8) = happyGoto action_29
action_185 (10) = happyGoto action_30
action_185 (12) = happyGoto action_31
action_185 (14) = happyGoto action_32
action_185 (17) = happyGoto action_33
action_185 (18) = happyGoto action_34
action_185 (25) = happyGoto action_35
action_185 (26) = happyGoto action_36
action_185 (28) = happyGoto action_37
action_185 (31) = happyGoto action_191
action_185 _ = happyFail (happyExpListPerState 185)

action_186 _ = happyReduce_33

action_187 _ = happyReduce_32

action_188 _ = happyReduce_15

action_189 (52) = happyShift action_6
action_189 (8) = happyGoto action_55
action_189 (9) = happyGoto action_190
action_189 _ = happyReduce_6

action_190 _ = happyReduce_7

action_191 (57) = happyShift action_62
action_191 (58) = happyShift action_63
action_191 (59) = happyShift action_64
action_191 (60) = happyShift action_65
action_191 (61) = happyShift action_66
action_191 (62) = happyShift action_67
action_191 (63) = happyShift action_68
action_191 (64) = happyShift action_69
action_191 (65) = happyShift action_70
action_191 (66) = happyShift action_71
action_191 (67) = happyShift action_72
action_191 (68) = happyShift action_73
action_191 (69) = happyShift action_74
action_191 (70) = happyShift action_75
action_191 (71) = happyShift action_76
action_191 (72) = happyShift action_77
action_191 (112) = happyShift action_78
action_191 _ = happyReduce_50

action_192 (98) = happyShift action_229
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (52) = happyShift action_6
action_193 (8) = happyGoto action_228
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (100) = happyShift action_227
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (52) = happyShift action_6
action_195 (8) = happyGoto action_226
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (52) = happyShift action_6
action_196 (8) = happyGoto action_181
action_196 (19) = happyGoto action_225
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (102) = happyShift action_224
action_197 _ = happyFail (happyExpListPerState 197)

action_198 (52) = happyShift action_6
action_198 (8) = happyGoto action_179
action_198 (21) = happyGoto action_223
action_198 _ = happyReduce_27

action_199 (52) = happyShift action_6
action_199 (53) = happyShift action_39
action_199 (54) = happyShift action_40
action_199 (55) = happyShift action_41
action_199 (78) = happyShift action_42
action_199 (81) = happyShift action_43
action_199 (97) = happyShift action_44
action_199 (99) = happyShift action_45
action_199 (101) = happyShift action_46
action_199 (112) = happyShift action_47
action_199 (114) = happyShift action_48
action_199 (8) = happyGoto action_29
action_199 (10) = happyGoto action_30
action_199 (12) = happyGoto action_31
action_199 (14) = happyGoto action_32
action_199 (17) = happyGoto action_33
action_199 (18) = happyGoto action_34
action_199 (25) = happyGoto action_35
action_199 (26) = happyGoto action_36
action_199 (28) = happyGoto action_37
action_199 (31) = happyGoto action_222
action_199 _ = happyFail (happyExpListPerState 199)

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
action_200 (69) = happyShift action_74
action_200 (70) = happyShift action_75
action_200 (71) = happyShift action_76
action_200 (72) = happyShift action_77
action_200 (112) = happyShift action_78
action_200 _ = happyReduce_22

action_201 _ = happyReduce_80

action_202 (52) = happyShift action_6
action_202 (8) = happyGoto action_206
action_202 (39) = happyGoto action_207
action_202 (40) = happyGoto action_221
action_202 _ = happyFail (happyExpListPerState 202)

action_203 (52) = happyShift action_6
action_203 (8) = happyGoto action_206
action_203 (39) = happyGoto action_207
action_203 (40) = happyGoto action_220
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (52) = happyShift action_6
action_204 (8) = happyGoto action_171
action_204 (47) = happyGoto action_219
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (52) = happyShift action_6
action_205 (53) = happyShift action_22
action_205 (54) = happyShift action_23
action_205 (55) = happyShift action_24
action_205 (97) = happyShift action_25
action_205 (101) = happyShift action_26
action_205 (111) = happyShift action_27
action_205 (114) = happyShift action_28
action_205 (8) = happyGoto action_14
action_205 (33) = happyGoto action_15
action_205 (35) = happyGoto action_16
action_205 (36) = happyGoto action_17
action_205 (37) = happyGoto action_18
action_205 (41) = happyGoto action_19
action_205 (42) = happyGoto action_20
action_205 (43) = happyGoto action_218
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (94) = happyShift action_214
action_206 (109) = happyShift action_215
action_206 (110) = happyShift action_216
action_206 (111) = happyShift action_217
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_100

action_208 (105) = happyShift action_213
action_208 _ = happyReduce_113

action_209 (52) = happyShift action_6
action_209 (53) = happyShift action_22
action_209 (54) = happyShift action_23
action_209 (55) = happyShift action_24
action_209 (97) = happyShift action_25
action_209 (101) = happyShift action_26
action_209 (111) = happyShift action_27
action_209 (114) = happyShift action_28
action_209 (8) = happyGoto action_14
action_209 (33) = happyGoto action_15
action_209 (35) = happyGoto action_16
action_209 (36) = happyGoto action_17
action_209 (37) = happyGoto action_18
action_209 (41) = happyGoto action_19
action_209 (42) = happyGoto action_20
action_209 (43) = happyGoto action_212
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (57) = happyShift action_62
action_210 (58) = happyShift action_63
action_210 (59) = happyShift action_64
action_210 (60) = happyShift action_65
action_210 (61) = happyShift action_66
action_210 (62) = happyShift action_67
action_210 (63) = happyShift action_68
action_210 (64) = happyShift action_69
action_210 (65) = happyShift action_70
action_210 (66) = happyShift action_71
action_210 (67) = happyShift action_72
action_210 (68) = happyShift action_73
action_210 (69) = happyShift action_74
action_210 (70) = happyShift action_75
action_210 (71) = happyShift action_76
action_210 (72) = happyShift action_77
action_210 (76) = happyShift action_211
action_210 (112) = happyShift action_78
action_210 _ = happyReduce_123

action_211 (52) = happyShift action_6
action_211 (8) = happyGoto action_236
action_211 (29) = happyGoto action_237
action_211 (30) = happyGoto action_238
action_211 _ = happyFail (happyExpListPerState 211)

action_212 (106) = happyShift action_85
action_212 _ = happyReduce_114

action_213 (52) = happyShift action_6
action_213 (8) = happyGoto action_206
action_213 (39) = happyGoto action_235
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (52) = happyShift action_6
action_214 (53) = happyShift action_22
action_214 (54) = happyShift action_23
action_214 (55) = happyShift action_24
action_214 (97) = happyShift action_25
action_214 (101) = happyShift action_26
action_214 (111) = happyShift action_27
action_214 (114) = happyShift action_28
action_214 (8) = happyGoto action_14
action_214 (33) = happyGoto action_15
action_214 (35) = happyGoto action_16
action_214 (36) = happyGoto action_17
action_214 (37) = happyGoto action_18
action_214 (41) = happyGoto action_19
action_214 (42) = happyGoto action_20
action_214 (43) = happyGoto action_234
action_214 _ = happyFail (happyExpListPerState 214)

action_215 (52) = happyShift action_6
action_215 (53) = happyShift action_22
action_215 (54) = happyShift action_23
action_215 (55) = happyShift action_24
action_215 (97) = happyShift action_25
action_215 (101) = happyShift action_26
action_215 (111) = happyShift action_27
action_215 (114) = happyShift action_28
action_215 (8) = happyGoto action_14
action_215 (33) = happyGoto action_15
action_215 (35) = happyGoto action_16
action_215 (36) = happyGoto action_17
action_215 (37) = happyGoto action_18
action_215 (41) = happyGoto action_19
action_215 (42) = happyGoto action_20
action_215 (43) = happyGoto action_233
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (52) = happyShift action_6
action_216 (53) = happyShift action_22
action_216 (54) = happyShift action_23
action_216 (55) = happyShift action_24
action_216 (97) = happyShift action_25
action_216 (101) = happyShift action_26
action_216 (111) = happyShift action_27
action_216 (114) = happyShift action_28
action_216 (8) = happyGoto action_14
action_216 (33) = happyGoto action_15
action_216 (35) = happyGoto action_16
action_216 (36) = happyGoto action_17
action_216 (37) = happyGoto action_18
action_216 (41) = happyGoto action_19
action_216 (42) = happyGoto action_20
action_216 (43) = happyGoto action_232
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (52) = happyShift action_6
action_217 (53) = happyShift action_22
action_217 (54) = happyShift action_23
action_217 (55) = happyShift action_24
action_217 (97) = happyShift action_25
action_217 (101) = happyShift action_26
action_217 (111) = happyShift action_27
action_217 (114) = happyShift action_28
action_217 (8) = happyGoto action_14
action_217 (33) = happyGoto action_15
action_217 (35) = happyGoto action_16
action_217 (36) = happyGoto action_17
action_217 (37) = happyGoto action_18
action_217 (41) = happyGoto action_19
action_217 (42) = happyGoto action_20
action_217 (43) = happyGoto action_231
action_217 _ = happyFail (happyExpListPerState 217)

action_218 (106) = happyShift action_85
action_218 _ = happyReduce_120

action_219 _ = happyReduce_122

action_220 (105) = happyShift action_213
action_220 _ = happyReduce_126

action_221 (105) = happyShift action_213
action_221 _ = happyReduce_128

action_222 (57) = happyShift action_62
action_222 (58) = happyShift action_63
action_222 (59) = happyShift action_64
action_222 (60) = happyShift action_65
action_222 (61) = happyShift action_66
action_222 (62) = happyShift action_67
action_222 (63) = happyShift action_68
action_222 (64) = happyShift action_69
action_222 (65) = happyShift action_70
action_222 (66) = happyShift action_71
action_222 (67) = happyShift action_72
action_222 (68) = happyShift action_73
action_222 (69) = happyShift action_74
action_222 (70) = happyShift action_75
action_222 (71) = happyShift action_76
action_222 (72) = happyShift action_77
action_222 (112) = happyShift action_78
action_222 _ = happyReduce_51

action_223 _ = happyReduce_29

action_224 _ = happyReduce_39

action_225 _ = happyReduce_24

action_226 _ = happyReduce_31

action_227 _ = happyReduce_38

action_228 (105) = happyShift action_193
action_228 (20) = happyGoto action_230
action_228 _ = happyReduce_25

action_229 _ = happyReduce_36

action_230 _ = happyReduce_26

action_231 (106) = happyShift action_85
action_231 _ = happyReduce_99

action_232 (106) = happyShift action_85
action_232 _ = happyReduce_96

action_233 (106) = happyShift action_85
action_233 _ = happyReduce_98

action_234 (106) = happyShift action_85
action_234 _ = happyReduce_97

action_235 _ = happyReduce_101

action_236 (110) = happyShift action_240
action_236 _ = happyFail (happyExpListPerState 236)

action_237 _ = happyReduce_54

action_238 (105) = happyShift action_239
action_238 _ = happyReduce_124

action_239 (52) = happyShift action_6
action_239 (8) = happyGoto action_236
action_239 (29) = happyGoto action_242
action_239 _ = happyFail (happyExpListPerState 239)

action_240 (52) = happyShift action_6
action_240 (53) = happyShift action_39
action_240 (54) = happyShift action_40
action_240 (55) = happyShift action_41
action_240 (78) = happyShift action_42
action_240 (81) = happyShift action_43
action_240 (97) = happyShift action_44
action_240 (99) = happyShift action_45
action_240 (101) = happyShift action_46
action_240 (112) = happyShift action_47
action_240 (114) = happyShift action_48
action_240 (8) = happyGoto action_29
action_240 (10) = happyGoto action_30
action_240 (12) = happyGoto action_31
action_240 (14) = happyGoto action_32
action_240 (17) = happyGoto action_33
action_240 (18) = happyGoto action_34
action_240 (25) = happyGoto action_35
action_240 (26) = happyGoto action_36
action_240 (28) = happyGoto action_37
action_240 (31) = happyGoto action_241
action_240 _ = happyFail (happyExpListPerState 240)

action_241 (57) = happyShift action_62
action_241 (58) = happyShift action_63
action_241 (59) = happyShift action_64
action_241 (60) = happyShift action_65
action_241 (61) = happyShift action_66
action_241 (62) = happyShift action_67
action_241 (63) = happyShift action_68
action_241 (64) = happyShift action_69
action_241 (65) = happyShift action_70
action_241 (66) = happyShift action_71
action_241 (67) = happyShift action_72
action_241 (68) = happyShift action_73
action_241 (69) = happyShift action_74
action_241 (70) = happyShift action_75
action_241 (71) = happyShift action_76
action_241 (72) = happyShift action_77
action_241 (112) = happyShift action_78
action_241 _ = happyReduce_53

action_242 _ = happyReduce_55

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
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn31  happy_var_3)
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
happyReduction_11 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
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
happyReduction_14 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn13
		 ([happy_var_2]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 14 happyReduction_16
happyReduction_16 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn31  happy_var_2) `HappyStk`
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
happyReduction_20 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  17 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn17
		 (P.fnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 18 happyReduction_22
happyReduction_22 ((HappyAbsSyn31  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_2) `HappyStk`
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

happyReduce_30 = happySpecReduce_0  22 happyReduction_30
happyReduction_30  =  HappyAbsSyn22
		 (Nothing
	)

happyReduce_31 = happySpecReduce_2  22 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Just happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  23 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  23 happyReduction_33
happyReduction_33 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  24 happyReduction_34
happyReduction_34 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn24
		 (P.pattern $ P.Var happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  24 happyReduction_35
happyReduction_35 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (P.pattern $ P.Term $ fmap HM.Term happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 24 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (P.pattern $ P.Tuple (happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  24 happyReduction_37
happyReduction_37 _
	_
	 =  HappyAbsSyn24
		 (P.pattern $ P.List [] Nothing
	)

happyReduce_38 = happyReduce 4 24 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (P.pattern $ P.List happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 24 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (P.pattern $ P.Record happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (P.pattern $ P.Tagged happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.number HM.LInt happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  25 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.string HM.LString happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.boolean HM.LBool happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  26 happyReduction_44
happyReduction_44 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  26 happyReduction_45
happyReduction_45 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (P.term happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  26 happyReduction_47
happyReduction_47 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  26 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 4 27 happyReduction_50
happyReduction_50 ((HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 ([P.matchCase happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 5 27 happyReduction_51
happyReduction_51 ((HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (happy_var_1 ++ [P.matchCase happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  28 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  29 happyReduction_53
happyReduction_53 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn29
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  30 happyReduction_54
happyReduction_54 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  30 happyReduction_55
happyReduction_55 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  31 happyReduction_56
happyReduction_56 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  31 happyReduction_58
happyReduction_58 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 31 happyReduction_59
happyReduction_59 ((HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_1  31 happyReduction_60
happyReduction_60 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  31 happyReduction_61
happyReduction_61 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (P.dotLambda happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  31 happyReduction_62
happyReduction_62 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  31 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  31 happyReduction_64
happyReduction_64 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  31 happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  31 happyReduction_66
happyReduction_66 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  31 happyReduction_67
happyReduction_67 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  31 happyReduction_68
happyReduction_68 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  31 happyReduction_69
happyReduction_69 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  31 happyReduction_70
happyReduction_70 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  31 happyReduction_71
happyReduction_71 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  31 happyReduction_72
happyReduction_72 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  31 happyReduction_73
happyReduction_73 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  31 happyReduction_74
happyReduction_74 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  31 happyReduction_75
happyReduction_75 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  31 happyReduction_76
happyReduction_76 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  31 happyReduction_77
happyReduction_77 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  31 happyReduction_78
happyReduction_78 (HappyAbsSyn31  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_0  32 happyReduction_79
happyReduction_79  =  HappyAbsSyn32
		 ([]
	)

happyReduce_80 = happyReduce 5 32 happyReduction_80
happyReduction_80 ((HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_81 = happySpecReduce_3  32 happyReduction_81
happyReduction_81 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn32
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  33 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  34 happyReduction_83
happyReduction_83 (HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn34
		 ([happy_var_2]
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  34 happyReduction_84
happyReduction_84 (HappyAbsSyn34  happy_var_3)
	(HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn34
		 (happy_var_2 : happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 4 35 happyReduction_85
happyReduction_85 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyAbsSyn43  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_1  36 happyReduction_86
happyReduction_86 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  36 happyReduction_87
happyReduction_87 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  36 happyReduction_88
happyReduction_88 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  36 happyReduction_89
happyReduction_89 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  36 happyReduction_90
happyReduction_90 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  37 happyReduction_91
happyReduction_91 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  37 happyReduction_92
happyReduction_92 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyIdentifier happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  37 happyReduction_93
happyReduction_93 (HappyTerminal happy_var_3)
	(HappyAbsSyn43  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_0  38 happyReduction_94
happyReduction_94  =  HappyAbsSyn38
		 ([]
	)

happyReduce_95 = happySpecReduce_2  38 happyReduction_95
happyReduction_95 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_95 _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  39 happyReduction_96
happyReduction_96 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  39 happyReduction_97
happyReduction_97 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  39 happyReduction_98
happyReduction_98 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  39 happyReduction_99
happyReduction_99 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  40 happyReduction_100
happyReduction_100 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  40 happyReduction_101
happyReduction_101 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  41 happyReduction_102
happyReduction_102 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn41
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  42 happyReduction_103
happyReduction_103 (HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn42
		 ([happy_var_2]
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  42 happyReduction_104
happyReduction_104 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  43 happyReduction_105
happyReduction_105 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  43 happyReduction_106
happyReduction_106 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  43 happyReduction_107
happyReduction_107 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn43
		 (P.typeUnion happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  43 happyReduction_108
happyReduction_108 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happyReduce 4 43 happyReduction_109
happyReduction_109 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_3  43 happyReduction_110
happyReduction_110 (HappyTerminal happy_var_3)
	(HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn43
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_110 _ _ _  = notHappyAtAll 

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

happyReduce_113 = happyReduce 4 44 happyReduction_113
happyReduction_113 ((HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_114 = happyReduce 5 44 happyReduction_114
happyReduction_114 ((HappyAbsSyn43  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_115 = happySpecReduce_3  45 happyReduction_115
happyReduction_115 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  45 happyReduction_116
happyReduction_116 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (happy_var_2
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  45 happyReduction_117
happyReduction_117 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn45
		 (P.kindId happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_0  46 happyReduction_118
happyReduction_118  =  HappyAbsSyn46
		 (Nothing
	)

happyReduce_119 = happySpecReduce_2  46 happyReduction_119
happyReduction_119 (HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Just happy_var_2
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  47 happyReduction_120
happyReduction_120 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn47
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  48 happyReduction_121
happyReduction_121 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_3  48 happyReduction_122
happyReduction_122 (HappyAbsSyn47  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_122 _ _ _  = notHappyAtAll 

happyReduce_123 = happyReduce 6 49 happyReduction_123
happyReduction_123 ((HappyAbsSyn31  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_124 = happyReduce 8 49 happyReduction_124
happyReduction_124 ((HappyAbsSyn30  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_125 = happyReduce 5 49 happyReduction_125
happyReduction_125 ((HappyAbsSyn48  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_126 = happyReduce 7 49 happyReduction_126
happyReduction_126 ((HappyAbsSyn40  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_127 = happyReduce 5 49 happyReduction_127
happyReduction_127 ((HappyAbsSyn43  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_128 = happyReduce 7 49 happyReduction_128
happyReduction_128 ((HappyAbsSyn40  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_129 = happySpecReduce_1  50 happyReduction_129
happyReduction_129 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn50
		 ([happy_var_1]
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_2  50 happyReduction_130
happyReduction_130 (HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_130 _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  51 happyReduction_131
happyReduction_131 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn51
		 (P.script happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 117 117 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 52;
	L.RangedToken (T.Number _) _ -> cont 53;
	L.RangedToken (T.String _) _ -> cont 54;
	L.RangedToken (T.Boolean _) _ -> cont 55;
	L.RangedToken (T.Operator "!") _ -> cont 56;
	L.RangedToken (T.Operator "+") _ -> cont 57;
	L.RangedToken (T.Operator "-") _ -> cont 58;
	L.RangedToken (T.Operator "*") _ -> cont 59;
	L.RangedToken (T.Operator "/") _ -> cont 60;
	L.RangedToken (T.Operator "^") _ -> cont 61;
	L.RangedToken (T.Operator "++") _ -> cont 62;
	L.RangedToken (T.Operator "==") _ -> cont 63;
	L.RangedToken (T.Operator "!=") _ -> cont 64;
	L.RangedToken (T.Operator "<") _ -> cont 65;
	L.RangedToken (T.Operator "<=") _ -> cont 66;
	L.RangedToken (T.Operator ">") _ -> cont 67;
	L.RangedToken (T.Operator ">=") _ -> cont 68;
	L.RangedToken (T.Operator "||") _ -> cont 69;
	L.RangedToken (T.Operator "&&") _ -> cont 70;
	L.RangedToken (T.Operator "|>") _ -> cont 71;
	L.RangedToken (T.Operator "<|") _ -> cont 72;
	L.RangedToken (T.Operator _) _ -> cont 73;
	L.RangedToken T.Let _ -> cont 74;
	L.RangedToken T.In _ -> cont 75;
	L.RangedToken T.Where _ -> cont 76;
	L.RangedToken T.With _ -> cont 77;
	L.RangedToken T.If _ -> cont 78;
	L.RangedToken T.Then _ -> cont 79;
	L.RangedToken T.Else _ -> cont 80;
	L.RangedToken T.Match _ -> cont 81;
	L.RangedToken T.Return _ -> cont 82;
	L.RangedToken T.Data _ -> cont 83;
	L.RangedToken T.Type _ -> cont 84;
	L.RangedToken T.Alias _ -> cont 85;
	L.RangedToken T.Kind _ -> cont 86;
	L.RangedToken T.Forall _ -> cont 87;
	L.RangedToken T.Exists _ -> cont 88;
	L.RangedToken T.Proof _ -> cont 89;
	L.RangedToken T.Infer _ -> cont 90;
	L.RangedToken T.Protocol _ -> cont 91;
	L.RangedToken T.Interface _ -> cont 92;
	L.RangedToken T.Instance _ -> cont 93;
	L.RangedToken T.Implements _ -> cont 94;
	L.RangedToken T.Module _ -> cont 95;
	L.RangedToken T.Import _ -> cont 96;
	L.RangedToken T.LParen _ -> cont 97;
	L.RangedToken T.RParen _ -> cont 98;
	L.RangedToken T.LBrack _ -> cont 99;
	L.RangedToken T.RBrack _ -> cont 100;
	L.RangedToken T.LCurly _ -> cont 101;
	L.RangedToken T.RCurly _ -> cont 102;
	L.RangedToken T.Colon _ -> cont 103;
	L.RangedToken T.SemiColon _ -> cont 104;
	L.RangedToken T.Comma _ -> cont 105;
	L.RangedToken T.Arrow _ -> cont 106;
	L.RangedToken T.BackArrow _ -> cont 107;
	L.RangedToken T.FatArrow _ -> cont 108;
	L.RangedToken T.PipeArrow _ -> cont 109;
	L.RangedToken T.Equals _ -> cont 110;
	L.RangedToken T.Pipe _ -> cont 111;
	L.RangedToken T.Dot _ -> cont 112;
	L.RangedToken T.Section _ -> cont 113;
	L.RangedToken T.BackSlash _ -> cont 114;
	L.RangedToken T.Newline _ -> cont 115;
	L.RangedToken T.EOF _ -> cont 116;
	_ -> happyError' (tk, [])
	})

happyError_ explist 117 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn51 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn45 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn49 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


runSagaScript :: String -> Either String (P.ParsedData HM.Script)
runSagaScript input = input `P.run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData HM.TypeExpr)
runSagaType input = input `P.run` parseSagaType

runSagaKind :: String -> Either String (P.ParsedData HM.Kind)
runSagaKind input = input `P.run` parseSagaKind

runSagaDec :: String -> Either String (P.ParsedData HM.Declaration)
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
