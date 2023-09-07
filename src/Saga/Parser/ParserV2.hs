{-# OPTIONS_GHC -w #-}
module Saga.Parser.ParserV2 where


import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T


import qualified Saga.Parser.Expr as PE
import qualified Saga.Parser.Literals as PL
import qualified Saga.Parser.Types as PT
import qualified Saga.Parser.Shared as P
import qualified Saga.Parser.ParsingInfo as P
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1024) ([0,0,992,0,36,672,80,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,2,0,0,0,0,2016,0,36,672,80,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3968,0,144,2688,320,0,0,62,16384,2,42,5,0,63488,0,2304,43008,5120,0,0,992,0,36,672,80,0,32768,15,36864,32769,16394,1,0,512,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34816,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,16384,0,0,0,62,16384,2,42,5,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,4,0,0,0,0,0,1024,0,0,0,0,0,0,2064,0,0,0,0,0,0,2048,0,0,0,0,32,0,0,0,0,2,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,32768,15,36864,32768,16394,1,0,15872,0,576,10752,1280,0,0,0,512,0,0,0,0,0,8,8,0,0,0,0,8064,0,144,2688,320,0,0,0,0,0,0,0,0,63488,0,2304,43008,5120,0,0,0,0,0,0,8,0,32768,14,0,32768,10,0,0,0,0,0,0,0,0,0,248,0,9,168,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,992,0,100,672,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,248,0,9,168,20,0,57344,3,9216,40960,20482,0,0,0,0,0,0,0,0,0,62,16384,2,42,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,16384,0,0,32768,14,0,32768,10,0,0,512,0,0,4096,0,0,0,8,0,0,0,0,0,40960,3,0,40960,2,0,0,0,0,64,0,0,0,0,62,16384,2,42,5,0,63488,0,2304,43008,5120,0,0,2016,0,36,672,80,0,32768,31,36864,32768,16394,1,0,15872,0,576,10752,1280,0,0,0,0,0,4096,0,0,0,0,0,0,32,0,0,0,0,0,0,32,0,0,0,0,0,512,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,160,0,0,0,0,0,1024,0,0,0,248,0,9,168,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,8192,0,0,0,0,0,0,0,0,0,1024,0,0,0,2,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,1024,0,0,32768,0,0,0,0,0,0,15872,0,576,10752,1280,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","identifier","hole","expr","expr1","expr2","exprBacktick","expr3","expr4","expr5","expr6","block","statement","exprAtom","term","record","pairs","keyValPair","list","tuple","patListElems","patTupleElems","patRecordKeys","patRest","patData","pattern","patterns","cases","defaultSeparator","manyOrEmpty__identifier__","separated__expr__','__","separated__statement__';'__","separatedOrEmpty__expr__','__","trailing__';'__","many__identifier__","id","HOLE","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'^'","'++'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","'`'","'#'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 106
        bit_end = (st Prelude.+ 1) Prelude.* 106
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..105]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (38) = happyShift action_2
action_0 (39) = happyShift action_17
action_0 (40) = happyShift action_18
action_0 (41) = happyShift action_19
action_0 (42) = happyShift action_20
action_0 (67) = happyShift action_21
action_0 (70) = happyShift action_22
action_0 (86) = happyShift action_23
action_0 (88) = happyShift action_24
action_0 (90) = happyShift action_25
action_0 (101) = happyShift action_26
action_0 (103) = happyShift action_27
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 (13) = happyGoto action_11
action_0 (16) = happyGoto action_12
action_0 (17) = happyGoto action_13
action_0 (18) = happyGoto action_14
action_0 (21) = happyGoto action_15
action_0 (22) = happyGoto action_16
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_27

action_4 _ = happyReduce_26

action_5 (106) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (38) = happyReduce_3
action_6 (39) = happyReduce_3
action_6 (40) = happyReduce_3
action_6 (41) = happyReduce_3
action_6 (42) = happyReduce_3
action_6 (43) = happyReduce_3
action_6 (44) = happyShift action_50
action_6 (60) = happyReduce_3
action_6 (67) = happyReduce_3
action_6 (68) = happyReduce_3
action_6 (69) = happyReduce_3
action_6 (70) = happyReduce_3
action_6 (86) = happyReduce_3
action_6 (87) = happyReduce_3
action_6 (88) = happyReduce_3
action_6 (89) = happyReduce_3
action_6 (90) = happyReduce_3
action_6 (91) = happyReduce_3
action_6 (93) = happyReduce_3
action_6 (94) = happyReduce_3
action_6 (100) = happyReduce_3
action_6 (101) = happyReduce_3
action_6 (103) = happyReduce_3
action_6 (106) = happyReduce_3
action_6 _ = happyReduce_3

action_7 (38) = happyReduce_4
action_7 (39) = happyReduce_4
action_7 (40) = happyReduce_4
action_7 (41) = happyReduce_4
action_7 (42) = happyReduce_4
action_7 (43) = happyReduce_4
action_7 (44) = happyReduce_4
action_7 (60) = happyShift action_49
action_7 (67) = happyReduce_4
action_7 (68) = happyReduce_4
action_7 (69) = happyReduce_4
action_7 (70) = happyReduce_4
action_7 (86) = happyReduce_4
action_7 (87) = happyReduce_4
action_7 (88) = happyReduce_4
action_7 (89) = happyReduce_4
action_7 (90) = happyReduce_4
action_7 (91) = happyReduce_4
action_7 (93) = happyReduce_4
action_7 (94) = happyReduce_4
action_7 (100) = happyReduce_4
action_7 (101) = happyReduce_4
action_7 (103) = happyReduce_4
action_7 (106) = happyReduce_4
action_7 _ = happyReduce_4

action_8 (38) = happyShift action_2
action_8 (39) = happyShift action_17
action_8 (40) = happyShift action_18
action_8 (41) = happyShift action_19
action_8 (42) = happyShift action_20
action_8 (43) = happyShift action_48
action_8 (44) = happyReduce_6
action_8 (60) = happyReduce_6
action_8 (67) = happyShift action_21
action_8 (68) = happyReduce_6
action_8 (69) = happyReduce_6
action_8 (70) = happyShift action_22
action_8 (86) = happyShift action_23
action_8 (87) = happyReduce_6
action_8 (88) = happyShift action_24
action_8 (89) = happyReduce_6
action_8 (90) = happyShift action_25
action_8 (91) = happyReduce_6
action_8 (93) = happyReduce_6
action_8 (94) = happyReduce_6
action_8 (100) = happyReduce_6
action_8 (101) = happyShift action_26
action_8 (103) = happyShift action_27
action_8 (106) = happyReduce_6
action_8 (4) = happyGoto action_3
action_8 (5) = happyGoto action_4
action_8 (11) = happyGoto action_47
action_8 (12) = happyGoto action_10
action_8 (13) = happyGoto action_11
action_8 (16) = happyGoto action_12
action_8 (17) = happyGoto action_13
action_8 (18) = happyGoto action_14
action_8 (21) = happyGoto action_15
action_8 (22) = happyGoto action_16
action_8 _ = happyReduce_6

action_9 (38) = happyReduce_10
action_9 (39) = happyReduce_10
action_9 (40) = happyReduce_10
action_9 (41) = happyReduce_10
action_9 (42) = happyReduce_10
action_9 (43) = happyReduce_10
action_9 (44) = happyReduce_10
action_9 (60) = happyReduce_10
action_9 (67) = happyReduce_10
action_9 (68) = happyReduce_10
action_9 (69) = happyReduce_10
action_9 (70) = happyReduce_10
action_9 (86) = happyReduce_10
action_9 (87) = happyReduce_10
action_9 (88) = happyReduce_10
action_9 (89) = happyReduce_10
action_9 (90) = happyReduce_10
action_9 (91) = happyReduce_10
action_9 (93) = happyReduce_10
action_9 (94) = happyReduce_10
action_9 (100) = happyReduce_10
action_9 (101) = happyShift action_46
action_9 (103) = happyReduce_10
action_9 (106) = happyReduce_10
action_9 _ = happyReduce_10

action_10 _ = happyReduce_13

action_11 _ = happyReduce_15

action_12 _ = happyReduce_20

action_13 _ = happyReduce_28

action_14 _ = happyReduce_31

action_15 _ = happyReduce_30

action_16 _ = happyReduce_29

action_17 _ = happyReduce_2

action_18 _ = happyReduce_33

action_19 _ = happyReduce_34

action_20 _ = happyReduce_35

action_21 (38) = happyShift action_2
action_21 (39) = happyShift action_17
action_21 (40) = happyShift action_18
action_21 (41) = happyShift action_19
action_21 (42) = happyShift action_20
action_21 (67) = happyShift action_21
action_21 (70) = happyShift action_22
action_21 (86) = happyShift action_23
action_21 (88) = happyShift action_24
action_21 (90) = happyShift action_25
action_21 (101) = happyShift action_26
action_21 (103) = happyShift action_27
action_21 (4) = happyGoto action_3
action_21 (5) = happyGoto action_4
action_21 (6) = happyGoto action_45
action_21 (7) = happyGoto action_6
action_21 (8) = happyGoto action_7
action_21 (10) = happyGoto action_8
action_21 (11) = happyGoto action_9
action_21 (12) = happyGoto action_10
action_21 (13) = happyGoto action_11
action_21 (16) = happyGoto action_12
action_21 (17) = happyGoto action_13
action_21 (18) = happyGoto action_14
action_21 (21) = happyGoto action_15
action_21 (22) = happyGoto action_16
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (38) = happyShift action_2
action_22 (39) = happyShift action_17
action_22 (40) = happyShift action_18
action_22 (41) = happyShift action_19
action_22 (42) = happyShift action_20
action_22 (67) = happyShift action_21
action_22 (70) = happyShift action_22
action_22 (86) = happyShift action_23
action_22 (88) = happyShift action_24
action_22 (90) = happyShift action_25
action_22 (101) = happyShift action_26
action_22 (103) = happyShift action_27
action_22 (4) = happyGoto action_3
action_22 (5) = happyGoto action_4
action_22 (6) = happyGoto action_44
action_22 (7) = happyGoto action_6
action_22 (8) = happyGoto action_7
action_22 (10) = happyGoto action_8
action_22 (11) = happyGoto action_9
action_22 (12) = happyGoto action_10
action_22 (13) = happyGoto action_11
action_22 (16) = happyGoto action_12
action_22 (17) = happyGoto action_13
action_22 (18) = happyGoto action_14
action_22 (21) = happyGoto action_15
action_22 (22) = happyGoto action_16
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (38) = happyShift action_2
action_23 (39) = happyShift action_17
action_23 (40) = happyShift action_18
action_23 (41) = happyShift action_19
action_23 (42) = happyShift action_20
action_23 (67) = happyShift action_21
action_23 (70) = happyShift action_22
action_23 (86) = happyShift action_23
action_23 (88) = happyShift action_24
action_23 (90) = happyShift action_25
action_23 (101) = happyShift action_26
action_23 (103) = happyShift action_27
action_23 (4) = happyGoto action_3
action_23 (5) = happyGoto action_4
action_23 (6) = happyGoto action_42
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (10) = happyGoto action_8
action_23 (11) = happyGoto action_9
action_23 (12) = happyGoto action_10
action_23 (13) = happyGoto action_11
action_23 (16) = happyGoto action_12
action_23 (17) = happyGoto action_13
action_23 (18) = happyGoto action_14
action_23 (21) = happyGoto action_15
action_23 (22) = happyGoto action_16
action_23 (33) = happyGoto action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (38) = happyShift action_2
action_24 (39) = happyShift action_17
action_24 (40) = happyShift action_18
action_24 (41) = happyShift action_19
action_24 (42) = happyShift action_20
action_24 (67) = happyShift action_21
action_24 (70) = happyShift action_22
action_24 (86) = happyShift action_23
action_24 (88) = happyShift action_24
action_24 (90) = happyShift action_25
action_24 (101) = happyShift action_26
action_24 (103) = happyShift action_27
action_24 (4) = happyGoto action_3
action_24 (5) = happyGoto action_4
action_24 (6) = happyGoto action_39
action_24 (7) = happyGoto action_6
action_24 (8) = happyGoto action_7
action_24 (10) = happyGoto action_8
action_24 (11) = happyGoto action_9
action_24 (12) = happyGoto action_10
action_24 (13) = happyGoto action_11
action_24 (16) = happyGoto action_12
action_24 (17) = happyGoto action_13
action_24 (18) = happyGoto action_14
action_24 (21) = happyGoto action_15
action_24 (22) = happyGoto action_16
action_24 (33) = happyGoto action_40
action_24 (35) = happyGoto action_41
action_24 _ = happyReduce_74

action_25 (38) = happyShift action_2
action_25 (39) = happyShift action_17
action_25 (40) = happyShift action_18
action_25 (41) = happyShift action_19
action_25 (42) = happyShift action_20
action_25 (67) = happyShift action_21
action_25 (70) = happyShift action_22
action_25 (71) = happyShift action_38
action_25 (86) = happyShift action_23
action_25 (88) = happyShift action_24
action_25 (90) = happyShift action_25
action_25 (101) = happyShift action_26
action_25 (103) = happyShift action_27
action_25 (4) = happyGoto action_32
action_25 (5) = happyGoto action_4
action_25 (6) = happyGoto action_33
action_25 (7) = happyGoto action_6
action_25 (8) = happyGoto action_7
action_25 (10) = happyGoto action_8
action_25 (11) = happyGoto action_9
action_25 (12) = happyGoto action_10
action_25 (13) = happyGoto action_11
action_25 (14) = happyGoto action_34
action_25 (15) = happyGoto action_35
action_25 (16) = happyGoto action_12
action_25 (17) = happyGoto action_13
action_25 (18) = happyGoto action_14
action_25 (19) = happyGoto action_36
action_25 (21) = happyGoto action_15
action_25 (22) = happyGoto action_16
action_25 (34) = happyGoto action_37
action_25 _ = happyReduce_37

action_26 (38) = happyShift action_2
action_26 (4) = happyGoto action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (38) = happyShift action_2
action_27 (4) = happyGoto action_28
action_27 (32) = happyGoto action_29
action_27 (37) = happyGoto action_30
action_27 _ = happyReduce_68

action_28 _ = happyReduce_78

action_29 (95) = happyShift action_70
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (38) = happyShift action_2
action_30 (4) = happyGoto action_69
action_30 _ = happyReduce_69

action_31 _ = happyReduce_19

action_32 (92) = happyShift action_67
action_32 (96) = happyShift action_68
action_32 _ = happyReduce_27

action_33 _ = happyReduce_24

action_34 (91) = happyShift action_66
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (91) = happyReduce_72
action_35 (93) = happyReduce_72
action_35 _ = happyReduce_72

action_36 (91) = happyShift action_65
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (91) = happyReduce_76
action_37 (93) = happyShift action_64
action_37 (36) = happyGoto action_63
action_37 _ = happyReduce_76

action_38 (38) = happyShift action_2
action_38 (39) = happyShift action_17
action_38 (40) = happyShift action_18
action_38 (41) = happyShift action_19
action_38 (42) = happyShift action_20
action_38 (67) = happyShift action_21
action_38 (70) = happyShift action_22
action_38 (86) = happyShift action_23
action_38 (88) = happyShift action_24
action_38 (90) = happyShift action_25
action_38 (101) = happyShift action_26
action_38 (103) = happyShift action_27
action_38 (4) = happyGoto action_3
action_38 (5) = happyGoto action_4
action_38 (6) = happyGoto action_62
action_38 (7) = happyGoto action_6
action_38 (8) = happyGoto action_7
action_38 (10) = happyGoto action_8
action_38 (11) = happyGoto action_9
action_38 (12) = happyGoto action_10
action_38 (13) = happyGoto action_11
action_38 (16) = happyGoto action_12
action_38 (17) = happyGoto action_13
action_38 (18) = happyGoto action_14
action_38 (21) = happyGoto action_15
action_38 (22) = happyGoto action_16
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (89) = happyReduce_70
action_39 (94) = happyReduce_70
action_39 _ = happyReduce_70

action_40 (89) = happyReduce_75
action_40 (94) = happyShift action_59
action_40 _ = happyReduce_75

action_41 (89) = happyShift action_61
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (87) = happyShift action_60
action_42 (94) = happyReduce_70
action_42 _ = happyReduce_70

action_43 (87) = happyShift action_58
action_43 (94) = happyShift action_59
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (100) = happyShift action_57
action_44 (30) = happyGoto action_56
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (68) = happyShift action_55
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (38) = happyShift action_2
action_46 (4) = happyGoto action_54
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (38) = happyReduce_11
action_47 (39) = happyReduce_11
action_47 (40) = happyReduce_11
action_47 (41) = happyReduce_11
action_47 (42) = happyReduce_11
action_47 (43) = happyReduce_11
action_47 (44) = happyReduce_11
action_47 (60) = happyReduce_11
action_47 (67) = happyReduce_11
action_47 (68) = happyReduce_11
action_47 (69) = happyReduce_11
action_47 (70) = happyReduce_11
action_47 (86) = happyReduce_11
action_47 (87) = happyReduce_11
action_47 (88) = happyReduce_11
action_47 (89) = happyReduce_11
action_47 (90) = happyReduce_11
action_47 (91) = happyReduce_11
action_47 (93) = happyReduce_11
action_47 (94) = happyReduce_11
action_47 (100) = happyReduce_11
action_47 (101) = happyShift action_46
action_47 (103) = happyReduce_11
action_47 (106) = happyReduce_11
action_47 _ = happyReduce_11

action_48 _ = happyReduce_12

action_49 (38) = happyShift action_2
action_49 (39) = happyShift action_17
action_49 (40) = happyShift action_18
action_49 (41) = happyShift action_19
action_49 (42) = happyShift action_20
action_49 (67) = happyShift action_21
action_49 (70) = happyShift action_22
action_49 (86) = happyShift action_23
action_49 (88) = happyShift action_24
action_49 (90) = happyShift action_25
action_49 (101) = happyShift action_26
action_49 (103) = happyShift action_27
action_49 (4) = happyGoto action_3
action_49 (5) = happyGoto action_4
action_49 (9) = happyGoto action_52
action_49 (10) = happyGoto action_53
action_49 (11) = happyGoto action_9
action_49 (12) = happyGoto action_10
action_49 (13) = happyGoto action_11
action_49 (16) = happyGoto action_12
action_49 (17) = happyGoto action_13
action_49 (18) = happyGoto action_14
action_49 (21) = happyGoto action_15
action_49 (22) = happyGoto action_16
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (38) = happyShift action_2
action_50 (39) = happyShift action_17
action_50 (40) = happyShift action_18
action_50 (41) = happyShift action_19
action_50 (42) = happyShift action_20
action_50 (67) = happyShift action_21
action_50 (70) = happyShift action_22
action_50 (86) = happyShift action_23
action_50 (88) = happyShift action_24
action_50 (90) = happyShift action_25
action_50 (101) = happyShift action_26
action_50 (103) = happyShift action_27
action_50 (4) = happyGoto action_3
action_50 (5) = happyGoto action_4
action_50 (8) = happyGoto action_51
action_50 (10) = happyGoto action_8
action_50 (11) = happyGoto action_9
action_50 (12) = happyGoto action_10
action_50 (13) = happyGoto action_11
action_50 (16) = happyGoto action_12
action_50 (17) = happyGoto action_13
action_50 (18) = happyGoto action_14
action_50 (21) = happyGoto action_15
action_50 (22) = happyGoto action_16
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (38) = happyReduce_5
action_51 (39) = happyReduce_5
action_51 (40) = happyReduce_5
action_51 (41) = happyReduce_5
action_51 (42) = happyReduce_5
action_51 (43) = happyReduce_5
action_51 (44) = happyReduce_5
action_51 (60) = happyShift action_49
action_51 (67) = happyReduce_5
action_51 (68) = happyReduce_5
action_51 (69) = happyReduce_5
action_51 (70) = happyReduce_5
action_51 (86) = happyReduce_5
action_51 (87) = happyReduce_5
action_51 (88) = happyReduce_5
action_51 (89) = happyReduce_5
action_51 (90) = happyReduce_5
action_51 (91) = happyReduce_5
action_51 (93) = happyReduce_5
action_51 (94) = happyReduce_5
action_51 (100) = happyReduce_5
action_51 (101) = happyReduce_5
action_51 (103) = happyReduce_5
action_51 (106) = happyReduce_5
action_51 _ = happyReduce_5

action_52 (44) = happyShift action_86
action_52 (60) = happyShift action_87
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (38) = happyShift action_2
action_53 (39) = happyShift action_17
action_53 (40) = happyShift action_18
action_53 (41) = happyShift action_19
action_53 (42) = happyShift action_20
action_53 (43) = happyShift action_48
action_53 (67) = happyShift action_21
action_53 (70) = happyShift action_22
action_53 (86) = happyShift action_23
action_53 (88) = happyShift action_24
action_53 (90) = happyShift action_25
action_53 (101) = happyShift action_26
action_53 (103) = happyShift action_27
action_53 (4) = happyGoto action_3
action_53 (5) = happyGoto action_4
action_53 (11) = happyGoto action_47
action_53 (12) = happyGoto action_10
action_53 (13) = happyGoto action_11
action_53 (16) = happyGoto action_12
action_53 (17) = happyGoto action_13
action_53 (18) = happyGoto action_14
action_53 (21) = happyGoto action_15
action_53 (22) = happyGoto action_16
action_53 _ = happyReduce_8

action_54 (38) = happyReduce_14
action_54 (39) = happyReduce_14
action_54 (40) = happyReduce_14
action_54 (41) = happyReduce_14
action_54 (42) = happyReduce_14
action_54 (43) = happyReduce_14
action_54 (44) = happyReduce_14
action_54 (60) = happyReduce_14
action_54 (67) = happyReduce_14
action_54 (68) = happyReduce_14
action_54 (69) = happyReduce_14
action_54 (70) = happyReduce_14
action_54 (86) = happyReduce_14
action_54 (87) = happyReduce_14
action_54 (88) = happyReduce_14
action_54 (89) = happyReduce_14
action_54 (90) = happyReduce_14
action_54 (91) = happyReduce_14
action_54 (93) = happyReduce_14
action_54 (94) = happyReduce_14
action_54 (100) = happyReduce_14
action_54 (101) = happyReduce_14
action_54 (103) = happyReduce_14
action_54 (106) = happyReduce_14
action_54 _ = happyReduce_14

action_55 (38) = happyShift action_2
action_55 (39) = happyShift action_17
action_55 (40) = happyShift action_18
action_55 (41) = happyShift action_19
action_55 (42) = happyShift action_20
action_55 (67) = happyShift action_21
action_55 (70) = happyShift action_22
action_55 (86) = happyShift action_23
action_55 (88) = happyShift action_24
action_55 (90) = happyShift action_25
action_55 (101) = happyShift action_26
action_55 (103) = happyShift action_27
action_55 (4) = happyGoto action_3
action_55 (5) = happyGoto action_4
action_55 (6) = happyGoto action_85
action_55 (7) = happyGoto action_6
action_55 (8) = happyGoto action_7
action_55 (10) = happyGoto action_8
action_55 (11) = happyGoto action_9
action_55 (12) = happyGoto action_10
action_55 (13) = happyGoto action_11
action_55 (16) = happyGoto action_12
action_55 (17) = happyGoto action_13
action_55 (18) = happyGoto action_14
action_55 (21) = happyGoto action_15
action_55 (22) = happyGoto action_16
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (38) = happyReduce_17
action_56 (39) = happyReduce_17
action_56 (40) = happyReduce_17
action_56 (41) = happyReduce_17
action_56 (42) = happyReduce_17
action_56 (43) = happyReduce_17
action_56 (44) = happyReduce_17
action_56 (60) = happyReduce_17
action_56 (67) = happyReduce_17
action_56 (68) = happyReduce_17
action_56 (69) = happyReduce_17
action_56 (70) = happyReduce_17
action_56 (86) = happyReduce_17
action_56 (87) = happyReduce_17
action_56 (88) = happyReduce_17
action_56 (89) = happyReduce_17
action_56 (90) = happyReduce_17
action_56 (91) = happyReduce_17
action_56 (93) = happyReduce_17
action_56 (94) = happyReduce_17
action_56 (100) = happyShift action_84
action_56 (101) = happyReduce_17
action_56 (103) = happyReduce_17
action_56 (106) = happyReduce_17
action_56 _ = happyReduce_17

action_57 (38) = happyShift action_2
action_57 (40) = happyShift action_18
action_57 (41) = happyShift action_19
action_57 (42) = happyShift action_20
action_57 (86) = happyShift action_81
action_57 (88) = happyShift action_82
action_57 (90) = happyShift action_83
action_57 (4) = happyGoto action_77
action_57 (17) = happyGoto action_78
action_57 (27) = happyGoto action_79
action_57 (28) = happyGoto action_80
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_42

action_59 (38) = happyShift action_2
action_59 (39) = happyShift action_17
action_59 (40) = happyShift action_18
action_59 (41) = happyShift action_19
action_59 (42) = happyShift action_20
action_59 (67) = happyShift action_21
action_59 (70) = happyShift action_22
action_59 (86) = happyShift action_23
action_59 (88) = happyShift action_24
action_59 (90) = happyShift action_25
action_59 (101) = happyShift action_26
action_59 (103) = happyShift action_27
action_59 (4) = happyGoto action_3
action_59 (5) = happyGoto action_4
action_59 (6) = happyGoto action_76
action_59 (7) = happyGoto action_6
action_59 (8) = happyGoto action_7
action_59 (10) = happyGoto action_8
action_59 (11) = happyGoto action_9
action_59 (12) = happyGoto action_10
action_59 (13) = happyGoto action_11
action_59 (16) = happyGoto action_12
action_59 (17) = happyGoto action_13
action_59 (18) = happyGoto action_14
action_59 (21) = happyGoto action_15
action_59 (22) = happyGoto action_16
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_32

action_61 _ = happyReduce_41

action_62 _ = happyReduce_25

action_63 _ = happyReduce_22

action_64 (38) = happyShift action_2
action_64 (39) = happyShift action_17
action_64 (40) = happyShift action_18
action_64 (41) = happyShift action_19
action_64 (42) = happyShift action_20
action_64 (67) = happyShift action_21
action_64 (70) = happyShift action_22
action_64 (71) = happyShift action_38
action_64 (86) = happyShift action_23
action_64 (88) = happyShift action_24
action_64 (90) = happyShift action_25
action_64 (101) = happyShift action_26
action_64 (103) = happyShift action_27
action_64 (4) = happyGoto action_74
action_64 (5) = happyGoto action_4
action_64 (6) = happyGoto action_33
action_64 (7) = happyGoto action_6
action_64 (8) = happyGoto action_7
action_64 (10) = happyGoto action_8
action_64 (11) = happyGoto action_9
action_64 (12) = happyGoto action_10
action_64 (13) = happyGoto action_11
action_64 (15) = happyGoto action_75
action_64 (16) = happyGoto action_12
action_64 (17) = happyGoto action_13
action_64 (18) = happyGoto action_14
action_64 (21) = happyGoto action_15
action_64 (22) = happyGoto action_16
action_64 _ = happyReduce_77

action_65 _ = happyReduce_36

action_66 _ = happyReduce_21

action_67 (38) = happyShift action_2
action_67 (39) = happyShift action_17
action_67 (40) = happyShift action_18
action_67 (41) = happyShift action_19
action_67 (42) = happyShift action_20
action_67 (67) = happyShift action_21
action_67 (70) = happyShift action_22
action_67 (86) = happyShift action_23
action_67 (88) = happyShift action_24
action_67 (90) = happyShift action_25
action_67 (101) = happyShift action_26
action_67 (103) = happyShift action_27
action_67 (4) = happyGoto action_3
action_67 (5) = happyGoto action_4
action_67 (6) = happyGoto action_73
action_67 (7) = happyGoto action_6
action_67 (8) = happyGoto action_7
action_67 (10) = happyGoto action_8
action_67 (11) = happyGoto action_9
action_67 (12) = happyGoto action_10
action_67 (13) = happyGoto action_11
action_67 (16) = happyGoto action_12
action_67 (17) = happyGoto action_13
action_67 (18) = happyGoto action_14
action_67 (21) = happyGoto action_15
action_67 (22) = happyGoto action_16
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (38) = happyShift action_2
action_68 (39) = happyShift action_17
action_68 (40) = happyShift action_18
action_68 (41) = happyShift action_19
action_68 (42) = happyShift action_20
action_68 (67) = happyShift action_21
action_68 (70) = happyShift action_22
action_68 (86) = happyShift action_23
action_68 (88) = happyShift action_24
action_68 (90) = happyShift action_25
action_68 (101) = happyShift action_26
action_68 (103) = happyShift action_27
action_68 (4) = happyGoto action_3
action_68 (5) = happyGoto action_4
action_68 (6) = happyGoto action_72
action_68 (7) = happyGoto action_6
action_68 (8) = happyGoto action_7
action_68 (10) = happyGoto action_8
action_68 (11) = happyGoto action_9
action_68 (12) = happyGoto action_10
action_68 (13) = happyGoto action_11
action_68 (16) = happyGoto action_12
action_68 (17) = happyGoto action_13
action_68 (18) = happyGoto action_14
action_68 (21) = happyGoto action_15
action_68 (22) = happyGoto action_16
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_79

action_70 (38) = happyShift action_2
action_70 (39) = happyShift action_17
action_70 (40) = happyShift action_18
action_70 (41) = happyShift action_19
action_70 (42) = happyShift action_20
action_70 (67) = happyShift action_21
action_70 (70) = happyShift action_22
action_70 (86) = happyShift action_23
action_70 (88) = happyShift action_24
action_70 (90) = happyShift action_25
action_70 (101) = happyShift action_26
action_70 (103) = happyShift action_27
action_70 (4) = happyGoto action_3
action_70 (5) = happyGoto action_4
action_70 (6) = happyGoto action_71
action_70 (7) = happyGoto action_6
action_70 (8) = happyGoto action_7
action_70 (10) = happyGoto action_8
action_70 (11) = happyGoto action_9
action_70 (12) = happyGoto action_10
action_70 (13) = happyGoto action_11
action_70 (16) = happyGoto action_12
action_70 (17) = happyGoto action_13
action_70 (18) = happyGoto action_14
action_70 (21) = happyGoto action_15
action_70 (22) = happyGoto action_16
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_18

action_72 _ = happyReduce_23

action_73 (93) = happyShift action_102
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (96) = happyShift action_68
action_74 _ = happyReduce_27

action_75 _ = happyReduce_73

action_76 _ = happyReduce_71

action_77 (92) = happyShift action_101
action_77 _ = happyReduce_54

action_78 _ = happyReduce_55

action_79 (38) = happyShift action_2
action_79 (4) = happyGoto action_100
action_79 _ = happyReduce_56

action_80 (95) = happyShift action_99
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (38) = happyShift action_2
action_81 (40) = happyShift action_18
action_81 (41) = happyShift action_19
action_81 (42) = happyShift action_20
action_81 (86) = happyShift action_81
action_81 (88) = happyShift action_82
action_81 (90) = happyShift action_83
action_81 (4) = happyGoto action_97
action_81 (17) = happyGoto action_78
action_81 (27) = happyGoto action_79
action_81 (28) = happyGoto action_98
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (38) = happyShift action_2
action_82 (89) = happyShift action_96
action_82 (4) = happyGoto action_94
action_82 (23) = happyGoto action_95
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (38) = happyShift action_2
action_83 (4) = happyGoto action_92
action_83 (25) = happyGoto action_93
action_83 _ = happyReduce_47

action_84 (38) = happyShift action_2
action_84 (40) = happyShift action_18
action_84 (41) = happyShift action_19
action_84 (42) = happyShift action_20
action_84 (86) = happyShift action_81
action_84 (88) = happyShift action_82
action_84 (90) = happyShift action_83
action_84 (4) = happyGoto action_77
action_84 (17) = happyGoto action_78
action_84 (27) = happyGoto action_79
action_84 (28) = happyGoto action_91
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (69) = happyShift action_90
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (38) = happyShift action_2
action_86 (39) = happyShift action_17
action_86 (40) = happyShift action_18
action_86 (41) = happyShift action_19
action_86 (42) = happyShift action_20
action_86 (67) = happyShift action_21
action_86 (70) = happyShift action_22
action_86 (86) = happyShift action_23
action_86 (88) = happyShift action_24
action_86 (90) = happyShift action_25
action_86 (101) = happyShift action_26
action_86 (103) = happyShift action_27
action_86 (4) = happyGoto action_3
action_86 (5) = happyGoto action_4
action_86 (10) = happyGoto action_89
action_86 (11) = happyGoto action_9
action_86 (12) = happyGoto action_10
action_86 (13) = happyGoto action_11
action_86 (16) = happyGoto action_12
action_86 (17) = happyGoto action_13
action_86 (18) = happyGoto action_14
action_86 (21) = happyGoto action_15
action_86 (22) = happyGoto action_16
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (38) = happyShift action_2
action_87 (39) = happyShift action_17
action_87 (40) = happyShift action_18
action_87 (41) = happyShift action_19
action_87 (42) = happyShift action_20
action_87 (67) = happyShift action_21
action_87 (70) = happyShift action_22
action_87 (86) = happyShift action_23
action_87 (88) = happyShift action_24
action_87 (90) = happyShift action_25
action_87 (101) = happyShift action_26
action_87 (103) = happyShift action_27
action_87 (4) = happyGoto action_3
action_87 (5) = happyGoto action_4
action_87 (10) = happyGoto action_88
action_87 (11) = happyGoto action_9
action_87 (12) = happyGoto action_10
action_87 (13) = happyGoto action_11
action_87 (16) = happyGoto action_12
action_87 (17) = happyGoto action_13
action_87 (18) = happyGoto action_14
action_87 (21) = happyGoto action_15
action_87 (22) = happyGoto action_16
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (38) = happyShift action_2
action_88 (39) = happyShift action_17
action_88 (40) = happyShift action_18
action_88 (41) = happyShift action_19
action_88 (42) = happyShift action_20
action_88 (43) = happyShift action_48
action_88 (44) = happyReduce_7
action_88 (60) = happyReduce_7
action_88 (67) = happyShift action_21
action_88 (68) = happyReduce_7
action_88 (69) = happyReduce_7
action_88 (70) = happyShift action_22
action_88 (86) = happyShift action_23
action_88 (87) = happyReduce_7
action_88 (88) = happyShift action_24
action_88 (89) = happyReduce_7
action_88 (90) = happyShift action_25
action_88 (91) = happyReduce_7
action_88 (93) = happyReduce_7
action_88 (94) = happyReduce_7
action_88 (100) = happyReduce_7
action_88 (101) = happyShift action_26
action_88 (103) = happyShift action_27
action_88 (106) = happyReduce_7
action_88 (4) = happyGoto action_3
action_88 (5) = happyGoto action_4
action_88 (11) = happyGoto action_47
action_88 (12) = happyGoto action_10
action_88 (13) = happyGoto action_11
action_88 (16) = happyGoto action_12
action_88 (17) = happyGoto action_13
action_88 (18) = happyGoto action_14
action_88 (21) = happyGoto action_15
action_88 (22) = happyGoto action_16
action_88 _ = happyReduce_7

action_89 (38) = happyShift action_2
action_89 (39) = happyShift action_17
action_89 (40) = happyShift action_18
action_89 (41) = happyShift action_19
action_89 (42) = happyShift action_20
action_89 (43) = happyShift action_48
action_89 (67) = happyShift action_21
action_89 (70) = happyShift action_22
action_89 (86) = happyShift action_23
action_89 (88) = happyShift action_24
action_89 (90) = happyShift action_25
action_89 (101) = happyShift action_26
action_89 (103) = happyShift action_27
action_89 (4) = happyGoto action_3
action_89 (5) = happyGoto action_4
action_89 (11) = happyGoto action_47
action_89 (12) = happyGoto action_10
action_89 (13) = happyGoto action_11
action_89 (16) = happyGoto action_12
action_89 (17) = happyGoto action_13
action_89 (18) = happyGoto action_14
action_89 (21) = happyGoto action_15
action_89 (22) = happyGoto action_16
action_89 _ = happyReduce_9

action_90 (38) = happyShift action_2
action_90 (39) = happyShift action_17
action_90 (40) = happyShift action_18
action_90 (41) = happyShift action_19
action_90 (42) = happyShift action_20
action_90 (67) = happyShift action_21
action_90 (70) = happyShift action_22
action_90 (86) = happyShift action_23
action_90 (88) = happyShift action_24
action_90 (90) = happyShift action_25
action_90 (101) = happyShift action_26
action_90 (103) = happyShift action_27
action_90 (4) = happyGoto action_3
action_90 (5) = happyGoto action_4
action_90 (6) = happyGoto action_115
action_90 (7) = happyGoto action_6
action_90 (8) = happyGoto action_7
action_90 (10) = happyGoto action_8
action_90 (11) = happyGoto action_9
action_90 (12) = happyGoto action_10
action_90 (13) = happyGoto action_11
action_90 (16) = happyGoto action_12
action_90 (17) = happyGoto action_13
action_90 (18) = happyGoto action_14
action_90 (21) = happyGoto action_15
action_90 (22) = happyGoto action_16
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (95) = happyShift action_114
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (94) = happyShift action_113
action_92 _ = happyReduce_48

action_93 (100) = happyShift action_110
action_93 (26) = happyGoto action_112
action_93 _ = happyReduce_50

action_94 (94) = happyShift action_111
action_94 _ = happyReduce_43

action_95 (100) = happyShift action_110
action_95 (26) = happyGoto action_109
action_95 _ = happyReduce_50

action_96 _ = happyReduce_58

action_97 (92) = happyShift action_101
action_97 (94) = happyShift action_108
action_97 (24) = happyGoto action_107
action_97 _ = happyReduce_54

action_98 (87) = happyShift action_106
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (38) = happyShift action_2
action_99 (39) = happyShift action_17
action_99 (40) = happyShift action_18
action_99 (41) = happyShift action_19
action_99 (42) = happyShift action_20
action_99 (67) = happyShift action_21
action_99 (70) = happyShift action_22
action_99 (86) = happyShift action_23
action_99 (88) = happyShift action_24
action_99 (90) = happyShift action_25
action_99 (101) = happyShift action_26
action_99 (103) = happyShift action_27
action_99 (4) = happyGoto action_3
action_99 (5) = happyGoto action_4
action_99 (6) = happyGoto action_105
action_99 (7) = happyGoto action_6
action_99 (8) = happyGoto action_7
action_99 (10) = happyGoto action_8
action_99 (11) = happyGoto action_9
action_99 (12) = happyGoto action_10
action_99 (13) = happyGoto action_11
action_99 (16) = happyGoto action_12
action_99 (17) = happyGoto action_13
action_99 (18) = happyGoto action_14
action_99 (21) = happyGoto action_15
action_99 (22) = happyGoto action_16
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_53

action_101 (38) = happyReduce_52
action_101 (87) = happyReduce_52
action_101 (95) = happyReduce_52
action_101 _ = happyReduce_52

action_102 (38) = happyShift action_2
action_102 (4) = happyGoto action_103
action_102 (19) = happyGoto action_104
action_102 _ = happyReduce_37

action_103 (92) = happyShift action_67
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_38

action_105 _ = happyReduce_64

action_106 _ = happyReduce_61

action_107 (87) = happyShift action_123
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (38) = happyShift action_2
action_108 (4) = happyGoto action_122
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (89) = happyShift action_121
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (38) = happyShift action_2
action_110 (4) = happyGoto action_120
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (38) = happyShift action_2
action_111 (4) = happyGoto action_94
action_111 (23) = happyGoto action_119
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (91) = happyShift action_118
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (38) = happyShift action_2
action_113 (4) = happyGoto action_92
action_113 (25) = happyGoto action_117
action_113 _ = happyReduce_47

action_114 (38) = happyShift action_2
action_114 (39) = happyShift action_17
action_114 (40) = happyShift action_18
action_114 (41) = happyShift action_19
action_114 (42) = happyShift action_20
action_114 (67) = happyShift action_21
action_114 (70) = happyShift action_22
action_114 (86) = happyShift action_23
action_114 (88) = happyShift action_24
action_114 (90) = happyShift action_25
action_114 (101) = happyShift action_26
action_114 (103) = happyShift action_27
action_114 (4) = happyGoto action_3
action_114 (5) = happyGoto action_4
action_114 (6) = happyGoto action_116
action_114 (7) = happyGoto action_6
action_114 (8) = happyGoto action_7
action_114 (10) = happyGoto action_8
action_114 (11) = happyGoto action_9
action_114 (12) = happyGoto action_10
action_114 (13) = happyGoto action_11
action_114 (16) = happyGoto action_12
action_114 (17) = happyGoto action_13
action_114 (18) = happyGoto action_14
action_114 (21) = happyGoto action_15
action_114 (22) = happyGoto action_16
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_16

action_116 _ = happyReduce_65

action_117 _ = happyReduce_49

action_118 _ = happyReduce_60

action_119 _ = happyReduce_44

action_120 _ = happyReduce_51

action_121 _ = happyReduce_59

action_122 (94) = happyShift action_108
action_122 (24) = happyGoto action_124
action_122 _ = happyReduce_45

action_123 _ = happyReduce_57

action_124 _ = happyReduce_46

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (P.identifier happy_var_1 PE.Identifier
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (P.identifier happy_var_1 PE.Hole
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (P.infixApplication happy_var_3 [happy_var_1, happy_var_5]
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (P.fnApplication happy_var_1 [happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (P.parenthesised happy_var_1 happy_var_2 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 6 12 happyReduction_16
happyReduction_16 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  12 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (P.dotLambda happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (P.block happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  14 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (P.backcall [P.pattern $ P.Var happy_var_1] happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn15
		 (fmap PE.Procedure happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  15 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (P.returnStmt happy_var_2 happy_var_1
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (P.term happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.number PL.LInt happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.string PL.LString happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.boolean PL.LBool happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_3)
	(HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (P.record happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  19 happyReduction_37
happyReduction_37  =  HappyAbsSyn19
		 ([]
	)

happyReduce_38 = happyReduce 5 19 happyReduction_38
happyReduction_38 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 19 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ([P.keyValPair happy_var_1 happy_var_3]
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  20 happyReduction_40
happyReduction_40 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (P.keyValPair happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (P.list happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.tuple happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  23 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  23 happyReduction_44
happyReduction_44 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  24 happyReduction_45
happyReduction_45 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn24
		 ([happy_var_2]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  24 happyReduction_46
happyReduction_46 (HappyAbsSyn24  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2 : happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  25 happyReduction_47
happyReduction_47  =  HappyAbsSyn25
		 ([]
	)

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  25 happyReduction_49
happyReduction_49 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  26 happyReduction_50
happyReduction_50  =  HappyAbsSyn26
		 (Nothing
	)

happyReduce_51 = happySpecReduce_2  26 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Just happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  27 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  27 happyReduction_53
happyReduction_53 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn28
		 (P.pattern $ P.Var happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  28 happyReduction_55
happyReduction_55 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn28
		 (P.pattern $ P.Term $ fmap PE.Literal happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  28 happyReduction_56
happyReduction_56 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (P.pattern $ P.Tagged happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 28 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (P.pattern $ P.Tuple (happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_2  28 happyReduction_58
happyReduction_58 _
	_
	 =  HappyAbsSyn28
		 (P.pattern $ P.List [] Nothing
	)

happyReduce_59 = happyReduce 4 28 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (P.pattern $ P.List happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 4 28 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (P.pattern $ P.Record happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_3  28 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  29 happyReduction_62
happyReduction_62 _
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  29 happyReduction_63
happyReduction_63 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happyReduce 4 30 happyReduction_64
happyReduction_64 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 ([P.matchCase happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_65 = happyReduce 5 30 happyReduction_65
happyReduction_65 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (happy_var_1 ++ [P.matchCase happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_1  31 happyReduction_66
happyReduction_66 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  31 happyReduction_67
happyReduction_67 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  32 happyReduction_68
happyReduction_68  =  HappyAbsSyn32
		 ([]
	)

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  33 happyReduction_70
happyReduction_70 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  33 happyReduction_71
happyReduction_71 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  34 happyReduction_72
happyReduction_72 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn34
		 ([happy_var_1]
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  34 happyReduction_73
happyReduction_73 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  35 happyReduction_74
happyReduction_74  =  HappyAbsSyn35
		 ([]
	)

happyReduce_75 = happySpecReduce_1  35 happyReduction_75
happyReduction_75 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_0  36 happyReduction_76
happyReduction_76  =  HappyAbsSyn36
		 (Nothing
	)

happyReduce_77 = happySpecReduce_1  36 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (Just happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  37 happyReduction_78
happyReduction_78 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  37 happyReduction_79
happyReduction_79 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_79 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 106 106 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 38;
	L.RangedToken (T.Hole _) _ -> cont 39;
	L.RangedToken (T.Number _) _ -> cont 40;
	L.RangedToken (T.String _) _ -> cont 41;
	L.RangedToken (T.Boolean _) _ -> cont 42;
	L.RangedToken (T.Operator "!") _ -> cont 43;
	L.RangedToken (T.Operator "+") _ -> cont 44;
	L.RangedToken (T.Operator "-") _ -> cont 45;
	L.RangedToken (T.Operator "*") _ -> cont 46;
	L.RangedToken (T.Operator "/") _ -> cont 47;
	L.RangedToken (T.Operator "^") _ -> cont 48;
	L.RangedToken (T.Operator "++") _ -> cont 49;
	L.RangedToken (T.Operator "==") _ -> cont 50;
	L.RangedToken (T.Operator "!=") _ -> cont 51;
	L.RangedToken (T.Operator "<") _ -> cont 52;
	L.RangedToken (T.Operator "<=") _ -> cont 53;
	L.RangedToken (T.Operator ">") _ -> cont 54;
	L.RangedToken (T.Operator ">=") _ -> cont 55;
	L.RangedToken (T.Operator "||") _ -> cont 56;
	L.RangedToken (T.Operator "&&") _ -> cont 57;
	L.RangedToken (T.Operator "|>") _ -> cont 58;
	L.RangedToken (T.Operator "<|") _ -> cont 59;
	L.RangedToken (T.Operator "<|") _ -> cont 60;
	L.RangedToken (T.Operator "#") _ -> cont 61;
	L.RangedToken (T.Operator _) _ -> cont 62;
	L.RangedToken T.Let _ -> cont 63;
	L.RangedToken T.In _ -> cont 64;
	L.RangedToken T.Where _ -> cont 65;
	L.RangedToken T.With _ -> cont 66;
	L.RangedToken T.If _ -> cont 67;
	L.RangedToken T.Then _ -> cont 68;
	L.RangedToken T.Else _ -> cont 69;
	L.RangedToken T.Match _ -> cont 70;
	L.RangedToken T.Return _ -> cont 71;
	L.RangedToken T.Data _ -> cont 72;
	L.RangedToken T.Type _ -> cont 73;
	L.RangedToken T.Alias _ -> cont 74;
	L.RangedToken T.Kind _ -> cont 75;
	L.RangedToken T.Forall _ -> cont 76;
	L.RangedToken T.Exists _ -> cont 77;
	L.RangedToken T.Proof _ -> cont 78;
	L.RangedToken T.Infer _ -> cont 79;
	L.RangedToken T.Protocol _ -> cont 80;
	L.RangedToken T.Interface _ -> cont 81;
	L.RangedToken T.Instance _ -> cont 82;
	L.RangedToken T.Implements _ -> cont 83;
	L.RangedToken T.Module _ -> cont 84;
	L.RangedToken T.Import _ -> cont 85;
	L.RangedToken T.LParen _ -> cont 86;
	L.RangedToken T.RParen _ -> cont 87;
	L.RangedToken T.LBrack _ -> cont 88;
	L.RangedToken T.RBrack _ -> cont 89;
	L.RangedToken T.LCurly _ -> cont 90;
	L.RangedToken T.RCurly _ -> cont 91;
	L.RangedToken T.Colon _ -> cont 92;
	L.RangedToken T.SemiColon _ -> cont 93;
	L.RangedToken T.Comma _ -> cont 94;
	L.RangedToken T.Arrow _ -> cont 95;
	L.RangedToken T.BackArrow _ -> cont 96;
	L.RangedToken T.FatArrow _ -> cont 97;
	L.RangedToken T.PipeArrow _ -> cont 98;
	L.RangedToken T.Equals _ -> cont 99;
	L.RangedToken T.Pipe _ -> cont 100;
	L.RangedToken T.Dot _ -> cont 101;
	L.RangedToken T.Section _ -> cont 102;
	L.RangedToken T.BackSlash _ -> cont 103;
	L.RangedToken T.Newline _ -> cont 104;
	L.RangedToken T.EOF _ -> cont 105;
	_ -> happyError' (tk, [])
	})

happyError_ explist 106 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


runSagaExpr :: String -> Either String (P.ParsedData PE.Expr)
runSagaExpr input = input `P.run` parseSagaExpr
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
