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
happyExpList = Happy_Data_Array.listArray (0,1231) ([0,0,992,0,18,336,40,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,8,0,0,0,0,1984,0,36,672,64,0,0,0,0,0,8192,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,496,0,9,168,20,0,57344,3,4608,20480,10241,0,0,1984,0,36,672,80,0,32768,15,18432,16384,40965,0,0,256,0,256,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,17408,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,32768,0,0,0,496,0,9,168,20,0,0,0,0,0,16,0,0,0,0,0,256,0,0,0,0,0,32768,64,0,0,0,0,0,0,32,0,0,0,16384,0,0,0,0,32768,0,0,0,256,0,0,8,0,0,0,0,0,4096,0,0,0,0,0,0,1024,0,0,0,8,0,49152,7,9216,40960,16386,0,0,3968,0,72,1344,128,0,0,0,64,0,0,0,0,32768,32768,0,0,0,0,0,124,16384,2,42,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,15,18432,16384,40965,0,0,0,0,0,0,32,0,0,58,0,0,21,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,61440,1,2304,43008,5120,0,0,0,0,0,0,0,0,49152,7,9216,40960,20482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,3,4608,20480,10241,0,0,1984,0,36,672,80,0,0,0,0,0,0,0,0,7936,0,144,2688,320,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,59392,0,0,21504,0,0,0,16,0,0,64,0,0,8192,0,0,0,0,0,0,1856,0,0,672,0,0,0,0,8192,0,0,0,0,7936,0,144,2688,256,0,0,62,8192,1,21,2,0,31744,0,576,10752,1024,0,0,248,32768,4,84,8,0,61440,1,2304,43008,5120,0,0,0,0,0,8192,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,128,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,16,0,0,57344,3,4608,20480,10241,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,128,0,0,0,0,0,0,0,0,0,4,0,0,512,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,32768,0,0,0,16,0,0,0,0,0,57344,3,4608,20480,10241,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","identifier","hole","patListElems","patTupleElems","patRecordKeys","patRest","patData","pattern","cases","matchExpr","patterns","backcall","statement","stmts","returnStmt","block","expr","expr1","expr2","exprBacktick","expr3","expr4","bangApply","expr5","params","expr6","exprAtom","term","record","pairs","list","listElements","tuple","tupleElems","id","HOLE","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'^'","'++'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","'`'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 105
        bit_end = (st Prelude.+ 1) Prelude.* 105
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..104]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (38) = happyShift action_2
action_0 (39) = happyShift action_19
action_0 (40) = happyShift action_20
action_0 (41) = happyShift action_21
action_0 (42) = happyShift action_22
action_0 (66) = happyShift action_23
action_0 (69) = happyShift action_24
action_0 (85) = happyShift action_25
action_0 (87) = happyShift action_26
action_0 (89) = happyShift action_27
action_0 (100) = happyShift action_28
action_0 (102) = happyShift action_29
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (13) = happyGoto action_5
action_0 (20) = happyGoto action_6
action_0 (21) = happyGoto action_7
action_0 (22) = happyGoto action_8
action_0 (24) = happyGoto action_9
action_0 (25) = happyGoto action_10
action_0 (26) = happyGoto action_11
action_0 (27) = happyGoto action_12
action_0 (29) = happyGoto action_13
action_0 (30) = happyGoto action_14
action_0 (31) = happyGoto action_15
action_0 (32) = happyGoto action_16
action_0 (34) = happyGoto action_17
action_0 (36) = happyGoto action_18
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_58

action_4 _ = happyReduce_57

action_5 _ = happyReduce_51

action_6 (105) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (38) = happyReduce_34
action_7 (39) = happyReduce_34
action_7 (40) = happyReduce_34
action_7 (41) = happyReduce_34
action_7 (42) = happyReduce_34
action_7 (43) = happyReduce_34
action_7 (44) = happyShift action_50
action_7 (60) = happyReduce_34
action_7 (66) = happyReduce_34
action_7 (67) = happyReduce_34
action_7 (68) = happyReduce_34
action_7 (69) = happyReduce_34
action_7 (70) = happyReduce_34
action_7 (85) = happyReduce_34
action_7 (86) = happyReduce_34
action_7 (87) = happyReduce_34
action_7 (88) = happyReduce_34
action_7 (89) = happyReduce_34
action_7 (90) = happyReduce_34
action_7 (93) = happyReduce_34
action_7 (99) = happyReduce_34
action_7 (100) = happyReduce_34
action_7 (102) = happyReduce_34
action_7 (105) = happyReduce_34
action_7 _ = happyReduce_34

action_8 (38) = happyReduce_35
action_8 (39) = happyReduce_35
action_8 (40) = happyReduce_35
action_8 (41) = happyReduce_35
action_8 (42) = happyReduce_35
action_8 (43) = happyReduce_35
action_8 (44) = happyReduce_35
action_8 (60) = happyShift action_49
action_8 (66) = happyReduce_35
action_8 (67) = happyReduce_35
action_8 (68) = happyReduce_35
action_8 (69) = happyReduce_35
action_8 (70) = happyReduce_35
action_8 (85) = happyReduce_35
action_8 (86) = happyReduce_35
action_8 (87) = happyReduce_35
action_8 (88) = happyReduce_35
action_8 (89) = happyReduce_35
action_8 (90) = happyReduce_35
action_8 (93) = happyReduce_35
action_8 (99) = happyReduce_35
action_8 (100) = happyReduce_35
action_8 (102) = happyReduce_35
action_8 (105) = happyReduce_35
action_8 _ = happyReduce_35

action_9 (38) = happyShift action_2
action_9 (39) = happyShift action_19
action_9 (40) = happyShift action_20
action_9 (41) = happyShift action_21
action_9 (42) = happyShift action_22
action_9 (43) = happyReduce_38
action_9 (44) = happyReduce_38
action_9 (60) = happyReduce_38
action_9 (66) = happyShift action_23
action_9 (67) = happyReduce_38
action_9 (68) = happyReduce_38
action_9 (69) = happyShift action_24
action_9 (70) = happyReduce_38
action_9 (85) = happyShift action_25
action_9 (86) = happyReduce_38
action_9 (87) = happyShift action_26
action_9 (88) = happyReduce_38
action_9 (89) = happyShift action_27
action_9 (90) = happyReduce_38
action_9 (93) = happyReduce_38
action_9 (99) = happyReduce_38
action_9 (100) = happyReduce_38
action_9 (102) = happyShift action_29
action_9 (105) = happyReduce_38
action_9 (4) = happyGoto action_3
action_9 (5) = happyGoto action_4
action_9 (13) = happyGoto action_5
action_9 (25) = happyGoto action_48
action_9 (26) = happyGoto action_11
action_9 (27) = happyGoto action_12
action_9 (29) = happyGoto action_13
action_9 (30) = happyGoto action_14
action_9 (31) = happyGoto action_15
action_9 (32) = happyGoto action_16
action_9 (34) = happyGoto action_17
action_9 (36) = happyGoto action_18
action_9 _ = happyReduce_38

action_10 (38) = happyReduce_42
action_10 (39) = happyReduce_42
action_10 (40) = happyReduce_42
action_10 (41) = happyReduce_42
action_10 (42) = happyReduce_42
action_10 (43) = happyReduce_42
action_10 (44) = happyReduce_42
action_10 (60) = happyReduce_42
action_10 (66) = happyReduce_42
action_10 (67) = happyReduce_42
action_10 (68) = happyReduce_42
action_10 (69) = happyReduce_42
action_10 (70) = happyReduce_42
action_10 (85) = happyReduce_42
action_10 (86) = happyReduce_42
action_10 (87) = happyReduce_42
action_10 (88) = happyReduce_42
action_10 (89) = happyReduce_42
action_10 (90) = happyReduce_42
action_10 (93) = happyReduce_42
action_10 (99) = happyReduce_42
action_10 (100) = happyShift action_47
action_10 (102) = happyReduce_42
action_10 (105) = happyReduce_42
action_10 _ = happyReduce_42

action_11 (100) = happyShift action_46
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_45

action_13 _ = happyReduce_49

action_14 (38) = happyShift action_2
action_14 (39) = happyShift action_19
action_14 (40) = happyShift action_20
action_14 (41) = happyShift action_21
action_14 (42) = happyShift action_22
action_14 (66) = happyShift action_23
action_14 (69) = happyShift action_24
action_14 (85) = happyShift action_25
action_14 (87) = happyShift action_26
action_14 (89) = happyShift action_27
action_14 (102) = happyShift action_29
action_14 (4) = happyGoto action_3
action_14 (5) = happyGoto action_4
action_14 (13) = happyGoto action_5
action_14 (25) = happyGoto action_45
action_14 (26) = happyGoto action_11
action_14 (27) = happyGoto action_12
action_14 (29) = happyGoto action_13
action_14 (30) = happyGoto action_14
action_14 (31) = happyGoto action_15
action_14 (32) = happyGoto action_16
action_14 (34) = happyGoto action_17
action_14 (36) = happyGoto action_18
action_14 _ = happyReduce_55

action_15 _ = happyReduce_59

action_16 _ = happyReduce_62

action_17 _ = happyReduce_61

action_18 _ = happyReduce_60

action_19 _ = happyReduce_2

action_20 _ = happyReduce_64

action_21 _ = happyReduce_65

action_22 _ = happyReduce_66

action_23 (38) = happyShift action_2
action_23 (39) = happyShift action_19
action_23 (40) = happyShift action_20
action_23 (41) = happyShift action_21
action_23 (42) = happyShift action_22
action_23 (66) = happyShift action_23
action_23 (69) = happyShift action_24
action_23 (85) = happyShift action_25
action_23 (87) = happyShift action_26
action_23 (89) = happyShift action_27
action_23 (100) = happyShift action_28
action_23 (102) = happyShift action_29
action_23 (4) = happyGoto action_3
action_23 (5) = happyGoto action_4
action_23 (13) = happyGoto action_5
action_23 (20) = happyGoto action_44
action_23 (21) = happyGoto action_7
action_23 (22) = happyGoto action_8
action_23 (24) = happyGoto action_9
action_23 (25) = happyGoto action_10
action_23 (26) = happyGoto action_11
action_23 (27) = happyGoto action_12
action_23 (29) = happyGoto action_13
action_23 (30) = happyGoto action_14
action_23 (31) = happyGoto action_15
action_23 (32) = happyGoto action_16
action_23 (34) = happyGoto action_17
action_23 (36) = happyGoto action_18
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (38) = happyShift action_2
action_24 (39) = happyShift action_19
action_24 (40) = happyShift action_20
action_24 (41) = happyShift action_21
action_24 (42) = happyShift action_22
action_24 (66) = happyShift action_23
action_24 (69) = happyShift action_24
action_24 (85) = happyShift action_25
action_24 (87) = happyShift action_26
action_24 (89) = happyShift action_27
action_24 (100) = happyShift action_28
action_24 (102) = happyShift action_29
action_24 (4) = happyGoto action_3
action_24 (5) = happyGoto action_4
action_24 (13) = happyGoto action_5
action_24 (20) = happyGoto action_43
action_24 (21) = happyGoto action_7
action_24 (22) = happyGoto action_8
action_24 (24) = happyGoto action_9
action_24 (25) = happyGoto action_10
action_24 (26) = happyGoto action_11
action_24 (27) = happyGoto action_12
action_24 (29) = happyGoto action_13
action_24 (30) = happyGoto action_14
action_24 (31) = happyGoto action_15
action_24 (32) = happyGoto action_16
action_24 (34) = happyGoto action_17
action_24 (36) = happyGoto action_18
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (38) = happyShift action_2
action_25 (39) = happyShift action_19
action_25 (40) = happyShift action_20
action_25 (41) = happyShift action_21
action_25 (42) = happyShift action_22
action_25 (66) = happyShift action_23
action_25 (69) = happyShift action_24
action_25 (85) = happyShift action_25
action_25 (87) = happyShift action_26
action_25 (89) = happyShift action_27
action_25 (100) = happyShift action_28
action_25 (102) = happyShift action_29
action_25 (4) = happyGoto action_3
action_25 (5) = happyGoto action_4
action_25 (13) = happyGoto action_5
action_25 (20) = happyGoto action_42
action_25 (21) = happyGoto action_7
action_25 (22) = happyGoto action_8
action_25 (24) = happyGoto action_9
action_25 (25) = happyGoto action_10
action_25 (26) = happyGoto action_11
action_25 (27) = happyGoto action_12
action_25 (29) = happyGoto action_13
action_25 (30) = happyGoto action_14
action_25 (31) = happyGoto action_15
action_25 (32) = happyGoto action_16
action_25 (34) = happyGoto action_17
action_25 (36) = happyGoto action_18
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (38) = happyShift action_2
action_26 (39) = happyShift action_19
action_26 (40) = happyShift action_20
action_26 (41) = happyShift action_21
action_26 (42) = happyShift action_22
action_26 (66) = happyShift action_23
action_26 (69) = happyShift action_24
action_26 (85) = happyShift action_25
action_26 (87) = happyShift action_26
action_26 (89) = happyShift action_27
action_26 (100) = happyShift action_28
action_26 (102) = happyShift action_29
action_26 (4) = happyGoto action_3
action_26 (5) = happyGoto action_4
action_26 (13) = happyGoto action_5
action_26 (20) = happyGoto action_40
action_26 (21) = happyGoto action_7
action_26 (22) = happyGoto action_8
action_26 (24) = happyGoto action_9
action_26 (25) = happyGoto action_10
action_26 (26) = happyGoto action_11
action_26 (27) = happyGoto action_12
action_26 (29) = happyGoto action_13
action_26 (30) = happyGoto action_14
action_26 (31) = happyGoto action_15
action_26 (32) = happyGoto action_16
action_26 (34) = happyGoto action_17
action_26 (35) = happyGoto action_41
action_26 (36) = happyGoto action_18
action_26 _ = happyReduce_72

action_27 (38) = happyShift action_2
action_27 (70) = happyShift action_39
action_27 (4) = happyGoto action_32
action_27 (15) = happyGoto action_33
action_27 (16) = happyGoto action_34
action_27 (17) = happyGoto action_35
action_27 (18) = happyGoto action_36
action_27 (19) = happyGoto action_37
action_27 (33) = happyGoto action_38
action_27 _ = happyReduce_68

action_28 (38) = happyShift action_2
action_28 (4) = happyGoto action_31
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (28) = happyGoto action_30
action_29 _ = happyReduce_53

action_30 (38) = happyShift action_2
action_30 (94) = happyShift action_75
action_30 (4) = happyGoto action_74
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_37

action_32 (91) = happyShift action_72
action_32 (95) = happyShift action_73
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_28

action_34 _ = happyReduce_29

action_35 (38) = happyShift action_2
action_35 (70) = happyShift action_39
action_35 (4) = happyGoto action_69
action_35 (15) = happyGoto action_33
action_35 (16) = happyGoto action_70
action_35 (18) = happyGoto action_71
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_32

action_37 (90) = happyShift action_68
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (90) = happyShift action_67
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (38) = happyShift action_2
action_39 (39) = happyShift action_19
action_39 (40) = happyShift action_20
action_39 (41) = happyShift action_21
action_39 (42) = happyShift action_22
action_39 (66) = happyShift action_23
action_39 (69) = happyShift action_24
action_39 (85) = happyShift action_25
action_39 (87) = happyShift action_26
action_39 (89) = happyShift action_27
action_39 (100) = happyShift action_28
action_39 (102) = happyShift action_29
action_39 (4) = happyGoto action_3
action_39 (5) = happyGoto action_4
action_39 (13) = happyGoto action_5
action_39 (20) = happyGoto action_66
action_39 (21) = happyGoto action_7
action_39 (22) = happyGoto action_8
action_39 (24) = happyGoto action_9
action_39 (25) = happyGoto action_10
action_39 (26) = happyGoto action_11
action_39 (27) = happyGoto action_12
action_39 (29) = happyGoto action_13
action_39 (30) = happyGoto action_14
action_39 (31) = happyGoto action_15
action_39 (32) = happyGoto action_16
action_39 (34) = happyGoto action_17
action_39 (36) = happyGoto action_18
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (93) = happyShift action_65
action_40 _ = happyReduce_73

action_41 (88) = happyShift action_64
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (86) = happyShift action_62
action_42 (93) = happyShift action_63
action_42 (37) = happyGoto action_61
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (99) = happyShift action_60
action_43 (12) = happyGoto action_59
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (67) = happyShift action_58
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (43) = happyShift action_57
action_45 (100) = happyShift action_47
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (38) = happyShift action_2
action_46 (4) = happyGoto action_56
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (38) = happyShift action_2
action_47 (4) = happyGoto action_55
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (38) = happyReduce_43
action_48 (39) = happyReduce_43
action_48 (40) = happyReduce_43
action_48 (41) = happyReduce_43
action_48 (42) = happyReduce_43
action_48 (43) = happyShift action_54
action_48 (44) = happyReduce_43
action_48 (60) = happyReduce_43
action_48 (66) = happyReduce_43
action_48 (67) = happyReduce_43
action_48 (68) = happyReduce_43
action_48 (69) = happyReduce_43
action_48 (70) = happyReduce_43
action_48 (85) = happyReduce_43
action_48 (86) = happyReduce_43
action_48 (87) = happyReduce_43
action_48 (88) = happyReduce_43
action_48 (89) = happyReduce_43
action_48 (90) = happyReduce_43
action_48 (93) = happyReduce_43
action_48 (99) = happyReduce_43
action_48 (100) = happyShift action_47
action_48 (102) = happyReduce_43
action_48 (105) = happyReduce_43
action_48 _ = happyReduce_43

action_49 (38) = happyShift action_2
action_49 (39) = happyShift action_19
action_49 (40) = happyShift action_20
action_49 (41) = happyShift action_21
action_49 (42) = happyShift action_22
action_49 (66) = happyShift action_23
action_49 (69) = happyShift action_24
action_49 (85) = happyShift action_25
action_49 (87) = happyShift action_26
action_49 (89) = happyShift action_27
action_49 (102) = happyShift action_29
action_49 (4) = happyGoto action_3
action_49 (5) = happyGoto action_4
action_49 (13) = happyGoto action_5
action_49 (23) = happyGoto action_52
action_49 (24) = happyGoto action_53
action_49 (25) = happyGoto action_10
action_49 (26) = happyGoto action_11
action_49 (27) = happyGoto action_12
action_49 (29) = happyGoto action_13
action_49 (30) = happyGoto action_14
action_49 (31) = happyGoto action_15
action_49 (32) = happyGoto action_16
action_49 (34) = happyGoto action_17
action_49 (36) = happyGoto action_18
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (38) = happyShift action_2
action_50 (39) = happyShift action_19
action_50 (40) = happyShift action_20
action_50 (41) = happyShift action_21
action_50 (42) = happyShift action_22
action_50 (66) = happyShift action_23
action_50 (69) = happyShift action_24
action_50 (85) = happyShift action_25
action_50 (87) = happyShift action_26
action_50 (89) = happyShift action_27
action_50 (102) = happyShift action_29
action_50 (4) = happyGoto action_3
action_50 (5) = happyGoto action_4
action_50 (13) = happyGoto action_5
action_50 (22) = happyGoto action_51
action_50 (24) = happyGoto action_9
action_50 (25) = happyGoto action_10
action_50 (26) = happyGoto action_11
action_50 (27) = happyGoto action_12
action_50 (29) = happyGoto action_13
action_50 (30) = happyGoto action_14
action_50 (31) = happyGoto action_15
action_50 (32) = happyGoto action_16
action_50 (34) = happyGoto action_17
action_50 (36) = happyGoto action_18
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (38) = happyReduce_36
action_51 (39) = happyReduce_36
action_51 (40) = happyReduce_36
action_51 (41) = happyReduce_36
action_51 (42) = happyReduce_36
action_51 (43) = happyReduce_36
action_51 (44) = happyReduce_36
action_51 (60) = happyShift action_49
action_51 (66) = happyReduce_36
action_51 (67) = happyReduce_36
action_51 (68) = happyReduce_36
action_51 (69) = happyReduce_36
action_51 (70) = happyReduce_36
action_51 (85) = happyReduce_36
action_51 (86) = happyReduce_36
action_51 (87) = happyReduce_36
action_51 (88) = happyReduce_36
action_51 (89) = happyReduce_36
action_51 (90) = happyReduce_36
action_51 (93) = happyReduce_36
action_51 (99) = happyReduce_36
action_51 (100) = happyReduce_36
action_51 (102) = happyReduce_36
action_51 (105) = happyReduce_36
action_51 _ = happyReduce_36

action_52 (44) = happyShift action_91
action_52 (60) = happyShift action_92
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (38) = happyShift action_2
action_53 (39) = happyShift action_19
action_53 (40) = happyShift action_20
action_53 (41) = happyShift action_21
action_53 (42) = happyShift action_22
action_53 (66) = happyShift action_23
action_53 (69) = happyShift action_24
action_53 (85) = happyShift action_25
action_53 (87) = happyShift action_26
action_53 (89) = happyShift action_27
action_53 (102) = happyShift action_29
action_53 (4) = happyGoto action_3
action_53 (5) = happyGoto action_4
action_53 (13) = happyGoto action_5
action_53 (25) = happyGoto action_48
action_53 (26) = happyGoto action_11
action_53 (27) = happyGoto action_12
action_53 (29) = happyGoto action_13
action_53 (30) = happyGoto action_14
action_53 (31) = happyGoto action_15
action_53 (32) = happyGoto action_16
action_53 (34) = happyGoto action_17
action_53 (36) = happyGoto action_18
action_53 _ = happyReduce_40

action_54 _ = happyReduce_44

action_55 (38) = happyReduce_46
action_55 (39) = happyReduce_46
action_55 (40) = happyReduce_46
action_55 (41) = happyReduce_46
action_55 (42) = happyReduce_46
action_55 (43) = happyReduce_46
action_55 (44) = happyReduce_46
action_55 (60) = happyReduce_46
action_55 (66) = happyReduce_46
action_55 (67) = happyReduce_46
action_55 (68) = happyReduce_46
action_55 (69) = happyReduce_46
action_55 (70) = happyReduce_46
action_55 (85) = happyReduce_46
action_55 (86) = happyReduce_46
action_55 (87) = happyReduce_46
action_55 (88) = happyReduce_46
action_55 (89) = happyReduce_46
action_55 (90) = happyReduce_46
action_55 (93) = happyReduce_46
action_55 (99) = happyReduce_46
action_55 (100) = happyReduce_46
action_55 (102) = happyReduce_46
action_55 (105) = happyReduce_46
action_55 _ = happyReduce_46

action_56 (38) = happyReduce_47
action_56 (39) = happyReduce_47
action_56 (40) = happyReduce_47
action_56 (41) = happyReduce_47
action_56 (42) = happyReduce_47
action_56 (43) = happyReduce_47
action_56 (44) = happyReduce_47
action_56 (60) = happyReduce_47
action_56 (66) = happyReduce_47
action_56 (67) = happyReduce_47
action_56 (68) = happyReduce_47
action_56 (69) = happyReduce_47
action_56 (70) = happyReduce_47
action_56 (85) = happyReduce_47
action_56 (86) = happyReduce_47
action_56 (87) = happyReduce_47
action_56 (88) = happyReduce_47
action_56 (89) = happyReduce_47
action_56 (90) = happyReduce_47
action_56 (93) = happyReduce_47
action_56 (99) = happyReduce_47
action_56 (100) = happyReduce_47
action_56 (102) = happyReduce_47
action_56 (105) = happyReduce_47
action_56 _ = happyReduce_47

action_57 _ = happyReduce_48

action_58 (38) = happyShift action_2
action_58 (39) = happyShift action_19
action_58 (40) = happyShift action_20
action_58 (41) = happyShift action_21
action_58 (42) = happyShift action_22
action_58 (66) = happyShift action_23
action_58 (69) = happyShift action_24
action_58 (85) = happyShift action_25
action_58 (87) = happyShift action_26
action_58 (89) = happyShift action_27
action_58 (100) = happyShift action_28
action_58 (102) = happyShift action_29
action_58 (4) = happyGoto action_3
action_58 (5) = happyGoto action_4
action_58 (13) = happyGoto action_5
action_58 (20) = happyGoto action_90
action_58 (21) = happyGoto action_7
action_58 (22) = happyGoto action_8
action_58 (24) = happyGoto action_9
action_58 (25) = happyGoto action_10
action_58 (26) = happyGoto action_11
action_58 (27) = happyGoto action_12
action_58 (29) = happyGoto action_13
action_58 (30) = happyGoto action_14
action_58 (31) = happyGoto action_15
action_58 (32) = happyGoto action_16
action_58 (34) = happyGoto action_17
action_58 (36) = happyGoto action_18
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (38) = happyReduce_24
action_59 (39) = happyReduce_24
action_59 (40) = happyReduce_24
action_59 (41) = happyReduce_24
action_59 (42) = happyReduce_24
action_59 (43) = happyReduce_24
action_59 (44) = happyReduce_24
action_59 (60) = happyReduce_24
action_59 (66) = happyReduce_24
action_59 (67) = happyReduce_24
action_59 (68) = happyReduce_24
action_59 (69) = happyReduce_24
action_59 (70) = happyReduce_24
action_59 (85) = happyReduce_24
action_59 (86) = happyReduce_24
action_59 (87) = happyReduce_24
action_59 (88) = happyReduce_24
action_59 (89) = happyReduce_24
action_59 (90) = happyReduce_24
action_59 (93) = happyReduce_24
action_59 (99) = happyShift action_89
action_59 (100) = happyReduce_24
action_59 (102) = happyReduce_24
action_59 (105) = happyReduce_24
action_59 _ = happyReduce_24

action_60 (38) = happyShift action_2
action_60 (40) = happyShift action_20
action_60 (41) = happyShift action_21
action_60 (42) = happyShift action_22
action_60 (85) = happyShift action_86
action_60 (87) = happyShift action_87
action_60 (89) = happyShift action_88
action_60 (4) = happyGoto action_82
action_60 (10) = happyGoto action_83
action_60 (11) = happyGoto action_84
action_60 (31) = happyGoto action_85
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (86) = happyShift action_81
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_63

action_63 (38) = happyShift action_2
action_63 (39) = happyShift action_19
action_63 (40) = happyShift action_20
action_63 (41) = happyShift action_21
action_63 (42) = happyShift action_22
action_63 (66) = happyShift action_23
action_63 (69) = happyShift action_24
action_63 (85) = happyShift action_25
action_63 (87) = happyShift action_26
action_63 (89) = happyShift action_27
action_63 (100) = happyShift action_28
action_63 (102) = happyShift action_29
action_63 (4) = happyGoto action_3
action_63 (5) = happyGoto action_4
action_63 (13) = happyGoto action_5
action_63 (20) = happyGoto action_80
action_63 (21) = happyGoto action_7
action_63 (22) = happyGoto action_8
action_63 (24) = happyGoto action_9
action_63 (25) = happyGoto action_10
action_63 (26) = happyGoto action_11
action_63 (27) = happyGoto action_12
action_63 (29) = happyGoto action_13
action_63 (30) = happyGoto action_14
action_63 (31) = happyGoto action_15
action_63 (32) = happyGoto action_16
action_63 (34) = happyGoto action_17
action_63 (36) = happyGoto action_18
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_71

action_65 (38) = happyShift action_2
action_65 (39) = happyShift action_19
action_65 (40) = happyShift action_20
action_65 (41) = happyShift action_21
action_65 (42) = happyShift action_22
action_65 (66) = happyShift action_23
action_65 (69) = happyShift action_24
action_65 (85) = happyShift action_25
action_65 (87) = happyShift action_26
action_65 (89) = happyShift action_27
action_65 (100) = happyShift action_28
action_65 (102) = happyShift action_29
action_65 (4) = happyGoto action_3
action_65 (5) = happyGoto action_4
action_65 (13) = happyGoto action_5
action_65 (20) = happyGoto action_40
action_65 (21) = happyGoto action_7
action_65 (22) = happyGoto action_8
action_65 (24) = happyGoto action_9
action_65 (25) = happyGoto action_10
action_65 (26) = happyGoto action_11
action_65 (27) = happyGoto action_12
action_65 (29) = happyGoto action_13
action_65 (30) = happyGoto action_14
action_65 (31) = happyGoto action_15
action_65 (32) = happyGoto action_16
action_65 (34) = happyGoto action_17
action_65 (35) = happyGoto action_79
action_65 (36) = happyGoto action_18
action_65 _ = happyReduce_72

action_66 _ = happyReduce_31

action_67 _ = happyReduce_67

action_68 _ = happyReduce_56

action_69 (95) = happyShift action_73
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_30

action_71 _ = happyReduce_33

action_72 (38) = happyShift action_2
action_72 (39) = happyShift action_19
action_72 (40) = happyShift action_20
action_72 (41) = happyShift action_21
action_72 (42) = happyShift action_22
action_72 (66) = happyShift action_23
action_72 (69) = happyShift action_24
action_72 (85) = happyShift action_25
action_72 (87) = happyShift action_26
action_72 (89) = happyShift action_27
action_72 (100) = happyShift action_28
action_72 (102) = happyShift action_29
action_72 (4) = happyGoto action_3
action_72 (5) = happyGoto action_4
action_72 (13) = happyGoto action_5
action_72 (20) = happyGoto action_78
action_72 (21) = happyGoto action_7
action_72 (22) = happyGoto action_8
action_72 (24) = happyGoto action_9
action_72 (25) = happyGoto action_10
action_72 (26) = happyGoto action_11
action_72 (27) = happyGoto action_12
action_72 (29) = happyGoto action_13
action_72 (30) = happyGoto action_14
action_72 (31) = happyGoto action_15
action_72 (32) = happyGoto action_16
action_72 (34) = happyGoto action_17
action_72 (36) = happyGoto action_18
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (38) = happyShift action_2
action_73 (39) = happyShift action_19
action_73 (40) = happyShift action_20
action_73 (41) = happyShift action_21
action_73 (42) = happyShift action_22
action_73 (66) = happyShift action_23
action_73 (69) = happyShift action_24
action_73 (85) = happyShift action_25
action_73 (87) = happyShift action_26
action_73 (89) = happyShift action_27
action_73 (100) = happyShift action_28
action_73 (102) = happyShift action_29
action_73 (4) = happyGoto action_3
action_73 (5) = happyGoto action_4
action_73 (13) = happyGoto action_5
action_73 (20) = happyGoto action_77
action_73 (21) = happyGoto action_7
action_73 (22) = happyGoto action_8
action_73 (24) = happyGoto action_9
action_73 (25) = happyGoto action_10
action_73 (26) = happyGoto action_11
action_73 (27) = happyGoto action_12
action_73 (29) = happyGoto action_13
action_73 (30) = happyGoto action_14
action_73 (31) = happyGoto action_15
action_73 (32) = happyGoto action_16
action_73 (34) = happyGoto action_17
action_73 (36) = happyGoto action_18
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_54

action_75 (38) = happyShift action_2
action_75 (39) = happyShift action_19
action_75 (40) = happyShift action_20
action_75 (41) = happyShift action_21
action_75 (42) = happyShift action_22
action_75 (66) = happyShift action_23
action_75 (69) = happyShift action_24
action_75 (85) = happyShift action_25
action_75 (87) = happyShift action_26
action_75 (89) = happyShift action_27
action_75 (100) = happyShift action_28
action_75 (102) = happyShift action_29
action_75 (4) = happyGoto action_3
action_75 (5) = happyGoto action_4
action_75 (13) = happyGoto action_5
action_75 (20) = happyGoto action_76
action_75 (21) = happyGoto action_7
action_75 (22) = happyGoto action_8
action_75 (24) = happyGoto action_9
action_75 (25) = happyGoto action_10
action_75 (26) = happyGoto action_11
action_75 (27) = happyGoto action_12
action_75 (29) = happyGoto action_13
action_75 (30) = happyGoto action_14
action_75 (31) = happyGoto action_15
action_75 (32) = happyGoto action_16
action_75 (34) = happyGoto action_17
action_75 (36) = happyGoto action_18
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_52

action_77 _ = happyReduce_27

action_78 (93) = happyShift action_108
action_78 _ = happyReduce_70

action_79 _ = happyReduce_74

action_80 (93) = happyShift action_63
action_80 (37) = happyGoto action_107
action_80 _ = happyReduce_76

action_81 _ = happyReduce_75

action_82 (91) = happyShift action_106
action_82 _ = happyReduce_14

action_83 (38) = happyShift action_2
action_83 (4) = happyGoto action_105
action_83 _ = happyReduce_16

action_84 (94) = happyShift action_104
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_15

action_86 (38) = happyShift action_2
action_86 (40) = happyShift action_20
action_86 (41) = happyShift action_21
action_86 (42) = happyShift action_22
action_86 (85) = happyShift action_86
action_86 (87) = happyShift action_87
action_86 (89) = happyShift action_88
action_86 (4) = happyGoto action_102
action_86 (10) = happyGoto action_83
action_86 (11) = happyGoto action_103
action_86 (31) = happyGoto action_85
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (38) = happyShift action_2
action_87 (88) = happyShift action_101
action_87 (4) = happyGoto action_99
action_87 (6) = happyGoto action_100
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (38) = happyShift action_2
action_88 (4) = happyGoto action_97
action_88 (8) = happyGoto action_98
action_88 _ = happyReduce_7

action_89 (38) = happyShift action_2
action_89 (40) = happyShift action_20
action_89 (41) = happyShift action_21
action_89 (42) = happyShift action_22
action_89 (85) = happyShift action_86
action_89 (87) = happyShift action_87
action_89 (89) = happyShift action_88
action_89 (4) = happyGoto action_82
action_89 (10) = happyGoto action_83
action_89 (11) = happyGoto action_96
action_89 (31) = happyGoto action_85
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (68) = happyShift action_95
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (38) = happyShift action_2
action_91 (39) = happyShift action_19
action_91 (40) = happyShift action_20
action_91 (41) = happyShift action_21
action_91 (42) = happyShift action_22
action_91 (66) = happyShift action_23
action_91 (69) = happyShift action_24
action_91 (85) = happyShift action_25
action_91 (87) = happyShift action_26
action_91 (89) = happyShift action_27
action_91 (102) = happyShift action_29
action_91 (4) = happyGoto action_3
action_91 (5) = happyGoto action_4
action_91 (13) = happyGoto action_5
action_91 (24) = happyGoto action_94
action_91 (25) = happyGoto action_10
action_91 (26) = happyGoto action_11
action_91 (27) = happyGoto action_12
action_91 (29) = happyGoto action_13
action_91 (30) = happyGoto action_14
action_91 (31) = happyGoto action_15
action_91 (32) = happyGoto action_16
action_91 (34) = happyGoto action_17
action_91 (36) = happyGoto action_18
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (38) = happyShift action_2
action_92 (39) = happyShift action_19
action_92 (40) = happyShift action_20
action_92 (41) = happyShift action_21
action_92 (42) = happyShift action_22
action_92 (66) = happyShift action_23
action_92 (69) = happyShift action_24
action_92 (85) = happyShift action_25
action_92 (87) = happyShift action_26
action_92 (89) = happyShift action_27
action_92 (102) = happyShift action_29
action_92 (4) = happyGoto action_3
action_92 (5) = happyGoto action_4
action_92 (13) = happyGoto action_5
action_92 (24) = happyGoto action_93
action_92 (25) = happyGoto action_10
action_92 (26) = happyGoto action_11
action_92 (27) = happyGoto action_12
action_92 (29) = happyGoto action_13
action_92 (30) = happyGoto action_14
action_92 (31) = happyGoto action_15
action_92 (32) = happyGoto action_16
action_92 (34) = happyGoto action_17
action_92 (36) = happyGoto action_18
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (38) = happyShift action_2
action_93 (39) = happyShift action_19
action_93 (40) = happyShift action_20
action_93 (41) = happyShift action_21
action_93 (42) = happyShift action_22
action_93 (43) = happyReduce_39
action_93 (44) = happyReduce_39
action_93 (60) = happyReduce_39
action_93 (66) = happyShift action_23
action_93 (67) = happyReduce_39
action_93 (68) = happyReduce_39
action_93 (69) = happyShift action_24
action_93 (70) = happyReduce_39
action_93 (85) = happyShift action_25
action_93 (86) = happyReduce_39
action_93 (87) = happyShift action_26
action_93 (88) = happyReduce_39
action_93 (89) = happyShift action_27
action_93 (90) = happyReduce_39
action_93 (93) = happyReduce_39
action_93 (99) = happyReduce_39
action_93 (100) = happyReduce_39
action_93 (102) = happyShift action_29
action_93 (105) = happyReduce_39
action_93 (4) = happyGoto action_3
action_93 (5) = happyGoto action_4
action_93 (13) = happyGoto action_5
action_93 (25) = happyGoto action_48
action_93 (26) = happyGoto action_11
action_93 (27) = happyGoto action_12
action_93 (29) = happyGoto action_13
action_93 (30) = happyGoto action_14
action_93 (31) = happyGoto action_15
action_93 (32) = happyGoto action_16
action_93 (34) = happyGoto action_17
action_93 (36) = happyGoto action_18
action_93 _ = happyReduce_39

action_94 (38) = happyShift action_2
action_94 (39) = happyShift action_19
action_94 (40) = happyShift action_20
action_94 (41) = happyShift action_21
action_94 (42) = happyShift action_22
action_94 (66) = happyShift action_23
action_94 (69) = happyShift action_24
action_94 (85) = happyShift action_25
action_94 (87) = happyShift action_26
action_94 (89) = happyShift action_27
action_94 (102) = happyShift action_29
action_94 (4) = happyGoto action_3
action_94 (5) = happyGoto action_4
action_94 (13) = happyGoto action_5
action_94 (25) = happyGoto action_48
action_94 (26) = happyGoto action_11
action_94 (27) = happyGoto action_12
action_94 (29) = happyGoto action_13
action_94 (30) = happyGoto action_14
action_94 (31) = happyGoto action_15
action_94 (32) = happyGoto action_16
action_94 (34) = happyGoto action_17
action_94 (36) = happyGoto action_18
action_94 _ = happyReduce_41

action_95 (38) = happyShift action_2
action_95 (39) = happyShift action_19
action_95 (40) = happyShift action_20
action_95 (41) = happyShift action_21
action_95 (42) = happyShift action_22
action_95 (66) = happyShift action_23
action_95 (69) = happyShift action_24
action_95 (85) = happyShift action_25
action_95 (87) = happyShift action_26
action_95 (89) = happyShift action_27
action_95 (100) = happyShift action_28
action_95 (102) = happyShift action_29
action_95 (4) = happyGoto action_3
action_95 (5) = happyGoto action_4
action_95 (13) = happyGoto action_5
action_95 (20) = happyGoto action_121
action_95 (21) = happyGoto action_7
action_95 (22) = happyGoto action_8
action_95 (24) = happyGoto action_9
action_95 (25) = happyGoto action_10
action_95 (26) = happyGoto action_11
action_95 (27) = happyGoto action_12
action_95 (29) = happyGoto action_13
action_95 (30) = happyGoto action_14
action_95 (31) = happyGoto action_15
action_95 (32) = happyGoto action_16
action_95 (34) = happyGoto action_17
action_95 (36) = happyGoto action_18
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (94) = happyShift action_120
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (93) = happyShift action_119
action_97 _ = happyReduce_8

action_98 (99) = happyShift action_116
action_98 (9) = happyGoto action_118
action_98 _ = happyReduce_10

action_99 (93) = happyShift action_117
action_99 _ = happyReduce_3

action_100 (99) = happyShift action_116
action_100 (9) = happyGoto action_115
action_100 _ = happyReduce_10

action_101 _ = happyReduce_18

action_102 (91) = happyShift action_106
action_102 (93) = happyShift action_114
action_102 (7) = happyGoto action_113
action_102 _ = happyReduce_14

action_103 (86) = happyShift action_112
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (38) = happyShift action_2
action_104 (39) = happyShift action_19
action_104 (40) = happyShift action_20
action_104 (41) = happyShift action_21
action_104 (42) = happyShift action_22
action_104 (66) = happyShift action_23
action_104 (69) = happyShift action_24
action_104 (85) = happyShift action_25
action_104 (87) = happyShift action_26
action_104 (89) = happyShift action_27
action_104 (100) = happyShift action_28
action_104 (102) = happyShift action_29
action_104 (4) = happyGoto action_3
action_104 (5) = happyGoto action_4
action_104 (13) = happyGoto action_5
action_104 (20) = happyGoto action_111
action_104 (21) = happyGoto action_7
action_104 (22) = happyGoto action_8
action_104 (24) = happyGoto action_9
action_104 (25) = happyGoto action_10
action_104 (26) = happyGoto action_11
action_104 (27) = happyGoto action_12
action_104 (29) = happyGoto action_13
action_104 (30) = happyGoto action_14
action_104 (31) = happyGoto action_15
action_104 (32) = happyGoto action_16
action_104 (34) = happyGoto action_17
action_104 (36) = happyGoto action_18
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_13

action_106 (38) = happyReduce_12
action_106 (86) = happyReduce_12
action_106 (94) = happyReduce_12
action_106 _ = happyReduce_12

action_107 _ = happyReduce_77

action_108 (38) = happyShift action_2
action_108 (4) = happyGoto action_109
action_108 (33) = happyGoto action_110
action_108 _ = happyReduce_68

action_109 (91) = happyShift action_72
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_69

action_111 _ = happyReduce_22

action_112 _ = happyReduce_21

action_113 (86) = happyShift action_129
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (38) = happyShift action_2
action_114 (4) = happyGoto action_128
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (88) = happyShift action_127
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (38) = happyShift action_2
action_116 (4) = happyGoto action_126
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (38) = happyShift action_2
action_117 (4) = happyGoto action_99
action_117 (6) = happyGoto action_125
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (90) = happyShift action_124
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (38) = happyShift action_2
action_119 (4) = happyGoto action_97
action_119 (8) = happyGoto action_123
action_119 _ = happyReduce_7

action_120 (38) = happyShift action_2
action_120 (39) = happyShift action_19
action_120 (40) = happyShift action_20
action_120 (41) = happyShift action_21
action_120 (42) = happyShift action_22
action_120 (66) = happyShift action_23
action_120 (69) = happyShift action_24
action_120 (85) = happyShift action_25
action_120 (87) = happyShift action_26
action_120 (89) = happyShift action_27
action_120 (100) = happyShift action_28
action_120 (102) = happyShift action_29
action_120 (4) = happyGoto action_3
action_120 (5) = happyGoto action_4
action_120 (13) = happyGoto action_5
action_120 (20) = happyGoto action_122
action_120 (21) = happyGoto action_7
action_120 (22) = happyGoto action_8
action_120 (24) = happyGoto action_9
action_120 (25) = happyGoto action_10
action_120 (26) = happyGoto action_11
action_120 (27) = happyGoto action_12
action_120 (29) = happyGoto action_13
action_120 (30) = happyGoto action_14
action_120 (31) = happyGoto action_15
action_120 (32) = happyGoto action_16
action_120 (34) = happyGoto action_17
action_120 (36) = happyGoto action_18
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_50

action_122 _ = happyReduce_23

action_123 _ = happyReduce_9

action_124 _ = happyReduce_20

action_125 _ = happyReduce_4

action_126 _ = happyReduce_11

action_127 _ = happyReduce_19

action_128 (93) = happyShift action_114
action_128 (7) = happyGoto action_130
action_128 _ = happyReduce_5

action_129 _ = happyReduce_17

action_130 _ = happyReduce_6

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
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ([happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 (Nothing
	)

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Just happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 (P.pattern $ P.Var happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn11
		 (P.pattern $ P.Term $ fmap PE.Literal happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (P.pattern $ P.Tagged happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (P.pattern $ P.Tuple (happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn11
		 (P.pattern $ P.List [] Nothing
	)

happyReduce_19 = happyReduce 4 11 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (P.pattern $ P.List happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 11 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (P.pattern $ P.Record happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ([P.matchCase happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (happy_var_1 ++ [P.matchCase happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (P.backcall [P.pattern $ P.Var happy_var_1] happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  18 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (P.returnStmt happy_var_2 happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  19 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  21 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (P.dotLambda happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 5 22 happyReduction_39
happyReduction_39 ((HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (P.infixApplication happy_var_3 [happy_var_1, happy_var_5]
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  23 happyReduction_41
happyReduction_41 (HappyAbsSyn24  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  24 happyReduction_43
happyReduction_43 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (P.fnApp happy_var_1 [happy_var_2]
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  24 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (P.fnApp happy_var_1 [happy_var_2]
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  26 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn26
		 (P.fnApp happy_var_1 [happy_var_2]
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  27 happyReduction_49
happyReduction_49 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyReduce 6 27 happyReduction_50
happyReduction_50 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  27 happyReduction_51
happyReduction_51 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 27 happyReduction_52
happyReduction_52 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_0  28 happyReduction_53
happyReduction_53  =  HappyAbsSyn28
		 ([]
	)

happyReduce_54 = happySpecReduce_2  28 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  29 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  29 happyReduction_56
happyReduction_56 (HappyTerminal happy_var_3)
	(HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (P.block happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  30 happyReduction_57
happyReduction_57 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  30 happyReduction_58
happyReduction_58 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  30 happyReduction_59
happyReduction_59 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (P.term happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  30 happyReduction_60
happyReduction_60 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  30 happyReduction_61
happyReduction_61 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  30 happyReduction_62
happyReduction_62 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  30 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_3)
	(HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  31 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (P.number PL.LInt happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  31 happyReduction_65
happyReduction_65 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (P.string PL.LString happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  31 happyReduction_66
happyReduction_66 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (P.boolean PL.LBool happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  32 happyReduction_67
happyReduction_67 (HappyTerminal happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (P.record happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  33 happyReduction_68
happyReduction_68  =  HappyAbsSyn33
		 ([]
	)

happyReduce_69 = happyReduce 5 33 happyReduction_69
happyReduction_69 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_3  33 happyReduction_70
happyReduction_70 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn33
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  34 happyReduction_71
happyReduction_71 (HappyTerminal happy_var_3)
	(HappyAbsSyn35  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (P.list happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  35 happyReduction_72
happyReduction_72  =  HappyAbsSyn35
		 ([]
	)

happyReduce_73 = happySpecReduce_1  35 happyReduction_73
happyReduction_73 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn35
		 ([happy_var_1]
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  35 happyReduction_74
happyReduction_74 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 : happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 36 happyReduction_75
happyReduction_75 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (P.tuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_2  37 happyReduction_76
happyReduction_76 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn37
		 ([happy_var_2]
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  37 happyReduction_77
happyReduction_77 (HappyAbsSyn37  happy_var_3)
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2 : happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 105 105 tk (HappyState action) sts stk;
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
	L.RangedToken (T.Operator _) _ -> cont 61;
	L.RangedToken T.Let _ -> cont 62;
	L.RangedToken T.In _ -> cont 63;
	L.RangedToken T.Where _ -> cont 64;
	L.RangedToken T.With _ -> cont 65;
	L.RangedToken T.If _ -> cont 66;
	L.RangedToken T.Then _ -> cont 67;
	L.RangedToken T.Else _ -> cont 68;
	L.RangedToken T.Match _ -> cont 69;
	L.RangedToken T.Return _ -> cont 70;
	L.RangedToken T.Data _ -> cont 71;
	L.RangedToken T.Type _ -> cont 72;
	L.RangedToken T.Alias _ -> cont 73;
	L.RangedToken T.Kind _ -> cont 74;
	L.RangedToken T.Forall _ -> cont 75;
	L.RangedToken T.Exists _ -> cont 76;
	L.RangedToken T.Proof _ -> cont 77;
	L.RangedToken T.Infer _ -> cont 78;
	L.RangedToken T.Protocol _ -> cont 79;
	L.RangedToken T.Interface _ -> cont 80;
	L.RangedToken T.Instance _ -> cont 81;
	L.RangedToken T.Implements _ -> cont 82;
	L.RangedToken T.Module _ -> cont 83;
	L.RangedToken T.Import _ -> cont 84;
	L.RangedToken T.LParen _ -> cont 85;
	L.RangedToken T.RParen _ -> cont 86;
	L.RangedToken T.LBrack _ -> cont 87;
	L.RangedToken T.RBrack _ -> cont 88;
	L.RangedToken T.LCurly _ -> cont 89;
	L.RangedToken T.RCurly _ -> cont 90;
	L.RangedToken T.Colon _ -> cont 91;
	L.RangedToken T.SemiColon _ -> cont 92;
	L.RangedToken T.Comma _ -> cont 93;
	L.RangedToken T.Arrow _ -> cont 94;
	L.RangedToken T.BackArrow _ -> cont 95;
	L.RangedToken T.FatArrow _ -> cont 96;
	L.RangedToken T.PipeArrow _ -> cont 97;
	L.RangedToken T.Equals _ -> cont 98;
	L.RangedToken T.Pipe _ -> cont 99;
	L.RangedToken T.Dot _ -> cont 100;
	L.RangedToken T.Section _ -> cont 101;
	L.RangedToken T.BackSlash _ -> cont 102;
	L.RangedToken T.Newline _ -> cont 103;
	L.RangedToken T.EOF _ -> cont 104;
	_ -> happyError' (tk, [])
	})

happyError_ explist 105 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

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
