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

data HappyAbsSyn t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61 t62 t63 t64 t65
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
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
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61
	| HappyAbsSyn62 t62
	| HappyAbsSyn63 t63
	| HappyAbsSyn64 t64
	| HappyAbsSyn65 t65

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1924) ([0,0,0,0,62,16384,2,42,5,0,0,0,0,0,1537,0,0,0,0,0,0,0,32832,1,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,24,0,0,0,0,0,128,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65408,127,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,57344,7,9216,40960,20482,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63488,0,2304,43008,5120,0,0,0,0,62,16384,2,42,5,0,0,0,3968,0,144,2688,320,0,0,0,57344,3,9216,40960,20482,0,0,0,0,248,0,25,168,20,0,0,0,512,0,0,0,0,0,0,0,32768,15,0,32768,10,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,15872,0,0,10752,0,0,0,0,32768,15,0,32768,10,0,0,0,0,992,0,0,672,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,256,0,0,0,0,3968,0,144,2688,320,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,8256,0,0,0,0,0,0,0,0,512,0,0,0,0,0,32768,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,576,10752,1280,0,0,0,32768,15,36864,32768,16394,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,232,0,32768,136,18,0,0,0,0,0,0,0,64,0,0,0,32768,0,0,32768,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,2,0,0,0,0,0,0,65408,255,0,0,0,0,0,0,8064,0,144,2688,320,0,0,0,0,0,0,0,0,0,0,0,0,248,0,9,168,20,0,0,0,0,0,0,0,128,0,0,0,32768,15,0,32768,10,0,0,0,0,0,0,0,0,0,0,0,0,63488,0,2304,43008,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,1600,10752,1280,0,0,0,32768,15,36864,32768,16394,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3968,0,144,2688,320,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,33280,0,0,0,0,0,0,0,0,0,0,0,0,57344,3,9216,40960,20482,0,0,0,0,248,0,0,168,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,992,0,0,672,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,57344,3,0,40960,2,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,1024,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,992,0,0,672,0,0,0,0,0,0,1024,0,0,0,0,0,0,62,16384,2,42,5,0,0,0,3968,0,144,2688,320,0,0,0,40960,3,0,8704,18434,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,32768,0,0,32768,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,14848,0,0,8736,1152,0,0,0,32768,0,0,0,0,0,0,0,0,928,0,0,546,72,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,3968,0,144,2688,320,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,928,0,0,546,64,0,0,0,59392,0,0,34944,4608,0,0,0,0,2,0,0,0,0,0,0,0,3712,0,0,2184,288,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,128,0,0,0,0,0,0,0,257,0,0,0,0,32,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,126,16384,2,42,5,0,0,0,8064,0,144,2688,320,0,0,0,57344,3,9216,40960,20482,0,0,0,0,0,0,0,4096,0,0,0,0,15872,0,576,10752,1280,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,16384,2,42,5,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,59392,0,0,34944,4608,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,928,0,0,546,72,0,0,0,0,0,0,0,0,0,0,0,0,58,0,8192,32802,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,232,0,32768,136,18,0,0,0,0,0,0,32768,0,0,0,0,32768,14,0,34816,8200,1,0,0,0,0,0,1,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3712,0,0,2184,288,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,928,0,0,546,72,0,0,0,59392,0,0,34944,4608,0,0,0,0,58,0,8192,32802,4,0,0,0,3712,0,0,2184,288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,128,0,0,0,0,0,0,0,57344,3,9216,40960,20482,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","%start_parseSagaDec","%start_parseSagaScript","identifier","hole","expr","expr1","expr2","exprBacktick","expr3","expr4","expr5","expr6","block","statement","exprAtom","term","record","pairs","list","tuple","pattern","patternAtom","patRecordKeys","patRest","cases","typeExpr","typeExpr1","union","typeExpr2","typeExpr3","typeExpr4","typeExpr5","typeAtom","tpairs","script","dec","letdec","binding","tbinding","typeAnnotation","kindAnnotation","kindExpr","dataExpr","dataExprs","defaultSeparator","operator","many__dec__","many__identifier__","many__patternAtom__","manyOrEmpty__identifier__","manyOrEmpty__pattern__","separated__binding__','__","separated__expr__','__","separated__pattern__','__","separated__statement__';'__","separated__tbinding__','__","separated__typeExpr__','__","separatedOrEmpty__expr__','__","separatedOrEmpty__pattern__','__","trailing__';'__","trailing__patRest__","many__pattern__","id","HOLE","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'^'","'++'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","'`'","'#'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 134
        bit_end = (st Prelude.+ 1) Prelude.* 134
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..133]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (66) = happyShift action_4
action_0 (67) = happyShift action_27
action_0 (68) = happyShift action_28
action_0 (69) = happyShift action_29
action_0 (70) = happyShift action_30
action_0 (95) = happyShift action_31
action_0 (98) = happyShift action_32
action_0 (114) = happyShift action_33
action_0 (116) = happyShift action_34
action_0 (118) = happyShift action_35
action_0 (129) = happyShift action_36
action_0 (131) = happyShift action_37
action_0 (6) = happyGoto action_13
action_0 (7) = happyGoto action_14
action_0 (8) = happyGoto action_15
action_0 (9) = happyGoto action_16
action_0 (10) = happyGoto action_17
action_0 (12) = happyGoto action_18
action_0 (13) = happyGoto action_19
action_0 (14) = happyGoto action_20
action_0 (15) = happyGoto action_21
action_0 (18) = happyGoto action_22
action_0 (19) = happyGoto action_23
action_0 (20) = happyGoto action_24
action_0 (22) = happyGoto action_25
action_0 (23) = happyGoto action_26
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (91) = happyShift action_9
action_1 (100) = happyShift action_10
action_1 (101) = happyShift action_11
action_1 (39) = happyGoto action_12
action_1 (40) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (91) = happyShift action_9
action_2 (100) = happyShift action_10
action_2 (101) = happyShift action_11
action_2 (38) = happyGoto action_5
action_2 (39) = happyGoto action_6
action_2 (40) = happyGoto action_7
action_2 (50) = happyGoto action_8
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (66) = happyShift action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (134) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_127

action_7 _ = happyReduce_85

action_8 (91) = happyShift action_9
action_8 (100) = happyShift action_10
action_8 (101) = happyShift action_11
action_8 (39) = happyGoto action_89
action_8 (40) = happyGoto action_7
action_8 _ = happyReduce_84

action_9 (66) = happyShift action_4
action_9 (6) = happyGoto action_88
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (66) = happyShift action_4
action_10 (6) = happyGoto action_87
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (66) = happyShift action_4
action_11 (6) = happyGoto action_86
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (134) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (66) = happyReduce_29
action_13 (67) = happyReduce_29
action_13 (68) = happyReduce_29
action_13 (69) = happyReduce_29
action_13 (70) = happyReduce_29
action_13 (71) = happyReduce_29
action_13 (72) = happyReduce_29
action_13 (73) = happyReduce_29
action_13 (74) = happyReduce_29
action_13 (75) = happyReduce_29
action_13 (76) = happyReduce_29
action_13 (77) = happyReduce_29
action_13 (78) = happyReduce_29
action_13 (79) = happyReduce_29
action_13 (80) = happyReduce_29
action_13 (81) = happyReduce_29
action_13 (82) = happyReduce_29
action_13 (83) = happyReduce_29
action_13 (84) = happyReduce_29
action_13 (85) = happyReduce_29
action_13 (86) = happyReduce_29
action_13 (87) = happyReduce_29
action_13 (88) = happyReduce_29
action_13 (91) = happyReduce_29
action_13 (93) = happyReduce_29
action_13 (95) = happyReduce_29
action_13 (96) = happyReduce_29
action_13 (97) = happyReduce_29
action_13 (98) = happyReduce_29
action_13 (100) = happyReduce_29
action_13 (101) = happyReduce_29
action_13 (114) = happyReduce_29
action_13 (115) = happyReduce_29
action_13 (116) = happyReduce_29
action_13 (117) = happyReduce_29
action_13 (118) = happyReduce_29
action_13 (119) = happyReduce_29
action_13 (121) = happyReduce_29
action_13 (122) = happyReduce_29
action_13 (128) = happyReduce_29
action_13 (129) = happyReduce_29
action_13 (131) = happyReduce_29
action_13 (134) = happyReduce_29
action_13 _ = happyReduce_29

action_14 _ = happyReduce_28

action_15 (134) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (66) = happyReduce_5
action_16 (67) = happyReduce_5
action_16 (68) = happyReduce_5
action_16 (69) = happyReduce_5
action_16 (70) = happyReduce_5
action_16 (71) = happyReduce_5
action_16 (72) = happyShift action_70
action_16 (73) = happyShift action_71
action_16 (74) = happyShift action_72
action_16 (75) = happyShift action_73
action_16 (76) = happyShift action_74
action_16 (77) = happyShift action_75
action_16 (78) = happyShift action_76
action_16 (79) = happyShift action_77
action_16 (80) = happyShift action_78
action_16 (81) = happyShift action_79
action_16 (82) = happyShift action_80
action_16 (83) = happyShift action_81
action_16 (84) = happyShift action_82
action_16 (85) = happyShift action_83
action_16 (86) = happyShift action_84
action_16 (87) = happyShift action_85
action_16 (88) = happyReduce_5
action_16 (91) = happyReduce_5
action_16 (93) = happyReduce_5
action_16 (95) = happyReduce_5
action_16 (96) = happyReduce_5
action_16 (97) = happyReduce_5
action_16 (98) = happyReduce_5
action_16 (100) = happyReduce_5
action_16 (101) = happyReduce_5
action_16 (114) = happyReduce_5
action_16 (115) = happyReduce_5
action_16 (116) = happyReduce_5
action_16 (117) = happyReduce_5
action_16 (118) = happyReduce_5
action_16 (119) = happyReduce_5
action_16 (121) = happyReduce_5
action_16 (122) = happyReduce_5
action_16 (128) = happyReduce_5
action_16 (129) = happyReduce_5
action_16 (131) = happyReduce_5
action_16 (134) = happyReduce_5
action_16 (49) = happyGoto action_69
action_16 _ = happyReduce_5

action_17 (66) = happyReduce_6
action_17 (67) = happyReduce_6
action_17 (68) = happyReduce_6
action_17 (69) = happyReduce_6
action_17 (70) = happyReduce_6
action_17 (71) = happyReduce_6
action_17 (72) = happyReduce_6
action_17 (73) = happyReduce_6
action_17 (74) = happyReduce_6
action_17 (75) = happyReduce_6
action_17 (76) = happyReduce_6
action_17 (77) = happyReduce_6
action_17 (78) = happyReduce_6
action_17 (79) = happyReduce_6
action_17 (80) = happyReduce_6
action_17 (81) = happyReduce_6
action_17 (82) = happyReduce_6
action_17 (83) = happyReduce_6
action_17 (84) = happyReduce_6
action_17 (85) = happyReduce_6
action_17 (86) = happyReduce_6
action_17 (87) = happyReduce_6
action_17 (88) = happyShift action_68
action_17 (91) = happyReduce_6
action_17 (93) = happyReduce_6
action_17 (95) = happyReduce_6
action_17 (96) = happyReduce_6
action_17 (97) = happyReduce_6
action_17 (98) = happyReduce_6
action_17 (100) = happyReduce_6
action_17 (101) = happyReduce_6
action_17 (114) = happyReduce_6
action_17 (115) = happyReduce_6
action_17 (116) = happyReduce_6
action_17 (117) = happyReduce_6
action_17 (118) = happyReduce_6
action_17 (119) = happyReduce_6
action_17 (121) = happyReduce_6
action_17 (122) = happyReduce_6
action_17 (128) = happyReduce_6
action_17 (129) = happyReduce_6
action_17 (131) = happyReduce_6
action_17 (134) = happyReduce_6
action_17 _ = happyReduce_6

action_18 (66) = happyShift action_4
action_18 (67) = happyShift action_27
action_18 (68) = happyShift action_28
action_18 (69) = happyShift action_29
action_18 (70) = happyShift action_30
action_18 (71) = happyShift action_67
action_18 (72) = happyReduce_8
action_18 (73) = happyReduce_8
action_18 (74) = happyReduce_8
action_18 (75) = happyReduce_8
action_18 (76) = happyReduce_8
action_18 (77) = happyReduce_8
action_18 (78) = happyReduce_8
action_18 (79) = happyReduce_8
action_18 (80) = happyReduce_8
action_18 (81) = happyReduce_8
action_18 (82) = happyReduce_8
action_18 (83) = happyReduce_8
action_18 (84) = happyReduce_8
action_18 (85) = happyReduce_8
action_18 (86) = happyReduce_8
action_18 (87) = happyReduce_8
action_18 (88) = happyReduce_8
action_18 (91) = happyReduce_8
action_18 (93) = happyReduce_8
action_18 (95) = happyShift action_31
action_18 (96) = happyReduce_8
action_18 (97) = happyReduce_8
action_18 (98) = happyShift action_32
action_18 (100) = happyReduce_8
action_18 (101) = happyReduce_8
action_18 (114) = happyShift action_33
action_18 (115) = happyReduce_8
action_18 (116) = happyShift action_34
action_18 (117) = happyReduce_8
action_18 (118) = happyShift action_35
action_18 (119) = happyReduce_8
action_18 (121) = happyReduce_8
action_18 (122) = happyReduce_8
action_18 (128) = happyReduce_8
action_18 (129) = happyShift action_36
action_18 (131) = happyShift action_37
action_18 (134) = happyReduce_8
action_18 (6) = happyGoto action_13
action_18 (7) = happyGoto action_14
action_18 (13) = happyGoto action_66
action_18 (14) = happyGoto action_20
action_18 (15) = happyGoto action_21
action_18 (18) = happyGoto action_22
action_18 (19) = happyGoto action_23
action_18 (20) = happyGoto action_24
action_18 (22) = happyGoto action_25
action_18 (23) = happyGoto action_26
action_18 _ = happyReduce_8

action_19 (66) = happyReduce_12
action_19 (67) = happyReduce_12
action_19 (68) = happyReduce_12
action_19 (69) = happyReduce_12
action_19 (70) = happyReduce_12
action_19 (71) = happyReduce_12
action_19 (72) = happyReduce_12
action_19 (73) = happyReduce_12
action_19 (74) = happyReduce_12
action_19 (75) = happyReduce_12
action_19 (76) = happyReduce_12
action_19 (77) = happyReduce_12
action_19 (78) = happyReduce_12
action_19 (79) = happyReduce_12
action_19 (80) = happyReduce_12
action_19 (81) = happyReduce_12
action_19 (82) = happyReduce_12
action_19 (83) = happyReduce_12
action_19 (84) = happyReduce_12
action_19 (85) = happyReduce_12
action_19 (86) = happyReduce_12
action_19 (87) = happyReduce_12
action_19 (88) = happyReduce_12
action_19 (91) = happyReduce_12
action_19 (93) = happyReduce_12
action_19 (95) = happyReduce_12
action_19 (96) = happyReduce_12
action_19 (97) = happyReduce_12
action_19 (98) = happyReduce_12
action_19 (100) = happyReduce_12
action_19 (101) = happyReduce_12
action_19 (114) = happyReduce_12
action_19 (115) = happyReduce_12
action_19 (116) = happyReduce_12
action_19 (117) = happyReduce_12
action_19 (118) = happyReduce_12
action_19 (119) = happyReduce_12
action_19 (121) = happyReduce_12
action_19 (122) = happyReduce_12
action_19 (128) = happyReduce_12
action_19 (129) = happyShift action_65
action_19 (131) = happyReduce_12
action_19 (134) = happyReduce_12
action_19 _ = happyReduce_12

action_20 _ = happyReduce_15

action_21 _ = happyReduce_17

action_22 _ = happyReduce_22

action_23 _ = happyReduce_30

action_24 _ = happyReduce_33

action_25 _ = happyReduce_32

action_26 _ = happyReduce_31

action_27 _ = happyReduce_4

action_28 _ = happyReduce_35

action_29 _ = happyReduce_36

action_30 _ = happyReduce_37

action_31 (66) = happyShift action_4
action_31 (67) = happyShift action_27
action_31 (68) = happyShift action_28
action_31 (69) = happyShift action_29
action_31 (70) = happyShift action_30
action_31 (95) = happyShift action_31
action_31 (98) = happyShift action_32
action_31 (114) = happyShift action_33
action_31 (116) = happyShift action_34
action_31 (118) = happyShift action_35
action_31 (129) = happyShift action_36
action_31 (131) = happyShift action_37
action_31 (6) = happyGoto action_13
action_31 (7) = happyGoto action_14
action_31 (8) = happyGoto action_64
action_31 (9) = happyGoto action_16
action_31 (10) = happyGoto action_17
action_31 (12) = happyGoto action_18
action_31 (13) = happyGoto action_19
action_31 (14) = happyGoto action_20
action_31 (15) = happyGoto action_21
action_31 (18) = happyGoto action_22
action_31 (19) = happyGoto action_23
action_31 (20) = happyGoto action_24
action_31 (22) = happyGoto action_25
action_31 (23) = happyGoto action_26
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (66) = happyShift action_4
action_32 (67) = happyShift action_27
action_32 (68) = happyShift action_28
action_32 (69) = happyShift action_29
action_32 (70) = happyShift action_30
action_32 (95) = happyShift action_31
action_32 (98) = happyShift action_32
action_32 (114) = happyShift action_33
action_32 (116) = happyShift action_34
action_32 (118) = happyShift action_35
action_32 (129) = happyShift action_36
action_32 (131) = happyShift action_37
action_32 (6) = happyGoto action_13
action_32 (7) = happyGoto action_14
action_32 (8) = happyGoto action_63
action_32 (9) = happyGoto action_16
action_32 (10) = happyGoto action_17
action_32 (12) = happyGoto action_18
action_32 (13) = happyGoto action_19
action_32 (14) = happyGoto action_20
action_32 (15) = happyGoto action_21
action_32 (18) = happyGoto action_22
action_32 (19) = happyGoto action_23
action_32 (20) = happyGoto action_24
action_32 (22) = happyGoto action_25
action_32 (23) = happyGoto action_26
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (66) = happyShift action_4
action_33 (67) = happyShift action_27
action_33 (68) = happyShift action_28
action_33 (69) = happyShift action_29
action_33 (70) = happyShift action_30
action_33 (95) = happyShift action_31
action_33 (98) = happyShift action_32
action_33 (114) = happyShift action_33
action_33 (116) = happyShift action_34
action_33 (118) = happyShift action_35
action_33 (129) = happyShift action_36
action_33 (131) = happyShift action_37
action_33 (6) = happyGoto action_13
action_33 (7) = happyGoto action_14
action_33 (8) = happyGoto action_61
action_33 (9) = happyGoto action_16
action_33 (10) = happyGoto action_17
action_33 (12) = happyGoto action_18
action_33 (13) = happyGoto action_19
action_33 (14) = happyGoto action_20
action_33 (15) = happyGoto action_21
action_33 (18) = happyGoto action_22
action_33 (19) = happyGoto action_23
action_33 (20) = happyGoto action_24
action_33 (22) = happyGoto action_25
action_33 (23) = happyGoto action_26
action_33 (56) = happyGoto action_62
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (66) = happyShift action_4
action_34 (67) = happyShift action_27
action_34 (68) = happyShift action_28
action_34 (69) = happyShift action_29
action_34 (70) = happyShift action_30
action_34 (95) = happyShift action_31
action_34 (98) = happyShift action_32
action_34 (114) = happyShift action_33
action_34 (116) = happyShift action_34
action_34 (118) = happyShift action_35
action_34 (129) = happyShift action_36
action_34 (131) = happyShift action_37
action_34 (6) = happyGoto action_13
action_34 (7) = happyGoto action_14
action_34 (8) = happyGoto action_58
action_34 (9) = happyGoto action_16
action_34 (10) = happyGoto action_17
action_34 (12) = happyGoto action_18
action_34 (13) = happyGoto action_19
action_34 (14) = happyGoto action_20
action_34 (15) = happyGoto action_21
action_34 (18) = happyGoto action_22
action_34 (19) = happyGoto action_23
action_34 (20) = happyGoto action_24
action_34 (22) = happyGoto action_25
action_34 (23) = happyGoto action_26
action_34 (56) = happyGoto action_59
action_34 (61) = happyGoto action_60
action_34 _ = happyReduce_149

action_35 (66) = happyShift action_4
action_35 (67) = happyShift action_27
action_35 (68) = happyShift action_28
action_35 (69) = happyShift action_29
action_35 (70) = happyShift action_30
action_35 (95) = happyShift action_31
action_35 (98) = happyShift action_32
action_35 (99) = happyShift action_57
action_35 (114) = happyShift action_33
action_35 (116) = happyShift action_34
action_35 (118) = happyShift action_35
action_35 (124) = happyReduce_133
action_35 (129) = happyShift action_36
action_35 (131) = happyShift action_37
action_35 (6) = happyGoto action_49
action_35 (7) = happyGoto action_14
action_35 (8) = happyGoto action_50
action_35 (9) = happyGoto action_16
action_35 (10) = happyGoto action_17
action_35 (12) = happyGoto action_18
action_35 (13) = happyGoto action_19
action_35 (14) = happyGoto action_20
action_35 (15) = happyGoto action_21
action_35 (16) = happyGoto action_51
action_35 (17) = happyGoto action_52
action_35 (18) = happyGoto action_22
action_35 (19) = happyGoto action_23
action_35 (20) = happyGoto action_24
action_35 (21) = happyGoto action_53
action_35 (22) = happyGoto action_25
action_35 (23) = happyGoto action_26
action_35 (51) = happyGoto action_54
action_35 (53) = happyGoto action_55
action_35 (58) = happyGoto action_56
action_35 _ = happyReduce_39

action_36 (66) = happyShift action_4
action_36 (6) = happyGoto action_48
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (66) = happyShift action_4
action_37 (67) = happyShift action_27
action_37 (68) = happyShift action_28
action_37 (69) = happyShift action_29
action_37 (70) = happyShift action_30
action_37 (114) = happyShift action_45
action_37 (116) = happyShift action_46
action_37 (118) = happyShift action_47
action_37 (6) = happyGoto action_38
action_37 (7) = happyGoto action_39
action_37 (19) = happyGoto action_40
action_37 (24) = happyGoto action_41
action_37 (25) = happyGoto action_42
action_37 (54) = happyGoto action_43
action_37 (65) = happyGoto action_44
action_37 _ = happyReduce_135

action_38 (66) = happyReduce_46
action_38 (67) = happyReduce_46
action_38 (68) = happyReduce_46
action_38 (69) = happyReduce_46
action_38 (70) = happyReduce_46
action_38 (114) = happyReduce_46
action_38 (115) = happyReduce_46
action_38 (116) = happyReduce_46
action_38 (117) = happyReduce_46
action_38 (118) = happyReduce_46
action_38 (120) = happyShift action_123
action_38 (122) = happyReduce_46
action_38 (123) = happyReduce_46
action_38 (128) = happyReduce_46
action_38 _ = happyReduce_46

action_39 (66) = happyReduce_47
action_39 (67) = happyReduce_47
action_39 (68) = happyReduce_47
action_39 (69) = happyReduce_47
action_39 (70) = happyReduce_47
action_39 (114) = happyReduce_47
action_39 (115) = happyReduce_47
action_39 (116) = happyReduce_47
action_39 (117) = happyReduce_47
action_39 (118) = happyReduce_47
action_39 (122) = happyReduce_47
action_39 (123) = happyReduce_47
action_39 (128) = happyReduce_47
action_39 _ = happyReduce_47

action_40 (66) = happyReduce_48
action_40 (67) = happyReduce_48
action_40 (68) = happyReduce_48
action_40 (69) = happyReduce_48
action_40 (70) = happyReduce_48
action_40 (114) = happyReduce_48
action_40 (115) = happyReduce_48
action_40 (116) = happyReduce_48
action_40 (117) = happyReduce_48
action_40 (118) = happyReduce_48
action_40 (122) = happyReduce_48
action_40 (123) = happyReduce_48
action_40 (128) = happyReduce_48
action_40 _ = happyReduce_48

action_41 _ = happyReduce_157

action_42 _ = happyReduce_44

action_43 (123) = happyShift action_122
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (66) = happyShift action_4
action_44 (67) = happyShift action_27
action_44 (68) = happyShift action_28
action_44 (69) = happyShift action_29
action_44 (70) = happyShift action_30
action_44 (114) = happyShift action_45
action_44 (116) = happyShift action_46
action_44 (118) = happyShift action_47
action_44 (6) = happyGoto action_38
action_44 (7) = happyGoto action_39
action_44 (19) = happyGoto action_40
action_44 (24) = happyGoto action_121
action_44 (25) = happyGoto action_42
action_44 _ = happyReduce_136

action_45 (66) = happyShift action_4
action_45 (67) = happyShift action_27
action_45 (68) = happyShift action_28
action_45 (69) = happyShift action_29
action_45 (70) = happyShift action_30
action_45 (114) = happyShift action_45
action_45 (116) = happyShift action_46
action_45 (118) = happyShift action_47
action_45 (6) = happyGoto action_38
action_45 (7) = happyGoto action_39
action_45 (19) = happyGoto action_40
action_45 (24) = happyGoto action_119
action_45 (25) = happyGoto action_42
action_45 (57) = happyGoto action_120
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (66) = happyShift action_4
action_46 (67) = happyShift action_27
action_46 (68) = happyShift action_28
action_46 (69) = happyShift action_29
action_46 (70) = happyShift action_30
action_46 (114) = happyShift action_45
action_46 (116) = happyShift action_46
action_46 (118) = happyShift action_47
action_46 (6) = happyGoto action_38
action_46 (7) = happyGoto action_39
action_46 (19) = happyGoto action_40
action_46 (24) = happyGoto action_116
action_46 (25) = happyGoto action_42
action_46 (57) = happyGoto action_117
action_46 (62) = happyGoto action_118
action_46 _ = happyReduce_151

action_47 (66) = happyShift action_4
action_47 (6) = happyGoto action_114
action_47 (26) = happyGoto action_115
action_47 _ = happyReduce_53

action_48 _ = happyReduce_21

action_49 (67) = happyReduce_29
action_49 (68) = happyReduce_29
action_49 (69) = happyReduce_29
action_49 (70) = happyReduce_29
action_49 (71) = happyReduce_29
action_49 (72) = happyReduce_29
action_49 (73) = happyReduce_29
action_49 (74) = happyReduce_29
action_49 (75) = happyReduce_29
action_49 (76) = happyReduce_29
action_49 (77) = happyReduce_29
action_49 (78) = happyReduce_29
action_49 (79) = happyReduce_29
action_49 (80) = happyReduce_29
action_49 (81) = happyReduce_29
action_49 (82) = happyReduce_29
action_49 (83) = happyReduce_29
action_49 (84) = happyReduce_29
action_49 (85) = happyReduce_29
action_49 (86) = happyReduce_29
action_49 (87) = happyReduce_29
action_49 (88) = happyReduce_29
action_49 (95) = happyReduce_29
action_49 (98) = happyReduce_29
action_49 (114) = happyReduce_29
action_49 (116) = happyReduce_29
action_49 (118) = happyReduce_29
action_49 (119) = happyReduce_29
action_49 (120) = happyShift action_113
action_49 (121) = happyReduce_29
action_49 (129) = happyReduce_29
action_49 (131) = happyReduce_29
action_49 _ = happyReduce_129

action_50 _ = happyReduce_26

action_51 (119) = happyShift action_112
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (119) = happyReduce_143
action_52 (121) = happyReduce_143
action_52 _ = happyReduce_143

action_53 (119) = happyShift action_111
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (66) = happyShift action_4
action_54 (6) = happyGoto action_110
action_54 _ = happyReduce_134

action_55 (124) = happyShift action_109
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (119) = happyReduce_153
action_56 (121) = happyShift action_108
action_56 (63) = happyGoto action_107
action_56 _ = happyReduce_153

action_57 (66) = happyShift action_4
action_57 (67) = happyShift action_27
action_57 (68) = happyShift action_28
action_57 (69) = happyShift action_29
action_57 (70) = happyShift action_30
action_57 (95) = happyShift action_31
action_57 (98) = happyShift action_32
action_57 (114) = happyShift action_33
action_57 (116) = happyShift action_34
action_57 (118) = happyShift action_35
action_57 (129) = happyShift action_36
action_57 (131) = happyShift action_37
action_57 (6) = happyGoto action_13
action_57 (7) = happyGoto action_14
action_57 (8) = happyGoto action_106
action_57 (9) = happyGoto action_16
action_57 (10) = happyGoto action_17
action_57 (12) = happyGoto action_18
action_57 (13) = happyGoto action_19
action_57 (14) = happyGoto action_20
action_57 (15) = happyGoto action_21
action_57 (18) = happyGoto action_22
action_57 (19) = happyGoto action_23
action_57 (20) = happyGoto action_24
action_57 (22) = happyGoto action_25
action_57 (23) = happyGoto action_26
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (117) = happyReduce_139
action_58 (122) = happyReduce_139
action_58 _ = happyReduce_139

action_59 (117) = happyReduce_150
action_59 (122) = happyShift action_103
action_59 _ = happyReduce_150

action_60 (117) = happyShift action_105
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (115) = happyShift action_104
action_61 (122) = happyReduce_139
action_61 _ = happyReduce_139

action_62 (115) = happyShift action_102
action_62 (122) = happyShift action_103
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (128) = happyShift action_101
action_63 (28) = happyGoto action_100
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (96) = happyShift action_99
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (66) = happyShift action_4
action_65 (6) = happyGoto action_98
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (66) = happyReduce_13
action_66 (67) = happyReduce_13
action_66 (68) = happyReduce_13
action_66 (69) = happyReduce_13
action_66 (70) = happyReduce_13
action_66 (71) = happyReduce_13
action_66 (72) = happyReduce_13
action_66 (73) = happyReduce_13
action_66 (74) = happyReduce_13
action_66 (75) = happyReduce_13
action_66 (76) = happyReduce_13
action_66 (77) = happyReduce_13
action_66 (78) = happyReduce_13
action_66 (79) = happyReduce_13
action_66 (80) = happyReduce_13
action_66 (81) = happyReduce_13
action_66 (82) = happyReduce_13
action_66 (83) = happyReduce_13
action_66 (84) = happyReduce_13
action_66 (85) = happyReduce_13
action_66 (86) = happyReduce_13
action_66 (87) = happyReduce_13
action_66 (88) = happyReduce_13
action_66 (91) = happyReduce_13
action_66 (93) = happyReduce_13
action_66 (95) = happyReduce_13
action_66 (96) = happyReduce_13
action_66 (97) = happyReduce_13
action_66 (98) = happyReduce_13
action_66 (100) = happyReduce_13
action_66 (101) = happyReduce_13
action_66 (114) = happyReduce_13
action_66 (115) = happyReduce_13
action_66 (116) = happyReduce_13
action_66 (117) = happyReduce_13
action_66 (118) = happyReduce_13
action_66 (119) = happyReduce_13
action_66 (121) = happyReduce_13
action_66 (122) = happyReduce_13
action_66 (128) = happyReduce_13
action_66 (129) = happyShift action_65
action_66 (131) = happyReduce_13
action_66 (134) = happyReduce_13
action_66 _ = happyReduce_13

action_67 _ = happyReduce_14

action_68 (66) = happyShift action_4
action_68 (67) = happyShift action_27
action_68 (68) = happyShift action_28
action_68 (69) = happyShift action_29
action_68 (70) = happyShift action_30
action_68 (95) = happyShift action_31
action_68 (98) = happyShift action_32
action_68 (114) = happyShift action_33
action_68 (116) = happyShift action_34
action_68 (118) = happyShift action_35
action_68 (129) = happyShift action_36
action_68 (131) = happyShift action_37
action_68 (6) = happyGoto action_13
action_68 (7) = happyGoto action_14
action_68 (11) = happyGoto action_96
action_68 (12) = happyGoto action_97
action_68 (13) = happyGoto action_19
action_68 (14) = happyGoto action_20
action_68 (15) = happyGoto action_21
action_68 (18) = happyGoto action_22
action_68 (19) = happyGoto action_23
action_68 (20) = happyGoto action_24
action_68 (22) = happyGoto action_25
action_68 (23) = happyGoto action_26
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (66) = happyShift action_4
action_69 (67) = happyShift action_27
action_69 (68) = happyShift action_28
action_69 (69) = happyShift action_29
action_69 (70) = happyShift action_30
action_69 (95) = happyShift action_31
action_69 (98) = happyShift action_32
action_69 (114) = happyShift action_33
action_69 (116) = happyShift action_34
action_69 (118) = happyShift action_35
action_69 (129) = happyShift action_36
action_69 (131) = happyShift action_37
action_69 (6) = happyGoto action_13
action_69 (7) = happyGoto action_14
action_69 (10) = happyGoto action_95
action_69 (12) = happyGoto action_18
action_69 (13) = happyGoto action_19
action_69 (14) = happyGoto action_20
action_69 (15) = happyGoto action_21
action_69 (18) = happyGoto action_22
action_69 (19) = happyGoto action_23
action_69 (20) = happyGoto action_24
action_69 (22) = happyGoto action_25
action_69 (23) = happyGoto action_26
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_111

action_71 _ = happyReduce_112

action_72 _ = happyReduce_113

action_73 _ = happyReduce_114

action_74 _ = happyReduce_115

action_75 _ = happyReduce_126

action_76 _ = happyReduce_118

action_77 _ = happyReduce_119

action_78 _ = happyReduce_120

action_79 _ = happyReduce_122

action_80 _ = happyReduce_121

action_81 _ = happyReduce_123

action_82 _ = happyReduce_116

action_83 _ = happyReduce_117

action_84 _ = happyReduce_124

action_85 _ = happyReduce_125

action_86 (130) = happyShift action_93
action_86 (44) = happyGoto action_94
action_86 _ = happyReduce_101

action_87 (130) = happyShift action_93
action_87 (44) = happyGoto action_92
action_87 _ = happyReduce_101

action_88 (120) = happyShift action_91
action_88 (43) = happyGoto action_90
action_88 _ = happyReduce_97

action_89 _ = happyReduce_128

action_90 (130) = happyShift action_93
action_90 (44) = happyGoto action_168
action_90 _ = happyReduce_101

action_91 (66) = happyShift action_4
action_91 (68) = happyShift action_160
action_91 (69) = happyShift action_161
action_91 (70) = happyShift action_162
action_91 (110) = happyShift action_163
action_91 (114) = happyShift action_164
action_91 (118) = happyShift action_165
action_91 (128) = happyShift action_166
action_91 (131) = happyShift action_167
action_91 (6) = happyGoto action_151
action_91 (29) = happyGoto action_152
action_91 (30) = happyGoto action_153
action_91 (31) = happyGoto action_154
action_91 (32) = happyGoto action_155
action_91 (33) = happyGoto action_156
action_91 (34) = happyGoto action_157
action_91 (35) = happyGoto action_158
action_91 (36) = happyGoto action_159
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (127) = happyShift action_150
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (66) = happyShift action_4
action_93 (114) = happyShift action_149
action_93 (6) = happyGoto action_147
action_93 (45) = happyGoto action_148
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (127) = happyShift action_146
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (66) = happyReduce_7
action_95 (67) = happyReduce_7
action_95 (68) = happyReduce_7
action_95 (69) = happyReduce_7
action_95 (70) = happyReduce_7
action_95 (71) = happyReduce_7
action_95 (72) = happyReduce_7
action_95 (73) = happyReduce_7
action_95 (74) = happyReduce_7
action_95 (75) = happyReduce_7
action_95 (76) = happyReduce_7
action_95 (77) = happyReduce_7
action_95 (78) = happyReduce_7
action_95 (79) = happyReduce_7
action_95 (80) = happyReduce_7
action_95 (81) = happyReduce_7
action_95 (82) = happyReduce_7
action_95 (83) = happyReduce_7
action_95 (84) = happyReduce_7
action_95 (85) = happyReduce_7
action_95 (86) = happyReduce_7
action_95 (87) = happyReduce_7
action_95 (88) = happyShift action_68
action_95 (91) = happyReduce_7
action_95 (93) = happyReduce_7
action_95 (95) = happyReduce_7
action_95 (96) = happyReduce_7
action_95 (97) = happyReduce_7
action_95 (98) = happyReduce_7
action_95 (100) = happyReduce_7
action_95 (101) = happyReduce_7
action_95 (114) = happyReduce_7
action_95 (115) = happyReduce_7
action_95 (116) = happyReduce_7
action_95 (117) = happyReduce_7
action_95 (118) = happyReduce_7
action_95 (119) = happyReduce_7
action_95 (121) = happyReduce_7
action_95 (122) = happyReduce_7
action_95 (128) = happyReduce_7
action_95 (129) = happyReduce_7
action_95 (131) = happyReduce_7
action_95 (134) = happyReduce_7
action_95 _ = happyReduce_7

action_96 (72) = happyShift action_70
action_96 (73) = happyShift action_71
action_96 (74) = happyShift action_72
action_96 (75) = happyShift action_73
action_96 (76) = happyShift action_74
action_96 (77) = happyShift action_75
action_96 (78) = happyShift action_76
action_96 (79) = happyShift action_77
action_96 (80) = happyShift action_78
action_96 (81) = happyShift action_79
action_96 (82) = happyShift action_80
action_96 (83) = happyShift action_81
action_96 (84) = happyShift action_82
action_96 (85) = happyShift action_83
action_96 (86) = happyShift action_84
action_96 (87) = happyShift action_85
action_96 (88) = happyShift action_145
action_96 (49) = happyGoto action_144
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (66) = happyShift action_4
action_97 (67) = happyShift action_27
action_97 (68) = happyShift action_28
action_97 (69) = happyShift action_29
action_97 (70) = happyShift action_30
action_97 (71) = happyShift action_67
action_97 (95) = happyShift action_31
action_97 (98) = happyShift action_32
action_97 (114) = happyShift action_33
action_97 (116) = happyShift action_34
action_97 (118) = happyShift action_35
action_97 (129) = happyShift action_36
action_97 (131) = happyShift action_37
action_97 (6) = happyGoto action_13
action_97 (7) = happyGoto action_14
action_97 (13) = happyGoto action_66
action_97 (14) = happyGoto action_20
action_97 (15) = happyGoto action_21
action_97 (18) = happyGoto action_22
action_97 (19) = happyGoto action_23
action_97 (20) = happyGoto action_24
action_97 (22) = happyGoto action_25
action_97 (23) = happyGoto action_26
action_97 _ = happyReduce_10

action_98 (66) = happyReduce_16
action_98 (67) = happyReduce_16
action_98 (68) = happyReduce_16
action_98 (69) = happyReduce_16
action_98 (70) = happyReduce_16
action_98 (71) = happyReduce_16
action_98 (72) = happyReduce_16
action_98 (73) = happyReduce_16
action_98 (74) = happyReduce_16
action_98 (75) = happyReduce_16
action_98 (76) = happyReduce_16
action_98 (77) = happyReduce_16
action_98 (78) = happyReduce_16
action_98 (79) = happyReduce_16
action_98 (80) = happyReduce_16
action_98 (81) = happyReduce_16
action_98 (82) = happyReduce_16
action_98 (83) = happyReduce_16
action_98 (84) = happyReduce_16
action_98 (85) = happyReduce_16
action_98 (86) = happyReduce_16
action_98 (87) = happyReduce_16
action_98 (88) = happyReduce_16
action_98 (91) = happyReduce_16
action_98 (93) = happyReduce_16
action_98 (95) = happyReduce_16
action_98 (96) = happyReduce_16
action_98 (97) = happyReduce_16
action_98 (98) = happyReduce_16
action_98 (100) = happyReduce_16
action_98 (101) = happyReduce_16
action_98 (114) = happyReduce_16
action_98 (115) = happyReduce_16
action_98 (116) = happyReduce_16
action_98 (117) = happyReduce_16
action_98 (118) = happyReduce_16
action_98 (119) = happyReduce_16
action_98 (121) = happyReduce_16
action_98 (122) = happyReduce_16
action_98 (128) = happyReduce_16
action_98 (129) = happyReduce_16
action_98 (131) = happyReduce_16
action_98 (134) = happyReduce_16
action_98 _ = happyReduce_16

action_99 (66) = happyShift action_4
action_99 (67) = happyShift action_27
action_99 (68) = happyShift action_28
action_99 (69) = happyShift action_29
action_99 (70) = happyShift action_30
action_99 (95) = happyShift action_31
action_99 (98) = happyShift action_32
action_99 (114) = happyShift action_33
action_99 (116) = happyShift action_34
action_99 (118) = happyShift action_35
action_99 (129) = happyShift action_36
action_99 (131) = happyShift action_37
action_99 (6) = happyGoto action_13
action_99 (7) = happyGoto action_14
action_99 (8) = happyGoto action_143
action_99 (9) = happyGoto action_16
action_99 (10) = happyGoto action_17
action_99 (12) = happyGoto action_18
action_99 (13) = happyGoto action_19
action_99 (14) = happyGoto action_20
action_99 (15) = happyGoto action_21
action_99 (18) = happyGoto action_22
action_99 (19) = happyGoto action_23
action_99 (20) = happyGoto action_24
action_99 (22) = happyGoto action_25
action_99 (23) = happyGoto action_26
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (66) = happyReduce_19
action_100 (67) = happyReduce_19
action_100 (68) = happyReduce_19
action_100 (69) = happyReduce_19
action_100 (70) = happyReduce_19
action_100 (71) = happyReduce_19
action_100 (72) = happyReduce_19
action_100 (73) = happyReduce_19
action_100 (74) = happyReduce_19
action_100 (75) = happyReduce_19
action_100 (76) = happyReduce_19
action_100 (77) = happyReduce_19
action_100 (78) = happyReduce_19
action_100 (79) = happyReduce_19
action_100 (80) = happyReduce_19
action_100 (81) = happyReduce_19
action_100 (82) = happyReduce_19
action_100 (83) = happyReduce_19
action_100 (84) = happyReduce_19
action_100 (85) = happyReduce_19
action_100 (86) = happyReduce_19
action_100 (87) = happyReduce_19
action_100 (88) = happyReduce_19
action_100 (91) = happyReduce_19
action_100 (93) = happyReduce_19
action_100 (95) = happyReduce_19
action_100 (96) = happyReduce_19
action_100 (97) = happyReduce_19
action_100 (98) = happyReduce_19
action_100 (100) = happyReduce_19
action_100 (101) = happyReduce_19
action_100 (114) = happyReduce_19
action_100 (115) = happyReduce_19
action_100 (116) = happyReduce_19
action_100 (117) = happyReduce_19
action_100 (118) = happyReduce_19
action_100 (119) = happyReduce_19
action_100 (121) = happyReduce_19
action_100 (122) = happyReduce_19
action_100 (128) = happyShift action_142
action_100 (129) = happyReduce_19
action_100 (131) = happyReduce_19
action_100 (134) = happyReduce_19
action_100 _ = happyReduce_19

action_101 (66) = happyShift action_4
action_101 (67) = happyShift action_27
action_101 (68) = happyShift action_28
action_101 (69) = happyShift action_29
action_101 (70) = happyShift action_30
action_101 (114) = happyShift action_45
action_101 (116) = happyShift action_46
action_101 (118) = happyShift action_47
action_101 (6) = happyGoto action_38
action_101 (7) = happyGoto action_39
action_101 (19) = happyGoto action_40
action_101 (24) = happyGoto action_141
action_101 (25) = happyGoto action_42
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_43

action_103 (66) = happyShift action_4
action_103 (67) = happyShift action_27
action_103 (68) = happyShift action_28
action_103 (69) = happyShift action_29
action_103 (70) = happyShift action_30
action_103 (95) = happyShift action_31
action_103 (98) = happyShift action_32
action_103 (114) = happyShift action_33
action_103 (116) = happyShift action_34
action_103 (118) = happyShift action_35
action_103 (129) = happyShift action_36
action_103 (131) = happyShift action_37
action_103 (6) = happyGoto action_13
action_103 (7) = happyGoto action_14
action_103 (8) = happyGoto action_140
action_103 (9) = happyGoto action_16
action_103 (10) = happyGoto action_17
action_103 (12) = happyGoto action_18
action_103 (13) = happyGoto action_19
action_103 (14) = happyGoto action_20
action_103 (15) = happyGoto action_21
action_103 (18) = happyGoto action_22
action_103 (19) = happyGoto action_23
action_103 (20) = happyGoto action_24
action_103 (22) = happyGoto action_25
action_103 (23) = happyGoto action_26
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_34

action_105 _ = happyReduce_42

action_106 _ = happyReduce_27

action_107 _ = happyReduce_24

action_108 (66) = happyShift action_4
action_108 (67) = happyShift action_27
action_108 (68) = happyShift action_28
action_108 (69) = happyShift action_29
action_108 (70) = happyShift action_30
action_108 (95) = happyShift action_31
action_108 (98) = happyShift action_32
action_108 (99) = happyShift action_57
action_108 (114) = happyShift action_33
action_108 (116) = happyShift action_34
action_108 (118) = happyShift action_35
action_108 (124) = happyReduce_133
action_108 (129) = happyShift action_36
action_108 (131) = happyShift action_37
action_108 (6) = happyGoto action_138
action_108 (7) = happyGoto action_14
action_108 (8) = happyGoto action_50
action_108 (9) = happyGoto action_16
action_108 (10) = happyGoto action_17
action_108 (12) = happyGoto action_18
action_108 (13) = happyGoto action_19
action_108 (14) = happyGoto action_20
action_108 (15) = happyGoto action_21
action_108 (17) = happyGoto action_139
action_108 (18) = happyGoto action_22
action_108 (19) = happyGoto action_23
action_108 (20) = happyGoto action_24
action_108 (22) = happyGoto action_25
action_108 (23) = happyGoto action_26
action_108 (51) = happyGoto action_54
action_108 (53) = happyGoto action_55
action_108 _ = happyReduce_154

action_109 (66) = happyShift action_4
action_109 (67) = happyShift action_27
action_109 (68) = happyShift action_28
action_109 (69) = happyShift action_29
action_109 (70) = happyShift action_30
action_109 (95) = happyShift action_31
action_109 (98) = happyShift action_32
action_109 (114) = happyShift action_33
action_109 (116) = happyShift action_34
action_109 (118) = happyShift action_35
action_109 (129) = happyShift action_36
action_109 (131) = happyShift action_37
action_109 (6) = happyGoto action_13
action_109 (7) = happyGoto action_14
action_109 (8) = happyGoto action_137
action_109 (9) = happyGoto action_16
action_109 (10) = happyGoto action_17
action_109 (12) = happyGoto action_18
action_109 (13) = happyGoto action_19
action_109 (14) = happyGoto action_20
action_109 (15) = happyGoto action_21
action_109 (18) = happyGoto action_22
action_109 (19) = happyGoto action_23
action_109 (20) = happyGoto action_24
action_109 (22) = happyGoto action_25
action_109 (23) = happyGoto action_26
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_130

action_111 _ = happyReduce_38

action_112 _ = happyReduce_23

action_113 (66) = happyShift action_4
action_113 (67) = happyShift action_27
action_113 (68) = happyShift action_28
action_113 (69) = happyShift action_29
action_113 (70) = happyShift action_30
action_113 (95) = happyShift action_31
action_113 (98) = happyShift action_32
action_113 (114) = happyShift action_33
action_113 (116) = happyShift action_34
action_113 (118) = happyShift action_35
action_113 (129) = happyShift action_36
action_113 (131) = happyShift action_37
action_113 (6) = happyGoto action_13
action_113 (7) = happyGoto action_14
action_113 (8) = happyGoto action_136
action_113 (9) = happyGoto action_16
action_113 (10) = happyGoto action_17
action_113 (12) = happyGoto action_18
action_113 (13) = happyGoto action_19
action_113 (14) = happyGoto action_20
action_113 (15) = happyGoto action_21
action_113 (18) = happyGoto action_22
action_113 (19) = happyGoto action_23
action_113 (20) = happyGoto action_24
action_113 (22) = happyGoto action_25
action_113 (23) = happyGoto action_26
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (122) = happyShift action_135
action_114 _ = happyReduce_54

action_115 (119) = happyReduce_155
action_115 (128) = happyShift action_131
action_115 (27) = happyGoto action_128
action_115 (64) = happyGoto action_134
action_115 _ = happyReduce_155

action_116 (117) = happyReduce_141
action_116 (122) = happyReduce_141
action_116 (128) = happyReduce_141
action_116 _ = happyReduce_141

action_117 (117) = happyReduce_152
action_117 (122) = happyShift action_130
action_117 (128) = happyReduce_152
action_117 _ = happyReduce_152

action_118 (117) = happyReduce_155
action_118 (128) = happyShift action_131
action_118 (27) = happyGoto action_128
action_118 (64) = happyGoto action_133
action_118 _ = happyReduce_155

action_119 (115) = happyShift action_132
action_119 (122) = happyReduce_141
action_119 (128) = happyReduce_141
action_119 _ = happyReduce_141

action_120 (115) = happyReduce_155
action_120 (122) = happyShift action_130
action_120 (128) = happyShift action_131
action_120 (27) = happyGoto action_128
action_120 (64) = happyGoto action_129
action_120 _ = happyReduce_155

action_121 _ = happyReduce_158

action_122 (66) = happyShift action_4
action_122 (67) = happyShift action_27
action_122 (68) = happyShift action_28
action_122 (69) = happyShift action_29
action_122 (70) = happyShift action_30
action_122 (95) = happyShift action_31
action_122 (98) = happyShift action_32
action_122 (114) = happyShift action_33
action_122 (116) = happyShift action_34
action_122 (118) = happyShift action_35
action_122 (129) = happyShift action_36
action_122 (131) = happyShift action_37
action_122 (6) = happyGoto action_13
action_122 (7) = happyGoto action_14
action_122 (8) = happyGoto action_127
action_122 (9) = happyGoto action_16
action_122 (10) = happyGoto action_17
action_122 (12) = happyGoto action_18
action_122 (13) = happyGoto action_19
action_122 (14) = happyGoto action_20
action_122 (15) = happyGoto action_21
action_122 (18) = happyGoto action_22
action_122 (19) = happyGoto action_23
action_122 (20) = happyGoto action_24
action_122 (22) = happyGoto action_25
action_122 (23) = happyGoto action_26
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (66) = happyShift action_4
action_123 (67) = happyShift action_27
action_123 (68) = happyShift action_28
action_123 (69) = happyShift action_29
action_123 (70) = happyShift action_30
action_123 (114) = happyShift action_45
action_123 (116) = happyShift action_46
action_123 (118) = happyShift action_47
action_123 (6) = happyGoto action_124
action_123 (7) = happyGoto action_39
action_123 (19) = happyGoto action_40
action_123 (25) = happyGoto action_125
action_123 (52) = happyGoto action_126
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (66) = happyReduce_46
action_124 (67) = happyReduce_46
action_124 (68) = happyReduce_46
action_124 (69) = happyReduce_46
action_124 (70) = happyReduce_46
action_124 (114) = happyReduce_46
action_124 (115) = happyReduce_46
action_124 (116) = happyReduce_46
action_124 (117) = happyReduce_46
action_124 (118) = happyReduce_46
action_124 (122) = happyReduce_46
action_124 (123) = happyReduce_46
action_124 (128) = happyReduce_46
action_124 _ = happyReduce_46

action_125 _ = happyReduce_131

action_126 (66) = happyShift action_4
action_126 (67) = happyShift action_27
action_126 (68) = happyShift action_28
action_126 (69) = happyShift action_29
action_126 (70) = happyShift action_30
action_126 (114) = happyShift action_45
action_126 (115) = happyReduce_45
action_126 (116) = happyShift action_46
action_126 (117) = happyReduce_45
action_126 (118) = happyShift action_47
action_126 (122) = happyReduce_45
action_126 (123) = happyReduce_45
action_126 (128) = happyReduce_45
action_126 (6) = happyGoto action_124
action_126 (7) = happyGoto action_39
action_126 (19) = happyGoto action_40
action_126 (25) = happyGoto action_204
action_126 _ = happyReduce_45

action_127 _ = happyReduce_20

action_128 _ = happyReduce_156

action_129 (115) = happyShift action_203
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (66) = happyShift action_4
action_130 (67) = happyShift action_27
action_130 (68) = happyShift action_28
action_130 (69) = happyShift action_29
action_130 (70) = happyShift action_30
action_130 (114) = happyShift action_45
action_130 (116) = happyShift action_46
action_130 (118) = happyShift action_47
action_130 (6) = happyGoto action_38
action_130 (7) = happyGoto action_39
action_130 (19) = happyGoto action_40
action_130 (24) = happyGoto action_202
action_130 (25) = happyGoto action_42
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (66) = happyShift action_4
action_131 (6) = happyGoto action_201
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_52

action_133 (117) = happyShift action_200
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (119) = happyShift action_199
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (66) = happyShift action_4
action_135 (6) = happyGoto action_114
action_135 (26) = happyGoto action_198
action_135 _ = happyReduce_53

action_136 (119) = happyReduce_41
action_136 (122) = happyShift action_197
action_136 _ = happyReduce_41

action_137 (119) = happyReduce_25
action_137 (121) = happyReduce_25
action_137 _ = happyReduce_25

action_138 (67) = happyReduce_29
action_138 (68) = happyReduce_29
action_138 (69) = happyReduce_29
action_138 (70) = happyReduce_29
action_138 (71) = happyReduce_29
action_138 (72) = happyReduce_29
action_138 (73) = happyReduce_29
action_138 (74) = happyReduce_29
action_138 (75) = happyReduce_29
action_138 (76) = happyReduce_29
action_138 (77) = happyReduce_29
action_138 (78) = happyReduce_29
action_138 (79) = happyReduce_29
action_138 (80) = happyReduce_29
action_138 (81) = happyReduce_29
action_138 (82) = happyReduce_29
action_138 (83) = happyReduce_29
action_138 (84) = happyReduce_29
action_138 (85) = happyReduce_29
action_138 (86) = happyReduce_29
action_138 (87) = happyReduce_29
action_138 (88) = happyReduce_29
action_138 (95) = happyReduce_29
action_138 (98) = happyReduce_29
action_138 (114) = happyReduce_29
action_138 (116) = happyReduce_29
action_138 (118) = happyReduce_29
action_138 (119) = happyReduce_29
action_138 (121) = happyReduce_29
action_138 (129) = happyReduce_29
action_138 (131) = happyReduce_29
action_138 _ = happyReduce_129

action_139 _ = happyReduce_144

action_140 _ = happyReduce_140

action_141 (123) = happyShift action_196
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (66) = happyShift action_4
action_142 (67) = happyShift action_27
action_142 (68) = happyShift action_28
action_142 (69) = happyShift action_29
action_142 (70) = happyShift action_30
action_142 (114) = happyShift action_45
action_142 (116) = happyShift action_46
action_142 (118) = happyShift action_47
action_142 (6) = happyGoto action_38
action_142 (7) = happyGoto action_39
action_142 (19) = happyGoto action_40
action_142 (24) = happyGoto action_195
action_142 (25) = happyGoto action_42
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (97) = happyShift action_194
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (66) = happyShift action_4
action_144 (67) = happyShift action_27
action_144 (68) = happyShift action_28
action_144 (69) = happyShift action_29
action_144 (70) = happyShift action_30
action_144 (95) = happyShift action_31
action_144 (98) = happyShift action_32
action_144 (114) = happyShift action_33
action_144 (116) = happyShift action_34
action_144 (118) = happyShift action_35
action_144 (129) = happyShift action_36
action_144 (131) = happyShift action_37
action_144 (6) = happyGoto action_13
action_144 (7) = happyGoto action_14
action_144 (12) = happyGoto action_193
action_144 (13) = happyGoto action_19
action_144 (14) = happyGoto action_20
action_144 (15) = happyGoto action_21
action_144 (18) = happyGoto action_22
action_144 (19) = happyGoto action_23
action_144 (20) = happyGoto action_24
action_144 (22) = happyGoto action_25
action_144 (23) = happyGoto action_26
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (66) = happyShift action_4
action_145 (67) = happyShift action_27
action_145 (68) = happyShift action_28
action_145 (69) = happyShift action_29
action_145 (70) = happyShift action_30
action_145 (95) = happyShift action_31
action_145 (98) = happyShift action_32
action_145 (114) = happyShift action_33
action_145 (116) = happyShift action_34
action_145 (118) = happyShift action_35
action_145 (129) = happyShift action_36
action_145 (131) = happyShift action_37
action_145 (6) = happyGoto action_13
action_145 (7) = happyGoto action_14
action_145 (12) = happyGoto action_192
action_145 (13) = happyGoto action_19
action_145 (14) = happyGoto action_20
action_145 (15) = happyGoto action_21
action_145 (18) = happyGoto action_22
action_145 (19) = happyGoto action_23
action_145 (20) = happyGoto action_24
action_145 (22) = happyGoto action_25
action_145 (23) = happyGoto action_26
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (66) = happyShift action_4
action_146 (68) = happyShift action_160
action_146 (69) = happyShift action_161
action_146 (70) = happyShift action_162
action_146 (110) = happyShift action_173
action_146 (114) = happyShift action_164
action_146 (118) = happyShift action_165
action_146 (128) = happyShift action_166
action_146 (131) = happyShift action_167
action_146 (6) = happyGoto action_151
action_146 (29) = happyGoto action_191
action_146 (30) = happyGoto action_153
action_146 (31) = happyGoto action_154
action_146 (32) = happyGoto action_155
action_146 (33) = happyGoto action_156
action_146 (34) = happyGoto action_157
action_146 (35) = happyGoto action_158
action_146 (36) = happyGoto action_159
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_105

action_148 (123) = happyShift action_190
action_148 _ = happyReduce_102

action_149 (66) = happyShift action_4
action_149 (114) = happyShift action_149
action_149 (6) = happyGoto action_147
action_149 (45) = happyGoto action_189
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (66) = happyShift action_4
action_150 (6) = happyGoto action_186
action_150 (46) = happyGoto action_187
action_150 (47) = happyGoto action_188
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (120) = happyShift action_185
action_151 _ = happyReduce_77

action_152 (93) = happyShift action_184
action_152 _ = happyReduce_98

action_153 _ = happyReduce_59

action_154 (66) = happyReduce_60
action_154 (68) = happyReduce_60
action_154 (69) = happyReduce_60
action_154 (70) = happyReduce_60
action_154 (71) = happyReduce_60
action_154 (91) = happyReduce_60
action_154 (93) = happyReduce_60
action_154 (100) = happyReduce_60
action_154 (101) = happyReduce_60
action_154 (114) = happyReduce_60
action_154 (115) = happyReduce_60
action_154 (118) = happyReduce_60
action_154 (119) = happyReduce_60
action_154 (122) = happyReduce_60
action_154 (123) = happyReduce_60
action_154 (127) = happyReduce_60
action_154 (128) = happyShift action_183
action_154 (130) = happyReduce_60
action_154 (131) = happyReduce_60
action_154 (134) = happyReduce_60
action_154 _ = happyReduce_60

action_155 (66) = happyReduce_61
action_155 (68) = happyReduce_61
action_155 (69) = happyReduce_61
action_155 (70) = happyReduce_61
action_155 (71) = happyReduce_61
action_155 (91) = happyReduce_61
action_155 (93) = happyReduce_61
action_155 (100) = happyReduce_61
action_155 (101) = happyReduce_61
action_155 (114) = happyReduce_61
action_155 (115) = happyReduce_61
action_155 (118) = happyReduce_61
action_155 (119) = happyReduce_61
action_155 (122) = happyReduce_61
action_155 (123) = happyShift action_182
action_155 (127) = happyReduce_61
action_155 (128) = happyReduce_61
action_155 (130) = happyReduce_61
action_155 (131) = happyReduce_61
action_155 (134) = happyReduce_61
action_155 _ = happyReduce_61

action_156 (66) = happyReduce_65
action_156 (68) = happyReduce_65
action_156 (69) = happyReduce_65
action_156 (70) = happyReduce_65
action_156 (71) = happyReduce_65
action_156 (91) = happyReduce_65
action_156 (93) = happyReduce_65
action_156 (100) = happyReduce_65
action_156 (101) = happyReduce_65
action_156 (114) = happyReduce_65
action_156 (115) = happyReduce_65
action_156 (118) = happyReduce_65
action_156 (119) = happyReduce_65
action_156 (122) = happyReduce_65
action_156 (123) = happyReduce_65
action_156 (127) = happyReduce_65
action_156 (128) = happyReduce_65
action_156 (130) = happyReduce_65
action_156 (131) = happyReduce_65
action_156 (134) = happyReduce_65
action_156 _ = happyReduce_65

action_157 (66) = happyShift action_4
action_157 (68) = happyShift action_160
action_157 (69) = happyShift action_161
action_157 (70) = happyShift action_162
action_157 (71) = happyShift action_181
action_157 (114) = happyShift action_164
action_157 (118) = happyShift action_165
action_157 (131) = happyShift action_167
action_157 (6) = happyGoto action_179
action_157 (35) = happyGoto action_180
action_157 (36) = happyGoto action_159
action_157 _ = happyReduce_67

action_158 _ = happyReduce_69

action_159 _ = happyReduce_72

action_160 _ = happyReduce_74

action_161 _ = happyReduce_76

action_162 _ = happyReduce_75

action_163 (66) = happyShift action_4
action_163 (6) = happyGoto action_178
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (66) = happyShift action_4
action_164 (68) = happyShift action_160
action_164 (69) = happyShift action_161
action_164 (70) = happyShift action_162
action_164 (110) = happyShift action_173
action_164 (114) = happyShift action_164
action_164 (118) = happyShift action_165
action_164 (128) = happyShift action_166
action_164 (131) = happyShift action_167
action_164 (6) = happyGoto action_151
action_164 (29) = happyGoto action_176
action_164 (30) = happyGoto action_153
action_164 (31) = happyGoto action_154
action_164 (32) = happyGoto action_155
action_164 (33) = happyGoto action_156
action_164 (34) = happyGoto action_157
action_164 (35) = happyGoto action_158
action_164 (36) = happyGoto action_159
action_164 (60) = happyGoto action_177
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (66) = happyShift action_4
action_165 (6) = happyGoto action_174
action_165 (37) = happyGoto action_175
action_165 _ = happyReduce_81

action_166 (66) = happyShift action_4
action_166 (68) = happyShift action_160
action_166 (69) = happyShift action_161
action_166 (70) = happyShift action_162
action_166 (110) = happyShift action_173
action_166 (114) = happyShift action_164
action_166 (118) = happyShift action_165
action_166 (128) = happyShift action_166
action_166 (131) = happyShift action_167
action_166 (6) = happyGoto action_151
action_166 (29) = happyGoto action_172
action_166 (30) = happyGoto action_153
action_166 (31) = happyGoto action_154
action_166 (32) = happyGoto action_155
action_166 (33) = happyGoto action_156
action_166 (34) = happyGoto action_157
action_166 (35) = happyGoto action_158
action_166 (36) = happyGoto action_159
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (66) = happyShift action_4
action_167 (6) = happyGoto action_170
action_167 (51) = happyGoto action_171
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (127) = happyShift action_169
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (66) = happyShift action_4
action_169 (67) = happyShift action_27
action_169 (68) = happyShift action_28
action_169 (69) = happyShift action_29
action_169 (70) = happyShift action_30
action_169 (95) = happyShift action_31
action_169 (98) = happyShift action_32
action_169 (114) = happyShift action_33
action_169 (116) = happyShift action_34
action_169 (118) = happyShift action_35
action_169 (129) = happyShift action_36
action_169 (131) = happyShift action_37
action_169 (6) = happyGoto action_13
action_169 (7) = happyGoto action_14
action_169 (8) = happyGoto action_230
action_169 (9) = happyGoto action_16
action_169 (10) = happyGoto action_17
action_169 (12) = happyGoto action_18
action_169 (13) = happyGoto action_19
action_169 (14) = happyGoto action_20
action_169 (15) = happyGoto action_21
action_169 (18) = happyGoto action_22
action_169 (19) = happyGoto action_23
action_169 (20) = happyGoto action_24
action_169 (22) = happyGoto action_25
action_169 (23) = happyGoto action_26
action_169 _ = happyFail (happyExpListPerState 169)

action_170 _ = happyReduce_129

action_171 (66) = happyShift action_4
action_171 (125) = happyShift action_229
action_171 (6) = happyGoto action_110
action_171 _ = happyFail (happyExpListPerState 171)

action_172 _ = happyReduce_63

action_173 (66) = happyShift action_4
action_173 (6) = happyGoto action_228
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (120) = happyShift action_227
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (119) = happyShift action_226
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (115) = happyShift action_225
action_176 (122) = happyReduce_147
action_176 _ = happyReduce_147

action_177 (115) = happyShift action_223
action_177 (122) = happyShift action_224
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (120) = happyShift action_222
action_178 _ = happyFail (happyExpListPerState 178)

action_179 _ = happyReduce_77

action_180 _ = happyReduce_70

action_181 _ = happyReduce_71

action_182 (66) = happyShift action_4
action_182 (68) = happyShift action_160
action_182 (69) = happyShift action_161
action_182 (70) = happyShift action_162
action_182 (110) = happyShift action_173
action_182 (114) = happyShift action_164
action_182 (118) = happyShift action_165
action_182 (131) = happyShift action_167
action_182 (6) = happyGoto action_179
action_182 (33) = happyGoto action_221
action_182 (34) = happyGoto action_157
action_182 (35) = happyGoto action_158
action_182 (36) = happyGoto action_159
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (66) = happyShift action_4
action_183 (68) = happyShift action_160
action_183 (69) = happyShift action_161
action_183 (70) = happyShift action_162
action_183 (110) = happyShift action_173
action_183 (114) = happyShift action_164
action_183 (118) = happyShift action_165
action_183 (128) = happyShift action_166
action_183 (131) = happyShift action_167
action_183 (6) = happyGoto action_151
action_183 (29) = happyGoto action_220
action_183 (30) = happyGoto action_153
action_183 (31) = happyGoto action_154
action_183 (32) = happyGoto action_155
action_183 (33) = happyGoto action_156
action_183 (34) = happyGoto action_157
action_183 (35) = happyGoto action_158
action_183 (36) = happyGoto action_159
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (66) = happyShift action_4
action_184 (6) = happyGoto action_217
action_184 (42) = happyGoto action_218
action_184 (59) = happyGoto action_219
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (66) = happyShift action_4
action_185 (68) = happyShift action_160
action_185 (69) = happyShift action_161
action_185 (70) = happyShift action_162
action_185 (110) = happyShift action_173
action_185 (114) = happyShift action_164
action_185 (118) = happyShift action_165
action_185 (128) = happyShift action_166
action_185 (131) = happyShift action_167
action_185 (6) = happyGoto action_151
action_185 (29) = happyGoto action_216
action_185 (30) = happyGoto action_153
action_185 (31) = happyGoto action_154
action_185 (32) = happyGoto action_155
action_185 (33) = happyGoto action_156
action_185 (34) = happyGoto action_157
action_185 (35) = happyGoto action_158
action_185 (36) = happyGoto action_159
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (120) = happyShift action_215
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_107

action_188 (93) = happyShift action_213
action_188 (128) = happyShift action_214
action_188 _ = happyReduce_87

action_189 (115) = happyShift action_212
action_189 (123) = happyShift action_190
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (66) = happyShift action_4
action_190 (114) = happyShift action_149
action_190 (6) = happyGoto action_147
action_190 (45) = happyGoto action_211
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (93) = happyShift action_210
action_191 _ = happyReduce_89

action_192 (66) = happyShift action_4
action_192 (67) = happyShift action_27
action_192 (68) = happyShift action_28
action_192 (69) = happyShift action_29
action_192 (70) = happyShift action_30
action_192 (71) = happyShift action_67
action_192 (72) = happyReduce_9
action_192 (73) = happyReduce_9
action_192 (74) = happyReduce_9
action_192 (75) = happyReduce_9
action_192 (76) = happyReduce_9
action_192 (77) = happyReduce_9
action_192 (78) = happyReduce_9
action_192 (79) = happyReduce_9
action_192 (80) = happyReduce_9
action_192 (81) = happyReduce_9
action_192 (82) = happyReduce_9
action_192 (83) = happyReduce_9
action_192 (84) = happyReduce_9
action_192 (85) = happyReduce_9
action_192 (86) = happyReduce_9
action_192 (87) = happyReduce_9
action_192 (88) = happyReduce_9
action_192 (91) = happyReduce_9
action_192 (93) = happyReduce_9
action_192 (95) = happyShift action_31
action_192 (96) = happyReduce_9
action_192 (97) = happyReduce_9
action_192 (98) = happyShift action_32
action_192 (100) = happyReduce_9
action_192 (101) = happyReduce_9
action_192 (114) = happyShift action_33
action_192 (115) = happyReduce_9
action_192 (116) = happyShift action_34
action_192 (117) = happyReduce_9
action_192 (118) = happyShift action_35
action_192 (119) = happyReduce_9
action_192 (121) = happyReduce_9
action_192 (122) = happyReduce_9
action_192 (128) = happyReduce_9
action_192 (129) = happyShift action_36
action_192 (131) = happyShift action_37
action_192 (134) = happyReduce_9
action_192 (6) = happyGoto action_13
action_192 (7) = happyGoto action_14
action_192 (13) = happyGoto action_66
action_192 (14) = happyGoto action_20
action_192 (15) = happyGoto action_21
action_192 (18) = happyGoto action_22
action_192 (19) = happyGoto action_23
action_192 (20) = happyGoto action_24
action_192 (22) = happyGoto action_25
action_192 (23) = happyGoto action_26
action_192 _ = happyReduce_9

action_193 (66) = happyShift action_4
action_193 (67) = happyShift action_27
action_193 (68) = happyShift action_28
action_193 (69) = happyShift action_29
action_193 (70) = happyShift action_30
action_193 (71) = happyShift action_67
action_193 (95) = happyShift action_31
action_193 (98) = happyShift action_32
action_193 (114) = happyShift action_33
action_193 (116) = happyShift action_34
action_193 (118) = happyShift action_35
action_193 (129) = happyShift action_36
action_193 (131) = happyShift action_37
action_193 (6) = happyGoto action_13
action_193 (7) = happyGoto action_14
action_193 (13) = happyGoto action_66
action_193 (14) = happyGoto action_20
action_193 (15) = happyGoto action_21
action_193 (18) = happyGoto action_22
action_193 (19) = happyGoto action_23
action_193 (20) = happyGoto action_24
action_193 (22) = happyGoto action_25
action_193 (23) = happyGoto action_26
action_193 _ = happyReduce_11

action_194 (66) = happyShift action_4
action_194 (67) = happyShift action_27
action_194 (68) = happyShift action_28
action_194 (69) = happyShift action_29
action_194 (70) = happyShift action_30
action_194 (95) = happyShift action_31
action_194 (98) = happyShift action_32
action_194 (114) = happyShift action_33
action_194 (116) = happyShift action_34
action_194 (118) = happyShift action_35
action_194 (129) = happyShift action_36
action_194 (131) = happyShift action_37
action_194 (6) = happyGoto action_13
action_194 (7) = happyGoto action_14
action_194 (8) = happyGoto action_209
action_194 (9) = happyGoto action_16
action_194 (10) = happyGoto action_17
action_194 (12) = happyGoto action_18
action_194 (13) = happyGoto action_19
action_194 (14) = happyGoto action_20
action_194 (15) = happyGoto action_21
action_194 (18) = happyGoto action_22
action_194 (19) = happyGoto action_23
action_194 (20) = happyGoto action_24
action_194 (22) = happyGoto action_25
action_194 (23) = happyGoto action_26
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (123) = happyShift action_208
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (66) = happyShift action_4
action_196 (67) = happyShift action_27
action_196 (68) = happyShift action_28
action_196 (69) = happyShift action_29
action_196 (70) = happyShift action_30
action_196 (95) = happyShift action_31
action_196 (98) = happyShift action_32
action_196 (114) = happyShift action_33
action_196 (116) = happyShift action_34
action_196 (118) = happyShift action_35
action_196 (129) = happyShift action_36
action_196 (131) = happyShift action_37
action_196 (6) = happyGoto action_13
action_196 (7) = happyGoto action_14
action_196 (8) = happyGoto action_207
action_196 (9) = happyGoto action_16
action_196 (10) = happyGoto action_17
action_196 (12) = happyGoto action_18
action_196 (13) = happyGoto action_19
action_196 (14) = happyGoto action_20
action_196 (15) = happyGoto action_21
action_196 (18) = happyGoto action_22
action_196 (19) = happyGoto action_23
action_196 (20) = happyGoto action_24
action_196 (22) = happyGoto action_25
action_196 (23) = happyGoto action_26
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (66) = happyShift action_4
action_197 (6) = happyGoto action_205
action_197 (21) = happyGoto action_206
action_197 _ = happyReduce_39

action_198 _ = happyReduce_55

action_199 _ = happyReduce_51

action_200 _ = happyReduce_50

action_201 _ = happyReduce_56

action_202 _ = happyReduce_142

action_203 _ = happyReduce_49

action_204 _ = happyReduce_132

action_205 (120) = happyShift action_113
action_205 _ = happyFail (happyExpListPerState 205)

action_206 _ = happyReduce_40

action_207 _ = happyReduce_57

action_208 (66) = happyShift action_4
action_208 (67) = happyShift action_27
action_208 (68) = happyShift action_28
action_208 (69) = happyShift action_29
action_208 (70) = happyShift action_30
action_208 (95) = happyShift action_31
action_208 (98) = happyShift action_32
action_208 (114) = happyShift action_33
action_208 (116) = happyShift action_34
action_208 (118) = happyShift action_35
action_208 (129) = happyShift action_36
action_208 (131) = happyShift action_37
action_208 (6) = happyGoto action_13
action_208 (7) = happyGoto action_14
action_208 (8) = happyGoto action_246
action_208 (9) = happyGoto action_16
action_208 (10) = happyGoto action_17
action_208 (12) = happyGoto action_18
action_208 (13) = happyGoto action_19
action_208 (14) = happyGoto action_20
action_208 (15) = happyGoto action_21
action_208 (18) = happyGoto action_22
action_208 (19) = happyGoto action_23
action_208 (20) = happyGoto action_24
action_208 (22) = happyGoto action_25
action_208 (23) = happyGoto action_26
action_208 _ = happyFail (happyExpListPerState 208)

action_209 _ = happyReduce_18

action_210 (66) = happyShift action_4
action_210 (6) = happyGoto action_217
action_210 (42) = happyGoto action_218
action_210 (59) = happyGoto action_245
action_210 _ = happyFail (happyExpListPerState 210)

action_211 (123) = happyShift action_190
action_211 _ = happyReduce_103

action_212 _ = happyReduce_104

action_213 (66) = happyShift action_4
action_213 (6) = happyGoto action_217
action_213 (42) = happyGoto action_218
action_213 (59) = happyGoto action_244
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (66) = happyShift action_4
action_214 (6) = happyGoto action_186
action_214 (46) = happyGoto action_243
action_214 _ = happyFail (happyExpListPerState 214)

action_215 (66) = happyShift action_4
action_215 (68) = happyShift action_160
action_215 (69) = happyShift action_161
action_215 (70) = happyShift action_162
action_215 (110) = happyShift action_173
action_215 (114) = happyShift action_164
action_215 (118) = happyShift action_165
action_215 (128) = happyShift action_166
action_215 (131) = happyShift action_167
action_215 (6) = happyGoto action_151
action_215 (29) = happyGoto action_242
action_215 (30) = happyGoto action_153
action_215 (31) = happyGoto action_154
action_215 (32) = happyGoto action_155
action_215 (33) = happyGoto action_156
action_215 (34) = happyGoto action_157
action_215 (35) = happyGoto action_158
action_215 (36) = happyGoto action_159
action_215 _ = happyFail (happyExpListPerState 215)

action_216 _ = happyReduce_62

action_217 (111) = happyShift action_238
action_217 (126) = happyShift action_239
action_217 (127) = happyShift action_240
action_217 (128) = happyShift action_241
action_217 _ = happyFail (happyExpListPerState 217)

action_218 (91) = happyReduce_145
action_218 (100) = happyReduce_145
action_218 (101) = happyReduce_145
action_218 (122) = happyReduce_145
action_218 (127) = happyReduce_145
action_218 (130) = happyReduce_145
action_218 (134) = happyReduce_145
action_218 _ = happyReduce_145

action_219 (122) = happyShift action_237
action_219 _ = happyReduce_99

action_220 _ = happyReduce_64

action_221 _ = happyReduce_66

action_222 (66) = happyShift action_4
action_222 (68) = happyShift action_160
action_222 (69) = happyShift action_161
action_222 (70) = happyShift action_162
action_222 (110) = happyShift action_173
action_222 (114) = happyShift action_164
action_222 (118) = happyShift action_165
action_222 (128) = happyShift action_166
action_222 (131) = happyShift action_167
action_222 (6) = happyGoto action_151
action_222 (29) = happyGoto action_236
action_222 (30) = happyGoto action_153
action_222 (31) = happyGoto action_154
action_222 (32) = happyGoto action_155
action_222 (33) = happyGoto action_156
action_222 (34) = happyGoto action_157
action_222 (35) = happyGoto action_158
action_222 (36) = happyGoto action_159
action_222 _ = happyFail (happyExpListPerState 222)

action_223 _ = happyReduce_79

action_224 (66) = happyShift action_4
action_224 (68) = happyShift action_160
action_224 (69) = happyShift action_161
action_224 (70) = happyShift action_162
action_224 (110) = happyShift action_173
action_224 (114) = happyShift action_164
action_224 (118) = happyShift action_165
action_224 (128) = happyShift action_166
action_224 (131) = happyShift action_167
action_224 (6) = happyGoto action_151
action_224 (29) = happyGoto action_235
action_224 (30) = happyGoto action_153
action_224 (31) = happyGoto action_154
action_224 (32) = happyGoto action_155
action_224 (33) = happyGoto action_156
action_224 (34) = happyGoto action_157
action_224 (35) = happyGoto action_158
action_224 (36) = happyGoto action_159
action_224 _ = happyFail (happyExpListPerState 224)

action_225 _ = happyReduce_78

action_226 _ = happyReduce_80

action_227 (66) = happyShift action_4
action_227 (68) = happyShift action_160
action_227 (69) = happyShift action_161
action_227 (70) = happyShift action_162
action_227 (110) = happyShift action_173
action_227 (114) = happyShift action_164
action_227 (118) = happyShift action_165
action_227 (128) = happyShift action_166
action_227 (131) = happyShift action_167
action_227 (6) = happyGoto action_151
action_227 (29) = happyGoto action_234
action_227 (30) = happyGoto action_153
action_227 (31) = happyGoto action_154
action_227 (32) = happyGoto action_155
action_227 (33) = happyGoto action_156
action_227 (34) = happyGoto action_157
action_227 (35) = happyGoto action_158
action_227 (36) = happyGoto action_159
action_227 _ = happyFail (happyExpListPerState 227)

action_228 (120) = happyShift action_233
action_228 _ = happyFail (happyExpListPerState 228)

action_229 (66) = happyShift action_4
action_229 (68) = happyShift action_160
action_229 (69) = happyShift action_161
action_229 (70) = happyShift action_162
action_229 (110) = happyShift action_173
action_229 (114) = happyShift action_164
action_229 (118) = happyShift action_165
action_229 (128) = happyShift action_166
action_229 (131) = happyShift action_167
action_229 (6) = happyGoto action_151
action_229 (29) = happyGoto action_232
action_229 (30) = happyGoto action_153
action_229 (31) = happyGoto action_154
action_229 (32) = happyGoto action_155
action_229 (33) = happyGoto action_156
action_229 (34) = happyGoto action_157
action_229 (35) = happyGoto action_158
action_229 (36) = happyGoto action_159
action_229 _ = happyFail (happyExpListPerState 229)

action_230 (93) = happyShift action_231
action_230 _ = happyReduce_91

action_231 (66) = happyShift action_4
action_231 (6) = happyGoto action_254
action_231 (41) = happyGoto action_255
action_231 (55) = happyGoto action_256
action_231 _ = happyFail (happyExpListPerState 231)

action_232 _ = happyReduce_73

action_233 (66) = happyShift action_4
action_233 (68) = happyShift action_160
action_233 (69) = happyShift action_161
action_233 (70) = happyShift action_162
action_233 (110) = happyShift action_173
action_233 (114) = happyShift action_164
action_233 (118) = happyShift action_165
action_233 (128) = happyShift action_166
action_233 (131) = happyShift action_167
action_233 (6) = happyGoto action_151
action_233 (29) = happyGoto action_253
action_233 (30) = happyGoto action_153
action_233 (31) = happyGoto action_154
action_233 (32) = happyGoto action_155
action_233 (33) = happyGoto action_156
action_233 (34) = happyGoto action_157
action_233 (35) = happyGoto action_158
action_233 (36) = happyGoto action_159
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (122) = happyShift action_252
action_234 _ = happyReduce_83

action_235 _ = happyReduce_148

action_236 (93) = happyReduce_68
action_236 (123) = happyReduce_68
action_236 (127) = happyReduce_100
action_236 (130) = happyReduce_100
action_236 _ = happyReduce_68

action_237 (66) = happyShift action_4
action_237 (6) = happyGoto action_217
action_237 (42) = happyGoto action_251
action_237 _ = happyFail (happyExpListPerState 237)

action_238 (66) = happyShift action_4
action_238 (68) = happyShift action_160
action_238 (69) = happyShift action_161
action_238 (70) = happyShift action_162
action_238 (110) = happyShift action_173
action_238 (114) = happyShift action_164
action_238 (118) = happyShift action_165
action_238 (128) = happyShift action_166
action_238 (131) = happyShift action_167
action_238 (6) = happyGoto action_151
action_238 (29) = happyGoto action_250
action_238 (30) = happyGoto action_153
action_238 (31) = happyGoto action_154
action_238 (32) = happyGoto action_155
action_238 (33) = happyGoto action_156
action_238 (34) = happyGoto action_157
action_238 (35) = happyGoto action_158
action_238 (36) = happyGoto action_159
action_238 _ = happyFail (happyExpListPerState 238)

action_239 (66) = happyShift action_4
action_239 (68) = happyShift action_160
action_239 (69) = happyShift action_161
action_239 (70) = happyShift action_162
action_239 (110) = happyShift action_173
action_239 (114) = happyShift action_164
action_239 (118) = happyShift action_165
action_239 (128) = happyShift action_166
action_239 (131) = happyShift action_167
action_239 (6) = happyGoto action_151
action_239 (29) = happyGoto action_249
action_239 (30) = happyGoto action_153
action_239 (31) = happyGoto action_154
action_239 (32) = happyGoto action_155
action_239 (33) = happyGoto action_156
action_239 (34) = happyGoto action_157
action_239 (35) = happyGoto action_158
action_239 (36) = happyGoto action_159
action_239 _ = happyFail (happyExpListPerState 239)

action_240 (66) = happyShift action_4
action_240 (68) = happyShift action_160
action_240 (69) = happyShift action_161
action_240 (70) = happyShift action_162
action_240 (110) = happyShift action_173
action_240 (114) = happyShift action_164
action_240 (118) = happyShift action_165
action_240 (128) = happyShift action_166
action_240 (131) = happyShift action_167
action_240 (6) = happyGoto action_151
action_240 (29) = happyGoto action_248
action_240 (30) = happyGoto action_153
action_240 (31) = happyGoto action_154
action_240 (32) = happyGoto action_155
action_240 (33) = happyGoto action_156
action_240 (34) = happyGoto action_157
action_240 (35) = happyGoto action_158
action_240 (36) = happyGoto action_159
action_240 _ = happyFail (happyExpListPerState 240)

action_241 (66) = happyShift action_4
action_241 (68) = happyShift action_160
action_241 (69) = happyShift action_161
action_241 (70) = happyShift action_162
action_241 (110) = happyShift action_173
action_241 (114) = happyShift action_164
action_241 (118) = happyShift action_165
action_241 (128) = happyShift action_166
action_241 (131) = happyShift action_167
action_241 (6) = happyGoto action_151
action_241 (29) = happyGoto action_247
action_241 (30) = happyGoto action_153
action_241 (31) = happyGoto action_154
action_241 (32) = happyGoto action_155
action_241 (33) = happyGoto action_156
action_241 (34) = happyGoto action_157
action_241 (35) = happyGoto action_158
action_241 (36) = happyGoto action_159
action_241 _ = happyFail (happyExpListPerState 241)

action_242 _ = happyReduce_106

action_243 _ = happyReduce_108

action_244 (122) = happyShift action_237
action_244 _ = happyReduce_88

action_245 (122) = happyShift action_237
action_245 _ = happyReduce_90

action_246 _ = happyReduce_58

action_247 _ = happyReduce_96

action_248 _ = happyReduce_93

action_249 _ = happyReduce_95

action_250 _ = happyReduce_94

action_251 _ = happyReduce_146

action_252 (66) = happyShift action_4
action_252 (6) = happyGoto action_174
action_252 (37) = happyGoto action_259
action_252 _ = happyReduce_81

action_253 (66) = happyReduce_68
action_253 (68) = happyReduce_68
action_253 (69) = happyReduce_68
action_253 (70) = happyReduce_68
action_253 (71) = happyReduce_68
action_253 (91) = happyReduce_68
action_253 (93) = happyReduce_68
action_253 (100) = happyReduce_68
action_253 (101) = happyReduce_68
action_253 (114) = happyReduce_68
action_253 (115) = happyReduce_68
action_253 (118) = happyReduce_68
action_253 (119) = happyReduce_68
action_253 (122) = happyReduce_68
action_253 (123) = happyReduce_68
action_253 (127) = happyReduce_68
action_253 (128) = happyReduce_68
action_253 (130) = happyReduce_68
action_253 (131) = happyReduce_68
action_253 (134) = happyReduce_68
action_253 _ = happyReduce_68

action_254 (127) = happyShift action_258
action_254 _ = happyFail (happyExpListPerState 254)

action_255 (91) = happyReduce_137
action_255 (100) = happyReduce_137
action_255 (101) = happyReduce_137
action_255 (122) = happyReduce_137
action_255 (134) = happyReduce_137
action_255 _ = happyReduce_137

action_256 (122) = happyShift action_257
action_256 _ = happyReduce_86

action_257 (66) = happyShift action_4
action_257 (6) = happyGoto action_254
action_257 (41) = happyGoto action_261
action_257 _ = happyFail (happyExpListPerState 257)

action_258 (66) = happyShift action_4
action_258 (67) = happyShift action_27
action_258 (68) = happyShift action_28
action_258 (69) = happyShift action_29
action_258 (70) = happyShift action_30
action_258 (95) = happyShift action_31
action_258 (98) = happyShift action_32
action_258 (114) = happyShift action_33
action_258 (116) = happyShift action_34
action_258 (118) = happyShift action_35
action_258 (129) = happyShift action_36
action_258 (131) = happyShift action_37
action_258 (6) = happyGoto action_13
action_258 (7) = happyGoto action_14
action_258 (8) = happyGoto action_260
action_258 (9) = happyGoto action_16
action_258 (10) = happyGoto action_17
action_258 (12) = happyGoto action_18
action_258 (13) = happyGoto action_19
action_258 (14) = happyGoto action_20
action_258 (15) = happyGoto action_21
action_258 (18) = happyGoto action_22
action_258 (19) = happyGoto action_23
action_258 (20) = happyGoto action_24
action_258 (22) = happyGoto action_25
action_258 (23) = happyGoto action_26
action_258 _ = happyFail (happyExpListPerState 258)

action_259 _ = happyReduce_82

action_260 _ = happyReduce_92

action_261 _ = happyReduce_138

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (P.identifier happy_var_1 PE.Identifier
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (P.identifier happy_var_1 PE.Hole
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  9 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  10 happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 10 happyReduction_9
happyReduction_9 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (P.infixApplication happy_var_3 [happy_var_1, happy_var_5]
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  11 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (P.fnApplication happy_var_1 [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (P.parenthesised happy_var_1 happy_var_2 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 14 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (P.controlFlow happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn28  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (P.match happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 14 happyReduction_20
happyReduction_20 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (P.lambda (fmap P.pattern happy_var_2) happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (P.dotLambda happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (P.block happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  16 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn17
		 (P.backcall happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn17
		 (fmap PE.Procedure happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  17 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (P.returnStmt happy_var_2 happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  18 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (P.term happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (P.number PL.LInt happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (P.string PL.LString happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (P.boolean PL.LBool happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_3)
	(HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (P.record happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  21 happyReduction_39
happyReduction_39  =  HappyAbsSyn21
		 ([]
	)

happyReduce_40 = happyReduce 5 21 happyReduction_40
happyReduction_40 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn21
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_3)
	(HappyAbsSyn61  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.list happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_3)
	(HappyAbsSyn56  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (P.tuple happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  24 happyReduction_45
happyReduction_45 (HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn24
		 (P.Tagged happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn25
		 (P.Var happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn25
		 (P.Hole happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn25
		 (P.Term $ fmap PE.Literal happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 25 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn64  happy_var_3) `HappyStk`
	(HappyAbsSyn57  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (P.Tuple happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 25 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn64  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (P.List happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 4 25 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn64  happy_var_3) `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (P.Record happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  25 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  26 happyReduction_53
happyReduction_53  =  HappyAbsSyn26
		 ([]
	)

happyReduce_54 = happySpecReduce_1  26 happyReduction_54
happyReduction_54 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn26
		 ([(happy_var_1, Nothing)]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  26 happyReduction_55
happyReduction_55 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1, Nothing) : happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  27 happyReduction_56
happyReduction_56 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 28 happyReduction_57
happyReduction_57 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ([P.matchCase (P.pattern happy_var_2) happy_var_4]
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 5 28 happyReduction_58
happyReduction_58 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (happy_var_1 ++ [P.matchCase (P.pattern happy_var_3) happy_var_5]
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_1  29 happyReduction_59
happyReduction_59 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  29 happyReduction_60
happyReduction_60 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn29
		 (P.typeUnion happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  30 happyReduction_61
happyReduction_61 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  30 happyReduction_62
happyReduction_62 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  31 happyReduction_63
happyReduction_63 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn31
		 ([happy_var_2]
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  31 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  32 happyReduction_66
happyReduction_66 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  33 happyReduction_67
happyReduction_67 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happyReduce 4 33 happyReduction_68
happyReduction_68 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (P.typeProtocolImplementation (P.tyIdentifier happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_1  34 happyReduction_69
happyReduction_69 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  34 happyReduction_70
happyReduction_70 (HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (P.typeFnApplication happy_var_1 [happy_var_2]
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  34 happyReduction_71
happyReduction_71 (HappyTerminal happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (P.tyParenthesised happy_var_1 happy_var_2 happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  35 happyReduction_72
happyReduction_72 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happyReduce 4 35 happyReduction_73
happyReduction_73 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (P.typeLambda (happy_var_2) happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_1  36 happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.number (PT.TLiteral . PL.LInt) happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  36 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.boolean (PT.TLiteral . PL.LBool) happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  36 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.string (PT.TLiteral . PL.LString) happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  36 happyReduction_77
happyReduction_77 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyIdentifier happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  36 happyReduction_78
happyReduction_78 (HappyTerminal happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  36 happyReduction_79
happyReduction_79 (HappyTerminal happy_var_3)
	(HappyAbsSyn60  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyTuple happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  36 happyReduction_80
happyReduction_80 (HappyTerminal happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_0  37 happyReduction_81
happyReduction_81  =  HappyAbsSyn37
		 ([]
	)

happyReduce_82 = happyReduce 5 37 happyReduction_82
happyReduction_82 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_83 = happySpecReduce_3  37 happyReduction_83
happyReduction_83 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn37
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  38 happyReduction_84
happyReduction_84 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn38
		 (P.script happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  39 happyReduction_85
happyReduction_85 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happyReduce 8 39 happyReduction_86
happyReduction_86 ((HappyAbsSyn55  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_4) `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_87 = happyReduce 5 39 happyReduction_87
happyReduction_87 ((HappyAbsSyn47  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 7 39 happyReduction_88
happyReduction_88 ((HappyAbsSyn59  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_89 = happyReduce 5 39 happyReduction_89
happyReduction_89 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_90 = happyReduce 7 39 happyReduction_90
happyReduction_90 ((HappyAbsSyn59  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_91 = happyReduce 6 40 happyReduction_91
happyReduction_91 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_4) `HappyStk`
	(HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_92 = happySpecReduce_3  41 happyReduction_92
happyReduction_92 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn41
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  42 happyReduction_93
happyReduction_93 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn42
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  42 happyReduction_94
happyReduction_94 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn42
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  42 happyReduction_95
happyReduction_95 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn42
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  42 happyReduction_96
happyReduction_96 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn42
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  43 happyReduction_97
happyReduction_97  =  HappyAbsSyn43
		 (Nothing
	)

happyReduce_98 = happySpecReduce_2  43 happyReduction_98
happyReduction_98 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (Just happy_var_2
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happyReduce 4 43 happyReduction_99
happyReduction_99 ((HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_100 = happyReduce 5 43 happyReduction_100
happyReduction_100 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_101 = happySpecReduce_0  44 happyReduction_101
happyReduction_101  =  HappyAbsSyn44
		 (Nothing
	)

happyReduce_102 = happySpecReduce_2  44 happyReduction_102
happyReduction_102 (HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (Just happy_var_2
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  45 happyReduction_103
happyReduction_103 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  45 happyReduction_104
happyReduction_104 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (happy_var_2
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  45 happyReduction_105
happyReduction_105 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn45
		 (P.kindId happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  46 happyReduction_106
happyReduction_106 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn46
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  47 happyReduction_107
happyReduction_107 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  47 happyReduction_108
happyReduction_108 (HappyAbsSyn46  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  48 happyReduction_109
happyReduction_109 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  48 happyReduction_110
happyReduction_110 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  49 happyReduction_111
happyReduction_111 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  49 happyReduction_112
happyReduction_112 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  49 happyReduction_113
happyReduction_113 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  49 happyReduction_114
happyReduction_114 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  49 happyReduction_115
happyReduction_115 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  49 happyReduction_116
happyReduction_116 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  49 happyReduction_117
happyReduction_117 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  49 happyReduction_118
happyReduction_118 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  49 happyReduction_119
happyReduction_119 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  49 happyReduction_120
happyReduction_120 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  49 happyReduction_121
happyReduction_121 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  49 happyReduction_122
happyReduction_122 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  49 happyReduction_123
happyReduction_123 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  49 happyReduction_124
happyReduction_124 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  49 happyReduction_125
happyReduction_125 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  49 happyReduction_126
happyReduction_126 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  50 happyReduction_127
happyReduction_127 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn50
		 ([happy_var_1]
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_2  50 happyReduction_128
happyReduction_128 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_128 _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  51 happyReduction_129
happyReduction_129 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn51
		 ([happy_var_1]
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_2  51 happyReduction_130
happyReduction_130 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_130 _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  52 happyReduction_131
happyReduction_131 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn52
		 ([happy_var_1]
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_2  52 happyReduction_132
happyReduction_132 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_132 _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_0  53 happyReduction_133
happyReduction_133  =  HappyAbsSyn53
		 ([]
	)

happyReduce_134 = happySpecReduce_1  53 happyReduction_134
happyReduction_134 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_0  54 happyReduction_135
happyReduction_135  =  HappyAbsSyn54
		 ([]
	)

happyReduce_136 = happySpecReduce_1  54 happyReduction_136
happyReduction_136 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  55 happyReduction_137
happyReduction_137 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_3  55 happyReduction_138
happyReduction_138 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_138 _ _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  56 happyReduction_139
happyReduction_139 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn56
		 ([happy_var_1]
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3  56 happyReduction_140
happyReduction_140 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  57 happyReduction_141
happyReduction_141 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn57
		 ([happy_var_1]
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  57 happyReduction_142
happyReduction_142 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  58 happyReduction_143
happyReduction_143 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_3  58 happyReduction_144
happyReduction_144 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1  59 happyReduction_145
happyReduction_145 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_3  59 happyReduction_146
happyReduction_146 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_146 _ _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_1  60 happyReduction_147
happyReduction_147 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn60
		 ([happy_var_1]
	)
happyReduction_147 _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_3  60 happyReduction_148
happyReduction_148 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_0  61 happyReduction_149
happyReduction_149  =  HappyAbsSyn61
		 ([]
	)

happyReduce_150 = happySpecReduce_1  61 happyReduction_150
happyReduction_150 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1
	)
happyReduction_150 _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_0  62 happyReduction_151
happyReduction_151  =  HappyAbsSyn62
		 ([]
	)

happyReduce_152 = happySpecReduce_1  62 happyReduction_152
happyReduction_152 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1
	)
happyReduction_152 _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_0  63 happyReduction_153
happyReduction_153  =  HappyAbsSyn63
		 (Nothing
	)

happyReduce_154 = happySpecReduce_1  63 happyReduction_154
happyReduction_154 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn63
		 (Just happy_var_1
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_0  64 happyReduction_155
happyReduction_155  =  HappyAbsSyn64
		 (Nothing
	)

happyReduce_156 = happySpecReduce_1  64 happyReduction_156
happyReduction_156 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn64
		 (Just happy_var_1
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  65 happyReduction_157
happyReduction_157 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn65
		 ([happy_var_1]
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_2  65 happyReduction_158
happyReduction_158 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_158 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 134 134 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 66;
	L.RangedToken (T.Hole _) _ -> cont 67;
	L.RangedToken (T.Number _) _ -> cont 68;
	L.RangedToken (T.String _) _ -> cont 69;
	L.RangedToken (T.Boolean _) _ -> cont 70;
	L.RangedToken (T.Operator "!") _ -> cont 71;
	L.RangedToken (T.Operator "+") _ -> cont 72;
	L.RangedToken (T.Operator "-") _ -> cont 73;
	L.RangedToken (T.Operator "*") _ -> cont 74;
	L.RangedToken (T.Operator "/") _ -> cont 75;
	L.RangedToken (T.Operator "^") _ -> cont 76;
	L.RangedToken (T.Operator "++") _ -> cont 77;
	L.RangedToken (T.Operator "==") _ -> cont 78;
	L.RangedToken (T.Operator "!=") _ -> cont 79;
	L.RangedToken (T.Operator "<") _ -> cont 80;
	L.RangedToken (T.Operator "<=") _ -> cont 81;
	L.RangedToken (T.Operator ">") _ -> cont 82;
	L.RangedToken (T.Operator ">=") _ -> cont 83;
	L.RangedToken (T.Operator "||") _ -> cont 84;
	L.RangedToken (T.Operator "&&") _ -> cont 85;
	L.RangedToken (T.Operator "|>") _ -> cont 86;
	L.RangedToken (T.Operator "<|") _ -> cont 87;
	L.RangedToken (T.Operator "<|") _ -> cont 88;
	L.RangedToken (T.Operator "#") _ -> cont 89;
	L.RangedToken (T.Operator _) _ -> cont 90;
	L.RangedToken T.Let _ -> cont 91;
	L.RangedToken T.In _ -> cont 92;
	L.RangedToken T.Where _ -> cont 93;
	L.RangedToken T.With _ -> cont 94;
	L.RangedToken T.If _ -> cont 95;
	L.RangedToken T.Then _ -> cont 96;
	L.RangedToken T.Else _ -> cont 97;
	L.RangedToken T.Match _ -> cont 98;
	L.RangedToken T.Return _ -> cont 99;
	L.RangedToken T.Data _ -> cont 100;
	L.RangedToken T.Type _ -> cont 101;
	L.RangedToken T.Alias _ -> cont 102;
	L.RangedToken T.Kind _ -> cont 103;
	L.RangedToken T.Forall _ -> cont 104;
	L.RangedToken T.Exists _ -> cont 105;
	L.RangedToken T.Proof _ -> cont 106;
	L.RangedToken T.Infer _ -> cont 107;
	L.RangedToken T.Protocol _ -> cont 108;
	L.RangedToken T.Interface _ -> cont 109;
	L.RangedToken T.Instance _ -> cont 110;
	L.RangedToken T.Implements _ -> cont 111;
	L.RangedToken T.Module _ -> cont 112;
	L.RangedToken T.Import _ -> cont 113;
	L.RangedToken T.LParen _ -> cont 114;
	L.RangedToken T.RParen _ -> cont 115;
	L.RangedToken T.LBrack _ -> cont 116;
	L.RangedToken T.RBrack _ -> cont 117;
	L.RangedToken T.LCurly _ -> cont 118;
	L.RangedToken T.RCurly _ -> cont 119;
	L.RangedToken T.Colon _ -> cont 120;
	L.RangedToken T.SemiColon _ -> cont 121;
	L.RangedToken T.Comma _ -> cont 122;
	L.RangedToken T.Arrow _ -> cont 123;
	L.RangedToken T.BackArrow _ -> cont 124;
	L.RangedToken T.FatArrow _ -> cont 125;
	L.RangedToken T.PipeArrow _ -> cont 126;
	L.RangedToken T.Equals _ -> cont 127;
	L.RangedToken T.Pipe _ -> cont 128;
	L.RangedToken T.Dot _ -> cont 129;
	L.RangedToken T.Section _ -> cont 130;
	L.RangedToken T.BackSlash _ -> cont 131;
	L.RangedToken T.Newline _ -> cont 132;
	L.RangedToken T.EOF _ -> cont 133;
	_ -> happyError' (tk, [])
	})

happyError_ explist 134 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaScript = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


runSagaExpr :: String -> Either String (P.ParsedData PE.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaDec :: String -> Either String (P.ParsedData PE.Declaration)
runSagaDec input = input `P.run` parseSagaDec

runSagaScript :: String -> Either String (P.ParsedData PE.Script)
runSagaScript input = input `P.run` parseSagaScript
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
