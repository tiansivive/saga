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

data HappyAbsSyn t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61 t62 t63 t64
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1593) ([0,0,0,0,31,8192,1,32789,2,0,0,0,0,16384,384,0,0,0,0,0,0,0,12296,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3074,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,64512,0,1152,21504,2560,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63488,0,2304,43008,5120,0,0,0,0,31,8192,1,32789,2,0,0,0,992,0,36,672,80,0,0,0,31744,0,1152,21504,2560,0,0,0,32768,15,36864,32769,16394,1,0,0,0,16,0,0,0,0,0,0,0,15872,0,0,512,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,61440,1,0,4096,0,0,0,0,0,62,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10240,0,0,0,0,0,0,0,32768,0,0,0,0,57344,3,9216,40960,20482,0,0,0,0,124,32768,4,84,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,4096,8,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,8,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,576,10752,1280,0,0,0,49152,7,18432,16384,40965,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,53248,1,0,4352,9217,0,0,0,0,0,0,0,16384,0,0,0,0,64,0,0,64,0,0,0,0,0,0,0,0,256,0,0,0,0,0,64,0,0,0,0,0,0,2048,2048,0,0,0,0,0,0,64512,0,1152,21504,2560,0,0,0,0,0,0,0,0,0,0,0,0,496,0,18,336,40,0,0,0,0,0,0,0,128,0,0,0,49152,7,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,7936,0,288,5376,640,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,33280,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,8192,3,32789,2,0,0,0,992,0,0,32,0,0,0,0,31744,0,1152,21504,2560,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,576,10752,1280,0,0,0,0,0,0,0,0,0,0,0,0,248,0,9,168,20,0,0,0,7936,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3968,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,7936,0,0,256,0,0,0,0,0,0,4096,0,0,0,0,0,0,124,32768,4,84,10,0,0,0,3968,0,144,2688,320,0,0,0,53248,1,0,4096,9217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,2048,0,0,2048,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,4,0,0,0,16384,15,0,16384,32772,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1856,0,0,1088,144,0,0,0,2048,0,0,0,0,0,0,0,0,29,0,0,16401,2,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,32768,15,36864,32768,16394,1,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,1032,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,59392,0,0,34816,4608,0,0,0,0,29,0,0,16401,2,0,0,0,32,0,0,0,0,0,0,0,29696,0,0,17408,2304,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,128,0,0,0,0,0,0,32768,128,0,0,0,0,8,0,0,8,0,0,0,0,0,0,8,0,0,0,0,0,57344,7,9216,40960,20482,0,0,0,0,252,32768,4,84,10,0,0,0,3968,0,144,2688,320,0,0,0,0,0,0,0,32,0,0,0,0,62,16384,2,42,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,576,10752,1280,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,53248,1,0,4096,9217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,14,0,32768,8200,1,0,0,0,0,0,0,0,0,0,0,0,14848,0,0,8704,1152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7424,0,0,4352,576,0,0,0,0,0,0,0,0,0,0,0,0,116,0,0,68,9,0,0,0,0,0,4,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,29696,0,0,17408,2304,0,0,0,32768,14,0,32768,8200,1,0,0,0,464,0,0,272,36,0,0,0,14848,0,0,8704,1152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,1,4608,20480,10241,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7936,0,288,5376,640,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaExpr","%start_parseSagaDec","%start_parseSagaScript","identifier","hole","expr","expr1","expr2","exprBacktick","expr3","expr4","expr5","expr6","block","statement","exprAtom","term","record","pairs","list","tuple","pattern","patternAtom","patRecordKeys","patRest","cases","typeExpr","typeExpr1","union","typeExpr2","typeExpr3","typeExpr4","typeExpr5","typeAtom","tpairs","script","dec","letdec","binding","bindings","tbinding","tbindings","typeAnnotation","kindAnnotation","kindExpr","dataExpr","dataExprs","defaultSeparator","many__binding__","many__dec__","many__identifier__","many__patternAtom__","many__tbinding__","manyOrEmpty__pattern__","separated__expr__','__","separated__pattern__','__","separated__statement__';'__","separated__typeExpr__','__","separatedOrEmpty__expr__','__","trailing__';'__","trailing__patRest__","many__pattern__","id","HOLE","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'^'","'++'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","'|>'","'<|'","'`'","'#'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 133
        bit_end = (st Prelude.+ 1) Prelude.* 133
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..132]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (65) = happyShift action_4
action_0 (66) = happyShift action_27
action_0 (67) = happyShift action_28
action_0 (68) = happyShift action_29
action_0 (69) = happyShift action_30
action_0 (94) = happyShift action_31
action_0 (97) = happyShift action_32
action_0 (113) = happyShift action_33
action_0 (115) = happyShift action_34
action_0 (117) = happyShift action_35
action_0 (128) = happyShift action_36
action_0 (130) = happyShift action_37
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

action_1 (90) = happyShift action_9
action_1 (99) = happyShift action_10
action_1 (100) = happyShift action_11
action_1 (39) = happyGoto action_12
action_1 (40) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (90) = happyShift action_9
action_2 (99) = happyShift action_10
action_2 (100) = happyShift action_11
action_2 (38) = happyGoto action_5
action_2 (39) = happyGoto action_6
action_2 (40) = happyGoto action_7
action_2 (52) = happyGoto action_8
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (65) = happyShift action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (133) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_114

action_7 _ = happyReduce_82

action_8 (90) = happyShift action_9
action_8 (99) = happyShift action_10
action_8 (100) = happyShift action_11
action_8 (39) = happyGoto action_74
action_8 (40) = happyGoto action_7
action_8 _ = happyReduce_81

action_9 (65) = happyShift action_4
action_9 (6) = happyGoto action_73
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (65) = happyShift action_4
action_10 (6) = happyGoto action_72
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (65) = happyShift action_4
action_11 (6) = happyGoto action_71
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (133) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_29

action_14 _ = happyReduce_28

action_15 (133) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (65) = happyReduce_5
action_16 (66) = happyReduce_5
action_16 (67) = happyReduce_5
action_16 (68) = happyReduce_5
action_16 (69) = happyReduce_5
action_16 (70) = happyReduce_5
action_16 (71) = happyShift action_70
action_16 (87) = happyReduce_5
action_16 (90) = happyReduce_5
action_16 (92) = happyReduce_5
action_16 (94) = happyReduce_5
action_16 (95) = happyReduce_5
action_16 (96) = happyReduce_5
action_16 (97) = happyReduce_5
action_16 (99) = happyReduce_5
action_16 (100) = happyReduce_5
action_16 (113) = happyReduce_5
action_16 (114) = happyReduce_5
action_16 (115) = happyReduce_5
action_16 (116) = happyReduce_5
action_16 (117) = happyReduce_5
action_16 (118) = happyReduce_5
action_16 (120) = happyReduce_5
action_16 (121) = happyReduce_5
action_16 (127) = happyReduce_5
action_16 (128) = happyReduce_5
action_16 (130) = happyReduce_5
action_16 (133) = happyReduce_5
action_16 _ = happyReduce_5

action_17 (65) = happyReduce_6
action_17 (66) = happyReduce_6
action_17 (67) = happyReduce_6
action_17 (68) = happyReduce_6
action_17 (69) = happyReduce_6
action_17 (70) = happyReduce_6
action_17 (71) = happyReduce_6
action_17 (87) = happyShift action_69
action_17 (90) = happyReduce_6
action_17 (92) = happyReduce_6
action_17 (94) = happyReduce_6
action_17 (95) = happyReduce_6
action_17 (96) = happyReduce_6
action_17 (97) = happyReduce_6
action_17 (99) = happyReduce_6
action_17 (100) = happyReduce_6
action_17 (113) = happyReduce_6
action_17 (114) = happyReduce_6
action_17 (115) = happyReduce_6
action_17 (116) = happyReduce_6
action_17 (117) = happyReduce_6
action_17 (118) = happyReduce_6
action_17 (120) = happyReduce_6
action_17 (121) = happyReduce_6
action_17 (127) = happyReduce_6
action_17 (128) = happyReduce_6
action_17 (130) = happyReduce_6
action_17 (133) = happyReduce_6
action_17 _ = happyReduce_6

action_18 (65) = happyShift action_4
action_18 (66) = happyShift action_27
action_18 (67) = happyShift action_28
action_18 (68) = happyShift action_29
action_18 (69) = happyShift action_30
action_18 (70) = happyShift action_68
action_18 (71) = happyReduce_8
action_18 (87) = happyReduce_8
action_18 (90) = happyReduce_8
action_18 (92) = happyReduce_8
action_18 (94) = happyShift action_31
action_18 (95) = happyReduce_8
action_18 (96) = happyReduce_8
action_18 (97) = happyShift action_32
action_18 (99) = happyReduce_8
action_18 (100) = happyReduce_8
action_18 (113) = happyShift action_33
action_18 (114) = happyReduce_8
action_18 (115) = happyShift action_34
action_18 (116) = happyReduce_8
action_18 (117) = happyShift action_35
action_18 (118) = happyReduce_8
action_18 (120) = happyReduce_8
action_18 (121) = happyReduce_8
action_18 (127) = happyReduce_8
action_18 (128) = happyShift action_36
action_18 (130) = happyShift action_37
action_18 (133) = happyReduce_8
action_18 (6) = happyGoto action_13
action_18 (7) = happyGoto action_14
action_18 (13) = happyGoto action_67
action_18 (14) = happyGoto action_20
action_18 (15) = happyGoto action_21
action_18 (18) = happyGoto action_22
action_18 (19) = happyGoto action_23
action_18 (20) = happyGoto action_24
action_18 (22) = happyGoto action_25
action_18 (23) = happyGoto action_26
action_18 _ = happyReduce_8

action_19 (65) = happyReduce_12
action_19 (66) = happyReduce_12
action_19 (67) = happyReduce_12
action_19 (68) = happyReduce_12
action_19 (69) = happyReduce_12
action_19 (70) = happyReduce_12
action_19 (71) = happyReduce_12
action_19 (87) = happyReduce_12
action_19 (90) = happyReduce_12
action_19 (92) = happyReduce_12
action_19 (94) = happyReduce_12
action_19 (95) = happyReduce_12
action_19 (96) = happyReduce_12
action_19 (97) = happyReduce_12
action_19 (99) = happyReduce_12
action_19 (100) = happyReduce_12
action_19 (113) = happyReduce_12
action_19 (114) = happyReduce_12
action_19 (115) = happyReduce_12
action_19 (116) = happyReduce_12
action_19 (117) = happyReduce_12
action_19 (118) = happyReduce_12
action_19 (120) = happyReduce_12
action_19 (121) = happyReduce_12
action_19 (127) = happyReduce_12
action_19 (128) = happyShift action_66
action_19 (130) = happyReduce_12
action_19 (133) = happyReduce_12
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

action_31 (65) = happyShift action_4
action_31 (66) = happyShift action_27
action_31 (67) = happyShift action_28
action_31 (68) = happyShift action_29
action_31 (69) = happyShift action_30
action_31 (94) = happyShift action_31
action_31 (97) = happyShift action_32
action_31 (113) = happyShift action_33
action_31 (115) = happyShift action_34
action_31 (117) = happyShift action_35
action_31 (128) = happyShift action_36
action_31 (130) = happyShift action_37
action_31 (6) = happyGoto action_13
action_31 (7) = happyGoto action_14
action_31 (8) = happyGoto action_65
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

action_32 (65) = happyShift action_4
action_32 (66) = happyShift action_27
action_32 (67) = happyShift action_28
action_32 (68) = happyShift action_29
action_32 (69) = happyShift action_30
action_32 (94) = happyShift action_31
action_32 (97) = happyShift action_32
action_32 (113) = happyShift action_33
action_32 (115) = happyShift action_34
action_32 (117) = happyShift action_35
action_32 (128) = happyShift action_36
action_32 (130) = happyShift action_37
action_32 (6) = happyGoto action_13
action_32 (7) = happyGoto action_14
action_32 (8) = happyGoto action_64
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

action_33 (65) = happyShift action_4
action_33 (66) = happyShift action_27
action_33 (67) = happyShift action_28
action_33 (68) = happyShift action_29
action_33 (69) = happyShift action_30
action_33 (94) = happyShift action_31
action_33 (97) = happyShift action_32
action_33 (113) = happyShift action_33
action_33 (115) = happyShift action_34
action_33 (117) = happyShift action_35
action_33 (128) = happyShift action_36
action_33 (130) = happyShift action_37
action_33 (6) = happyGoto action_13
action_33 (7) = happyGoto action_14
action_33 (8) = happyGoto action_62
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
action_33 (57) = happyGoto action_63
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (65) = happyShift action_4
action_34 (66) = happyShift action_27
action_34 (67) = happyShift action_28
action_34 (68) = happyShift action_29
action_34 (69) = happyShift action_30
action_34 (94) = happyShift action_31
action_34 (97) = happyShift action_32
action_34 (113) = happyShift action_33
action_34 (115) = happyShift action_34
action_34 (117) = happyShift action_35
action_34 (128) = happyShift action_36
action_34 (130) = happyShift action_37
action_34 (6) = happyGoto action_13
action_34 (7) = happyGoto action_14
action_34 (8) = happyGoto action_59
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
action_34 (57) = happyGoto action_60
action_34 (61) = happyGoto action_61
action_34 _ = happyReduce_132

action_35 (65) = happyShift action_4
action_35 (66) = happyShift action_27
action_35 (67) = happyShift action_28
action_35 (68) = happyShift action_29
action_35 (69) = happyShift action_30
action_35 (94) = happyShift action_31
action_35 (97) = happyShift action_32
action_35 (98) = happyShift action_57
action_35 (113) = happyShift action_58
action_35 (115) = happyShift action_34
action_35 (117) = happyShift action_35
action_35 (128) = happyShift action_36
action_35 (130) = happyShift action_37
action_35 (6) = happyGoto action_47
action_35 (7) = happyGoto action_48
action_35 (8) = happyGoto action_49
action_35 (9) = happyGoto action_16
action_35 (10) = happyGoto action_17
action_35 (12) = happyGoto action_18
action_35 (13) = happyGoto action_19
action_35 (14) = happyGoto action_20
action_35 (15) = happyGoto action_21
action_35 (16) = happyGoto action_50
action_35 (17) = happyGoto action_51
action_35 (18) = happyGoto action_22
action_35 (19) = happyGoto action_52
action_35 (20) = happyGoto action_24
action_35 (21) = happyGoto action_53
action_35 (22) = happyGoto action_25
action_35 (23) = happyGoto action_26
action_35 (24) = happyGoto action_54
action_35 (25) = happyGoto action_42
action_35 (58) = happyGoto action_55
action_35 (59) = happyGoto action_56
action_35 _ = happyReduce_39

action_36 (65) = happyShift action_4
action_36 (6) = happyGoto action_46
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (65) = happyShift action_4
action_37 (66) = happyShift action_27
action_37 (67) = happyShift action_28
action_37 (68) = happyShift action_29
action_37 (69) = happyShift action_30
action_37 (113) = happyShift action_45
action_37 (6) = happyGoto action_38
action_37 (7) = happyGoto action_39
action_37 (19) = happyGoto action_40
action_37 (24) = happyGoto action_41
action_37 (25) = happyGoto action_42
action_37 (56) = happyGoto action_43
action_37 (64) = happyGoto action_44
action_37 _ = happyReduce_122

action_38 (65) = happyReduce_46
action_38 (66) = happyReduce_46
action_38 (67) = happyReduce_46
action_38 (68) = happyReduce_46
action_38 (69) = happyReduce_46
action_38 (113) = happyReduce_46
action_38 (114) = happyReduce_46
action_38 (119) = happyShift action_104
action_38 (121) = happyReduce_46
action_38 (122) = happyReduce_46
action_38 (123) = happyReduce_46
action_38 (127) = happyReduce_46
action_38 _ = happyReduce_46

action_39 (65) = happyReduce_47
action_39 (66) = happyReduce_47
action_39 (67) = happyReduce_47
action_39 (68) = happyReduce_47
action_39 (69) = happyReduce_47
action_39 (113) = happyReduce_47
action_39 (114) = happyReduce_47
action_39 (121) = happyReduce_47
action_39 (122) = happyReduce_47
action_39 (123) = happyReduce_47
action_39 (127) = happyReduce_47
action_39 _ = happyReduce_47

action_40 (65) = happyReduce_48
action_40 (66) = happyReduce_48
action_40 (67) = happyReduce_48
action_40 (68) = happyReduce_48
action_40 (69) = happyReduce_48
action_40 (113) = happyReduce_48
action_40 (114) = happyReduce_48
action_40 (121) = happyReduce_48
action_40 (122) = happyReduce_48
action_40 (123) = happyReduce_48
action_40 (127) = happyReduce_48
action_40 _ = happyReduce_48

action_41 _ = happyReduce_138

action_42 _ = happyReduce_44

action_43 (122) = happyShift action_103
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (65) = happyShift action_4
action_44 (66) = happyShift action_27
action_44 (67) = happyShift action_28
action_44 (68) = happyShift action_29
action_44 (69) = happyShift action_30
action_44 (113) = happyShift action_45
action_44 (6) = happyGoto action_38
action_44 (7) = happyGoto action_39
action_44 (19) = happyGoto action_40
action_44 (24) = happyGoto action_102
action_44 (25) = happyGoto action_42
action_44 _ = happyReduce_123

action_45 (65) = happyShift action_4
action_45 (66) = happyShift action_27
action_45 (67) = happyShift action_28
action_45 (68) = happyShift action_29
action_45 (69) = happyShift action_30
action_45 (113) = happyShift action_45
action_45 (6) = happyGoto action_38
action_45 (7) = happyGoto action_39
action_45 (19) = happyGoto action_40
action_45 (24) = happyGoto action_92
action_45 (25) = happyGoto action_42
action_45 (58) = happyGoto action_93
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_21

action_47 (119) = happyShift action_101
action_47 (121) = happyReduce_46
action_47 (123) = happyReduce_46
action_47 _ = happyReduce_29

action_48 (123) = happyReduce_47
action_48 (127) = happyReduce_47
action_48 _ = happyReduce_28

action_49 _ = happyReduce_26

action_50 (118) = happyShift action_100
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (118) = happyReduce_128
action_51 (120) = happyReduce_128
action_51 _ = happyReduce_128

action_52 (123) = happyReduce_48
action_52 (127) = happyReduce_48
action_52 _ = happyReduce_30

action_53 (118) = happyShift action_99
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (121) = happyReduce_126
action_54 (123) = happyReduce_126
action_54 _ = happyReduce_126

action_55 (121) = happyShift action_97
action_55 (123) = happyShift action_98
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (118) = happyReduce_134
action_56 (120) = happyShift action_96
action_56 (62) = happyGoto action_95
action_56 _ = happyReduce_134

action_57 (65) = happyShift action_4
action_57 (66) = happyShift action_27
action_57 (67) = happyShift action_28
action_57 (68) = happyShift action_29
action_57 (69) = happyShift action_30
action_57 (94) = happyShift action_31
action_57 (97) = happyShift action_32
action_57 (113) = happyShift action_33
action_57 (115) = happyShift action_34
action_57 (117) = happyShift action_35
action_57 (128) = happyShift action_36
action_57 (130) = happyShift action_37
action_57 (6) = happyGoto action_13
action_57 (7) = happyGoto action_14
action_57 (8) = happyGoto action_94
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

action_58 (65) = happyShift action_4
action_58 (66) = happyShift action_27
action_58 (67) = happyShift action_28
action_58 (68) = happyShift action_29
action_58 (69) = happyShift action_30
action_58 (94) = happyShift action_31
action_58 (97) = happyShift action_32
action_58 (113) = happyShift action_58
action_58 (115) = happyShift action_34
action_58 (117) = happyShift action_35
action_58 (128) = happyShift action_36
action_58 (130) = happyShift action_37
action_58 (6) = happyGoto action_91
action_58 (7) = happyGoto action_48
action_58 (8) = happyGoto action_62
action_58 (9) = happyGoto action_16
action_58 (10) = happyGoto action_17
action_58 (12) = happyGoto action_18
action_58 (13) = happyGoto action_19
action_58 (14) = happyGoto action_20
action_58 (15) = happyGoto action_21
action_58 (18) = happyGoto action_22
action_58 (19) = happyGoto action_52
action_58 (20) = happyGoto action_24
action_58 (22) = happyGoto action_25
action_58 (23) = happyGoto action_26
action_58 (24) = happyGoto action_92
action_58 (25) = happyGoto action_42
action_58 (57) = happyGoto action_63
action_58 (58) = happyGoto action_93
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (116) = happyReduce_124
action_59 (121) = happyReduce_124
action_59 _ = happyReduce_124

action_60 (116) = happyReduce_133
action_60 (121) = happyShift action_88
action_60 _ = happyReduce_133

action_61 (116) = happyShift action_90
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (114) = happyShift action_89
action_62 (121) = happyReduce_124
action_62 _ = happyReduce_124

action_63 (114) = happyShift action_87
action_63 (121) = happyShift action_88
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (127) = happyShift action_86
action_64 (28) = happyGoto action_85
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (95) = happyShift action_84
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (65) = happyShift action_4
action_66 (6) = happyGoto action_83
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (65) = happyReduce_13
action_67 (66) = happyReduce_13
action_67 (67) = happyReduce_13
action_67 (68) = happyReduce_13
action_67 (69) = happyReduce_13
action_67 (70) = happyReduce_13
action_67 (71) = happyReduce_13
action_67 (87) = happyReduce_13
action_67 (90) = happyReduce_13
action_67 (92) = happyReduce_13
action_67 (94) = happyReduce_13
action_67 (95) = happyReduce_13
action_67 (96) = happyReduce_13
action_67 (97) = happyReduce_13
action_67 (99) = happyReduce_13
action_67 (100) = happyReduce_13
action_67 (113) = happyReduce_13
action_67 (114) = happyReduce_13
action_67 (115) = happyReduce_13
action_67 (116) = happyReduce_13
action_67 (117) = happyReduce_13
action_67 (118) = happyReduce_13
action_67 (120) = happyReduce_13
action_67 (121) = happyReduce_13
action_67 (127) = happyReduce_13
action_67 (128) = happyShift action_66
action_67 (130) = happyReduce_13
action_67 (133) = happyReduce_13
action_67 _ = happyReduce_13

action_68 _ = happyReduce_14

action_69 (65) = happyShift action_4
action_69 (66) = happyShift action_27
action_69 (67) = happyShift action_28
action_69 (68) = happyShift action_29
action_69 (69) = happyShift action_30
action_69 (94) = happyShift action_31
action_69 (97) = happyShift action_32
action_69 (113) = happyShift action_33
action_69 (115) = happyShift action_34
action_69 (117) = happyShift action_35
action_69 (128) = happyShift action_36
action_69 (130) = happyShift action_37
action_69 (6) = happyGoto action_13
action_69 (7) = happyGoto action_14
action_69 (11) = happyGoto action_81
action_69 (12) = happyGoto action_82
action_69 (13) = happyGoto action_19
action_69 (14) = happyGoto action_20
action_69 (15) = happyGoto action_21
action_69 (18) = happyGoto action_22
action_69 (19) = happyGoto action_23
action_69 (20) = happyGoto action_24
action_69 (22) = happyGoto action_25
action_69 (23) = happyGoto action_26
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (65) = happyShift action_4
action_70 (66) = happyShift action_27
action_70 (67) = happyShift action_28
action_70 (68) = happyShift action_29
action_70 (69) = happyShift action_30
action_70 (94) = happyShift action_31
action_70 (97) = happyShift action_32
action_70 (113) = happyShift action_33
action_70 (115) = happyShift action_34
action_70 (117) = happyShift action_35
action_70 (128) = happyShift action_36
action_70 (130) = happyShift action_37
action_70 (6) = happyGoto action_13
action_70 (7) = happyGoto action_14
action_70 (10) = happyGoto action_80
action_70 (12) = happyGoto action_18
action_70 (13) = happyGoto action_19
action_70 (14) = happyGoto action_20
action_70 (15) = happyGoto action_21
action_70 (18) = happyGoto action_22
action_70 (19) = happyGoto action_23
action_70 (20) = happyGoto action_24
action_70 (22) = happyGoto action_25
action_70 (23) = happyGoto action_26
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (129) = happyShift action_78
action_71 (46) = happyGoto action_79
action_71 _ = happyReduce_102

action_72 (129) = happyShift action_78
action_72 (46) = happyGoto action_77
action_72 _ = happyReduce_102

action_73 (119) = happyShift action_76
action_73 (45) = happyGoto action_75
action_73 _ = happyReduce_98

action_74 _ = happyReduce_115

action_75 (129) = happyShift action_78
action_75 (46) = happyGoto action_146
action_75 _ = happyReduce_102

action_76 (65) = happyShift action_4
action_76 (67) = happyShift action_138
action_76 (68) = happyShift action_139
action_76 (69) = happyShift action_140
action_76 (109) = happyShift action_141
action_76 (113) = happyShift action_142
action_76 (117) = happyShift action_143
action_76 (127) = happyShift action_144
action_76 (130) = happyShift action_145
action_76 (6) = happyGoto action_129
action_76 (29) = happyGoto action_130
action_76 (30) = happyGoto action_131
action_76 (31) = happyGoto action_132
action_76 (32) = happyGoto action_133
action_76 (33) = happyGoto action_134
action_76 (34) = happyGoto action_135
action_76 (35) = happyGoto action_136
action_76 (36) = happyGoto action_137
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (126) = happyShift action_128
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (65) = happyShift action_4
action_78 (113) = happyShift action_127
action_78 (6) = happyGoto action_125
action_78 (47) = happyGoto action_126
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (126) = happyShift action_124
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (65) = happyReduce_7
action_80 (66) = happyReduce_7
action_80 (67) = happyReduce_7
action_80 (68) = happyReduce_7
action_80 (69) = happyReduce_7
action_80 (70) = happyReduce_7
action_80 (71) = happyReduce_7
action_80 (87) = happyShift action_69
action_80 (90) = happyReduce_7
action_80 (92) = happyReduce_7
action_80 (94) = happyReduce_7
action_80 (95) = happyReduce_7
action_80 (96) = happyReduce_7
action_80 (97) = happyReduce_7
action_80 (99) = happyReduce_7
action_80 (100) = happyReduce_7
action_80 (113) = happyReduce_7
action_80 (114) = happyReduce_7
action_80 (115) = happyReduce_7
action_80 (116) = happyReduce_7
action_80 (117) = happyReduce_7
action_80 (118) = happyReduce_7
action_80 (120) = happyReduce_7
action_80 (121) = happyReduce_7
action_80 (127) = happyReduce_7
action_80 (128) = happyReduce_7
action_80 (130) = happyReduce_7
action_80 (133) = happyReduce_7
action_80 _ = happyReduce_7

action_81 (71) = happyShift action_122
action_81 (87) = happyShift action_123
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (65) = happyShift action_4
action_82 (66) = happyShift action_27
action_82 (67) = happyShift action_28
action_82 (68) = happyShift action_29
action_82 (69) = happyShift action_30
action_82 (70) = happyShift action_68
action_82 (94) = happyShift action_31
action_82 (97) = happyShift action_32
action_82 (113) = happyShift action_33
action_82 (115) = happyShift action_34
action_82 (117) = happyShift action_35
action_82 (128) = happyShift action_36
action_82 (130) = happyShift action_37
action_82 (6) = happyGoto action_13
action_82 (7) = happyGoto action_14
action_82 (13) = happyGoto action_67
action_82 (14) = happyGoto action_20
action_82 (15) = happyGoto action_21
action_82 (18) = happyGoto action_22
action_82 (19) = happyGoto action_23
action_82 (20) = happyGoto action_24
action_82 (22) = happyGoto action_25
action_82 (23) = happyGoto action_26
action_82 _ = happyReduce_10

action_83 (65) = happyReduce_16
action_83 (66) = happyReduce_16
action_83 (67) = happyReduce_16
action_83 (68) = happyReduce_16
action_83 (69) = happyReduce_16
action_83 (70) = happyReduce_16
action_83 (71) = happyReduce_16
action_83 (87) = happyReduce_16
action_83 (90) = happyReduce_16
action_83 (92) = happyReduce_16
action_83 (94) = happyReduce_16
action_83 (95) = happyReduce_16
action_83 (96) = happyReduce_16
action_83 (97) = happyReduce_16
action_83 (99) = happyReduce_16
action_83 (100) = happyReduce_16
action_83 (113) = happyReduce_16
action_83 (114) = happyReduce_16
action_83 (115) = happyReduce_16
action_83 (116) = happyReduce_16
action_83 (117) = happyReduce_16
action_83 (118) = happyReduce_16
action_83 (120) = happyReduce_16
action_83 (121) = happyReduce_16
action_83 (127) = happyReduce_16
action_83 (128) = happyReduce_16
action_83 (130) = happyReduce_16
action_83 (133) = happyReduce_16
action_83 _ = happyReduce_16

action_84 (65) = happyShift action_4
action_84 (66) = happyShift action_27
action_84 (67) = happyShift action_28
action_84 (68) = happyShift action_29
action_84 (69) = happyShift action_30
action_84 (94) = happyShift action_31
action_84 (97) = happyShift action_32
action_84 (113) = happyShift action_33
action_84 (115) = happyShift action_34
action_84 (117) = happyShift action_35
action_84 (128) = happyShift action_36
action_84 (130) = happyShift action_37
action_84 (6) = happyGoto action_13
action_84 (7) = happyGoto action_14
action_84 (8) = happyGoto action_121
action_84 (9) = happyGoto action_16
action_84 (10) = happyGoto action_17
action_84 (12) = happyGoto action_18
action_84 (13) = happyGoto action_19
action_84 (14) = happyGoto action_20
action_84 (15) = happyGoto action_21
action_84 (18) = happyGoto action_22
action_84 (19) = happyGoto action_23
action_84 (20) = happyGoto action_24
action_84 (22) = happyGoto action_25
action_84 (23) = happyGoto action_26
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (65) = happyReduce_19
action_85 (66) = happyReduce_19
action_85 (67) = happyReduce_19
action_85 (68) = happyReduce_19
action_85 (69) = happyReduce_19
action_85 (70) = happyReduce_19
action_85 (71) = happyReduce_19
action_85 (87) = happyReduce_19
action_85 (90) = happyReduce_19
action_85 (92) = happyReduce_19
action_85 (94) = happyReduce_19
action_85 (95) = happyReduce_19
action_85 (96) = happyReduce_19
action_85 (97) = happyReduce_19
action_85 (99) = happyReduce_19
action_85 (100) = happyReduce_19
action_85 (113) = happyReduce_19
action_85 (114) = happyReduce_19
action_85 (115) = happyReduce_19
action_85 (116) = happyReduce_19
action_85 (117) = happyReduce_19
action_85 (118) = happyReduce_19
action_85 (120) = happyReduce_19
action_85 (121) = happyReduce_19
action_85 (127) = happyShift action_120
action_85 (128) = happyReduce_19
action_85 (130) = happyReduce_19
action_85 (133) = happyReduce_19
action_85 _ = happyReduce_19

action_86 (65) = happyShift action_4
action_86 (66) = happyShift action_27
action_86 (67) = happyShift action_28
action_86 (68) = happyShift action_29
action_86 (69) = happyShift action_30
action_86 (113) = happyShift action_45
action_86 (6) = happyGoto action_38
action_86 (7) = happyGoto action_39
action_86 (19) = happyGoto action_40
action_86 (24) = happyGoto action_119
action_86 (25) = happyGoto action_42
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_43

action_88 (65) = happyShift action_4
action_88 (66) = happyShift action_27
action_88 (67) = happyShift action_28
action_88 (68) = happyShift action_29
action_88 (69) = happyShift action_30
action_88 (94) = happyShift action_31
action_88 (97) = happyShift action_32
action_88 (113) = happyShift action_33
action_88 (115) = happyShift action_34
action_88 (117) = happyShift action_35
action_88 (128) = happyShift action_36
action_88 (130) = happyShift action_37
action_88 (6) = happyGoto action_13
action_88 (7) = happyGoto action_14
action_88 (8) = happyGoto action_118
action_88 (9) = happyGoto action_16
action_88 (10) = happyGoto action_17
action_88 (12) = happyGoto action_18
action_88 (13) = happyGoto action_19
action_88 (14) = happyGoto action_20
action_88 (15) = happyGoto action_21
action_88 (18) = happyGoto action_22
action_88 (19) = happyGoto action_23
action_88 (20) = happyGoto action_24
action_88 (22) = happyGoto action_25
action_88 (23) = happyGoto action_26
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_34

action_90 _ = happyReduce_42

action_91 (119) = happyShift action_104
action_91 (123) = happyReduce_46
action_91 (127) = happyReduce_46
action_91 _ = happyReduce_29

action_92 (114) = happyShift action_117
action_92 (121) = happyReduce_126
action_92 (127) = happyReduce_126
action_92 _ = happyReduce_126

action_93 (114) = happyReduce_136
action_93 (121) = happyShift action_97
action_93 (127) = happyShift action_116
action_93 (27) = happyGoto action_114
action_93 (63) = happyGoto action_115
action_93 _ = happyReduce_136

action_94 _ = happyReduce_27

action_95 _ = happyReduce_24

action_96 (65) = happyShift action_4
action_96 (66) = happyShift action_27
action_96 (67) = happyShift action_28
action_96 (68) = happyShift action_29
action_96 (69) = happyShift action_30
action_96 (94) = happyShift action_31
action_96 (97) = happyShift action_32
action_96 (98) = happyShift action_57
action_96 (113) = happyShift action_58
action_96 (115) = happyShift action_34
action_96 (117) = happyShift action_35
action_96 (128) = happyShift action_36
action_96 (130) = happyShift action_37
action_96 (6) = happyGoto action_91
action_96 (7) = happyGoto action_48
action_96 (8) = happyGoto action_49
action_96 (9) = happyGoto action_16
action_96 (10) = happyGoto action_17
action_96 (12) = happyGoto action_18
action_96 (13) = happyGoto action_19
action_96 (14) = happyGoto action_20
action_96 (15) = happyGoto action_21
action_96 (17) = happyGoto action_113
action_96 (18) = happyGoto action_22
action_96 (19) = happyGoto action_52
action_96 (20) = happyGoto action_24
action_96 (22) = happyGoto action_25
action_96 (23) = happyGoto action_26
action_96 (24) = happyGoto action_54
action_96 (25) = happyGoto action_42
action_96 (58) = happyGoto action_55
action_96 _ = happyReduce_135

action_97 (65) = happyShift action_4
action_97 (66) = happyShift action_27
action_97 (67) = happyShift action_28
action_97 (68) = happyShift action_29
action_97 (69) = happyShift action_30
action_97 (113) = happyShift action_45
action_97 (6) = happyGoto action_38
action_97 (7) = happyGoto action_39
action_97 (19) = happyGoto action_40
action_97 (24) = happyGoto action_112
action_97 (25) = happyGoto action_42
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (65) = happyShift action_4
action_98 (66) = happyShift action_27
action_98 (67) = happyShift action_28
action_98 (68) = happyShift action_29
action_98 (69) = happyShift action_30
action_98 (94) = happyShift action_31
action_98 (97) = happyShift action_32
action_98 (113) = happyShift action_33
action_98 (115) = happyShift action_34
action_98 (117) = happyShift action_35
action_98 (128) = happyShift action_36
action_98 (130) = happyShift action_37
action_98 (6) = happyGoto action_13
action_98 (7) = happyGoto action_14
action_98 (8) = happyGoto action_111
action_98 (9) = happyGoto action_16
action_98 (10) = happyGoto action_17
action_98 (12) = happyGoto action_18
action_98 (13) = happyGoto action_19
action_98 (14) = happyGoto action_20
action_98 (15) = happyGoto action_21
action_98 (18) = happyGoto action_22
action_98 (19) = happyGoto action_23
action_98 (20) = happyGoto action_24
action_98 (22) = happyGoto action_25
action_98 (23) = happyGoto action_26
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_38

action_100 _ = happyReduce_23

action_101 (65) = happyShift action_4
action_101 (66) = happyShift action_27
action_101 (67) = happyShift action_28
action_101 (68) = happyShift action_29
action_101 (69) = happyShift action_30
action_101 (94) = happyShift action_31
action_101 (97) = happyShift action_32
action_101 (113) = happyShift action_58
action_101 (115) = happyShift action_34
action_101 (117) = happyShift action_35
action_101 (128) = happyShift action_36
action_101 (130) = happyShift action_37
action_101 (6) = happyGoto action_109
action_101 (7) = happyGoto action_48
action_101 (8) = happyGoto action_110
action_101 (9) = happyGoto action_16
action_101 (10) = happyGoto action_17
action_101 (12) = happyGoto action_18
action_101 (13) = happyGoto action_19
action_101 (14) = happyGoto action_20
action_101 (15) = happyGoto action_21
action_101 (18) = happyGoto action_22
action_101 (19) = happyGoto action_52
action_101 (20) = happyGoto action_24
action_101 (22) = happyGoto action_25
action_101 (23) = happyGoto action_26
action_101 (25) = happyGoto action_106
action_101 (54) = happyGoto action_107
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_139

action_103 (65) = happyShift action_4
action_103 (66) = happyShift action_27
action_103 (67) = happyShift action_28
action_103 (68) = happyShift action_29
action_103 (69) = happyShift action_30
action_103 (94) = happyShift action_31
action_103 (97) = happyShift action_32
action_103 (113) = happyShift action_33
action_103 (115) = happyShift action_34
action_103 (117) = happyShift action_35
action_103 (128) = happyShift action_36
action_103 (130) = happyShift action_37
action_103 (6) = happyGoto action_13
action_103 (7) = happyGoto action_14
action_103 (8) = happyGoto action_108
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

action_104 (65) = happyShift action_4
action_104 (66) = happyShift action_27
action_104 (67) = happyShift action_28
action_104 (68) = happyShift action_29
action_104 (69) = happyShift action_30
action_104 (113) = happyShift action_45
action_104 (6) = happyGoto action_105
action_104 (7) = happyGoto action_39
action_104 (19) = happyGoto action_40
action_104 (25) = happyGoto action_106
action_104 (54) = happyGoto action_107
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (65) = happyReduce_46
action_105 (66) = happyReduce_46
action_105 (67) = happyReduce_46
action_105 (68) = happyReduce_46
action_105 (69) = happyReduce_46
action_105 (113) = happyReduce_46
action_105 (114) = happyReduce_46
action_105 (121) = happyReduce_46
action_105 (122) = happyReduce_46
action_105 (123) = happyReduce_46
action_105 (127) = happyReduce_46
action_105 _ = happyReduce_46

action_106 _ = happyReduce_118

action_107 (65) = happyShift action_4
action_107 (66) = happyShift action_27
action_107 (67) = happyShift action_28
action_107 (68) = happyShift action_29
action_107 (69) = happyShift action_30
action_107 (113) = happyShift action_45
action_107 (114) = happyReduce_45
action_107 (121) = happyReduce_45
action_107 (122) = happyReduce_45
action_107 (123) = happyReduce_45
action_107 (127) = happyReduce_45
action_107 (6) = happyGoto action_105
action_107 (7) = happyGoto action_39
action_107 (19) = happyGoto action_40
action_107 (25) = happyGoto action_177
action_107 _ = happyReduce_45

action_108 _ = happyReduce_20

action_109 (121) = happyReduce_46
action_109 (123) = happyReduce_46
action_109 _ = happyReduce_29

action_110 (120) = happyShift action_176
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (118) = happyReduce_25
action_111 (120) = happyReduce_25
action_111 _ = happyReduce_25

action_112 _ = happyReduce_127

action_113 _ = happyReduce_129

action_114 _ = happyReduce_137

action_115 (114) = happyShift action_175
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (65) = happyShift action_4
action_116 (6) = happyGoto action_174
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_50

action_118 _ = happyReduce_125

action_119 (122) = happyShift action_173
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (65) = happyShift action_4
action_120 (66) = happyShift action_27
action_120 (67) = happyShift action_28
action_120 (68) = happyShift action_29
action_120 (69) = happyShift action_30
action_120 (113) = happyShift action_45
action_120 (6) = happyGoto action_38
action_120 (7) = happyGoto action_39
action_120 (19) = happyGoto action_40
action_120 (24) = happyGoto action_172
action_120 (25) = happyGoto action_42
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (96) = happyShift action_171
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (65) = happyShift action_4
action_122 (66) = happyShift action_27
action_122 (67) = happyShift action_28
action_122 (68) = happyShift action_29
action_122 (69) = happyShift action_30
action_122 (94) = happyShift action_31
action_122 (97) = happyShift action_32
action_122 (113) = happyShift action_33
action_122 (115) = happyShift action_34
action_122 (117) = happyShift action_35
action_122 (128) = happyShift action_36
action_122 (130) = happyShift action_37
action_122 (6) = happyGoto action_13
action_122 (7) = happyGoto action_14
action_122 (12) = happyGoto action_170
action_122 (13) = happyGoto action_19
action_122 (14) = happyGoto action_20
action_122 (15) = happyGoto action_21
action_122 (18) = happyGoto action_22
action_122 (19) = happyGoto action_23
action_122 (20) = happyGoto action_24
action_122 (22) = happyGoto action_25
action_122 (23) = happyGoto action_26
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (65) = happyShift action_4
action_123 (66) = happyShift action_27
action_123 (67) = happyShift action_28
action_123 (68) = happyShift action_29
action_123 (69) = happyShift action_30
action_123 (94) = happyShift action_31
action_123 (97) = happyShift action_32
action_123 (113) = happyShift action_33
action_123 (115) = happyShift action_34
action_123 (117) = happyShift action_35
action_123 (128) = happyShift action_36
action_123 (130) = happyShift action_37
action_123 (6) = happyGoto action_13
action_123 (7) = happyGoto action_14
action_123 (12) = happyGoto action_169
action_123 (13) = happyGoto action_19
action_123 (14) = happyGoto action_20
action_123 (15) = happyGoto action_21
action_123 (18) = happyGoto action_22
action_123 (19) = happyGoto action_23
action_123 (20) = happyGoto action_24
action_123 (22) = happyGoto action_25
action_123 (23) = happyGoto action_26
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (65) = happyShift action_4
action_124 (67) = happyShift action_138
action_124 (68) = happyShift action_139
action_124 (69) = happyShift action_140
action_124 (113) = happyShift action_142
action_124 (117) = happyShift action_143
action_124 (127) = happyShift action_144
action_124 (130) = happyShift action_145
action_124 (6) = happyGoto action_129
action_124 (29) = happyGoto action_168
action_124 (30) = happyGoto action_131
action_124 (31) = happyGoto action_132
action_124 (32) = happyGoto action_133
action_124 (33) = happyGoto action_134
action_124 (34) = happyGoto action_135
action_124 (35) = happyGoto action_136
action_124 (36) = happyGoto action_137
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_106

action_126 (122) = happyShift action_167
action_126 _ = happyReduce_103

action_127 (65) = happyShift action_4
action_127 (113) = happyShift action_127
action_127 (6) = happyGoto action_125
action_127 (47) = happyGoto action_166
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (65) = happyShift action_4
action_128 (6) = happyGoto action_163
action_128 (48) = happyGoto action_164
action_128 (49) = happyGoto action_165
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (119) = happyShift action_162
action_129 _ = happyReduce_74

action_130 (92) = happyShift action_161
action_130 _ = happyReduce_99

action_131 _ = happyReduce_57

action_132 (65) = happyReduce_58
action_132 (67) = happyReduce_58
action_132 (68) = happyReduce_58
action_132 (69) = happyReduce_58
action_132 (70) = happyReduce_58
action_132 (90) = happyReduce_58
action_132 (92) = happyReduce_58
action_132 (99) = happyReduce_58
action_132 (100) = happyReduce_58
action_132 (113) = happyReduce_58
action_132 (114) = happyReduce_58
action_132 (117) = happyReduce_58
action_132 (118) = happyReduce_58
action_132 (121) = happyReduce_58
action_132 (122) = happyReduce_58
action_132 (126) = happyReduce_58
action_132 (127) = happyShift action_160
action_132 (129) = happyReduce_58
action_132 (130) = happyReduce_58
action_132 (133) = happyReduce_58
action_132 _ = happyReduce_58

action_133 (65) = happyReduce_59
action_133 (67) = happyReduce_59
action_133 (68) = happyReduce_59
action_133 (69) = happyReduce_59
action_133 (70) = happyReduce_59
action_133 (90) = happyReduce_59
action_133 (92) = happyReduce_59
action_133 (99) = happyReduce_59
action_133 (100) = happyReduce_59
action_133 (113) = happyReduce_59
action_133 (114) = happyReduce_59
action_133 (117) = happyReduce_59
action_133 (118) = happyReduce_59
action_133 (121) = happyReduce_59
action_133 (122) = happyShift action_159
action_133 (126) = happyReduce_59
action_133 (127) = happyReduce_59
action_133 (129) = happyReduce_59
action_133 (130) = happyReduce_59
action_133 (133) = happyReduce_59
action_133 _ = happyReduce_59

action_134 (65) = happyShift action_4
action_134 (67) = happyShift action_138
action_134 (68) = happyShift action_139
action_134 (69) = happyShift action_140
action_134 (70) = happyShift action_158
action_134 (90) = happyReduce_63
action_134 (92) = happyReduce_63
action_134 (99) = happyReduce_63
action_134 (100) = happyReduce_63
action_134 (113) = happyShift action_142
action_134 (114) = happyReduce_63
action_134 (117) = happyShift action_143
action_134 (118) = happyReduce_63
action_134 (121) = happyReduce_63
action_134 (122) = happyReduce_63
action_134 (126) = happyReduce_63
action_134 (127) = happyReduce_63
action_134 (129) = happyReduce_63
action_134 (130) = happyShift action_145
action_134 (133) = happyReduce_63
action_134 (6) = happyGoto action_156
action_134 (34) = happyGoto action_157
action_134 (35) = happyGoto action_136
action_134 (36) = happyGoto action_137
action_134 _ = happyReduce_63

action_135 _ = happyReduce_65

action_136 _ = happyReduce_68

action_137 _ = happyReduce_70

action_138 _ = happyReduce_71

action_139 _ = happyReduce_73

action_140 _ = happyReduce_72

action_141 (65) = happyShift action_4
action_141 (6) = happyGoto action_155
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (65) = happyShift action_4
action_142 (67) = happyShift action_138
action_142 (68) = happyShift action_139
action_142 (69) = happyShift action_140
action_142 (113) = happyShift action_142
action_142 (117) = happyShift action_143
action_142 (127) = happyShift action_144
action_142 (130) = happyShift action_145
action_142 (6) = happyGoto action_129
action_142 (29) = happyGoto action_153
action_142 (30) = happyGoto action_131
action_142 (31) = happyGoto action_132
action_142 (32) = happyGoto action_133
action_142 (33) = happyGoto action_134
action_142 (34) = happyGoto action_135
action_142 (35) = happyGoto action_136
action_142 (36) = happyGoto action_137
action_142 (60) = happyGoto action_154
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (65) = happyShift action_4
action_143 (6) = happyGoto action_151
action_143 (37) = happyGoto action_152
action_143 _ = happyReduce_78

action_144 (65) = happyShift action_4
action_144 (67) = happyShift action_138
action_144 (68) = happyShift action_139
action_144 (69) = happyShift action_140
action_144 (113) = happyShift action_142
action_144 (117) = happyShift action_143
action_144 (127) = happyShift action_144
action_144 (130) = happyShift action_145
action_144 (6) = happyGoto action_129
action_144 (29) = happyGoto action_150
action_144 (30) = happyGoto action_131
action_144 (31) = happyGoto action_132
action_144 (32) = happyGoto action_133
action_144 (33) = happyGoto action_134
action_144 (34) = happyGoto action_135
action_144 (35) = happyGoto action_136
action_144 (36) = happyGoto action_137
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (65) = happyShift action_4
action_145 (6) = happyGoto action_148
action_145 (53) = happyGoto action_149
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (126) = happyShift action_147
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (65) = happyShift action_4
action_147 (66) = happyShift action_27
action_147 (67) = happyShift action_28
action_147 (68) = happyShift action_29
action_147 (69) = happyShift action_30
action_147 (94) = happyShift action_31
action_147 (97) = happyShift action_32
action_147 (113) = happyShift action_33
action_147 (115) = happyShift action_34
action_147 (117) = happyShift action_35
action_147 (128) = happyShift action_36
action_147 (130) = happyShift action_37
action_147 (6) = happyGoto action_13
action_147 (7) = happyGoto action_14
action_147 (8) = happyGoto action_203
action_147 (9) = happyGoto action_16
action_147 (10) = happyGoto action_17
action_147 (12) = happyGoto action_18
action_147 (13) = happyGoto action_19
action_147 (14) = happyGoto action_20
action_147 (15) = happyGoto action_21
action_147 (18) = happyGoto action_22
action_147 (19) = happyGoto action_23
action_147 (20) = happyGoto action_24
action_147 (22) = happyGoto action_25
action_147 (23) = happyGoto action_26
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_116

action_149 (65) = happyShift action_4
action_149 (124) = happyShift action_202
action_149 (6) = happyGoto action_201
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_61

action_151 (119) = happyShift action_200
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (118) = happyShift action_199
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (114) = happyShift action_198
action_153 (121) = happyReduce_130
action_153 _ = happyReduce_130

action_154 (114) = happyShift action_196
action_154 (121) = happyShift action_197
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (119) = happyShift action_195
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_74

action_157 _ = happyReduce_66

action_158 _ = happyReduce_67

action_159 (65) = happyShift action_4
action_159 (67) = happyShift action_138
action_159 (68) = happyShift action_139
action_159 (69) = happyShift action_140
action_159 (113) = happyShift action_142
action_159 (117) = happyShift action_143
action_159 (127) = happyShift action_144
action_159 (130) = happyShift action_145
action_159 (6) = happyGoto action_129
action_159 (29) = happyGoto action_194
action_159 (30) = happyGoto action_131
action_159 (31) = happyGoto action_132
action_159 (32) = happyGoto action_133
action_159 (33) = happyGoto action_134
action_159 (34) = happyGoto action_135
action_159 (35) = happyGoto action_136
action_159 (36) = happyGoto action_137
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (65) = happyShift action_4
action_160 (67) = happyShift action_138
action_160 (68) = happyShift action_139
action_160 (69) = happyShift action_140
action_160 (113) = happyShift action_142
action_160 (117) = happyShift action_143
action_160 (127) = happyShift action_144
action_160 (130) = happyShift action_145
action_160 (6) = happyGoto action_129
action_160 (29) = happyGoto action_193
action_160 (30) = happyGoto action_131
action_160 (31) = happyGoto action_132
action_160 (32) = happyGoto action_133
action_160 (33) = happyGoto action_134
action_160 (34) = happyGoto action_135
action_160 (35) = happyGoto action_136
action_160 (36) = happyGoto action_137
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (65) = happyShift action_4
action_161 (6) = happyGoto action_190
action_161 (43) = happyGoto action_191
action_161 (44) = happyGoto action_192
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (65) = happyShift action_4
action_162 (67) = happyShift action_138
action_162 (68) = happyShift action_139
action_162 (69) = happyShift action_140
action_162 (113) = happyShift action_142
action_162 (117) = happyShift action_143
action_162 (127) = happyShift action_144
action_162 (130) = happyShift action_145
action_162 (6) = happyGoto action_129
action_162 (29) = happyGoto action_189
action_162 (30) = happyGoto action_131
action_162 (31) = happyGoto action_132
action_162 (32) = happyGoto action_133
action_162 (33) = happyGoto action_134
action_162 (34) = happyGoto action_135
action_162 (35) = happyGoto action_136
action_162 (36) = happyGoto action_137
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (119) = happyShift action_188
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_108

action_165 (92) = happyShift action_186
action_165 (127) = happyShift action_187
action_165 _ = happyReduce_84

action_166 (114) = happyShift action_185
action_166 (122) = happyShift action_167
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (65) = happyShift action_4
action_167 (113) = happyShift action_127
action_167 (6) = happyGoto action_125
action_167 (47) = happyGoto action_184
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (92) = happyShift action_183
action_168 _ = happyReduce_86

action_169 (65) = happyShift action_4
action_169 (66) = happyShift action_27
action_169 (67) = happyShift action_28
action_169 (68) = happyShift action_29
action_169 (69) = happyShift action_30
action_169 (70) = happyShift action_68
action_169 (71) = happyReduce_9
action_169 (87) = happyReduce_9
action_169 (90) = happyReduce_9
action_169 (92) = happyReduce_9
action_169 (94) = happyShift action_31
action_169 (95) = happyReduce_9
action_169 (96) = happyReduce_9
action_169 (97) = happyShift action_32
action_169 (99) = happyReduce_9
action_169 (100) = happyReduce_9
action_169 (113) = happyShift action_33
action_169 (114) = happyReduce_9
action_169 (115) = happyShift action_34
action_169 (116) = happyReduce_9
action_169 (117) = happyShift action_35
action_169 (118) = happyReduce_9
action_169 (120) = happyReduce_9
action_169 (121) = happyReduce_9
action_169 (127) = happyReduce_9
action_169 (128) = happyShift action_36
action_169 (130) = happyShift action_37
action_169 (133) = happyReduce_9
action_169 (6) = happyGoto action_13
action_169 (7) = happyGoto action_14
action_169 (13) = happyGoto action_67
action_169 (14) = happyGoto action_20
action_169 (15) = happyGoto action_21
action_169 (18) = happyGoto action_22
action_169 (19) = happyGoto action_23
action_169 (20) = happyGoto action_24
action_169 (22) = happyGoto action_25
action_169 (23) = happyGoto action_26
action_169 _ = happyReduce_9

action_170 (65) = happyShift action_4
action_170 (66) = happyShift action_27
action_170 (67) = happyShift action_28
action_170 (68) = happyShift action_29
action_170 (69) = happyShift action_30
action_170 (70) = happyShift action_68
action_170 (94) = happyShift action_31
action_170 (97) = happyShift action_32
action_170 (113) = happyShift action_33
action_170 (115) = happyShift action_34
action_170 (117) = happyShift action_35
action_170 (128) = happyShift action_36
action_170 (130) = happyShift action_37
action_170 (6) = happyGoto action_13
action_170 (7) = happyGoto action_14
action_170 (13) = happyGoto action_67
action_170 (14) = happyGoto action_20
action_170 (15) = happyGoto action_21
action_170 (18) = happyGoto action_22
action_170 (19) = happyGoto action_23
action_170 (20) = happyGoto action_24
action_170 (22) = happyGoto action_25
action_170 (23) = happyGoto action_26
action_170 _ = happyReduce_11

action_171 (65) = happyShift action_4
action_171 (66) = happyShift action_27
action_171 (67) = happyShift action_28
action_171 (68) = happyShift action_29
action_171 (69) = happyShift action_30
action_171 (94) = happyShift action_31
action_171 (97) = happyShift action_32
action_171 (113) = happyShift action_33
action_171 (115) = happyShift action_34
action_171 (117) = happyShift action_35
action_171 (128) = happyShift action_36
action_171 (130) = happyShift action_37
action_171 (6) = happyGoto action_13
action_171 (7) = happyGoto action_14
action_171 (8) = happyGoto action_182
action_171 (9) = happyGoto action_16
action_171 (10) = happyGoto action_17
action_171 (12) = happyGoto action_18
action_171 (13) = happyGoto action_19
action_171 (14) = happyGoto action_20
action_171 (15) = happyGoto action_21
action_171 (18) = happyGoto action_22
action_171 (19) = happyGoto action_23
action_171 (20) = happyGoto action_24
action_171 (22) = happyGoto action_25
action_171 (23) = happyGoto action_26
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (122) = happyShift action_181
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (65) = happyShift action_4
action_173 (66) = happyShift action_27
action_173 (67) = happyShift action_28
action_173 (68) = happyShift action_29
action_173 (69) = happyShift action_30
action_173 (94) = happyShift action_31
action_173 (97) = happyShift action_32
action_173 (113) = happyShift action_33
action_173 (115) = happyShift action_34
action_173 (117) = happyShift action_35
action_173 (128) = happyShift action_36
action_173 (130) = happyShift action_37
action_173 (6) = happyGoto action_13
action_173 (7) = happyGoto action_14
action_173 (8) = happyGoto action_180
action_173 (9) = happyGoto action_16
action_173 (10) = happyGoto action_17
action_173 (12) = happyGoto action_18
action_173 (13) = happyGoto action_19
action_173 (14) = happyGoto action_20
action_173 (15) = happyGoto action_21
action_173 (18) = happyGoto action_22
action_173 (19) = happyGoto action_23
action_173 (20) = happyGoto action_24
action_173 (22) = happyGoto action_25
action_173 (23) = happyGoto action_26
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_54

action_175 _ = happyReduce_49

action_176 (65) = happyShift action_4
action_176 (6) = happyGoto action_178
action_176 (21) = happyGoto action_179
action_176 _ = happyReduce_39

action_177 _ = happyReduce_119

action_178 (119) = happyShift action_220
action_178 _ = happyFail (happyExpListPerState 178)

action_179 _ = happyReduce_40

action_180 _ = happyReduce_55

action_181 (65) = happyShift action_4
action_181 (66) = happyShift action_27
action_181 (67) = happyShift action_28
action_181 (68) = happyShift action_29
action_181 (69) = happyShift action_30
action_181 (94) = happyShift action_31
action_181 (97) = happyShift action_32
action_181 (113) = happyShift action_33
action_181 (115) = happyShift action_34
action_181 (117) = happyShift action_35
action_181 (128) = happyShift action_36
action_181 (130) = happyShift action_37
action_181 (6) = happyGoto action_13
action_181 (7) = happyGoto action_14
action_181 (8) = happyGoto action_219
action_181 (9) = happyGoto action_16
action_181 (10) = happyGoto action_17
action_181 (12) = happyGoto action_18
action_181 (13) = happyGoto action_19
action_181 (14) = happyGoto action_20
action_181 (15) = happyGoto action_21
action_181 (18) = happyGoto action_22
action_181 (19) = happyGoto action_23
action_181 (20) = happyGoto action_24
action_181 (22) = happyGoto action_25
action_181 (23) = happyGoto action_26
action_181 _ = happyFail (happyExpListPerState 181)

action_182 _ = happyReduce_18

action_183 (65) = happyShift action_4
action_183 (6) = happyGoto action_190
action_183 (43) = happyGoto action_216
action_183 (55) = happyGoto action_218
action_183 _ = happyFail (happyExpListPerState 183)

action_184 _ = happyReduce_104

action_185 _ = happyReduce_105

action_186 (65) = happyShift action_4
action_186 (6) = happyGoto action_190
action_186 (43) = happyGoto action_216
action_186 (55) = happyGoto action_217
action_186 _ = happyFail (happyExpListPerState 186)

action_187 (65) = happyShift action_4
action_187 (6) = happyGoto action_163
action_187 (48) = happyGoto action_215
action_187 _ = happyFail (happyExpListPerState 187)

action_188 (65) = happyShift action_4
action_188 (67) = happyShift action_138
action_188 (68) = happyShift action_139
action_188 (69) = happyShift action_140
action_188 (113) = happyShift action_142
action_188 (117) = happyShift action_143
action_188 (127) = happyShift action_144
action_188 (130) = happyShift action_145
action_188 (6) = happyGoto action_129
action_188 (29) = happyGoto action_214
action_188 (30) = happyGoto action_131
action_188 (31) = happyGoto action_132
action_188 (32) = happyGoto action_133
action_188 (33) = happyGoto action_134
action_188 (34) = happyGoto action_135
action_188 (35) = happyGoto action_136
action_188 (36) = happyGoto action_137
action_188 _ = happyFail (happyExpListPerState 188)

action_189 _ = happyReduce_60

action_190 (110) = happyShift action_210
action_190 (125) = happyShift action_211
action_190 (126) = happyShift action_212
action_190 (127) = happyShift action_213
action_190 _ = happyFail (happyExpListPerState 190)

action_191 _ = happyReduce_96

action_192 (121) = happyShift action_209
action_192 _ = happyReduce_100

action_193 _ = happyReduce_62

action_194 _ = happyReduce_64

action_195 (65) = happyShift action_4
action_195 (67) = happyShift action_138
action_195 (68) = happyShift action_139
action_195 (69) = happyShift action_140
action_195 (113) = happyShift action_142
action_195 (117) = happyShift action_143
action_195 (127) = happyShift action_144
action_195 (130) = happyShift action_145
action_195 (6) = happyGoto action_129
action_195 (29) = happyGoto action_208
action_195 (30) = happyGoto action_131
action_195 (31) = happyGoto action_132
action_195 (32) = happyGoto action_133
action_195 (33) = happyGoto action_134
action_195 (34) = happyGoto action_135
action_195 (35) = happyGoto action_136
action_195 (36) = happyGoto action_137
action_195 _ = happyFail (happyExpListPerState 195)

action_196 _ = happyReduce_76

action_197 (65) = happyShift action_4
action_197 (67) = happyShift action_138
action_197 (68) = happyShift action_139
action_197 (69) = happyShift action_140
action_197 (113) = happyShift action_142
action_197 (117) = happyShift action_143
action_197 (127) = happyShift action_144
action_197 (130) = happyShift action_145
action_197 (6) = happyGoto action_129
action_197 (29) = happyGoto action_207
action_197 (30) = happyGoto action_131
action_197 (31) = happyGoto action_132
action_197 (32) = happyGoto action_133
action_197 (33) = happyGoto action_134
action_197 (34) = happyGoto action_135
action_197 (35) = happyGoto action_136
action_197 (36) = happyGoto action_137
action_197 _ = happyFail (happyExpListPerState 197)

action_198 _ = happyReduce_75

action_199 _ = happyReduce_77

action_200 (65) = happyShift action_4
action_200 (67) = happyShift action_138
action_200 (68) = happyShift action_139
action_200 (69) = happyShift action_140
action_200 (113) = happyShift action_142
action_200 (117) = happyShift action_143
action_200 (127) = happyShift action_144
action_200 (130) = happyShift action_145
action_200 (6) = happyGoto action_129
action_200 (29) = happyGoto action_206
action_200 (30) = happyGoto action_131
action_200 (31) = happyGoto action_132
action_200 (32) = happyGoto action_133
action_200 (33) = happyGoto action_134
action_200 (34) = happyGoto action_135
action_200 (35) = happyGoto action_136
action_200 (36) = happyGoto action_137
action_200 _ = happyFail (happyExpListPerState 200)

action_201 _ = happyReduce_117

action_202 (65) = happyShift action_4
action_202 (67) = happyShift action_138
action_202 (68) = happyShift action_139
action_202 (69) = happyShift action_140
action_202 (113) = happyShift action_142
action_202 (117) = happyShift action_143
action_202 (127) = happyShift action_144
action_202 (130) = happyShift action_145
action_202 (6) = happyGoto action_129
action_202 (29) = happyGoto action_205
action_202 (30) = happyGoto action_131
action_202 (31) = happyGoto action_132
action_202 (32) = happyGoto action_133
action_202 (33) = happyGoto action_134
action_202 (34) = happyGoto action_135
action_202 (35) = happyGoto action_136
action_202 (36) = happyGoto action_137
action_202 _ = happyFail (happyExpListPerState 202)

action_203 (92) = happyShift action_204
action_203 _ = happyReduce_88

action_204 (65) = happyShift action_4
action_204 (6) = happyGoto action_228
action_204 (41) = happyGoto action_229
action_204 (51) = happyGoto action_230
action_204 _ = happyFail (happyExpListPerState 204)

action_205 _ = happyReduce_69

action_206 (121) = happyShift action_227
action_206 _ = happyReduce_80

action_207 _ = happyReduce_131

action_208 _ = happyReduce_101

action_209 (65) = happyShift action_4
action_209 (6) = happyGoto action_190
action_209 (43) = happyGoto action_226
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (65) = happyShift action_4
action_210 (67) = happyShift action_138
action_210 (68) = happyShift action_139
action_210 (69) = happyShift action_140
action_210 (113) = happyShift action_142
action_210 (117) = happyShift action_143
action_210 (127) = happyShift action_144
action_210 (130) = happyShift action_145
action_210 (6) = happyGoto action_129
action_210 (29) = happyGoto action_225
action_210 (30) = happyGoto action_131
action_210 (31) = happyGoto action_132
action_210 (32) = happyGoto action_133
action_210 (33) = happyGoto action_134
action_210 (34) = happyGoto action_135
action_210 (35) = happyGoto action_136
action_210 (36) = happyGoto action_137
action_210 _ = happyFail (happyExpListPerState 210)

action_211 (65) = happyShift action_4
action_211 (67) = happyShift action_138
action_211 (68) = happyShift action_139
action_211 (69) = happyShift action_140
action_211 (113) = happyShift action_142
action_211 (117) = happyShift action_143
action_211 (127) = happyShift action_144
action_211 (130) = happyShift action_145
action_211 (6) = happyGoto action_129
action_211 (29) = happyGoto action_224
action_211 (30) = happyGoto action_131
action_211 (31) = happyGoto action_132
action_211 (32) = happyGoto action_133
action_211 (33) = happyGoto action_134
action_211 (34) = happyGoto action_135
action_211 (35) = happyGoto action_136
action_211 (36) = happyGoto action_137
action_211 _ = happyFail (happyExpListPerState 211)

action_212 (65) = happyShift action_4
action_212 (67) = happyShift action_138
action_212 (68) = happyShift action_139
action_212 (69) = happyShift action_140
action_212 (113) = happyShift action_142
action_212 (117) = happyShift action_143
action_212 (127) = happyShift action_144
action_212 (130) = happyShift action_145
action_212 (6) = happyGoto action_129
action_212 (29) = happyGoto action_223
action_212 (30) = happyGoto action_131
action_212 (31) = happyGoto action_132
action_212 (32) = happyGoto action_133
action_212 (33) = happyGoto action_134
action_212 (34) = happyGoto action_135
action_212 (35) = happyGoto action_136
action_212 (36) = happyGoto action_137
action_212 _ = happyFail (happyExpListPerState 212)

action_213 (65) = happyShift action_4
action_213 (67) = happyShift action_138
action_213 (68) = happyShift action_139
action_213 (69) = happyShift action_140
action_213 (113) = happyShift action_142
action_213 (117) = happyShift action_143
action_213 (127) = happyShift action_144
action_213 (130) = happyShift action_145
action_213 (6) = happyGoto action_129
action_213 (29) = happyGoto action_222
action_213 (30) = happyGoto action_131
action_213 (31) = happyGoto action_132
action_213 (32) = happyGoto action_133
action_213 (33) = happyGoto action_134
action_213 (34) = happyGoto action_135
action_213 (35) = happyGoto action_136
action_213 (36) = happyGoto action_137
action_213 _ = happyFail (happyExpListPerState 213)

action_214 _ = happyReduce_107

action_215 _ = happyReduce_109

action_216 _ = happyReduce_120

action_217 (65) = happyShift action_4
action_217 (6) = happyGoto action_190
action_217 (43) = happyGoto action_221
action_217 _ = happyReduce_85

action_218 (65) = happyShift action_4
action_218 (6) = happyGoto action_190
action_218 (43) = happyGoto action_221
action_218 _ = happyReduce_87

action_219 _ = happyReduce_56

action_220 (65) = happyShift action_4
action_220 (66) = happyShift action_27
action_220 (67) = happyShift action_28
action_220 (68) = happyShift action_29
action_220 (69) = happyShift action_30
action_220 (94) = happyShift action_31
action_220 (97) = happyShift action_32
action_220 (113) = happyShift action_33
action_220 (115) = happyShift action_34
action_220 (117) = happyShift action_35
action_220 (128) = happyShift action_36
action_220 (130) = happyShift action_37
action_220 (6) = happyGoto action_13
action_220 (7) = happyGoto action_14
action_220 (8) = happyGoto action_110
action_220 (9) = happyGoto action_16
action_220 (10) = happyGoto action_17
action_220 (12) = happyGoto action_18
action_220 (13) = happyGoto action_19
action_220 (14) = happyGoto action_20
action_220 (15) = happyGoto action_21
action_220 (18) = happyGoto action_22
action_220 (19) = happyGoto action_23
action_220 (20) = happyGoto action_24
action_220 (22) = happyGoto action_25
action_220 (23) = happyGoto action_26
action_220 _ = happyFail (happyExpListPerState 220)

action_221 _ = happyReduce_121

action_222 _ = happyReduce_95

action_223 _ = happyReduce_92

action_224 _ = happyReduce_94

action_225 _ = happyReduce_93

action_226 _ = happyReduce_97

action_227 (65) = happyShift action_4
action_227 (6) = happyGoto action_151
action_227 (37) = happyGoto action_233
action_227 _ = happyReduce_78

action_228 (126) = happyShift action_232
action_228 _ = happyFail (happyExpListPerState 228)

action_229 _ = happyReduce_112

action_230 (65) = happyShift action_4
action_230 (6) = happyGoto action_228
action_230 (41) = happyGoto action_231
action_230 _ = happyReduce_83

action_231 _ = happyReduce_113

action_232 (65) = happyShift action_4
action_232 (66) = happyShift action_27
action_232 (67) = happyShift action_28
action_232 (68) = happyShift action_29
action_232 (69) = happyShift action_30
action_232 (94) = happyShift action_31
action_232 (97) = happyShift action_32
action_232 (113) = happyShift action_33
action_232 (115) = happyShift action_34
action_232 (117) = happyShift action_35
action_232 (128) = happyShift action_36
action_232 (130) = happyShift action_37
action_232 (6) = happyGoto action_13
action_232 (7) = happyGoto action_14
action_232 (8) = happyGoto action_234
action_232 (9) = happyGoto action_16
action_232 (10) = happyGoto action_17
action_232 (12) = happyGoto action_18
action_232 (13) = happyGoto action_19
action_232 (14) = happyGoto action_20
action_232 (15) = happyGoto action_21
action_232 (18) = happyGoto action_22
action_232 (19) = happyGoto action_23
action_232 (20) = happyGoto action_24
action_232 (22) = happyGoto action_25
action_232 (23) = happyGoto action_26
action_232 _ = happyFail (happyExpListPerState 232)

action_233 _ = happyReduce_79

action_234 _ = happyReduce_89

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
	(HappyTerminal happy_var_2)
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
	(HappyTerminal happy_var_2)
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
	(HappyAbsSyn56  happy_var_2) `HappyStk`
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
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn17
		 (P.backcall (fmap P.pattern happy_var_1) happy_var_3
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

happyReduce_41 = happyReduce 4 21 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ([P.keyValPair happy_var_1 happy_var_3]
	) `HappyStk` happyRest

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
	(HappyAbsSyn57  happy_var_2)
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
happyReduction_45 (HappyAbsSyn54  happy_var_3)
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
	(HappyAbsSyn63  happy_var_3) `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (P.Tuple happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0  26 happyReduction_51
happyReduction_51  =  HappyAbsSyn26
		 ([]
	)

happyReduce_52 = happySpecReduce_1  26 happyReduction_52
happyReduction_52 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn26
		 ([(happy_var_1, Nothing)]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  26 happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1, Nothing) : happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  27 happyReduction_54
happyReduction_54 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 28 happyReduction_55
happyReduction_55 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ([P.matchCase (P.pattern happy_var_2) happy_var_4]
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 5 28 happyReduction_56
happyReduction_56 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (happy_var_1 ++ [P.matchCase (P.pattern happy_var_3) happy_var_5]
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_1  29 happyReduction_57
happyReduction_57 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  29 happyReduction_58
happyReduction_58 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn29
		 (P.typeUnion happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  30 happyReduction_59
happyReduction_59 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  31 happyReduction_61
happyReduction_61 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn31
		 ([happy_var_2]
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  31 happyReduction_62
happyReduction_62 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  32 happyReduction_63
happyReduction_63 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  32 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  33 happyReduction_65
happyReduction_65 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  33 happyReduction_66
happyReduction_66 (HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (P.typeFnApplication happy_var_1 [happy_var_2]
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  33 happyReduction_67
happyReduction_67 (HappyTerminal happy_var_2)
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (P.tyParenthesised happy_var_1 happy_var_2 happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  34 happyReduction_68
happyReduction_68 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happyReduce 4 34 happyReduction_69
happyReduction_69 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (P.typeLambda (happy_var_2) happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_1  35 happyReduction_70
happyReduction_70 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  36 happyReduction_71
happyReduction_71 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.number (PT.TLiteral . PL.LInt) happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  36 happyReduction_72
happyReduction_72 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.boolean (PT.TLiteral . PL.LBool) happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  36 happyReduction_73
happyReduction_73 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.string (PT.TLiteral . PL.LString) happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  36 happyReduction_74
happyReduction_74 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyIdentifier happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  36 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  36 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_3)
	(HappyAbsSyn60  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyTuple happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  36 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_3)
	(HappyAbsSyn37  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_0  37 happyReduction_78
happyReduction_78  =  HappyAbsSyn37
		 ([]
	)

happyReduce_79 = happyReduce 5 37 happyReduction_79
happyReduction_79 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_3  37 happyReduction_80
happyReduction_80 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn37
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  38 happyReduction_81
happyReduction_81 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn38
		 (P.script happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  39 happyReduction_82
happyReduction_82 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happyReduce 8 39 happyReduction_83
happyReduction_83 ((HappyAbsSyn51  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn45  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 5 39 happyReduction_84
happyReduction_84 ((HappyAbsSyn49  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_85 = happyReduce 7 39 happyReduction_85
happyReduction_85 ((HappyAbsSyn55  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_86 = happyReduce 5 39 happyReduction_86
happyReduction_86 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_87 = happyReduce 7 39 happyReduction_87
happyReduction_87 ((HappyAbsSyn55  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 6 40 happyReduction_88
happyReduction_88 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn45  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_3  41 happyReduction_89
happyReduction_89 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn41
		 (P.binding happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  42 happyReduction_90
happyReduction_90 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn42
		 ([happy_var_1]
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  42 happyReduction_91
happyReduction_91 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  43 happyReduction_92
happyReduction_92 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn43
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  43 happyReduction_93
happyReduction_93 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn43
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  43 happyReduction_94
happyReduction_94 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn43
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  43 happyReduction_95
happyReduction_95 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn43
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  44 happyReduction_96
happyReduction_96 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  44 happyReduction_97
happyReduction_97 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  45 happyReduction_98
happyReduction_98  =  HappyAbsSyn45
		 (Nothing
	)

happyReduce_99 = happySpecReduce_2  45 happyReduction_99
happyReduction_99 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (Just happy_var_2
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happyReduce 4 45 happyReduction_100
happyReduction_100 ((HappyAbsSyn44  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_101 = happyReduce 5 45 happyReduction_101
happyReduction_101 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 (Just $ P.implementation happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_0  46 happyReduction_102
happyReduction_102  =  HappyAbsSyn46
		 (Nothing
	)

happyReduce_103 = happySpecReduce_2  46 happyReduction_103
happyReduction_103 (HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Just happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  47 happyReduction_104
happyReduction_104 (HappyAbsSyn47  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_3  47 happyReduction_105
happyReduction_105 _
	(HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn47
		 (happy_var_2
	)
happyReduction_105 _ _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  47 happyReduction_106
happyReduction_106 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn47
		 (P.kindId happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  48 happyReduction_107
happyReduction_107 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn48
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  49 happyReduction_108
happyReduction_108 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn49
		 ([happy_var_1]
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  49 happyReduction_109
happyReduction_109 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  50 happyReduction_110
happyReduction_110 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  50 happyReduction_111
happyReduction_111 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  51 happyReduction_112
happyReduction_112 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn51
		 ([happy_var_1]
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_2  51 happyReduction_113
happyReduction_113 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_113 _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  52 happyReduction_114
happyReduction_114 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn52
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  52 happyReduction_115
happyReduction_115 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  53 happyReduction_116
happyReduction_116 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_2  53 happyReduction_117
happyReduction_117 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_117 _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  54 happyReduction_118
happyReduction_118 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_2  54 happyReduction_119
happyReduction_119 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  55 happyReduction_120
happyReduction_120 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  55 happyReduction_121
happyReduction_121 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_0  56 happyReduction_122
happyReduction_122  =  HappyAbsSyn56
		 ([]
	)

happyReduce_123 = happySpecReduce_1  56 happyReduction_123
happyReduction_123 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  57 happyReduction_124
happyReduction_124 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn57
		 ([happy_var_1]
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  57 happyReduction_125
happyReduction_125 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  58 happyReduction_126
happyReduction_126 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  58 happyReduction_127
happyReduction_127 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  59 happyReduction_128
happyReduction_128 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_3  59 happyReduction_129
happyReduction_129 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_129 _ _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  60 happyReduction_130
happyReduction_130 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn60
		 ([happy_var_1]
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_3  60 happyReduction_131
happyReduction_131 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_0  61 happyReduction_132
happyReduction_132  =  HappyAbsSyn61
		 ([]
	)

happyReduce_133 = happySpecReduce_1  61 happyReduction_133
happyReduction_133 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_0  62 happyReduction_134
happyReduction_134  =  HappyAbsSyn62
		 (Nothing
	)

happyReduce_135 = happySpecReduce_1  62 happyReduction_135
happyReduction_135 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn62
		 (Just happy_var_1
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_0  63 happyReduction_136
happyReduction_136  =  HappyAbsSyn63
		 (Nothing
	)

happyReduce_137 = happySpecReduce_1  63 happyReduction_137
happyReduction_137 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn63
		 (Just happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_1  64 happyReduction_138
happyReduction_138 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn64
		 ([happy_var_1]
	)
happyReduction_138 _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_2  64 happyReduction_139
happyReduction_139 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn64
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_139 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 133 133 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 65;
	L.RangedToken (T.Hole _) _ -> cont 66;
	L.RangedToken (T.Number _) _ -> cont 67;
	L.RangedToken (T.String _) _ -> cont 68;
	L.RangedToken (T.Boolean _) _ -> cont 69;
	L.RangedToken (T.Operator "!") _ -> cont 70;
	L.RangedToken (T.Operator "+") _ -> cont 71;
	L.RangedToken (T.Operator "-") _ -> cont 72;
	L.RangedToken (T.Operator "*") _ -> cont 73;
	L.RangedToken (T.Operator "/") _ -> cont 74;
	L.RangedToken (T.Operator "^") _ -> cont 75;
	L.RangedToken (T.Operator "++") _ -> cont 76;
	L.RangedToken (T.Operator "==") _ -> cont 77;
	L.RangedToken (T.Operator "!=") _ -> cont 78;
	L.RangedToken (T.Operator "<") _ -> cont 79;
	L.RangedToken (T.Operator "<=") _ -> cont 80;
	L.RangedToken (T.Operator ">") _ -> cont 81;
	L.RangedToken (T.Operator ">=") _ -> cont 82;
	L.RangedToken (T.Operator "||") _ -> cont 83;
	L.RangedToken (T.Operator "&&") _ -> cont 84;
	L.RangedToken (T.Operator "|>") _ -> cont 85;
	L.RangedToken (T.Operator "<|") _ -> cont 86;
	L.RangedToken (T.Operator "<|") _ -> cont 87;
	L.RangedToken (T.Operator "#") _ -> cont 88;
	L.RangedToken (T.Operator _) _ -> cont 89;
	L.RangedToken T.Let _ -> cont 90;
	L.RangedToken T.In _ -> cont 91;
	L.RangedToken T.Where _ -> cont 92;
	L.RangedToken T.With _ -> cont 93;
	L.RangedToken T.If _ -> cont 94;
	L.RangedToken T.Then _ -> cont 95;
	L.RangedToken T.Else _ -> cont 96;
	L.RangedToken T.Match _ -> cont 97;
	L.RangedToken T.Return _ -> cont 98;
	L.RangedToken T.Data _ -> cont 99;
	L.RangedToken T.Type _ -> cont 100;
	L.RangedToken T.Alias _ -> cont 101;
	L.RangedToken T.Kind _ -> cont 102;
	L.RangedToken T.Forall _ -> cont 103;
	L.RangedToken T.Exists _ -> cont 104;
	L.RangedToken T.Proof _ -> cont 105;
	L.RangedToken T.Infer _ -> cont 106;
	L.RangedToken T.Protocol _ -> cont 107;
	L.RangedToken T.Interface _ -> cont 108;
	L.RangedToken T.Instance _ -> cont 109;
	L.RangedToken T.Implements _ -> cont 110;
	L.RangedToken T.Module _ -> cont 111;
	L.RangedToken T.Import _ -> cont 112;
	L.RangedToken T.LParen _ -> cont 113;
	L.RangedToken T.RParen _ -> cont 114;
	L.RangedToken T.LBrack _ -> cont 115;
	L.RangedToken T.RBrack _ -> cont 116;
	L.RangedToken T.LCurly _ -> cont 117;
	L.RangedToken T.RCurly _ -> cont 118;
	L.RangedToken T.Colon _ -> cont 119;
	L.RangedToken T.SemiColon _ -> cont 120;
	L.RangedToken T.Comma _ -> cont 121;
	L.RangedToken T.Arrow _ -> cont 122;
	L.RangedToken T.BackArrow _ -> cont 123;
	L.RangedToken T.FatArrow _ -> cont 124;
	L.RangedToken T.PipeArrow _ -> cont 125;
	L.RangedToken T.Equals _ -> cont 126;
	L.RangedToken T.Pipe _ -> cont 127;
	L.RangedToken T.Dot _ -> cont 128;
	L.RangedToken T.Section _ -> cont 129;
	L.RangedToken T.BackSlash _ -> cont 130;
	L.RangedToken T.Newline _ -> cont 131;
	L.RangedToken T.EOF _ -> cont 132;
	_ -> happyError' (tk, [])
	})

happyError_ explist 133 tk = happyError' (tk, explist)
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
