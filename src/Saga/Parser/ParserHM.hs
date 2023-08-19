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

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,571) ([0,0,0,1024,24,0,0,0,49152,3,16,2176,320,0,0,240,0,8192,16386,0,0,1024,0,0,0,0,0,0,0,6148,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,16384,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,0,0,544,64,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,64,8704,1280,0,0,960,4096,32768,16392,1,0,4096,0,0,0,0,0,0,60,0,34816,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32832,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,64,0,0,0,120,0,33024,64,0,0,7680,2048,0,4096,0,0,15360,0,1,136,20,0,0,15,64,8704,1280,0,0,960,4096,32768,16392,1,0,61440,0,4,544,80,0,0,4,0,0,0,0,0,7936,0,0,34,0,0,16384,0,0,0,4,0,0,0,0,0,8,0,0,0,0,0,256,0,0,0,0,0,1024,2,0,0,1984,0,32768,8,0,0,61440,0,0,544,64,0,0,60,0,34816,4096,0,0,256,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,8192,0,0,0,0,0,512,0,0,0,0,0,0,512,0,0,960,0,32768,8,1,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,49152,3,0,2176,256,0,0,0,0,0,0,0,0,15360,0,0,136,16,0,0,0,0,0,0,0,0,960,0,32768,8,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,384,0,0,0,0,0,960,4096,32768,16392,1,0,0,0,0,64,0,0,0,0,0,0,0,0,0,3840,16384,0,34,5,0,0,0,0,0,0,0,0,240,1024,8192,20482,0,0,15360,0,1,136,20,0,0,480,0,0,256,0,0,30720,0,0,16512,0,0,0,30,0,8192,16,0,0,0,0,0,0,0,0,57344,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,960,0,32768,8,1,0,0,0,0,16384,0,0,0,4,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,16,0,0,240,1024,8192,20482,0,0,1024,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,1,0,8,0,0,0,64,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,240,1024,8192,20482,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,16,0,0,0,0,0,0,0,0,256,0,0,0,0,0,16384,0,0,0,0,0,0,16,0,0,0,0,0,15360,0,0,136,16,0,0,0,0,64,224,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,1920,64,0,1024,0,0,256,0,0,0,0,0,16384,0,0,0,0,0,0,240,0,8192,16386,0,0,15360,0,0,136,16,0,0,15,0,8704,1024,0,0,960,0,32768,8,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,16,0,0,0,0,0,15360,0,1,136,20,0,0,480,0,0,256,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","tupleElems","tuple","params","args","fnApplication","controlFlow","term","atom","assignment","binding","bindings","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","';'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 102
        bit_end = (st Prelude.+ 1) Prelude.* 102
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..101]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (59) = happyShift action_8
action_0 (68) = happyShift action_9
action_0 (69) = happyShift action_10
action_0 (38) = happyGoto action_41
action_0 (39) = happyGoto action_42
action_0 (40) = happyGoto action_43
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (41) = happyShift action_6
action_1 (42) = happyShift action_33
action_1 (43) = happyShift action_34
action_1 (44) = happyShift action_35
action_1 (63) = happyShift action_36
action_1 (82) = happyShift action_37
action_1 (86) = happyShift action_38
action_1 (97) = happyShift action_39
action_1 (99) = happyShift action_40
action_1 (8) = happyGoto action_25
action_1 (10) = happyGoto action_26
action_1 (12) = happyGoto action_27
action_1 (15) = happyGoto action_28
action_1 (16) = happyGoto action_29
action_1 (17) = happyGoto action_30
action_1 (18) = happyGoto action_31
action_1 (22) = happyGoto action_32
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (41) = happyShift action_6
action_2 (42) = happyShift action_19
action_2 (43) = happyShift action_20
action_2 (44) = happyShift action_21
action_2 (82) = happyShift action_22
action_2 (86) = happyShift action_23
action_2 (99) = happyShift action_24
action_2 (8) = happyGoto action_13
action_2 (24) = happyGoto action_14
action_2 (26) = happyGoto action_15
action_2 (27) = happyGoto action_16
action_2 (28) = happyGoto action_17
action_2 (32) = happyGoto action_18
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (41) = happyShift action_6
action_3 (8) = happyGoto action_11
action_3 (34) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (59) = happyShift action_8
action_4 (68) = happyShift action_9
action_4 (69) = happyShift action_10
action_4 (38) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (41) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (102) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (41) = happyShift action_6
action_8 (8) = happyGoto action_67
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (41) = happyShift action_6
action_9 (8) = happyGoto action_66
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (41) = happyShift action_6
action_10 (8) = happyGoto action_65
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_73

action_12 (91) = happyShift action_64
action_12 (102) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (41) = happyReduce_54
action_13 (42) = happyReduce_54
action_13 (43) = happyReduce_54
action_13 (44) = happyReduce_54
action_13 (45) = happyReduce_54
action_13 (59) = happyReduce_54
action_13 (61) = happyReduce_54
action_13 (68) = happyReduce_54
action_13 (69) = happyReduce_54
action_13 (82) = happyReduce_54
action_13 (83) = happyReduce_54
action_13 (86) = happyReduce_54
action_13 (87) = happyReduce_54
action_13 (88) = happyShift action_63
action_13 (89) = happyReduce_54
action_13 (90) = happyReduce_54
action_13 (91) = happyReduce_54
action_13 (95) = happyReduce_54
action_13 (96) = happyReduce_54
action_13 (98) = happyReduce_54
action_13 (102) = happyReduce_54
action_13 _ = happyReduce_54

action_14 _ = happyReduce_52

action_15 _ = happyReduce_51

action_16 _ = happyReduce_53

action_17 (59) = happyReduce_67
action_17 (61) = happyReduce_67
action_17 (68) = happyReduce_67
action_17 (69) = happyReduce_67
action_17 (83) = happyReduce_67
action_17 (87) = happyReduce_67
action_17 (89) = happyReduce_67
action_17 (90) = happyReduce_67
action_17 (91) = happyShift action_62
action_17 (95) = happyReduce_67
action_17 (96) = happyReduce_67
action_17 (98) = happyReduce_67
action_17 (102) = happyReduce_67
action_17 (29) = happyGoto action_61
action_17 _ = happyReduce_56

action_18 (102) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_48

action_20 _ = happyReduce_50

action_21 _ = happyReduce_49

action_22 (41) = happyShift action_6
action_22 (42) = happyShift action_19
action_22 (43) = happyShift action_20
action_22 (44) = happyShift action_21
action_22 (82) = happyShift action_22
action_22 (86) = happyShift action_23
action_22 (99) = happyShift action_24
action_22 (8) = happyGoto action_13
action_22 (24) = happyGoto action_14
action_22 (26) = happyGoto action_15
action_22 (27) = happyGoto action_16
action_22 (28) = happyGoto action_17
action_22 (32) = happyGoto action_60
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (41) = happyShift action_6
action_23 (8) = happyGoto action_58
action_23 (23) = happyGoto action_59
action_23 _ = happyReduce_41

action_24 (13) = happyGoto action_57
action_24 _ = happyReduce_13

action_25 _ = happyReduce_22

action_26 _ = happyReduce_25

action_27 _ = happyReduce_24

action_28 _ = happyReduce_32

action_29 _ = happyReduce_31

action_30 _ = happyReduce_23

action_31 (46) = happyReduce_34
action_31 (47) = happyReduce_34
action_31 (48) = happyReduce_34
action_31 (49) = happyReduce_34
action_31 (59) = happyReduce_34
action_31 (61) = happyReduce_34
action_31 (64) = happyReduce_34
action_31 (65) = happyReduce_34
action_31 (68) = happyReduce_34
action_31 (69) = happyReduce_34
action_31 (83) = happyReduce_34
action_31 (87) = happyReduce_34
action_31 (90) = happyReduce_34
action_31 (97) = happyReduce_34
action_31 (102) = happyReduce_34
action_31 (14) = happyGoto action_56
action_31 _ = happyReduce_15

action_32 (46) = happyShift action_51
action_32 (47) = happyShift action_52
action_32 (48) = happyShift action_53
action_32 (49) = happyShift action_54
action_32 (97) = happyShift action_55
action_32 (102) = happyAccept
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_19

action_34 _ = happyReduce_21

action_35 _ = happyReduce_20

action_36 (41) = happyShift action_6
action_36 (42) = happyShift action_33
action_36 (43) = happyShift action_34
action_36 (44) = happyShift action_35
action_36 (63) = happyShift action_36
action_36 (82) = happyShift action_37
action_36 (86) = happyShift action_38
action_36 (97) = happyShift action_39
action_36 (99) = happyShift action_40
action_36 (8) = happyGoto action_25
action_36 (10) = happyGoto action_26
action_36 (12) = happyGoto action_27
action_36 (15) = happyGoto action_28
action_36 (16) = happyGoto action_29
action_36 (17) = happyGoto action_30
action_36 (18) = happyGoto action_31
action_36 (22) = happyGoto action_50
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (41) = happyShift action_6
action_37 (42) = happyShift action_33
action_37 (43) = happyShift action_34
action_37 (44) = happyShift action_35
action_37 (63) = happyShift action_36
action_37 (82) = happyShift action_37
action_37 (86) = happyShift action_38
action_37 (97) = happyShift action_39
action_37 (99) = happyShift action_40
action_37 (8) = happyGoto action_25
action_37 (10) = happyGoto action_26
action_37 (12) = happyGoto action_27
action_37 (15) = happyGoto action_28
action_37 (16) = happyGoto action_29
action_37 (17) = happyGoto action_30
action_37 (18) = happyGoto action_31
action_37 (22) = happyGoto action_49
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (41) = happyShift action_6
action_38 (8) = happyGoto action_47
action_38 (9) = happyGoto action_48
action_38 _ = happyReduce_6

action_39 (41) = happyShift action_6
action_39 (42) = happyShift action_33
action_39 (43) = happyShift action_34
action_39 (44) = happyShift action_35
action_39 (82) = happyShift action_37
action_39 (86) = happyShift action_38
action_39 (8) = happyGoto action_25
action_39 (10) = happyGoto action_26
action_39 (12) = happyGoto action_27
action_39 (17) = happyGoto action_30
action_39 (18) = happyGoto action_46
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (13) = happyGoto action_45
action_40 _ = happyReduce_13

action_41 _ = happyReduce_85

action_42 (59) = happyShift action_8
action_42 (68) = happyShift action_9
action_42 (69) = happyShift action_10
action_42 (38) = happyGoto action_44
action_42 _ = happyReduce_87

action_43 (102) = happyAccept
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_86

action_45 (41) = happyShift action_6
action_45 (91) = happyShift action_99
action_45 (8) = happyGoto action_84
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_35

action_47 (88) = happyShift action_98
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (87) = happyShift action_97
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (46) = happyShift action_51
action_49 (47) = happyShift action_52
action_49 (48) = happyShift action_53
action_49 (49) = happyShift action_54
action_49 (83) = happyShift action_95
action_49 (90) = happyShift action_96
action_49 (97) = happyShift action_55
action_49 (11) = happyGoto action_94
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (46) = happyShift action_51
action_50 (47) = happyShift action_52
action_50 (48) = happyShift action_53
action_50 (49) = happyShift action_54
action_50 (64) = happyShift action_93
action_50 (97) = happyShift action_55
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (41) = happyShift action_6
action_51 (42) = happyShift action_33
action_51 (43) = happyShift action_34
action_51 (44) = happyShift action_35
action_51 (63) = happyShift action_36
action_51 (82) = happyShift action_37
action_51 (86) = happyShift action_38
action_51 (97) = happyShift action_39
action_51 (99) = happyShift action_40
action_51 (8) = happyGoto action_25
action_51 (10) = happyGoto action_26
action_51 (12) = happyGoto action_27
action_51 (15) = happyGoto action_28
action_51 (16) = happyGoto action_29
action_51 (17) = happyGoto action_30
action_51 (18) = happyGoto action_31
action_51 (22) = happyGoto action_92
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (41) = happyShift action_6
action_52 (42) = happyShift action_33
action_52 (43) = happyShift action_34
action_52 (44) = happyShift action_35
action_52 (63) = happyShift action_36
action_52 (82) = happyShift action_37
action_52 (86) = happyShift action_38
action_52 (97) = happyShift action_39
action_52 (99) = happyShift action_40
action_52 (8) = happyGoto action_25
action_52 (10) = happyGoto action_26
action_52 (12) = happyGoto action_27
action_52 (15) = happyGoto action_28
action_52 (16) = happyGoto action_29
action_52 (17) = happyGoto action_30
action_52 (18) = happyGoto action_31
action_52 (22) = happyGoto action_91
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (41) = happyShift action_6
action_53 (42) = happyShift action_33
action_53 (43) = happyShift action_34
action_53 (44) = happyShift action_35
action_53 (63) = happyShift action_36
action_53 (82) = happyShift action_37
action_53 (86) = happyShift action_38
action_53 (97) = happyShift action_39
action_53 (99) = happyShift action_40
action_53 (8) = happyGoto action_25
action_53 (10) = happyGoto action_26
action_53 (12) = happyGoto action_27
action_53 (15) = happyGoto action_28
action_53 (16) = happyGoto action_29
action_53 (17) = happyGoto action_30
action_53 (18) = happyGoto action_31
action_53 (22) = happyGoto action_90
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (41) = happyShift action_6
action_54 (42) = happyShift action_33
action_54 (43) = happyShift action_34
action_54 (44) = happyShift action_35
action_54 (63) = happyShift action_36
action_54 (82) = happyShift action_37
action_54 (86) = happyShift action_38
action_54 (97) = happyShift action_39
action_54 (99) = happyShift action_40
action_54 (8) = happyGoto action_25
action_54 (10) = happyGoto action_26
action_54 (12) = happyGoto action_27
action_54 (15) = happyGoto action_28
action_54 (16) = happyGoto action_29
action_54 (17) = happyGoto action_30
action_54 (18) = happyGoto action_31
action_54 (22) = happyGoto action_89
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (41) = happyShift action_6
action_55 (8) = happyGoto action_88
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (41) = happyShift action_6
action_56 (42) = happyShift action_33
action_56 (43) = happyShift action_34
action_56 (44) = happyShift action_35
action_56 (45) = happyShift action_87
action_56 (82) = happyShift action_37
action_56 (86) = happyShift action_38
action_56 (8) = happyGoto action_25
action_56 (10) = happyGoto action_26
action_56 (12) = happyGoto action_27
action_56 (17) = happyGoto action_30
action_56 (18) = happyGoto action_86
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (41) = happyShift action_6
action_57 (93) = happyShift action_85
action_57 (8) = happyGoto action_84
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (88) = happyShift action_83
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (87) = happyShift action_82
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (83) = happyShift action_80
action_60 (90) = happyShift action_81
action_60 (25) = happyGoto action_79
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (41) = happyShift action_6
action_61 (42) = happyShift action_19
action_61 (43) = happyShift action_20
action_61 (44) = happyShift action_21
action_61 (45) = happyShift action_78
action_61 (82) = happyShift action_22
action_61 (86) = happyShift action_23
action_61 (8) = happyGoto action_76
action_61 (24) = happyGoto action_14
action_61 (26) = happyGoto action_15
action_61 (27) = happyGoto action_16
action_61 (28) = happyGoto action_77
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (41) = happyShift action_6
action_62 (42) = happyShift action_19
action_62 (43) = happyShift action_20
action_62 (44) = happyShift action_21
action_62 (82) = happyShift action_22
action_62 (86) = happyShift action_23
action_62 (99) = happyShift action_24
action_62 (8) = happyGoto action_13
action_62 (24) = happyGoto action_14
action_62 (26) = happyGoto action_15
action_62 (27) = happyGoto action_16
action_62 (28) = happyGoto action_17
action_62 (32) = happyGoto action_75
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (41) = happyShift action_6
action_63 (42) = happyShift action_19
action_63 (43) = happyShift action_20
action_63 (44) = happyShift action_21
action_63 (82) = happyShift action_22
action_63 (86) = happyShift action_23
action_63 (99) = happyShift action_24
action_63 (8) = happyGoto action_13
action_63 (24) = happyGoto action_14
action_63 (26) = happyGoto action_15
action_63 (27) = happyGoto action_16
action_63 (28) = happyGoto action_17
action_63 (32) = happyGoto action_74
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (41) = happyShift action_6
action_64 (8) = happyGoto action_11
action_64 (34) = happyGoto action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (98) = happyShift action_71
action_65 (35) = happyGoto action_72
action_65 _ = happyReduce_74

action_66 (98) = happyShift action_71
action_66 (35) = happyGoto action_70
action_66 _ = happyReduce_74

action_67 (88) = happyShift action_69
action_67 (33) = happyGoto action_68
action_67 _ = happyReduce_69

action_68 (98) = happyShift action_71
action_68 (35) = happyGoto action_113
action_68 _ = happyReduce_74

action_69 (41) = happyShift action_6
action_69 (42) = happyShift action_19
action_69 (43) = happyShift action_20
action_69 (44) = happyShift action_21
action_69 (82) = happyShift action_22
action_69 (86) = happyShift action_23
action_69 (99) = happyShift action_24
action_69 (8) = happyGoto action_13
action_69 (24) = happyGoto action_14
action_69 (26) = happyGoto action_15
action_69 (27) = happyGoto action_16
action_69 (28) = happyGoto action_17
action_69 (32) = happyGoto action_112
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (95) = happyShift action_111
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (41) = happyShift action_6
action_71 (8) = happyGoto action_11
action_71 (34) = happyGoto action_110
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (95) = happyShift action_109
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_72

action_74 _ = happyReduce_68

action_75 _ = happyReduce_64

action_76 (41) = happyReduce_54
action_76 (42) = happyReduce_54
action_76 (43) = happyReduce_54
action_76 (44) = happyReduce_54
action_76 (45) = happyReduce_54
action_76 (82) = happyReduce_54
action_76 (86) = happyReduce_54
action_76 _ = happyReduce_54

action_77 _ = happyReduce_57

action_78 _ = happyReduce_66

action_79 (83) = happyShift action_108
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_55

action_81 (41) = happyShift action_6
action_81 (42) = happyShift action_19
action_81 (43) = happyShift action_20
action_81 (44) = happyShift action_21
action_81 (82) = happyShift action_22
action_81 (86) = happyShift action_23
action_81 (99) = happyShift action_24
action_81 (8) = happyGoto action_13
action_81 (24) = happyGoto action_14
action_81 (26) = happyGoto action_15
action_81 (27) = happyGoto action_16
action_81 (28) = happyGoto action_17
action_81 (32) = happyGoto action_107
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_44

action_83 (41) = happyShift action_6
action_83 (42) = happyShift action_19
action_83 (43) = happyShift action_20
action_83 (44) = happyShift action_21
action_83 (82) = happyShift action_22
action_83 (86) = happyShift action_23
action_83 (99) = happyShift action_24
action_83 (8) = happyGoto action_13
action_83 (24) = happyGoto action_14
action_83 (26) = happyGoto action_15
action_83 (27) = happyGoto action_16
action_83 (28) = happyGoto action_17
action_83 (32) = happyGoto action_106
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_14

action_85 (41) = happyShift action_6
action_85 (42) = happyShift action_19
action_85 (43) = happyShift action_20
action_85 (44) = happyShift action_21
action_85 (82) = happyShift action_22
action_85 (86) = happyShift action_23
action_85 (99) = happyShift action_24
action_85 (8) = happyGoto action_13
action_85 (24) = happyGoto action_14
action_85 (26) = happyGoto action_15
action_85 (27) = happyGoto action_16
action_85 (28) = happyGoto action_17
action_85 (32) = happyGoto action_105
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_16

action_87 _ = happyReduce_17

action_88 _ = happyReduce_36

action_89 _ = happyReduce_40

action_90 _ = happyReduce_39

action_91 (48) = happyShift action_53
action_91 (49) = happyShift action_54
action_91 _ = happyReduce_38

action_92 (48) = happyShift action_53
action_92 (49) = happyShift action_54
action_92 _ = happyReduce_37

action_93 (41) = happyShift action_6
action_93 (42) = happyShift action_33
action_93 (43) = happyShift action_34
action_93 (44) = happyShift action_35
action_93 (63) = happyShift action_36
action_93 (82) = happyShift action_37
action_93 (86) = happyShift action_38
action_93 (97) = happyShift action_39
action_93 (99) = happyShift action_40
action_93 (8) = happyGoto action_25
action_93 (10) = happyGoto action_26
action_93 (12) = happyGoto action_27
action_93 (15) = happyGoto action_28
action_93 (16) = happyGoto action_29
action_93 (17) = happyGoto action_30
action_93 (18) = happyGoto action_31
action_93 (22) = happyGoto action_104
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (83) = happyShift action_103
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_26

action_96 (41) = happyShift action_6
action_96 (42) = happyShift action_33
action_96 (43) = happyShift action_34
action_96 (44) = happyShift action_35
action_96 (63) = happyShift action_36
action_96 (82) = happyShift action_37
action_96 (86) = happyShift action_38
action_96 (97) = happyShift action_39
action_96 (99) = happyShift action_40
action_96 (8) = happyGoto action_25
action_96 (10) = happyGoto action_26
action_96 (12) = happyGoto action_27
action_96 (15) = happyGoto action_28
action_96 (16) = happyGoto action_29
action_96 (17) = happyGoto action_30
action_96 (18) = happyGoto action_31
action_96 (22) = happyGoto action_102
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_9

action_98 (41) = happyShift action_6
action_98 (42) = happyShift action_33
action_98 (43) = happyShift action_34
action_98 (44) = happyShift action_35
action_98 (63) = happyShift action_36
action_98 (82) = happyShift action_37
action_98 (86) = happyShift action_38
action_98 (97) = happyShift action_39
action_98 (99) = happyShift action_40
action_98 (8) = happyGoto action_25
action_98 (10) = happyGoto action_26
action_98 (12) = happyGoto action_27
action_98 (15) = happyGoto action_28
action_98 (16) = happyGoto action_29
action_98 (17) = happyGoto action_30
action_98 (18) = happyGoto action_31
action_98 (22) = happyGoto action_101
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (41) = happyShift action_6
action_99 (42) = happyShift action_33
action_99 (43) = happyShift action_34
action_99 (44) = happyShift action_35
action_99 (63) = happyShift action_36
action_99 (82) = happyShift action_37
action_99 (86) = happyShift action_38
action_99 (97) = happyShift action_39
action_99 (99) = happyShift action_40
action_99 (8) = happyGoto action_25
action_99 (10) = happyGoto action_26
action_99 (12) = happyGoto action_27
action_99 (15) = happyGoto action_28
action_99 (16) = happyGoto action_29
action_99 (17) = happyGoto action_30
action_99 (18) = happyGoto action_31
action_99 (22) = happyGoto action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (46) = happyShift action_51
action_100 (47) = happyShift action_52
action_100 (48) = happyShift action_53
action_100 (49) = happyShift action_54
action_100 (97) = happyShift action_55
action_100 _ = happyReduce_33

action_101 (46) = happyShift action_51
action_101 (47) = happyShift action_52
action_101 (48) = happyShift action_53
action_101 (49) = happyShift action_54
action_101 (90) = happyShift action_124
action_101 (97) = happyShift action_55
action_101 _ = happyReduce_8

action_102 (46) = happyShift action_51
action_102 (47) = happyShift action_52
action_102 (48) = happyShift action_53
action_102 (49) = happyShift action_54
action_102 (90) = happyShift action_96
action_102 (97) = happyShift action_55
action_102 (11) = happyGoto action_123
action_102 _ = happyReduce_10

action_103 _ = happyReduce_12

action_104 (46) = happyShift action_51
action_104 (47) = happyShift action_52
action_104 (48) = happyShift action_53
action_104 (49) = happyShift action_54
action_104 (65) = happyShift action_122
action_104 (97) = happyShift action_55
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_65

action_106 (90) = happyShift action_121
action_106 _ = happyReduce_43

action_107 (90) = happyShift action_81
action_107 (25) = happyGoto action_120
action_107 _ = happyReduce_45

action_108 _ = happyReduce_47

action_109 (41) = happyShift action_6
action_109 (42) = happyShift action_19
action_109 (43) = happyShift action_20
action_109 (44) = happyShift action_21
action_109 (82) = happyShift action_22
action_109 (86) = happyShift action_23
action_109 (99) = happyShift action_24
action_109 (8) = happyGoto action_13
action_109 (24) = happyGoto action_14
action_109 (26) = happyGoto action_15
action_109 (27) = happyGoto action_16
action_109 (28) = happyGoto action_17
action_109 (32) = happyGoto action_119
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (91) = happyShift action_64
action_110 _ = happyReduce_75

action_111 (41) = happyShift action_6
action_111 (8) = happyGoto action_116
action_111 (36) = happyGoto action_117
action_111 (37) = happyGoto action_118
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (61) = happyShift action_115
action_112 _ = happyReduce_70

action_113 (95) = happyShift action_114
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (41) = happyShift action_6
action_114 (42) = happyShift action_33
action_114 (43) = happyShift action_34
action_114 (44) = happyShift action_35
action_114 (63) = happyShift action_36
action_114 (82) = happyShift action_37
action_114 (86) = happyShift action_38
action_114 (97) = happyShift action_39
action_114 (99) = happyShift action_40
action_114 (8) = happyGoto action_25
action_114 (10) = happyGoto action_26
action_114 (12) = happyGoto action_27
action_114 (15) = happyGoto action_28
action_114 (16) = happyGoto action_29
action_114 (17) = happyGoto action_30
action_114 (18) = happyGoto action_31
action_114 (22) = happyGoto action_135
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (41) = happyShift action_6
action_115 (8) = happyGoto action_132
action_115 (30) = happyGoto action_133
action_115 (31) = happyGoto action_134
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (88) = happyShift action_131
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_77

action_118 (61) = happyShift action_129
action_118 (96) = happyShift action_130
action_118 _ = happyReduce_81

action_119 (61) = happyShift action_128
action_119 _ = happyReduce_83

action_120 _ = happyReduce_46

action_121 (41) = happyShift action_6
action_121 (8) = happyGoto action_58
action_121 (23) = happyGoto action_127
action_121 _ = happyReduce_41

action_122 (41) = happyShift action_6
action_122 (42) = happyShift action_33
action_122 (43) = happyShift action_34
action_122 (44) = happyShift action_35
action_122 (63) = happyShift action_36
action_122 (82) = happyShift action_37
action_122 (86) = happyShift action_38
action_122 (97) = happyShift action_39
action_122 (99) = happyShift action_40
action_122 (8) = happyGoto action_25
action_122 (10) = happyGoto action_26
action_122 (12) = happyGoto action_27
action_122 (15) = happyGoto action_28
action_122 (16) = happyGoto action_29
action_122 (17) = happyGoto action_30
action_122 (18) = happyGoto action_31
action_122 (22) = happyGoto action_126
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_11

action_124 (41) = happyShift action_6
action_124 (8) = happyGoto action_47
action_124 (9) = happyGoto action_125
action_124 _ = happyReduce_6

action_125 _ = happyReduce_7

action_126 (46) = happyShift action_51
action_126 (47) = happyShift action_52
action_126 (48) = happyShift action_53
action_126 (49) = happyShift action_54
action_126 (97) = happyShift action_55
action_126 _ = happyReduce_18

action_127 _ = happyReduce_42

action_128 (41) = happyShift action_6
action_128 (8) = happyGoto action_132
action_128 (30) = happyGoto action_133
action_128 (31) = happyGoto action_145
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (41) = happyShift action_6
action_129 (8) = happyGoto action_132
action_129 (30) = happyGoto action_133
action_129 (31) = happyGoto action_144
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (41) = happyShift action_6
action_130 (8) = happyGoto action_116
action_130 (36) = happyGoto action_143
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (41) = happyShift action_6
action_131 (42) = happyShift action_19
action_131 (43) = happyShift action_20
action_131 (44) = happyShift action_21
action_131 (82) = happyShift action_22
action_131 (86) = happyShift action_23
action_131 (99) = happyShift action_24
action_131 (8) = happyGoto action_13
action_131 (24) = happyGoto action_14
action_131 (26) = happyGoto action_15
action_131 (27) = happyGoto action_16
action_131 (28) = happyGoto action_17
action_131 (32) = happyGoto action_142
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (79) = happyShift action_138
action_132 (94) = happyShift action_139
action_132 (95) = happyShift action_140
action_132 (96) = happyShift action_141
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_62

action_134 (89) = happyShift action_137
action_134 _ = happyReduce_71

action_135 (46) = happyShift action_51
action_135 (47) = happyShift action_52
action_135 (48) = happyShift action_53
action_135 (49) = happyShift action_54
action_135 (61) = happyShift action_136
action_135 (97) = happyShift action_55
action_135 _ = happyReduce_79

action_136 (41) = happyShift action_6
action_136 (8) = happyGoto action_151
action_136 (20) = happyGoto action_152
action_136 (21) = happyGoto action_153
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (41) = happyShift action_6
action_137 (8) = happyGoto action_132
action_137 (30) = happyGoto action_150
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (41) = happyShift action_6
action_138 (42) = happyShift action_19
action_138 (43) = happyShift action_20
action_138 (44) = happyShift action_21
action_138 (82) = happyShift action_22
action_138 (86) = happyShift action_23
action_138 (99) = happyShift action_24
action_138 (8) = happyGoto action_13
action_138 (24) = happyGoto action_14
action_138 (26) = happyGoto action_15
action_138 (27) = happyGoto action_16
action_138 (28) = happyGoto action_17
action_138 (32) = happyGoto action_149
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (41) = happyShift action_6
action_139 (42) = happyShift action_19
action_139 (43) = happyShift action_20
action_139 (44) = happyShift action_21
action_139 (82) = happyShift action_22
action_139 (86) = happyShift action_23
action_139 (99) = happyShift action_24
action_139 (8) = happyGoto action_13
action_139 (24) = happyGoto action_14
action_139 (26) = happyGoto action_15
action_139 (27) = happyGoto action_16
action_139 (28) = happyGoto action_17
action_139 (32) = happyGoto action_148
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (41) = happyShift action_6
action_140 (42) = happyShift action_19
action_140 (43) = happyShift action_20
action_140 (44) = happyShift action_21
action_140 (82) = happyShift action_22
action_140 (86) = happyShift action_23
action_140 (99) = happyShift action_24
action_140 (8) = happyGoto action_13
action_140 (24) = happyGoto action_14
action_140 (26) = happyGoto action_15
action_140 (27) = happyGoto action_16
action_140 (28) = happyGoto action_17
action_140 (32) = happyGoto action_147
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (41) = happyShift action_6
action_141 (42) = happyShift action_19
action_141 (43) = happyShift action_20
action_141 (44) = happyShift action_21
action_141 (82) = happyShift action_22
action_141 (86) = happyShift action_23
action_141 (99) = happyShift action_24
action_141 (8) = happyGoto action_13
action_141 (24) = happyGoto action_14
action_141 (26) = happyGoto action_15
action_141 (27) = happyGoto action_16
action_141 (28) = happyGoto action_17
action_141 (32) = happyGoto action_146
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_76

action_143 _ = happyReduce_78

action_144 (89) = happyShift action_137
action_144 _ = happyReduce_82

action_145 (89) = happyShift action_137
action_145 _ = happyReduce_84

action_146 _ = happyReduce_61

action_147 _ = happyReduce_58

action_148 _ = happyReduce_60

action_149 _ = happyReduce_59

action_150 _ = happyReduce_63

action_151 (95) = happyShift action_155
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_29

action_153 (90) = happyShift action_154
action_153 _ = happyReduce_80

action_154 (41) = happyShift action_6
action_154 (8) = happyGoto action_151
action_154 (20) = happyGoto action_157
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (41) = happyShift action_6
action_155 (42) = happyShift action_33
action_155 (43) = happyShift action_34
action_155 (44) = happyShift action_35
action_155 (63) = happyShift action_36
action_155 (82) = happyShift action_37
action_155 (86) = happyShift action_38
action_155 (97) = happyShift action_39
action_155 (99) = happyShift action_40
action_155 (8) = happyGoto action_25
action_155 (10) = happyGoto action_26
action_155 (12) = happyGoto action_27
action_155 (15) = happyGoto action_28
action_155 (16) = happyGoto action_29
action_155 (17) = happyGoto action_30
action_155 (18) = happyGoto action_31
action_155 (22) = happyGoto action_156
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (46) = happyShift action_51
action_156 (47) = happyShift action_52
action_156 (48) = happyShift action_53
action_156 (49) = happyShift action_54
action_156 (97) = happyShift action_55
action_156 _ = happyReduce_28

action_157 _ = happyReduce_30

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
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn32  happy_var_3)
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
happyReduction_45 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn25
		 ([happy_var_2]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2 : happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 4 26 happyReduction_47
happyReduction_47 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
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
	(HappyAbsSyn32  happy_var_2)
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
happyReduction_58 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  30 happyReduction_59
happyReduction_59 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn30
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn32  happy_var_3)
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
happyReduction_64 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn32
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 32 happyReduction_65
happyReduction_65 ((HappyAbsSyn32  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_3  32 happyReduction_66
happyReduction_66 (HappyTerminal happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn32
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  32 happyReduction_67
happyReduction_67 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  32 happyReduction_68
happyReduction_68 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn32
		 (P.tagged happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  33 happyReduction_69
happyReduction_69  =  HappyAbsSyn33
		 (Nothing
	)

happyReduce_70 = happySpecReduce_2  33 happyReduction_70
happyReduction_70 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (Just happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happyReduce 4 33 happyReduction_71
happyReduction_71 ((HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (Just $ P.typeClause happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_3  34 happyReduction_72
happyReduction_72 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  34 happyReduction_73
happyReduction_73 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn34
		 (P.kindId happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  35 happyReduction_74
happyReduction_74  =  HappyAbsSyn35
		 (Nothing
	)

happyReduce_75 = happySpecReduce_2  35 happyReduction_75
happyReduction_75 (HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (Just happy_var_2
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  36 happyReduction_76
happyReduction_76 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn36
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  37 happyReduction_77
happyReduction_77 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  37 happyReduction_78
happyReduction_78 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happyReduce 6 38 happyReduction_79
happyReduction_79 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 8 38 happyReduction_80
happyReduction_80 ((HappyAbsSyn21  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_4) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 (P.clause happy_var_6 happy_var_8)
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 5 38 happyReduction_81
happyReduction_81 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 7 38 happyReduction_82
happyReduction_82 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.dataType happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 5 38 happyReduction_83
happyReduction_83 ((HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 7 38 happyReduction_84
happyReduction_84 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (P.typeDef happy_var_2 happy_var_3 (P.typeClause happy_var_5 happy_var_7)
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_1  39 happyReduction_85
happyReduction_85 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn39
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_2  39 happyReduction_86
happyReduction_86 (HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  40 happyReduction_87
happyReduction_87 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 (P.script happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 102 102 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 41;
	L.RangedToken (T.Number _) _ -> cont 42;
	L.RangedToken (T.String _) _ -> cont 43;
	L.RangedToken (T.Boolean _) _ -> cont 44;
	L.RangedToken (T.Operator "!") _ -> cont 45;
	L.RangedToken (T.Operator "+") _ -> cont 46;
	L.RangedToken (T.Operator "-") _ -> cont 47;
	L.RangedToken (T.Operator "*") _ -> cont 48;
	L.RangedToken (T.Operator "/") _ -> cont 49;
	L.RangedToken (T.Operator "==") _ -> cont 50;
	L.RangedToken (T.Operator "!=") _ -> cont 51;
	L.RangedToken (T.Operator "<") _ -> cont 52;
	L.RangedToken (T.Operator "<=") _ -> cont 53;
	L.RangedToken (T.Operator ">") _ -> cont 54;
	L.RangedToken (T.Operator ">=") _ -> cont 55;
	L.RangedToken (T.Operator "||") _ -> cont 56;
	L.RangedToken (T.Operator "&&") _ -> cont 57;
	L.RangedToken (T.Operator _) _ -> cont 58;
	L.RangedToken T.Let _ -> cont 59;
	L.RangedToken T.In _ -> cont 60;
	L.RangedToken T.Where _ -> cont 61;
	L.RangedToken T.With _ -> cont 62;
	L.RangedToken T.If _ -> cont 63;
	L.RangedToken T.Then _ -> cont 64;
	L.RangedToken T.Else _ -> cont 65;
	L.RangedToken T.Match _ -> cont 66;
	L.RangedToken T.Return _ -> cont 67;
	L.RangedToken T.Data _ -> cont 68;
	L.RangedToken T.Type _ -> cont 69;
	L.RangedToken T.Alias _ -> cont 70;
	L.RangedToken T.Kind _ -> cont 71;
	L.RangedToken T.Forall _ -> cont 72;
	L.RangedToken T.Exists _ -> cont 73;
	L.RangedToken T.Proof _ -> cont 74;
	L.RangedToken T.Infer _ -> cont 75;
	L.RangedToken T.Protocol _ -> cont 76;
	L.RangedToken T.Interface _ -> cont 77;
	L.RangedToken T.Instance _ -> cont 78;
	L.RangedToken T.Implements _ -> cont 79;
	L.RangedToken T.Module _ -> cont 80;
	L.RangedToken T.Import _ -> cont 81;
	L.RangedToken T.LParen _ -> cont 82;
	L.RangedToken T.RParen _ -> cont 83;
	L.RangedToken T.LBrack _ -> cont 84;
	L.RangedToken T.RBrack _ -> cont 85;
	L.RangedToken T.LCurly _ -> cont 86;
	L.RangedToken T.RCurly _ -> cont 87;
	L.RangedToken T.Colon _ -> cont 88;
	L.RangedToken T.SemiColon _ -> cont 89;
	L.RangedToken T.Comma _ -> cont 90;
	L.RangedToken T.Arrow _ -> cont 91;
	L.RangedToken T.BackArrow _ -> cont 92;
	L.RangedToken T.FatArrow _ -> cont 93;
	L.RangedToken T.PipeArrow _ -> cont 94;
	L.RangedToken T.Equals _ -> cont 95;
	L.RangedToken T.Pipe _ -> cont 96;
	L.RangedToken T.Dot _ -> cont 97;
	L.RangedToken T.Section _ -> cont 98;
	L.RangedToken T.BackSlash _ -> cont 99;
	L.RangedToken T.Newline _ -> cont 100;
	L.RangedToken T.EOF _ -> cont 101;
	_ -> happyError' (tk, [])
	})

happyError_ explist 102 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn34 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

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
