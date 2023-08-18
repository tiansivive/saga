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

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,504) ([0,0,0,256,6,0,0,0,7680,32768,0,68,5,0,61440,0,0,544,32,0,32768,0,0,0,0,0,0,0,24592,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,2,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3840,0,0,34,2,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,30720,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,60,256,34816,2560,0,0,480,2048,16384,20484,0,0,256,0,0,0,0,0,30720,0,0,272,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32832,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,16384,0,0,0,0,0,0,128,0,0,0,0,0,4,0,0,0,0,0,16,0,0,49152,3,0,520,0,0,0,30,8,0,0,0,32768,7,32,4352,320,0,0,60,256,34816,2560,0,0,480,2048,16384,20484,0,0,3840,16384,0,32802,2,0,63488,0,0,272,0,0,16384,0,0,0,0,0,0,2,0,0,16,0,0,0,0,0,8,0,0,0,0,0,32,0,0,0,16384,0,1040,0,0,8192,0,0,0,0,0,0,31,0,8704,0,0,0,120,0,4096,4097,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,16,0,0,0,0,16384,0,0,0,0,0,0,1024,0,0,480,0,16384,16388,0,0,0,0,0,8192,0,0,2048,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,14,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,1,0,0,0,0,0,0,0,0,61440,0,0,544,32,0,0,0,0,0,0,0,0,60,0,34816,2048,0,0,0,0,0,0,0,0,3840,0,0,34,2,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,48,0,0,0,0,0,15,64,8704,640,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,7680,32768,0,68,5,0,0,0,0,0,0,0,32768,7,32,4352,320,0,0,60,256,34816,2560,0,0,15360,0,0,0,0,0,57344,1,0,256,0,0,0,15,0,2048,0,0,0,0,0,0,0,0,0,960,512,0,0,0,0,0,256,0,0,0,0,0,2048,0,128,0,0,0,16384,0,1024,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,120,0,4096,4097,0,0,960,0,32768,32776,0,0,7680,0,0,68,4,0,61440,0,0,544,32,0,32768,7,0,4352,256,0,0,0,0,0,8,0,0,32,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1,0,49152,3,16,2176,160,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,16384,0,0,0,0,0,0,2,0,0,0,0,0,16,0,0,0,0,0,128,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,60,256,34816,2560,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,120,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,1920,0,0,17,1,0,32768,7,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSagaScript","%start_parseSagaExpr","%start_parseSagaType","%start_parseSagaKind","%start_parseSagaDec","identifier","pairs","record","tupleElems","tuple","params","args","fnApplication","controlFlow","term","atom","assignment","expr","tpairs","trecord","ttupleElems","ttuple","type","typeAtom","typeArgs","tbinding","tbindings","typeExpr","typeAnnotation","kindExpr","kindAnnotation","dataExpr","dataExprs","dec","declarations","script","id","number","string","boolean","'!'","'+'","'-'","'*'","'/'","'=='","'!='","'<'","'<='","'>'","'>='","'||'","'&&'","op","let","in","where","with","if","then","else","match","return","data","ty","alias","kind","forall","exists","proof","infer","protocol","interface","instance","implements","module","import","'('","')'","'['","']'","'{'","'}'","':'","','","'->'","'<-'","'=>'","'|->'","'='","'|'","'.'","'::'","'\\\\'","newline","eof","%eof"]
        bit_start = st Prelude.* 99
        bit_end = (st Prelude.+ 1) Prelude.* 99
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..98]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (57) = happyShift action_8
action_0 (66) = happyShift action_9
action_0 (67) = happyShift action_10
action_0 (36) = happyGoto action_41
action_0 (37) = happyGoto action_42
action_0 (38) = happyGoto action_43
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (39) = happyShift action_6
action_1 (40) = happyShift action_33
action_1 (41) = happyShift action_34
action_1 (42) = happyShift action_35
action_1 (61) = happyShift action_36
action_1 (80) = happyShift action_37
action_1 (84) = happyShift action_38
action_1 (94) = happyShift action_39
action_1 (96) = happyShift action_40
action_1 (8) = happyGoto action_25
action_1 (10) = happyGoto action_26
action_1 (12) = happyGoto action_27
action_1 (15) = happyGoto action_28
action_1 (16) = happyGoto action_29
action_1 (17) = happyGoto action_30
action_1 (18) = happyGoto action_31
action_1 (20) = happyGoto action_32
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (39) = happyShift action_6
action_2 (40) = happyShift action_19
action_2 (41) = happyShift action_20
action_2 (42) = happyShift action_21
action_2 (80) = happyShift action_22
action_2 (84) = happyShift action_23
action_2 (96) = happyShift action_24
action_2 (8) = happyGoto action_13
action_2 (22) = happyGoto action_14
action_2 (24) = happyGoto action_15
action_2 (25) = happyGoto action_16
action_2 (26) = happyGoto action_17
action_2 (30) = happyGoto action_18
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (39) = happyShift action_6
action_3 (8) = happyGoto action_11
action_3 (32) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (57) = happyShift action_8
action_4 (66) = happyShift action_9
action_4 (67) = happyShift action_10
action_4 (36) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (39) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (99) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (39) = happyShift action_6
action_8 (8) = happyGoto action_67
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (39) = happyShift action_6
action_9 (8) = happyGoto action_66
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (39) = happyShift action_6
action_10 (8) = happyGoto action_65
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_69

action_12 (88) = happyShift action_64
action_12 (99) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (39) = happyReduce_51
action_13 (40) = happyReduce_51
action_13 (41) = happyReduce_51
action_13 (42) = happyReduce_51
action_13 (43) = happyReduce_51
action_13 (57) = happyReduce_51
action_13 (59) = happyReduce_51
action_13 (66) = happyReduce_51
action_13 (67) = happyReduce_51
action_13 (80) = happyReduce_51
action_13 (81) = happyReduce_51
action_13 (84) = happyReduce_51
action_13 (85) = happyReduce_51
action_13 (87) = happyReduce_51
action_13 (88) = happyReduce_51
action_13 (92) = happyReduce_51
action_13 (93) = happyReduce_51
action_13 (95) = happyReduce_51
action_13 (99) = happyReduce_51
action_13 _ = happyReduce_51

action_14 _ = happyReduce_49

action_15 _ = happyReduce_48

action_16 _ = happyReduce_50

action_17 (57) = happyReduce_64
action_17 (59) = happyReduce_64
action_17 (66) = happyReduce_64
action_17 (67) = happyReduce_64
action_17 (81) = happyReduce_64
action_17 (85) = happyReduce_64
action_17 (87) = happyReduce_64
action_17 (88) = happyShift action_63
action_17 (92) = happyReduce_64
action_17 (93) = happyReduce_64
action_17 (95) = happyReduce_64
action_17 (99) = happyReduce_64
action_17 (27) = happyGoto action_62
action_17 _ = happyReduce_53

action_18 (59) = happyShift action_61
action_18 (99) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_45

action_20 _ = happyReduce_47

action_21 _ = happyReduce_46

action_22 (39) = happyShift action_6
action_22 (40) = happyShift action_19
action_22 (41) = happyShift action_20
action_22 (42) = happyShift action_21
action_22 (80) = happyShift action_22
action_22 (84) = happyShift action_23
action_22 (96) = happyShift action_24
action_22 (8) = happyGoto action_13
action_22 (22) = happyGoto action_14
action_22 (24) = happyGoto action_15
action_22 (25) = happyGoto action_16
action_22 (26) = happyGoto action_17
action_22 (30) = happyGoto action_60
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (39) = happyShift action_6
action_23 (8) = happyGoto action_58
action_23 (21) = happyGoto action_59
action_23 _ = happyReduce_38

action_24 (13) = happyGoto action_57
action_24 _ = happyReduce_13

action_25 _ = happyReduce_22

action_26 _ = happyReduce_26

action_27 _ = happyReduce_25

action_28 _ = happyReduce_30

action_29 _ = happyReduce_29

action_30 _ = happyReduce_24

action_31 (44) = happyReduce_32
action_31 (45) = happyReduce_32
action_31 (46) = happyReduce_32
action_31 (47) = happyReduce_32
action_31 (57) = happyReduce_32
action_31 (62) = happyReduce_32
action_31 (63) = happyReduce_32
action_31 (66) = happyReduce_32
action_31 (67) = happyReduce_32
action_31 (81) = happyReduce_32
action_31 (85) = happyReduce_32
action_31 (87) = happyReduce_32
action_31 (94) = happyShift action_56
action_31 (99) = happyReduce_32
action_31 (14) = happyGoto action_55
action_31 _ = happyReduce_15

action_32 (44) = happyShift action_51
action_32 (45) = happyShift action_52
action_32 (46) = happyShift action_53
action_32 (47) = happyShift action_54
action_32 (99) = happyAccept
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_19

action_34 _ = happyReduce_21

action_35 _ = happyReduce_20

action_36 (39) = happyShift action_6
action_36 (40) = happyShift action_33
action_36 (41) = happyShift action_34
action_36 (42) = happyShift action_35
action_36 (61) = happyShift action_36
action_36 (80) = happyShift action_37
action_36 (84) = happyShift action_38
action_36 (94) = happyShift action_39
action_36 (96) = happyShift action_40
action_36 (8) = happyGoto action_25
action_36 (10) = happyGoto action_26
action_36 (12) = happyGoto action_27
action_36 (15) = happyGoto action_28
action_36 (16) = happyGoto action_29
action_36 (17) = happyGoto action_30
action_36 (18) = happyGoto action_31
action_36 (20) = happyGoto action_50
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (39) = happyShift action_6
action_37 (40) = happyShift action_33
action_37 (41) = happyShift action_34
action_37 (42) = happyShift action_35
action_37 (61) = happyShift action_36
action_37 (80) = happyShift action_37
action_37 (84) = happyShift action_38
action_37 (94) = happyShift action_39
action_37 (96) = happyShift action_40
action_37 (8) = happyGoto action_25
action_37 (10) = happyGoto action_26
action_37 (12) = happyGoto action_27
action_37 (15) = happyGoto action_28
action_37 (16) = happyGoto action_29
action_37 (17) = happyGoto action_30
action_37 (18) = happyGoto action_31
action_37 (20) = happyGoto action_49
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (39) = happyShift action_6
action_38 (8) = happyGoto action_47
action_38 (9) = happyGoto action_48
action_38 _ = happyReduce_6

action_39 (39) = happyShift action_6
action_39 (40) = happyShift action_33
action_39 (41) = happyShift action_34
action_39 (42) = happyShift action_35
action_39 (80) = happyShift action_37
action_39 (84) = happyShift action_38
action_39 (8) = happyGoto action_25
action_39 (10) = happyGoto action_26
action_39 (12) = happyGoto action_27
action_39 (17) = happyGoto action_30
action_39 (18) = happyGoto action_46
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (13) = happyGoto action_45
action_40 _ = happyReduce_13

action_41 _ = happyReduce_78

action_42 (57) = happyShift action_8
action_42 (66) = happyShift action_9
action_42 (67) = happyShift action_10
action_42 (36) = happyGoto action_44
action_42 _ = happyReduce_80

action_43 (99) = happyAccept
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_79

action_45 (39) = happyShift action_6
action_45 (88) = happyShift action_100
action_45 (8) = happyGoto action_85
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (94) = happyShift action_56
action_46 _ = happyReduce_33

action_47 (86) = happyShift action_99
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (85) = happyShift action_98
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (44) = happyShift action_51
action_49 (45) = happyShift action_52
action_49 (46) = happyShift action_53
action_49 (47) = happyShift action_54
action_49 (81) = happyShift action_96
action_49 (87) = happyShift action_97
action_49 (11) = happyGoto action_95
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (44) = happyShift action_51
action_50 (45) = happyShift action_52
action_50 (46) = happyShift action_53
action_50 (47) = happyShift action_54
action_50 (62) = happyShift action_94
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (39) = happyShift action_6
action_51 (40) = happyShift action_33
action_51 (41) = happyShift action_34
action_51 (42) = happyShift action_35
action_51 (61) = happyShift action_36
action_51 (80) = happyShift action_37
action_51 (84) = happyShift action_38
action_51 (94) = happyShift action_39
action_51 (96) = happyShift action_40
action_51 (8) = happyGoto action_25
action_51 (10) = happyGoto action_26
action_51 (12) = happyGoto action_27
action_51 (15) = happyGoto action_28
action_51 (16) = happyGoto action_29
action_51 (17) = happyGoto action_30
action_51 (18) = happyGoto action_31
action_51 (20) = happyGoto action_93
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (39) = happyShift action_6
action_52 (40) = happyShift action_33
action_52 (41) = happyShift action_34
action_52 (42) = happyShift action_35
action_52 (61) = happyShift action_36
action_52 (80) = happyShift action_37
action_52 (84) = happyShift action_38
action_52 (94) = happyShift action_39
action_52 (96) = happyShift action_40
action_52 (8) = happyGoto action_25
action_52 (10) = happyGoto action_26
action_52 (12) = happyGoto action_27
action_52 (15) = happyGoto action_28
action_52 (16) = happyGoto action_29
action_52 (17) = happyGoto action_30
action_52 (18) = happyGoto action_31
action_52 (20) = happyGoto action_92
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (39) = happyShift action_6
action_53 (40) = happyShift action_33
action_53 (41) = happyShift action_34
action_53 (42) = happyShift action_35
action_53 (61) = happyShift action_36
action_53 (80) = happyShift action_37
action_53 (84) = happyShift action_38
action_53 (94) = happyShift action_39
action_53 (96) = happyShift action_40
action_53 (8) = happyGoto action_25
action_53 (10) = happyGoto action_26
action_53 (12) = happyGoto action_27
action_53 (15) = happyGoto action_28
action_53 (16) = happyGoto action_29
action_53 (17) = happyGoto action_30
action_53 (18) = happyGoto action_31
action_53 (20) = happyGoto action_91
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (39) = happyShift action_6
action_54 (40) = happyShift action_33
action_54 (41) = happyShift action_34
action_54 (42) = happyShift action_35
action_54 (61) = happyShift action_36
action_54 (80) = happyShift action_37
action_54 (84) = happyShift action_38
action_54 (94) = happyShift action_39
action_54 (96) = happyShift action_40
action_54 (8) = happyGoto action_25
action_54 (10) = happyGoto action_26
action_54 (12) = happyGoto action_27
action_54 (15) = happyGoto action_28
action_54 (16) = happyGoto action_29
action_54 (17) = happyGoto action_30
action_54 (18) = happyGoto action_31
action_54 (20) = happyGoto action_90
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (39) = happyShift action_6
action_55 (40) = happyShift action_33
action_55 (41) = happyShift action_34
action_55 (42) = happyShift action_35
action_55 (43) = happyShift action_89
action_55 (80) = happyShift action_37
action_55 (84) = happyShift action_38
action_55 (8) = happyGoto action_25
action_55 (10) = happyGoto action_26
action_55 (12) = happyGoto action_27
action_55 (17) = happyGoto action_30
action_55 (18) = happyGoto action_88
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (39) = happyShift action_6
action_56 (8) = happyGoto action_87
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (39) = happyShift action_6
action_57 (90) = happyShift action_86
action_57 (8) = happyGoto action_85
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (86) = happyShift action_84
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (85) = happyShift action_83
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (59) = happyShift action_61
action_60 (81) = happyShift action_81
action_60 (87) = happyShift action_82
action_60 (23) = happyGoto action_80
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (39) = happyShift action_6
action_61 (8) = happyGoto action_77
action_61 (28) = happyGoto action_78
action_61 (29) = happyGoto action_79
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (39) = happyShift action_6
action_62 (40) = happyShift action_19
action_62 (41) = happyShift action_20
action_62 (42) = happyShift action_21
action_62 (43) = happyShift action_76
action_62 (80) = happyShift action_22
action_62 (84) = happyShift action_23
action_62 (8) = happyGoto action_13
action_62 (22) = happyGoto action_14
action_62 (24) = happyGoto action_15
action_62 (25) = happyGoto action_16
action_62 (26) = happyGoto action_75
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (39) = happyShift action_6
action_63 (40) = happyShift action_19
action_63 (41) = happyShift action_20
action_63 (42) = happyShift action_21
action_63 (80) = happyShift action_22
action_63 (84) = happyShift action_23
action_63 (96) = happyShift action_24
action_63 (8) = happyGoto action_13
action_63 (22) = happyGoto action_14
action_63 (24) = happyGoto action_15
action_63 (25) = happyGoto action_16
action_63 (26) = happyGoto action_17
action_63 (30) = happyGoto action_74
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (39) = happyShift action_6
action_64 (8) = happyGoto action_11
action_64 (32) = happyGoto action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (95) = happyShift action_71
action_65 (33) = happyGoto action_72
action_65 _ = happyReduce_70

action_66 (95) = happyShift action_71
action_66 (33) = happyGoto action_70
action_66 _ = happyReduce_70

action_67 (86) = happyShift action_69
action_67 (31) = happyGoto action_68
action_67 _ = happyReduce_66

action_68 (95) = happyShift action_71
action_68 (33) = happyGoto action_119
action_68 _ = happyReduce_70

action_69 (39) = happyShift action_6
action_69 (40) = happyShift action_19
action_69 (41) = happyShift action_20
action_69 (42) = happyShift action_21
action_69 (80) = happyShift action_22
action_69 (84) = happyShift action_23
action_69 (96) = happyShift action_24
action_69 (8) = happyGoto action_13
action_69 (22) = happyGoto action_14
action_69 (24) = happyGoto action_15
action_69 (25) = happyGoto action_16
action_69 (26) = happyGoto action_17
action_69 (30) = happyGoto action_118
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (92) = happyShift action_117
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (39) = happyShift action_6
action_71 (8) = happyGoto action_11
action_71 (32) = happyGoto action_116
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (92) = happyShift action_115
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_68

action_74 (59) = happyShift action_61
action_74 _ = happyReduce_61

action_75 _ = happyReduce_54

action_76 _ = happyReduce_63

action_77 (77) = happyShift action_111
action_77 (91) = happyShift action_112
action_77 (92) = happyShift action_113
action_77 (93) = happyShift action_114
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_59

action_79 (87) = happyShift action_110
action_79 _ = happyReduce_65

action_80 (81) = happyShift action_109
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_52

action_82 (39) = happyShift action_6
action_82 (40) = happyShift action_19
action_82 (41) = happyShift action_20
action_82 (42) = happyShift action_21
action_82 (80) = happyShift action_22
action_82 (84) = happyShift action_23
action_82 (96) = happyShift action_24
action_82 (8) = happyGoto action_13
action_82 (22) = happyGoto action_14
action_82 (24) = happyGoto action_15
action_82 (25) = happyGoto action_16
action_82 (26) = happyGoto action_17
action_82 (30) = happyGoto action_108
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_41

action_84 (39) = happyShift action_6
action_84 (40) = happyShift action_19
action_84 (41) = happyShift action_20
action_84 (42) = happyShift action_21
action_84 (80) = happyShift action_22
action_84 (84) = happyShift action_23
action_84 (96) = happyShift action_24
action_84 (8) = happyGoto action_13
action_84 (22) = happyGoto action_14
action_84 (24) = happyGoto action_15
action_84 (25) = happyGoto action_16
action_84 (26) = happyGoto action_17
action_84 (30) = happyGoto action_107
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_14

action_86 (39) = happyShift action_6
action_86 (40) = happyShift action_19
action_86 (41) = happyShift action_20
action_86 (42) = happyShift action_21
action_86 (80) = happyShift action_22
action_86 (84) = happyShift action_23
action_86 (96) = happyShift action_24
action_86 (8) = happyGoto action_13
action_86 (22) = happyGoto action_14
action_86 (24) = happyGoto action_15
action_86 (25) = happyGoto action_16
action_86 (26) = happyGoto action_17
action_86 (30) = happyGoto action_106
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_23

action_88 (94) = happyShift action_56
action_88 _ = happyReduce_16

action_89 _ = happyReduce_17

action_90 _ = happyReduce_37

action_91 _ = happyReduce_36

action_92 (46) = happyShift action_53
action_92 (47) = happyShift action_54
action_92 _ = happyReduce_35

action_93 (46) = happyShift action_53
action_93 (47) = happyShift action_54
action_93 _ = happyReduce_34

action_94 (39) = happyShift action_6
action_94 (40) = happyShift action_33
action_94 (41) = happyShift action_34
action_94 (42) = happyShift action_35
action_94 (61) = happyShift action_36
action_94 (80) = happyShift action_37
action_94 (84) = happyShift action_38
action_94 (94) = happyShift action_39
action_94 (96) = happyShift action_40
action_94 (8) = happyGoto action_25
action_94 (10) = happyGoto action_26
action_94 (12) = happyGoto action_27
action_94 (15) = happyGoto action_28
action_94 (16) = happyGoto action_29
action_94 (17) = happyGoto action_30
action_94 (18) = happyGoto action_31
action_94 (20) = happyGoto action_105
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (81) = happyShift action_104
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_27

action_97 (39) = happyShift action_6
action_97 (40) = happyShift action_33
action_97 (41) = happyShift action_34
action_97 (42) = happyShift action_35
action_97 (61) = happyShift action_36
action_97 (80) = happyShift action_37
action_97 (84) = happyShift action_38
action_97 (94) = happyShift action_39
action_97 (96) = happyShift action_40
action_97 (8) = happyGoto action_25
action_97 (10) = happyGoto action_26
action_97 (12) = happyGoto action_27
action_97 (15) = happyGoto action_28
action_97 (16) = happyGoto action_29
action_97 (17) = happyGoto action_30
action_97 (18) = happyGoto action_31
action_97 (20) = happyGoto action_103
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_9

action_99 (39) = happyShift action_6
action_99 (40) = happyShift action_33
action_99 (41) = happyShift action_34
action_99 (42) = happyShift action_35
action_99 (61) = happyShift action_36
action_99 (80) = happyShift action_37
action_99 (84) = happyShift action_38
action_99 (94) = happyShift action_39
action_99 (96) = happyShift action_40
action_99 (8) = happyGoto action_25
action_99 (10) = happyGoto action_26
action_99 (12) = happyGoto action_27
action_99 (15) = happyGoto action_28
action_99 (16) = happyGoto action_29
action_99 (17) = happyGoto action_30
action_99 (18) = happyGoto action_31
action_99 (20) = happyGoto action_102
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (39) = happyShift action_6
action_100 (40) = happyShift action_33
action_100 (41) = happyShift action_34
action_100 (42) = happyShift action_35
action_100 (61) = happyShift action_36
action_100 (80) = happyShift action_37
action_100 (84) = happyShift action_38
action_100 (94) = happyShift action_39
action_100 (96) = happyShift action_40
action_100 (8) = happyGoto action_25
action_100 (10) = happyGoto action_26
action_100 (12) = happyGoto action_27
action_100 (15) = happyGoto action_28
action_100 (16) = happyGoto action_29
action_100 (17) = happyGoto action_30
action_100 (18) = happyGoto action_31
action_100 (20) = happyGoto action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (44) = happyShift action_51
action_101 (45) = happyShift action_52
action_101 (46) = happyShift action_53
action_101 (47) = happyShift action_54
action_101 _ = happyReduce_31

action_102 (44) = happyShift action_51
action_102 (45) = happyShift action_52
action_102 (46) = happyShift action_53
action_102 (47) = happyShift action_54
action_102 (87) = happyShift action_134
action_102 _ = happyReduce_8

action_103 (44) = happyShift action_51
action_103 (45) = happyShift action_52
action_103 (46) = happyShift action_53
action_103 (47) = happyShift action_54
action_103 (87) = happyShift action_97
action_103 (11) = happyGoto action_133
action_103 _ = happyReduce_10

action_104 _ = happyReduce_12

action_105 (44) = happyShift action_51
action_105 (45) = happyShift action_52
action_105 (46) = happyShift action_53
action_105 (47) = happyShift action_54
action_105 (63) = happyShift action_132
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (59) = happyShift action_61
action_106 _ = happyReduce_62

action_107 (59) = happyShift action_61
action_107 (87) = happyShift action_131
action_107 _ = happyReduce_40

action_108 (59) = happyShift action_61
action_108 (87) = happyShift action_82
action_108 (23) = happyGoto action_130
action_108 _ = happyReduce_42

action_109 _ = happyReduce_44

action_110 (39) = happyShift action_6
action_110 (8) = happyGoto action_77
action_110 (28) = happyGoto action_129
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (39) = happyShift action_6
action_111 (40) = happyShift action_19
action_111 (41) = happyShift action_20
action_111 (42) = happyShift action_21
action_111 (80) = happyShift action_22
action_111 (84) = happyShift action_23
action_111 (96) = happyShift action_24
action_111 (8) = happyGoto action_13
action_111 (22) = happyGoto action_14
action_111 (24) = happyGoto action_15
action_111 (25) = happyGoto action_16
action_111 (26) = happyGoto action_17
action_111 (30) = happyGoto action_128
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (39) = happyShift action_6
action_112 (40) = happyShift action_19
action_112 (41) = happyShift action_20
action_112 (42) = happyShift action_21
action_112 (80) = happyShift action_22
action_112 (84) = happyShift action_23
action_112 (96) = happyShift action_24
action_112 (8) = happyGoto action_13
action_112 (22) = happyGoto action_14
action_112 (24) = happyGoto action_15
action_112 (25) = happyGoto action_16
action_112 (26) = happyGoto action_17
action_112 (30) = happyGoto action_127
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (39) = happyShift action_6
action_113 (40) = happyShift action_19
action_113 (41) = happyShift action_20
action_113 (42) = happyShift action_21
action_113 (80) = happyShift action_22
action_113 (84) = happyShift action_23
action_113 (96) = happyShift action_24
action_113 (8) = happyGoto action_13
action_113 (22) = happyGoto action_14
action_113 (24) = happyGoto action_15
action_113 (25) = happyGoto action_16
action_113 (26) = happyGoto action_17
action_113 (30) = happyGoto action_126
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (39) = happyShift action_6
action_114 (40) = happyShift action_19
action_114 (41) = happyShift action_20
action_114 (42) = happyShift action_21
action_114 (80) = happyShift action_22
action_114 (84) = happyShift action_23
action_114 (96) = happyShift action_24
action_114 (8) = happyGoto action_13
action_114 (22) = happyGoto action_14
action_114 (24) = happyGoto action_15
action_114 (25) = happyGoto action_16
action_114 (26) = happyGoto action_17
action_114 (30) = happyGoto action_125
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (39) = happyShift action_6
action_115 (40) = happyShift action_19
action_115 (41) = happyShift action_20
action_115 (42) = happyShift action_21
action_115 (80) = happyShift action_22
action_115 (84) = happyShift action_23
action_115 (96) = happyShift action_24
action_115 (8) = happyGoto action_13
action_115 (22) = happyGoto action_14
action_115 (24) = happyGoto action_15
action_115 (25) = happyGoto action_16
action_115 (26) = happyGoto action_17
action_115 (30) = happyGoto action_124
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (88) = happyShift action_64
action_116 _ = happyReduce_71

action_117 (39) = happyShift action_6
action_117 (8) = happyGoto action_121
action_117 (34) = happyGoto action_122
action_117 (35) = happyGoto action_123
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (59) = happyShift action_61
action_118 _ = happyReduce_67

action_119 (92) = happyShift action_120
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (39) = happyShift action_6
action_120 (40) = happyShift action_33
action_120 (41) = happyShift action_34
action_120 (42) = happyShift action_35
action_120 (61) = happyShift action_36
action_120 (80) = happyShift action_37
action_120 (84) = happyShift action_38
action_120 (94) = happyShift action_39
action_120 (96) = happyShift action_40
action_120 (8) = happyGoto action_25
action_120 (10) = happyGoto action_26
action_120 (12) = happyGoto action_27
action_120 (15) = happyGoto action_28
action_120 (16) = happyGoto action_29
action_120 (17) = happyGoto action_30
action_120 (18) = happyGoto action_31
action_120 (20) = happyGoto action_140
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (86) = happyShift action_139
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_73

action_123 (93) = happyShift action_138
action_123 _ = happyReduce_76

action_124 (59) = happyShift action_61
action_124 _ = happyReduce_77

action_125 (59) = happyShift action_61
action_125 _ = happyReduce_58

action_126 (59) = happyShift action_61
action_126 _ = happyReduce_55

action_127 (59) = happyShift action_61
action_127 _ = happyReduce_57

action_128 (59) = happyShift action_61
action_128 _ = happyReduce_56

action_129 _ = happyReduce_60

action_130 _ = happyReduce_43

action_131 (39) = happyShift action_6
action_131 (8) = happyGoto action_58
action_131 (21) = happyGoto action_137
action_131 _ = happyReduce_38

action_132 (39) = happyShift action_6
action_132 (40) = happyShift action_33
action_132 (41) = happyShift action_34
action_132 (42) = happyShift action_35
action_132 (61) = happyShift action_36
action_132 (80) = happyShift action_37
action_132 (84) = happyShift action_38
action_132 (94) = happyShift action_39
action_132 (96) = happyShift action_40
action_132 (8) = happyGoto action_25
action_132 (10) = happyGoto action_26
action_132 (12) = happyGoto action_27
action_132 (15) = happyGoto action_28
action_132 (16) = happyGoto action_29
action_132 (17) = happyGoto action_30
action_132 (18) = happyGoto action_31
action_132 (20) = happyGoto action_136
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_11

action_134 (39) = happyShift action_6
action_134 (8) = happyGoto action_47
action_134 (9) = happyGoto action_135
action_134 _ = happyReduce_6

action_135 _ = happyReduce_7

action_136 (44) = happyShift action_51
action_136 (45) = happyShift action_52
action_136 (46) = happyShift action_53
action_136 (47) = happyShift action_54
action_136 _ = happyReduce_18

action_137 _ = happyReduce_39

action_138 (39) = happyShift action_6
action_138 (8) = happyGoto action_121
action_138 (34) = happyGoto action_142
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (39) = happyShift action_6
action_139 (40) = happyShift action_19
action_139 (41) = happyShift action_20
action_139 (42) = happyShift action_21
action_139 (80) = happyShift action_22
action_139 (84) = happyShift action_23
action_139 (96) = happyShift action_24
action_139 (8) = happyGoto action_13
action_139 (22) = happyGoto action_14
action_139 (24) = happyGoto action_15
action_139 (25) = happyGoto action_16
action_139 (26) = happyGoto action_17
action_139 (30) = happyGoto action_141
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (44) = happyShift action_51
action_140 (45) = happyShift action_52
action_140 (46) = happyShift action_53
action_140 (47) = happyShift action_54
action_140 _ = happyReduce_75

action_141 (59) = happyShift action_61
action_141 _ = happyReduce_72

action_142 _ = happyReduce_74

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
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn20  happy_var_3)
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
happyReduction_10 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ([happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 12 happyReduction_12
happyReduction_12 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
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
happyReduction_18 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
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

happyReduce_23 = happySpecReduce_3  18 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (P.term happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_3)
	(HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (P.parenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  19 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn19
		 (P.assignment happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 20 happyReduction_31
happyReduction_31 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (P.lambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  20 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  20 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (P.dotLambda happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  20 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  20 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  20 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  20 happyReduction_37
happyReduction_37 (HappyAbsSyn20  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (P.binaryOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  21 happyReduction_38
happyReduction_38  =  HappyAbsSyn21
		 ([]
	)

happyReduce_39 = happyReduce 5 21 happyReduction_39
happyReduction_39 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((P.keyValPair happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn21
		 ([P.keyValPair happy_var_1 happy_var_3]
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  22 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_3)
	(HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (P.tyRecord happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  23 happyReduction_42
happyReduction_42 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn23
		 ([happy_var_2]
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn23  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2 : happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 24 happyReduction_44
happyReduction_44 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (P.tyTuple (happy_var_2:happy_var_3) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.number (HM.TTerm . HM.LInt) happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.boolean (HM.TTerm . HM.LBool) happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (P.string (HM.TTerm . HM.LString) happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn26
		 (P.tyIdentifier happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  26 happyReduction_52
happyReduction_52 (HappyTerminal happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (P.tyParenthesised happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  27 happyReduction_53
happyReduction_53  =  HappyAbsSyn27
		 ([]
	)

happyReduce_54 = happySpecReduce_2  27 happyReduction_54
happyReduction_54 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  28 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyBinding P.Id happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  28 happyReduction_56
happyReduction_56 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyBinding P.Impl happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  28 happyReduction_57
happyReduction_57 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyBinding P.Subtype happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  28 happyReduction_58
happyReduction_58 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn28
		 (P.tyBinding P.Refinement happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  29 happyReduction_59
happyReduction_59 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  29 happyReduction_60
happyReduction_60 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn30
		 (P.typeArrow happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happyReduce 4 30 happyReduction_62
happyReduction_62 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (P.typeLambda happy_var_2 happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_3  30 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn30
		 (P.typeFnApplication happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  30 happyReduction_64
happyReduction_64 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  30 happyReduction_65
happyReduction_65 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (P.typeClause happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  31 happyReduction_66
happyReduction_66  =  HappyAbsSyn31
		 (Nothing
	)

happyReduce_67 = happySpecReduce_2  31 happyReduction_67
happyReduction_67 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (Just happy_var_2
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  32 happyReduction_68
happyReduction_68 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (P.kindArrow happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn32
		 (P.kindId happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  33 happyReduction_70
happyReduction_70  =  HappyAbsSyn33
		 (Nothing
	)

happyReduce_71 = happySpecReduce_2  33 happyReduction_71
happyReduction_71 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (Just happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  34 happyReduction_72
happyReduction_72 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn34
		 (P.dataExpr happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  35 happyReduction_73
happyReduction_73 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 ([happy_var_1]
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  35 happyReduction_74
happyReduction_74 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 6 36 happyReduction_75
happyReduction_75 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (P.letdec happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 5 36 happyReduction_76
happyReduction_76 ((HappyAbsSyn35  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (P.dataType happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 5 36 happyReduction_77
happyReduction_77 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (P.typeDef happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_1  37 happyReduction_78
happyReduction_78 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  37 happyReduction_79
happyReduction_79 (HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  38 happyReduction_80
happyReduction_80 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 (P.script happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyNewToken action sts stk
	= P.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken T.EOF _ -> action 99 99 tk (HappyState action) sts stk;
	L.RangedToken (T.Id _) _ -> cont 39;
	L.RangedToken (T.Number _) _ -> cont 40;
	L.RangedToken (T.String _) _ -> cont 41;
	L.RangedToken (T.Boolean _) _ -> cont 42;
	L.RangedToken (T.Operator "!") _ -> cont 43;
	L.RangedToken (T.Operator "+") _ -> cont 44;
	L.RangedToken (T.Operator "-") _ -> cont 45;
	L.RangedToken (T.Operator "*") _ -> cont 46;
	L.RangedToken (T.Operator "/") _ -> cont 47;
	L.RangedToken (T.Operator "==") _ -> cont 48;
	L.RangedToken (T.Operator "!=") _ -> cont 49;
	L.RangedToken (T.Operator "<") _ -> cont 50;
	L.RangedToken (T.Operator "<=") _ -> cont 51;
	L.RangedToken (T.Operator ">") _ -> cont 52;
	L.RangedToken (T.Operator ">=") _ -> cont 53;
	L.RangedToken (T.Operator "||") _ -> cont 54;
	L.RangedToken (T.Operator "&&") _ -> cont 55;
	L.RangedToken (T.Operator _) _ -> cont 56;
	L.RangedToken T.Let _ -> cont 57;
	L.RangedToken T.In _ -> cont 58;
	L.RangedToken T.Where _ -> cont 59;
	L.RangedToken T.With _ -> cont 60;
	L.RangedToken T.If _ -> cont 61;
	L.RangedToken T.Then _ -> cont 62;
	L.RangedToken T.Else _ -> cont 63;
	L.RangedToken T.Match _ -> cont 64;
	L.RangedToken T.Return _ -> cont 65;
	L.RangedToken T.Data _ -> cont 66;
	L.RangedToken T.Type _ -> cont 67;
	L.RangedToken T.Alias _ -> cont 68;
	L.RangedToken T.Kind _ -> cont 69;
	L.RangedToken T.Forall _ -> cont 70;
	L.RangedToken T.Exists _ -> cont 71;
	L.RangedToken T.Proof _ -> cont 72;
	L.RangedToken T.Infer _ -> cont 73;
	L.RangedToken T.Protocol _ -> cont 74;
	L.RangedToken T.Interface _ -> cont 75;
	L.RangedToken T.Instance _ -> cont 76;
	L.RangedToken T.Implements _ -> cont 77;
	L.RangedToken T.Module _ -> cont 78;
	L.RangedToken T.Import _ -> cont 79;
	L.RangedToken T.LParen _ -> cont 80;
	L.RangedToken T.RParen _ -> cont 81;
	L.RangedToken T.LBrack _ -> cont 82;
	L.RangedToken T.RBrack _ -> cont 83;
	L.RangedToken T.LCurly _ -> cont 84;
	L.RangedToken T.RCurly _ -> cont 85;
	L.RangedToken T.Colon _ -> cont 86;
	L.RangedToken T.Comma _ -> cont 87;
	L.RangedToken T.Arrow _ -> cont 88;
	L.RangedToken T.BackArrow _ -> cont 89;
	L.RangedToken T.FatArrow _ -> cont 90;
	L.RangedToken T.PipeArrow _ -> cont 91;
	L.RangedToken T.Equals _ -> cont 92;
	L.RangedToken T.Pipe _ -> cont 93;
	L.RangedToken T.Dot _ -> cont 94;
	L.RangedToken T.Section _ -> cont 95;
	L.RangedToken T.BackSlash _ -> cont 96;
	L.RangedToken T.Newline _ -> cont 97;
	L.RangedToken T.EOF _ -> cont 98;
	_ -> happyError' (tk, [])
	})

happyError_ explist 99 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaType = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaKind = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

parseSagaDec = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn36 z -> happyReturn z; _other -> notHappyAtAll })

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
