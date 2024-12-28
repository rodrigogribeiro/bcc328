{-# OPTIONS_GHC -w #-}
module Imp.Frontend.Parser.Parser (impParser) where

import Imp.Frontend.Lexer.Lexer
import Imp.Syntax.Syntax
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,164) ([0,128,0,16384,0,0,0,0,0,0,0,0,16384,2126,304,0,0,16384,0,0,8,0,0,0,0,2,0,768,4100,32771,513,392,192,50177,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,6120,0,0,0,32864,25088,12288,64,49,0,0,0,0,0,32896,95,0,12226,0,256,0,256,0,24576,128,98,8192,764,0,16,0,4108,3136,0,0,0,0,0,384,34818,49153,256,196,32864,25088,12288,64,49,8216,6272,3072,16400,12,2054,1568,0,16,0,0,0,0,3058,0,0,0,0,0,0,4,0,0,63,0,1920,0,49152,11,0,0,0,40960,0,0,64,0,14336,0,0,382,0,0,0,0,0,0,16,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Program","CodeBlock","StmtList","Stmt","Id","PTy","Init","Exp","id","num","':='","'read'","'print'","'if'","'then'","'else'","'while'","';'","'('","')'","'{'","'}'","'+'","'*'","'-'","'/'","'=='","'<'","'!'","'&&'","'int'","'bool'","'true'","'false'","'skip'","%eof"]
        bit_start = st Prelude.* 39
        bit_end = (st Prelude.+ 1) Prelude.* 39
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..38]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (24) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (24) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyGoto action_5
action_3 _ = happyReduce_4

action_4 (39) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (12) = happyShift action_9
action_5 (15) = happyShift action_10
action_5 (16) = happyShift action_11
action_5 (17) = happyShift action_12
action_5 (20) = happyShift action_13
action_5 (25) = happyShift action_14
action_5 (34) = happyShift action_15
action_5 (35) = happyShift action_16
action_5 (38) = happyShift action_17
action_5 (7) = happyGoto action_6
action_5 (8) = happyGoto action_7
action_5 (9) = happyGoto action_8
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_3

action_7 (14) = happyShift action_30
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (12) = happyShift action_9
action_8 (8) = happyGoto action_29
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_12

action_10 (12) = happyShift action_9
action_10 (8) = happyGoto action_28
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (12) = happyShift action_9
action_11 (13) = happyShift action_21
action_11 (22) = happyShift action_22
action_11 (32) = happyShift action_23
action_11 (36) = happyShift action_24
action_11 (37) = happyShift action_25
action_11 (8) = happyGoto action_19
action_11 (11) = happyGoto action_27
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (12) = happyShift action_9
action_12 (13) = happyShift action_21
action_12 (22) = happyShift action_22
action_12 (32) = happyShift action_23
action_12 (36) = happyShift action_24
action_12 (37) = happyShift action_25
action_12 (8) = happyGoto action_19
action_12 (11) = happyGoto action_26
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (12) = happyShift action_9
action_13 (13) = happyShift action_21
action_13 (22) = happyShift action_22
action_13 (32) = happyShift action_23
action_13 (36) = happyShift action_24
action_13 (37) = happyShift action_25
action_13 (8) = happyGoto action_19
action_13 (11) = happyGoto action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_2

action_15 _ = happyReduce_13

action_16 _ = happyReduce_14

action_17 (21) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_5

action_19 _ = happyReduce_20

action_20 (24) = happyShift action_3
action_20 (26) = happyShift action_36
action_20 (27) = happyShift action_37
action_20 (28) = happyShift action_38
action_20 (29) = happyShift action_39
action_20 (30) = happyShift action_40
action_20 (31) = happyShift action_41
action_20 (33) = happyShift action_42
action_20 (5) = happyGoto action_46
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_17

action_22 (12) = happyShift action_9
action_22 (13) = happyShift action_21
action_22 (22) = happyShift action_22
action_22 (32) = happyShift action_23
action_22 (36) = happyShift action_24
action_22 (37) = happyShift action_25
action_22 (8) = happyGoto action_19
action_22 (11) = happyGoto action_45
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (12) = happyShift action_9
action_23 (13) = happyShift action_21
action_23 (22) = happyShift action_22
action_23 (32) = happyShift action_23
action_23 (36) = happyShift action_24
action_23 (37) = happyShift action_25
action_23 (8) = happyGoto action_19
action_23 (11) = happyGoto action_44
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_18

action_25 _ = happyReduce_19

action_26 (18) = happyShift action_43
action_26 (26) = happyShift action_36
action_26 (27) = happyShift action_37
action_26 (28) = happyShift action_38
action_26 (29) = happyShift action_39
action_26 (30) = happyShift action_40
action_26 (31) = happyShift action_41
action_26 (33) = happyShift action_42
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (21) = happyShift action_35
action_27 (26) = happyShift action_36
action_27 (27) = happyShift action_37
action_27 (28) = happyShift action_38
action_27 (29) = happyShift action_39
action_27 (30) = happyShift action_40
action_27 (31) = happyShift action_41
action_27 (33) = happyShift action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_34
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_33
action_29 (10) = happyGoto action_32
action_29 _ = happyReduce_16

action_30 (12) = happyShift action_9
action_30 (13) = happyShift action_21
action_30 (22) = happyShift action_22
action_30 (32) = happyShift action_23
action_30 (36) = happyShift action_24
action_30 (37) = happyShift action_25
action_30 (8) = happyGoto action_19
action_30 (11) = happyGoto action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_58
action_31 (26) = happyShift action_36
action_31 (27) = happyShift action_37
action_31 (28) = happyShift action_38
action_31 (29) = happyShift action_39
action_31 (30) = happyShift action_40
action_31 (31) = happyShift action_41
action_31 (33) = happyShift action_42
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_57
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (12) = happyShift action_9
action_33 (13) = happyShift action_21
action_33 (22) = happyShift action_22
action_33 (32) = happyShift action_23
action_33 (36) = happyShift action_24
action_33 (37) = happyShift action_25
action_33 (8) = happyGoto action_19
action_33 (11) = happyGoto action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_10

action_35 _ = happyReduce_9

action_36 (12) = happyShift action_9
action_36 (13) = happyShift action_21
action_36 (22) = happyShift action_22
action_36 (32) = happyShift action_23
action_36 (36) = happyShift action_24
action_36 (37) = happyShift action_25
action_36 (8) = happyGoto action_19
action_36 (11) = happyGoto action_55
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (12) = happyShift action_9
action_37 (13) = happyShift action_21
action_37 (22) = happyShift action_22
action_37 (32) = happyShift action_23
action_37 (36) = happyShift action_24
action_37 (37) = happyShift action_25
action_37 (8) = happyGoto action_19
action_37 (11) = happyGoto action_54
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (12) = happyShift action_9
action_38 (13) = happyShift action_21
action_38 (22) = happyShift action_22
action_38 (32) = happyShift action_23
action_38 (36) = happyShift action_24
action_38 (37) = happyShift action_25
action_38 (8) = happyGoto action_19
action_38 (11) = happyGoto action_53
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (12) = happyShift action_9
action_39 (13) = happyShift action_21
action_39 (22) = happyShift action_22
action_39 (32) = happyShift action_23
action_39 (36) = happyShift action_24
action_39 (37) = happyShift action_25
action_39 (8) = happyGoto action_19
action_39 (11) = happyGoto action_52
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (12) = happyShift action_9
action_40 (13) = happyShift action_21
action_40 (22) = happyShift action_22
action_40 (32) = happyShift action_23
action_40 (36) = happyShift action_24
action_40 (37) = happyShift action_25
action_40 (8) = happyGoto action_19
action_40 (11) = happyGoto action_51
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (12) = happyShift action_9
action_41 (13) = happyShift action_21
action_41 (22) = happyShift action_22
action_41 (32) = happyShift action_23
action_41 (36) = happyShift action_24
action_41 (37) = happyShift action_25
action_41 (8) = happyGoto action_19
action_41 (11) = happyGoto action_50
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_9
action_42 (13) = happyShift action_21
action_42 (22) = happyShift action_22
action_42 (32) = happyShift action_23
action_42 (36) = happyShift action_24
action_42 (37) = happyShift action_25
action_42 (8) = happyGoto action_19
action_42 (11) = happyGoto action_49
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (24) = happyShift action_3
action_43 (5) = happyGoto action_48
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_28

action_45 (23) = happyShift action_47
action_45 (26) = happyShift action_36
action_45 (27) = happyShift action_37
action_45 (28) = happyShift action_38
action_45 (29) = happyShift action_39
action_45 (30) = happyShift action_40
action_45 (31) = happyShift action_41
action_45 (33) = happyShift action_42
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_11

action_47 _ = happyReduce_29

action_48 (19) = happyShift action_59
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (26) = happyShift action_36
action_49 (27) = happyShift action_37
action_49 (28) = happyShift action_38
action_49 (29) = happyShift action_39
action_49 (30) = happyShift action_40
action_49 (31) = happyShift action_41
action_49 _ = happyReduce_27

action_50 (26) = happyShift action_36
action_50 (27) = happyShift action_37
action_50 (28) = happyShift action_38
action_50 (29) = happyShift action_39
action_50 (31) = happyFail []
action_50 _ = happyReduce_25

action_51 (26) = happyShift action_36
action_51 (27) = happyShift action_37
action_51 (28) = happyShift action_38
action_51 (29) = happyShift action_39
action_51 (30) = happyFail []
action_51 (31) = happyShift action_41
action_51 _ = happyReduce_26

action_52 _ = happyReduce_24

action_53 (27) = happyShift action_37
action_53 (29) = happyShift action_39
action_53 _ = happyReduce_23

action_54 (29) = happyShift action_39
action_54 _ = happyReduce_22

action_55 (27) = happyShift action_37
action_55 (28) = happyShift action_38
action_55 (29) = happyShift action_39
action_55 _ = happyReduce_21

action_56 (26) = happyShift action_36
action_56 (27) = happyShift action_37
action_56 (28) = happyShift action_38
action_56 (29) = happyShift action_39
action_56 (30) = happyShift action_40
action_56 (31) = happyShift action_41
action_56 (33) = happyShift action_42
action_56 _ = happyReduce_15

action_57 _ = happyReduce_7

action_58 _ = happyReduce_6

action_59 (24) = happyShift action_3
action_59 (5) = happyGoto action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_8

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Block (reverse happy_var_2)
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn7
		 (Skip
	)

happyReduce_6 = happyReduce 4 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 := happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Print happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (SRead happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (While happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (TInt
	)

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn9
		 (TBool
	)

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Just happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  10 happyReduction_16
happyReduction_16  =  HappyAbsSyn10
		 (Nothing
	)

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn11
		 (EValue (EInt happy_var_1)
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn11
		 (EValue (EBool True)
	)

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn11
		 (EValue (EBool False)
	)

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (EVar happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :+: happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :*: happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :-: happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :/: happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :<: happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :==: happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :&: happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  11 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (ENot happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  11 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 39 39 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token _ (TIdent happy_dollar_dollar) -> cont 12;
	Token _ (TNumber happy_dollar_dollar) -> cont 13;
	Token _ TAssign -> cont 14;
	Token _ TRead -> cont 15;
	Token _ TPrint -> cont 16;
	Token _ TIf -> cont 17;
	Token _ TThen -> cont 18;
	Token _ TElse -> cont 19;
	Token _ TWhile -> cont 20;
	Token _ TSemi -> cont 21;
	Token _ TLParen -> cont 22;
	Token _ TRParen -> cont 23;
	Token _ TLBrace -> cont 24;
	Token _ TRBrace -> cont 25;
	Token _ TPlus -> cont 26;
	Token _ TTimes -> cont 27;
	Token _ TMinus -> cont 28;
	Token _ TDiv -> cont 29;
	Token _ TEq -> cont 30;
	Token _ TLt -> cont 31;
	Token _ TNot -> cont 32;
	Token _ TAnd -> cont 33;
	Token _ TTInt -> cont 34;
	Token _ TTBool -> cont 35;
	Token _ TTrue -> cont 36;
	Token _ TFalse -> cont 37;
	Token _ TSkip -> cont 38;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 39 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error "Parse error!"
parseError (t : _) = error $ "Parse error " ++ (show t)


impParser :: String -> Program
impParser = parser . lexer
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
