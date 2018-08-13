\ наблюдение работы методов оптимизазии			Михаил Максимов


: DoTDTST
 BASE @ >R HEX
	DUP>R
 	1 AND 0= 
 IF   CR SOURCE TYPE
 THEN   
     BEGIN  CR R@ .  2DUP U. U.   :-SET U.  DUP 1- @ U.
            :-SET TOP6 UMAX

             BEGIN  DUP DP @ U<
             WHILE  CR  MINST
             REPEAT DROP CR

           KEY 0x20 OR
	DUP	[CHAR] q = THROW
	DUP	[CHAR] g = IF ['] DROP TO TDTST THEN
		[CHAR] d <> DUP 0=
           IF  DROP POSTPONE [  S" DDD.F" INCLUDED ] KEY DROP 
                    \ S" [  ] " EVALUATE
               FALSE
           THEN   
     UNTIL
  RDROP
 R> BASE ! 
 ;

\ [ ' DoTDTST TO TDTST ]
