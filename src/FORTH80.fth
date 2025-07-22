 6 LIST
 0 P ( ************************************************************) 
 1 P (                                                             ) 
 2 P (                     F  O  R  T  H  8  0                     )
 3 P (                                                             )
 4 P (                  A FORTH language processor                 )
 5 P (                 conforming FORTH-79 Standard                )
 6 P (          ( Requires a dedicated "virtual machine" )
 7 P (                                                             )
 8 P (                    S E L F - H O S T E D                    )
 9 P (                                                             )
10 P (                        Version 0.8.3                        )
11 P (                                        (C  2023-2024 Tsugu  )
12 P (             This software is released under the             )
13 P (                         MIT License.                        )
14 P (      (https://opensource.org/licenses/mit-license.php)
15 P ( ************************************************************)

7 LIST
 0 P ( FORTH80 restructuring -- Start Up )
 1 P ( Note! This screen should be input directly, not be loaded.  )
 2 P ( The following program repositions and rebuilds the FORTH    )
 3 P ( dictionary to the START address and late.                   )
 4 P FORTH DEFINITIONS  1 WARNING !
 5 P VARIABLE START HEX 3000 START !
 6 P 8 LOAD COLD
 7 P ( variables update -- It doesn't work on load!! )
 8 P FIND VARIABLE 10 + DUP DUP
 9 P FIRST ' USE ! ' USE CFA !
10 P FIRST ' PREV ! ' PREV CFA !
11 P 0 ' DISK-ERROR ! ' DISK-ERROR CFA !
12 P  
13 P  
14 P  
15 P  

8 LIST
 0 P ( Preparation #1 )
 1 P HEX
 2 P 0 CONSTANT NOP ( NOP's opcode in FORTH80's VM )
 3 P 1 CONSTANT JMP ( JMP's opcode in FORTH80's VM )
 4 P VARIABLE CLD                  VARIABLE WRM
 5 P VARIABLE _UVR                 VARIABLE UVREND
 6 P VARIABLE CTST
 7 P VARIABLE CIN                  VARIABLE STIN
 8 P VARIABLE COUT                 VARIABLE POUT
 9 P VARIABLE READ                 VARIABLE WRITE
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

9 LIST
 0 P ( Preparation #2 )
 1 P DECIMAL
 2 P : th_patch ( n --- a : th_patch[n], n = 1...20 )
 3 P   1 MAX 20 MIN 2* [ HERE 6 + ] LITERAL + ;
 4 P 20 ALLOT
 5 P  
 6 P HEX
 7 P  40 CONSTANT UVs       ( USER VARIABLEs area size )
 8 P 0A0 CONSTANT STACKSIZE ( stack size )
 9 P  
10 P VARIABLE null's_NFA
11 P  
12 P  
13 P  
14 P  
15 P -->

10 LIST
 0 P ( Preparation #2 )
 1 P DECIMAL
 2 P ( drive size )            720 ( KB ) CONSTANT DRSIZE
 3 P ( bytes per sector )             512 CONSTANT BPS
 4 P ( the max bytes of tokens )   32 2 + CONSTANT MAXTOKEN
 5 P ( word buffer size [> C/L] )  64 6 + CONSTANT WORDBUFSIZE
 6 P ( PAD size )                  80 1 + CONSTANT PADSIZE
 7 P ( temporary buffer area size )
 8 P                WORDBUFSIZE PADSIZE + CONSTANT TMPBUFSIZE
 9 P  
10 P ( the Inner Words for Inner Interpreter )
11 P  ( WPUSH ( --- W AX ; W @  AX @  NEXT )
12 P  ( APUSH ( --- AX ; AX @  NEXT )
13 P  ( NEXT  ( --- ; IP @ @ BX !  2 IP +!  NEXT1 )
14 P  ( NEXT1 ( --- ; BX @ W !  1 W +!  BX @ @ PC ! )
15 P -->

11 LIST
 0 P ( Warm Start & Cold Start: CLD WRM )
 1 P HEX
 2 P      NOP 0000 !
 3 P      JMP 0001 !
 4 P     0008 0002 !
 5 P      NOP 0004 !
 6 P      JMP 0005 !
 7 P     000C 0006 !
 8 P       02 0008 ! ( --- ; CLD 1+ IP !  S0 R0  NEXT )
 9 P ( COLD @ 0009 ! )
10 P       03 000B ! ( --- ; WRM 1+ IP !  NEXT )
11 P ( WARM @ 000C ! )
12 P  
13 P  0008 CLD !  000B WRM !
15 P ." 11 " -->

12 LIST
 0 P ( UVR +0 .. +26 -- User Variables Region for initializing )
 1 P HEX 000E _UVR !
 2 P    0 000E ! ( +0  release No. )
 3 P    8 0010 ! ( +2  revision No. )
 4 P 0301 0012 ! ( +4  user version [0..255]*256+[0..25] )
 5 P    ( 0014 ) ( +6  S0 )
 6 P    ( 0016 ) ( +8  R0 )
 7 P    ( 0018 ) ( +10 TIB )
 8 P   1F 001A ! ( +12 WIDTH )
 9 P    0 001C ! ( +14 WARNING )
10 P    ( 001E ) ( +16 FENCE )
11 P    ( 0020 ) ( +18 DP )
12 P    ( 0022 ) ( +20 VOC-LINK )
13 P    0 0024 ! ( +22 BLK )
14 P    0 0026 ! ( +24 >IN )
15 P    0 0028 ! ( +26 OUT )  ." 12 " -->

13 LIST
 0 P ( UVR +28 .. +50 )
 1 P    0 002A ! ( +28 SCR )
 2 P    0 002C ! ( +30 OFFSET )
 3 P    ( 002E ) ( +32 CONTEXT )
 4 P    ( 0030 ) ( +34 CURRENT )
 5 P    0 0032 ! ( +36 STATE )
 6 P   0A 0034 ! ( +38 BASE )
 7 P   -1 0036 ! ( +40 DPL )
 8 P    0 0038 ! ( +42 FLD )
 9 P    0 003A ! ( +44 CSP )
10 P    0 003C ! ( +46 R# )
11 P    0 003E ! ( +48 HLD )
12 P    0 0040 ! ( +50 PFLAG )
13 P    0 0042 ! ( +52 UTF-8 )
14 P    1 0044 ! ( +54 ECHO )
15 P    0 0046 ! ( +56 STDIN )  ." 13 " -->

14 LIST
 0 P ( UVR -- UVREND )
 1 P 0046 UVREND !
 2 P 0048 START @ MAX DP !
 3 P    0 FENCE !
 4 P  
 5 P  
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 14 " -->

15 LIST
 0 P ( Inner Words: CTST CIN STIN COUT POUT READ WRITE )
 1 P DECIMAL
 2 P ( --- 0 / 1 [hit] ; take a type-state of a keyboard )
 3 P HERE CTST ! HERE 2+ , 4 C,
 4 P ( --- c ; input a character from a keyboard )
 5 P HERE CIN ! HERE 2+ , 5 C,
 6 P ( --- ; input a character from "stdin" )
 7 P HERE STIN ! HERE 2+ , 6 C,
 8 P ( c --- ; output the character c [to "stderr"] )
 9 P HERE COUT ! HERE 2+ , 7 C,
10 P ( c --- ; printout the character c to "stdout" )
11 P HERE POUT ! HERE 2+ , 8 C,
12 P ( sector_No address drive_No --- 0 / 1 [error] ; load )
13 P HERE READ ! HERE 2+ , 9 C,
14 P ( sector_No address drive_No --- 0 / 1 [error] ; save )
15 P HERE WRITE ! HERE 2+ , 10 C,  ." 15 " -->

16 LIST
 0 P ( Nucleus Words: LIT EXECUTE BRANCH 0BRANCH (LOOP (+LOOP  )
 1 P ( --- n ; IP @ @ AX !  2 IP +!  APUSH )
 2 P CODE LIT 11 C, END-CODE
 3 P ( a --- ; BX @  NEXT1 )
 4 P CODE EXECUTE 12 C, END-CODE
 5 P ( --- ; IP @ @ IP +1  NEXT )
 6 P CODE BRANCH 13 C, END-CODE
 7 P ( f --- ; IF BRANCH ELSE 2 IP +! NEXT THEN )
 8 P CODE 0BRANCH 14 C, END-CODE
 9 P ( --- ; RP@ @ 1+ RP @ !  RP@ @ RP 2+ @ @ <      )
10 P       ( IF BRAN ELSE 4 RP +! 2 IP +! THEN  NEXT )
11 P CODE (LOOP) 15 C, END-CODE
12 P ( n --- ; RP @ @ + RP @ !  RP @ @ RP 2+ @ @ <     )
13 P         ( IF BRAN ELSE 4 RP +! 2 IP +! THEN  NEXT )
14 P CODE (+LOOP) 16 C, END-CODE
15 P -->

17 LIST
 0 P ( Nucleus Words: (DO  AND OR XOR SP@ SP0 RP@ )
 1 P ( n1 n2 --- ; SWAP >R >R  NEXT )
 2 P CODE (DO) 17 C, END-CODE
 3 P ( n1 n2 --- n1+n2 )
 4 P CODE AND 18 C, END-CODE
 5 P ( n1 n2 --- n1|n2 )
 6 P CODE OR 19 C, END-CODE
 7 P ( n1 n2 --- n1^n2 )
 8 P CODE XOR 20 C, END-CODE
 9 P ( --- [SP] ; SP @ AX !  APUSH )
10 P CODE SP@ 21 C, END-CODE
11 P ( all --- ; UP 6 + @ SP !  NEXT )
12 P CODE SP! 22 C, END-CODE
13 P ( --- [RP] ; RP @ AX !  APUSH )
14 P CODE RP@ 23 C, END-CODE
15 P -->

18 LIST
 0 P ( Nucleus Words: RP0 ;S >R R> R@ 0= 0< )
 1 P ( --- ; UP 8 + @ RP !  NEXT)
 2 P CODE RP! 24 C, END-CODE
 3 P ( --- ; RP @ @ IP !  2 IP +!  NEXT )
 4 P CODE ;S 25 C, END-CODE
 5 P ( n --- ; -2 RP +!  RP @ !  NEXT )
 6 P CODE >R 26 C, END-CODE
 7 P ( --- n ; RP @ @ AX !  2 RP +!  APUSH )
 8 P CODE R> 27 C, END-CODE
 9 P ( --- n ; RP @ @ AX !  APUSH )
10 P CODE R@ 28 C, END-CODE
11 P ( n --- n==0 )
12 P CODE 0= 29 C, END-CODE
13 P ( n --- n<0 )
14 P CODE 0< 30 C, END-CODE
15 P -->

19 LIST
 0 P ( Nucleus Words: + - D+ D- OVER DROP SWAP )
 1 P ( n1 n2 --- n1+n2 )
 2 P CODE + 31 C, END-CODE
 3 P ( n1 n2 --- n1-n2 )
 4 P CODE - 32 C, END-CODE
 5 P ( d1 d2 --- d1+d2 )
 6 P CODE D+ 33 C, END-CODE
 7 P ( d1 d2 --- d1-d2 )
 8 P CODE D- 34 C, END-CODE
 9 P ( n1 n2 --- n1 n2 n1 )
10 P CODE OVER 35 C, END-CODE
11 P ( n --- )
12 P CODE DROP 36 C, END-CODE
13 P ( n1 n2 --- n2 n1 )
14 P CODE SWAP 37 C, END-CODE 
15 P -->

20 LIST
 0 P ( Nucleus Words: DUP ROT U* U/ 2/ TOGGLE @ )
 1 P ( n --- n n )
 2 P CODE DUP 38 C, END-CODE
 3 P ( n1 n2 n3 --- n2 n3 n1 )
 4 P CODE ROT 39 C, END-CODE
 5 P ( u1 u2 --- u1*u2 )
 6 P CODE U* 40 C, END-CODE
 7 P ( ud u1 --- ud%u1 ud/u2 / -1 -1 ; 2DUP < 0= IF 2DROP DROP )
 8 P                                 ( -1 -1 ELSE ...          )
 9 P CODE U/ 41 C, END-CODE
10 P ( n --- n/2 )
11 P CODE 2/ 42 C, END-CODE
12 P ( a b --- ; OVER C@ ^ SWAP ! )
13 P CODE TOGGLE 43 C, END-CODE
14 P ( a --- n ; short *shp; shp = a; n = *shp; )
15 P CODE @ 44 C, END-CODE  -->

21 LIST
 0 P ( Nucleus Words: ! C! CMOVE <CMOVE FILL BYE )
 1 P ( n a --- ; short *shp; shp = a; *shp = n; )
 2 P CODE ! 45 C, END-CODE
 3 P ( c a --- ; char *chp; chp = a; *chp = c; )
 4 P CODE C! 46 C, END-CODE
 5 P ( a1 a2 n --- ; ROT SWAP OVER + SWAP         )
 6 P               ( DO I C@ OVER C! 1+ LOOP DROP )
 7 P CODE CMOVE 47 C, END-CODE
 8 P ( a1 a2 n --- ; SWAP OVER + 1- <ROT OVER + 1-    )
 9 P                      ( DO I C@ OVER C! 1- -1 +LOOP DROP )
10 P CODE <CMOVE 48 C, END-CODE
11 P ( a n b --- : <ROT OVER + SWAP DO DUP I C! LOOP DROP )
12 P CODE FILL 49 C, END-CODE
13 P ( --- ; exit FORTH )
14 P CODE BYE ( --- ) 53 C, END-CODE
15 P -->

22 LIST
 0 P ( Unique Nucleus Words: POPEN PCLOSE SET-INPUT SET-OUTPUT )
 1 P ( a1 a2 --- ud1 ud2 ; call popen(a1+1,a2+1  in C-lang )
 2 P CODE POPEN 54 C, END-CODE
 3 P ( ud1 ud2 --- d ; call pclose(ud2<<32|ud1  in C-lang )
 4 P CODE PCLOSE 55 C, END-CODE
 5 P ( ud1 ud2 --- ; Set ud2<<32|ud1 to input stream )
 6 P CODE SET-INPUT 56 C, END-CODE ( if NULL, input is default )
 7 P ( ud1 ud2 --- ; Set ud2<<32|ud1 to output stream )
 8 P CODE SET-OUTPUT 57 C, END-CODE ( if NULL, output is default )
 9 P  
10 P ( countermeasures for LIT embedded by old interpreters )
11 P FIND LIT FIND LITERAL 6 ( 6-th cell ) 2* + !
12 P  
13 P  
14 P  
15 P -->

23 LIST
 0 P ( * Umbilical Words: : ; (;CODE  DOES> CREATE )
 1 P DECIMAL
 2 P : : ( --- ; : <name> )
 3 P   ?EXEC !CSP CURRENT @ CONTEXT ! (CREATE) ]
 4 P   ;CODE 50 C, END-CODE IMMEDIATE
 5 P HERE 1- LATEST PFA CFA ! ( update CFA )
 6 P : (;CODE) ( --- ) R> LATEST PFA CFA ! ;
 7 P : ; ( --- ) ?CSP COMPILE ;S SMUDGE [COMPILE] [ ; IMMEDIATE
 8 P : DOES> ( --- a ) COMPILE (;CODE)
 9 P   [ JMP @ ] LITERAL C, [ HERE 8 + ] LITERAL , ;
10 P   51 C, IMMEDIATE
11 P  (   ^^ <-- HERE 8 + )
12 P : CREATE ( --- ; CREATE <name> )
13 P   (CREATE) SMUDGE ;CODE 52 C, END-CODE
14 P  
15 P -->

24 LIST
 0 P ( * Umbilical Words: IMMEDIATE COMPILE [COMPILE] [ ] )
 1 P HEX
 2 P : IMMEDIATE ( --- ) LATEST 40 ( 0b 100 0000 ) TOGGLE ;
 3 P : COMPILE ( --- ; COMPILE <word> )
 4 P   ?COMP R> DUP 2+ >R @ , ;
 5 P : [COMPILE] ( --- ; [COMPILE] <word> )
 6 P   FIND ?DUP 0= 0 ?ERROR , ; IMMEDIATE
 7 P : [ ( --- ) 0 STATE ! ; IMMEDIATE
 8 P : ] ( --- ) 0C0 ( 0b 1100 0000 ) STATE ! ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

25 LIST
 0 P ( * Umbilical Words: IF ELSE THEN DO LOOP +LOOP EXIT )
 1 P : IF ( --- a 1 ) ?COMP COMPILE 0BRANCH >MARK 1 ; IMMEDIATE
 2 P : ELSE ( a1 1 --- a2 1 ) 1 ?PAIRS
 3 P   COMPILE BRANCH >MARK SWAP >RESOLVE 1 ; IMMEDIATE
 4 P : THEN ( a 1 --- ) 1 ?PAIRS >RESOLVE ; IMMEDIATE
 5 P : DO ( --- a 2 : compiling ; n1 n2 --- ; execution )
 6 P   COMPILE (DO) <MARK 2 ; IMMEDIATE
 7 P : LOOP ( a 2 --- : compiling ; --- : execution )
 8 P   2 ?PAIRS COMPILE (LOOP) <RESOLVE ; IMMEDIATE
 9 P : +LOOP ( a 2 --- : compiling ; --- : execution )
10 P   2 ?PAIRS COMPILE (+LOOP) <RESOLVE ; IMMEDIATE
11 P : EXIT ( --- ) ?COMP COMPILE ;S ; IMMEDIATE
12 P  
13 P  
14 P  
15 P -->

26 LIST
 0 P ( * Umbilical Words: BEGIN AGAIN UNTIL WHILE REPEAT )
 1 P : BEGIN ( --- a 3 ) ?COMP <MARK 3 ; IMMEDIATE
 2 P : AGAIN ( a 3 --- ) 3 ?PAIRS
 3 P   COMPILE BRANCH <RESOLVE ; IMMEDIATE
 4 P : UNTIL ( a 3 --- ) 3 ?PAIRS
 5 P   COMPILE 0BRANCH <RESOLVE ; IMMEDIATE
 6 P : WHILE ( a1 3 --- a2 4 ) 3 ?PAIRS
 7 P   COMPILE 0BRANCH >MARK 4 ; IMMEDIATE
 8 P : REPEAT ( a 4 --- ) 4 ?PAIRS
 9 P   COMPILE BRANCH SWAP <RESOLVE >RESOLVE ; IMMEDIATE
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

27 LIST
 0 P ( * Umbilical Words: (."  ." ( LOAD --> )
 1 P HEX
 2 P : (.") ( --- ; type in-line string )
 3 P   R@ COUNT DUP 1+ R> + >R TYPE ;
 4 P : ." ( --- ) 22 ( '"' ) STATE @
 5 P   IF COMPILE (.") WORD C@ 1+ ALLOT ELSE WORD COUNT TYPE THEN ;
 6 P   IMMEDIATE
 7 P : ( ( --- ; skip input stream until right parenthesis )
 8 P   29 ( right parenthesis code ) WORD DROP ; IMMEDIATE
 9 P : LOAD ( --- ; interpret screen )
10 P   BLK @ >R >IN @ >R 0 >IN ! BLK !
11 P   INTERPRET R> >IN ! R> BLK ! ;
12 P : --> ( --- ; continue interpreting next screen )
13 P   ?LOADING 0 >IN ! 1 BLK +! ; IMMEDIATE
14 P  
15 P -->

28 LIST
 0 P ( * Umbilical Words: COLD WARM KEY EMIT )
 1 P : COLD ( --- ) [ _UVR @ ] LITERAL UP [ UVREND @ _UVR @ - 2+ ]
 2 P   LITERAL CMOVE EMPTY-BUFFERS ABORT ;
 3 P LATEST PFA CFA CLD @ 1+ !
 4 P : WARM ( --- ) EMPTY-BUFFERS ABORT ;
 5 P LATEST PFA CFA WRM @ 1+ !
 6 P : KEY ( --- c ) STDIN @
 7 P   IF STIN [ STIN @ HERE 2- ! ]
 8 P   ELSE CIN [ CIN @ HERE 2- ! ]
 9 P   THEN ;
10 P : EMIT ( c --- ) DUP COUT [ COUT @ HERE 2- ! ] PFLAG @
11 P   IF POUT [ POUT @ HERE 2- ! ] ELSE DROP THEN ;
12 P  
13 P  
14 P  
15 P -->

29 LIST
 0 P ( * Umbilical Words: READ-REC WRITE-REC )
 1 P : READ-REC ( n1 a n2 --- f )
 2 P   [ B/BUF BPS / ] LITERAL * [ B/BUF BPS / ] LITERAL 0
 3 P   DO 3 PICK 3 PICK 3 PICK READ [ READ @ HERE 2- ! ] DUP
 4 P     IF LEAVE ELSE I 1+ [ B/BUF BPS / ] LITERAL <
 5 P       IF DROP SWAP [ BPS ] LITERAL + SWAP 1+ THEN THEN
 6 P   LOOP >R 2DROP DROP R> ;
 7 P : WRITE-REC ( n1 a n2 --- f )
 8 P   [ B/BUF BPS / ] LITERAL * [ B/BUF BPS / ] LITERAL 0
 9 P   DO 3 PICK 3 PICK 3 PICK WRITE [ WRITE @ HERE 2- ! ] DUP
10 P     IF LEAVE ELSE I 1+ [ B/BUF BPS / ] LITERAL <
11 P       IF DROP SWAP [ BPS ] LITERAL + SWAP 1+ THEN THEN
12 P   LOOP >R 2DROP DROP R> ;
13 P  
14 P  
15 P -->

30 LIST
 0 P ( * Umbilical Words: 'null' )
 1 P HEX 8081 HERE  DUP null's_NFA !
 2 P : x ( --- ) BLK @
 3 P   IF ( disk ) 1 BLK +! 0 >IN ! ?EXEC R> DROP
 4 P   ELSE ( terminal ) R> DROP
 5 P   THEN ;
 6 P ! IMMEDIATE ( x is replaced by null code )
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

31 LIST
 0 P ( ** Umbilical "Variables" )
 1 P        USE CONSTANT USE        ( using buffer )
 2 P       PREV CONSTANT PREV       ( previous buffer )
 3 P DISK-ERROR CONSTANT DISK-ERROR ( disk error )
 4 P  
 5 P  
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

32 LIST
 0 P ( Constants )
 1 P : CONSTANT  CREATE [ HERE 1 th_patch ! ] , DOES> @ ;
 2 P             0 CONSTANT ORIGIN ( origin )
 3 P HEX      8000 CONSTANT LIMIT  ( limit of memory region )
 4 P           400 CONSTANT B/BUF  ( bytes per buffer )
 5 P             2 CONSTANT #BUFF  ( number of buffers )
 6 P     B/BUF 4 + CONSTANT BFLEN  ( buffer length )
 7 P LIMIT BFLEN #BUFF * - CONSTANT FIRST ( top of disk buffers )
 8 P   FIRST UVs - CONSTANT UP     ( top of user variable area )
 9 P            20 CONSTANT BL     ( blank ' ' code )
10 P            40 CONSTANT C/L    ( characters per line )
11 P             0 CONSTANT 0   1 CONSTANT 1   2 CONSTANT 2
12 P             3 CONSTANT 3  -1 CONSTANT -1
13 P            50 CONSTANT TIBLEN ( text input buffer length )
14 P             4 CONSTANT MSGSCR ( message screen )
15 P -->

33 LIST
 0 P ( User Variables +6 -- +28 )
 1 P : USER  CREATE [ HERE 2 th_patch ! ] , DOES> @ UP + ;
 2 P DECIMAL
 3 P  6 USER S0         ( stack pointer zero position )
 4 P  8 USER R0         ( return stack pointer zero position )
 5 P 10 USER TIB        ( terminal input buffer )
 6 P 12 USER WIDTH
 7 P 14 USER WARNING
 8 P 16 USER FENCE      ( fence for FORGETing )
 9 P 18 USER DP         ( dictionary pointer )
10 P 20 USER VOC-LINK   ( vocabulary link )
11 P 22 USER BLK        ( block )
12 P 24 USER >IN        ( to in )
13 P 26 USER OUT
14 P 28 USER SCR        ( screen )
15 P -->

34 LIST
 0 P ( User Variables +30 -- +50 )
 1 P 30 USER OFFSET     ( drive bytes offset )
 2 P 32 USER CONTEXT    ( context vocabulary )
 3 P 34 USER CURRENT    ( current vocabulary )
 4 P 36 USER STATE      ( compilation state )
 5 P 38 USER BASE       ( base n )
 6 P 40 USER DPL        ( decimal point location )
 7 P 42 USER FLD        ( number output field width )
 8 P 44 USER CSP        ( check stack pointer )
 9 P 46 USER R#         ( R number )
10 P 48 USER HLD        ( held )
11 P 50 USER PFLAG      ( printer flag )
12 P  
13 P  
14 P  
15 P -->

35 LIST
 0 P ( Unique Constants and Unique User Variables )
 1 P   ( screens per drive )
 2 P     DRSIZE CONSTANT SCR/DRIVE
 3 P   ( top of the initial values area for user variables )
 4 P      _UVR @ CONSTANT UVR
 5 P   ( the flag to whether use UTF-8 multi-byte characters )
 6 P DECIMAL 52 USER UTF-8
 7 P   ( the flag to echo input or not )
 8 P         54 USER ECHO
 9 P   ( the flag to whether use "stdin" or "getch" )
10 P         56 USER STDIN
11 P  
12 P  
13 P  
14 P  
15 P -->

36 LIST
 0 P ( ?TERMINAL )
 1 P : ?TERMINAL ( --- f ) CTST [ CTST @ HERE 2- ! ] ;
 2 P  
 3 P ( set the end of word search at START's lfa )
 4 P 0 ' START LFA !
 5 P  
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P -->

37 LIST
 0 P ( DECIMAL HEX 1+ 2+ 1- 2- 2* C@ 2DUP 2DROP 2@ )
 1 P : DECIMAL ( --- ) 10 BASE ! ;
 2 P : HEX ( --- ) 16 BASE ! ;
 3 P HEX
 4 P : 1+ ( n --- n+1 ) 1 + ;
 5 P : 2+ ( n --- n+2 ) 2 + ;
 6 P : 1- ( n --- n-1 ) 1 - ;
 7 P : 2- ( n --- n-2 ) 2 - ;
 8 P : 2* ( n --- n+n ) DUP + ;
 9 P  
10 P : C@ ( a --- c )   @ 0FF AND ;
11 P : 2DUP ( n1 n2 --- n1 n2 n1 n2 ) OVER OVER ;
12 P : 2DROP ( n1 n2 --- ) DROP DROP ;
13 P : 2@ ( a --- d ) DUP @ SWAP 2+ @ ;
14 P  
15 P ." 37 " -->

38 LIST
 0 P ( = <> < > TRAVERSE NFA LFA CFA PFA )
 1 P : = ( n1 n2 --- f ) - 0= ;
 2 P : < ( n1 n2 --- f ) - 0< ;
 3 P : > ( n1 n2 --- f ) SWAP < ;
 4 P  
 5 P : TRAVERSE ( a1 direction --- a2 ; traverse to the byte with )
 6 P                                  ( MSB=1                     )
 7 P   SWAP BEGIN OVER + 7F OVER C@ < UNTIL SWAP DROP ;
 8 P  
 9 P : NFA ( pfa --- nfa ) 5 - -1 TRAVERSE ;
10 P : LFA ( pfa --- lfa ) 4 - ;
11 P : CFA ( pfa --- cfa ) 2 - ;
12 P : PFA ( nfa --- pfa ) 1 TRAVERSE 5 + ;
13 P  
14 P  
15 P ." 38 " -->

39 LIST
 0 P ( +! HERE ALLOT , C, LATEST NEGATE DNEGATE <ROT )
 1 P : +! ( n a --- ) SWAP OVER @ + SWAP ! ;
 2 P : HERE ( --- a ) DP @ ;
 3 P : ALLOT ( n --- ) DP +! ;
 4 P : LATEST ( --- nfa ) CURRENT @ @ ;
 5 P : , ( n --- ) HERE ! 2 ALLOT ;
 6 P LATEST PFA CFA DUP 1 th_patch @ ! 2 th_patch @ !
 7 P : C, ( c --- ) HERE C! 1 ALLOT ;
 8 P : NEGATE ( n --- -n ) 0 SWAP - ;
 9 P : DNEGATE ( d --- -d ) >R >R 0.0 R> R> D- ;
10 P : <ROT ( n1 n2 n3 --- n3 n1 n2 ) ROT ROT ;
11 P  
12 P  
13 P  
14 P  
15 P ." 39 " -->

40 LIST
 0 P ( LITERAL DLITERAL DEFINITIONS VARIABLE 2CONSTANT 2VARIABLE )
 1 P : LITERAL ( n --- ) STATE @ IF COMPILE LIT , THEN ; IMMEDIATE
 2 P : DLITERAL ( d --- ) STATE @
 3 P   IF SWAP [COMPILE] LITERAL [COMPILE] LITERAL THEN ;
 4 P   IMMEDIATE
 5 P : DEFINITIONS ( --- ) CONTEXT @ CURRENT ! ;
 6 P : VARIABLE ( --- ; VARIABLE <name> ) CREATE 0 , DOES> ;
 7 P : 2CONSTANT ( d --- ; 2CONSTANT <name> ) CREATE , , DOES> @ @ ;
 8 P : 2VARIABLE ( --- ; 2VARIABLE <name> ) CREATE 0 , 0 , DOES> ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 40 " -->

41 LIST
 0 P ( PAD SMUDGE +ORIGIN I J LEAVE NOT ERASE BLANKS )
 1 P : PAD ( --- a ) HERE [ WORDBUFSIZE ] LITERAL + ;
 2 P : SMUDGE ( --- ) LATEST 20 ( 0b 10 0000 ) TOGGLE ;
 3 P : +ORIGIN ( n --- a )  ORIGIN + ;
 4 P : I ( --- n ) R> R@ SWAP >R ;
 5 P : J ( --- n ) R> R> R> R@ <ROT >R >R SWAP >R ;
 6 P : LEAVE ( --- ) R> R> DUP R> DROP >R >R >R ;
 7 P : NOT ( f1 --- f2 ) 0= ;
 8 P : ERASE ( a n --- ; fill with nulls ) 0 FILL ;
 9 P : BLANKS ( a n --- ; fill with blanks ) BL FILL ;
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 41 " -->

42 LIST
 0 P ( 0> U< 0< MIN MAX +- ABS D+- DABS ?DUP )
 1 P : <> ( n1 n2 --- f ) = NOT ;
 2 P : 0> ( n1 n2 --- f ) 0 > ;
 3 P : U< ( u1 u2 --- f ) 2DUP XOR 0<
 4 P   IF DROP 0< 0= ELSE - 0< THEN ;
 5 P : D< ( d1 d2 --- f ) D- SWAP DROP 0< ;
 6 P : MIN ( n1 n2 --- n3 ) 2DUP > IF SWAP THEN DROP ;
 7 P : MAX ( n1 n2 --- n3 ) 2DUP < IF SWAP THEN DROP ;
 8 P : +- ( n1 n2 --- n3 ) 0< IF NEGATE THEN ;
 9 P : ABS ( n --- u ) DUP +- ;
10 P : D+- ( d1 n --- d2 ) 0< IF DNEGATE THEN ;
11 P : DABS ( d --- ud ) DUP D+- ;
12 P : ?DUP ( n --- n n / 0 ) DUP IF DUP THEN ;
13 P  
14 P  
15 P ." 42 " -->

43 LIST
 0 P ( S->D M* * M/MOD M/ /MOD */MOD MOD / )
 1 P : S->D ( n --- d ) DUP 0< IF -1 ELSE 0 THEN ;
 2 P : M* ( n1 n2 --- d ) 2DUP XOR >R ABS SWAP ABS U* R> D+- ;
 3 P : * ( n1 n2 --- n3 ) M* DROP ;
 4 P : M/MOD ( ud u1 --- u2 u3 ) >R 0 R@ U/ R> SWAP >R U/ R> ;
 5 P : M/ ( ud u1 --- u2 ) OVER >R >R DABS R@ ABS U/ R> R@ XOR 
 6 P   +- SWAP R> +- SWAP ;
 7 P : /MOD ( n1 n2 --- n3 n4 ) >R S->D R> M/ ;
 8 P : */MOD ( n1 n2 n3 --- n4 n5 ) >R M* R> M/ ;
 9 P : MOD ( n1 n2 --- n3 ) /MOD DROP ;
10 P : / ( n1 n2 --- n3 ) /MOD SWAP DROP ;
11 P  
12 P  
13 P  
14 P  
15 P ." 43 " -->

44 LIST
 0 P ( HOLD # #S <# #> SIGN )
 1 P : HOLD ( c --- ) -1 HLD +! HLD @ C! ;
 2 P : # ( ud1 --- ud2 ) BASE @ M/MOD ROT 9 OVER <
 3 P   IF 7 ( ":;<=>?@" ) + THEN 30 ( '0' ) + HOLD ;
 4 P : #S ( ud --- 0 0 ) BEGIN # 2DUP OR 0= UNTIL ;
 5 P : <# ( --- ) PAD HLD ! ;
 6 P : #> ( d --- a n ) 2DROP HLD @ PAD OVER - ;
 7 P : SIGN  ( n ud --- ud ) ROT 0< IF 2D ( '-' ) HOLD THEN ;
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 44 " -->

45 LIST
 0 P ( COUT TYPE CR SPACE SPACES -TRAILING )
 1 P : COUNT ( a --- a+1 n ) DUP 1+ SWAP C@ ;
 2 P : TYPE ( a n --- ) ?DUP
 3 P   IF OVER + SWAP DO I C@ EMIT LOOP ELSE DROP THEN ;
 4 P : CR ( --- ) 0A ( LF code ) EMIT ;
 5 P : SPACE ( --- ) BL EMIT ;
 6 P : SPACES ( n --- ) 0 MAX ?DUP IF 0 DO SPACE LOOP THEN ;
 7 P : -TRAILING ( a n1 --- a n2 ; remove trailing blanks )
 8 P   DUP 0 DO 2DUP + 1- C@ BL - IF LEAVE ELSE 1- THEN LOOP ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 45 " -->

46 LIST
 0 P ( D.R D. U. .R . )
 1 P : D.R ( d n --- ) >R SWAP OVER DABS <# #S SIGN #>
 2 P   R> OVER - SPACES TYPE ;
 3 P : D. ( d --- ) 0 D.R SPACE ;
 4 P : U. ( u --- ) 0 D. ;
 5 P : .R ( n1 n2 --- ) >R S->D R> D.R ;
 6 P : . ( n --- ) S->D D. ;
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 46 " -->

47 LIST
 0 P ( R/W )
 1 P : R/W     ( a n f --- ; read if f=1, write if f=0 )
 2 P   >R [ DRSIZE ] LITERAL /MOD <ROT R>
 3 P   IF READ-REC 8 ( error #8 )
 4 P   ELSE WRITE-REC 9 ( error #9 )
 5 P   THEN
 6 P   OVER SWAP ( ?ERROR ) [ HERE 3 th_patch ! 2 ALLOT ] DUP
 7 P   IF 0 PREV @ ! THEN DISK-ERROR ! ;
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 47 " -->
 
48 LIST
 0 P ( +BUF BUFFER )
 1 P : +BUF ( a1 --- a2 f ; advance to next buffer address )
 2 P   BFLEN + DUP LIMIT =
 3 P   IF DROP FIRST THEN DUP PREV @ - ;
 4 P  
 5 P : BUFFER ( n --- a ; acquire buffer for block n )
 6 P   USE @ DUP >R
 7 P   BEGIN +BUF UNTIL USE ! R@ @ 0<
 8 P   IF ( updated ) R@ 2+ ( data area ) R@ @ 7FFF AND ( blk # )
 9 P     0 R/W ( write )
10 P   THEN
11 P   R@ ! R@ PREV ! R> 2+ ( data area ) ;
12 P  
13 P  
14 P  
15 P ." 48 " -->

49 LIST
 0 P ( UPDATE UPDATE EMPTY-BUFFERS SAVE-BUFFERS DR0 DR1 )
 1 P : UPDATE ( ---  )
 2 P   PREV @ @ 8000 ( set the MSB ) OR PREV @ ! ;
 3 P : EMPTY-BUFFERS ( --- )
 4 P   FIRST LIMIT OVER - ERASE ;
 5 P : SAVE-BUFFERS  ( --- )
 6 P   #BUFF 1+ 0 DO 0 BUFFER DROP LOOP ;
 7 P : DR0 ( --- ) 0 OFFSET ! ;
 8 P : DR1 ( --- ) [ DRSIZE ] LITERAL OFFSET ! ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 49 " -->

50 LIST
 0 P ( BLOCK (LINE  .LINE )
 1 P : BLOCK ( n --- a ; get buffer address for block n )
 2 P   OFFSET @ + >R PREV @ DUP @ R@ - 2* ( disregard UPDATE bit )
 3 P   IF ( not PREV )
 4 P     BEGIN +BUF 0= ( true upon reaching PREV )
 5 P       IF DROP R@ BUFFER DUP R@ 1 R/W ( read ) 2- THEN
 6 P       DUP @ R@ - 2* 0=
 7 P     UNTIL DUP PREV !
 8 P   THEN R> DROP 2+ ( data area ) ;
 9 P  
10 P : (LINE) ( line scr --- a n ; get a screen line )
11 P   >R C/L B/BUF */MOD R> + BLOCK + C/L ;
12 P  
13 P : .LINE ( line scr --- ; type out a screen line )
14 P   (LINE) -TRAILING TYPE ;
15 P ." 50 " -->

51 LIST
 0 P ( ENCLOSE WORD )
 1 P : ENCLOSE ( a c --- a n1 n2 n3 ) OVER DUP >R
 2 P   BEGIN
 3 P     2DUP C@ = OVER C@ 0=
 4 P     IF DROP SWAP DROP R> - DUP 1+ OVER EXIT THEN
 5 P   WHILE 1+ REPEAT DUP >R 1+
 6 P   BEGIN 2DUP C@ <> OVER C@ 0=
 7 P     IF DROP SWAP DROP OVER - R> R> - SWAP DUP EXIT THEN
 8 P   WHILE 1+ REPEAT SWAP DROP OVER - R> R> - SWAP DUP 1+ ;
 9 P  
10 P : WORD ( c --- a ; c id a delimiter ) BLK @
11 P   IF BLK @ BLOCK ELSE TIB @ THEN
12 P   >IN @ + SWAP ENCLOSE HERE [ MAXTOKEN 2+ ] LITERAL BLANKS
13 P   >IN +! OVER - >R R@ HERE C! + HERE 1+ R> CMOVE HERE ;
14 P  
15 P ." 51 " -->

52 LIST
 0 P ( (FIND  FIND )
 1 P : (FIND) ( a1 a2 --- a ff )
 2 P   BEGIN
 3 P     OVER 2DUP C@ SWAP C@ 3F AND =
 4 P     IF BEGIN 1+ SWAP 1+ SWAP 2DUP C@ SWAP C@ <> UNTIL
 5 P       C@ OVER C@ 7F AND = IF SWAP DROP 3 + EXIT THEN 1-
 6 P     ELSE DROP
 7 P     THEN
 8 P     BEGIN 1+ DUP C@ 80 AND UNTIL
 9 P     1+ @ DUP 00 ( not 0, for later modification ) =
10 P     IF 2DROP 0 EXIT THEN 
11 P   AGAIN ;
12 P  
13 P : FIND ( --- cfa / 0 ; FIND <name> )
14 P   BL WORD CONTEXT @ @ (FIND) DUP 0=
15 P   IF DROP HERE LATEST (FIND) THEN ;  ." 52 " -->

53 LIST
 0 P ( MESSAGE ERROR ?ERROR )
 1 P : MESSAGE ( n --- ; output n'th message )
 2 P   WARNING @
 3 P   IF ?DUP IF MSGSCR OFFSET @ - .LINE SPACE THEN
 4 P   ELSE ." MSG #" .
 5 P   THEN ;
 6 P  
 7 P : ERROR ( n --- ; output n'th error message )
 8 P   WARNING @ 0<
 9 P   IF ( ABORT ) [ HERE 4 th_patch ! 2 ALLOT ]
10 P   THEN HERE COUNT TYPE ."  ? " MESSAGE SP! BLK @ ?DUP
11 P   IF >IN @ SWAP THEN ( QUIT ) [ HERE 5 th_patch ! 2 ALLOT ] ;
12 P  
13 P : ?ERROR    ( f n --- ; execute ERROR on true flag )
14 P    SWAP IF ERROR ELSE DROP THEN ;
15 P LATEST PFA CFA 3 th_patch @ !  ." 53 " -->

54 LIST
 0 P ( ?COMP ?EXEC ?STACK ?PAIRS ?LOADING ?CSP !CSP )
 1 P : ?COMP ( --- ) STATE @ 0= 11 ?ERROR ;
 2 P : ?EXEC ( --- ) STATE @ 12 ?ERROR ;
 3 P : ?STACK ( --- ) SP@ S0 @ SWAP U< 1 ?ERROR SP@ HERE
 4 P   [ TMPBUFSIZE ] LITERAL + U< 7 ?ERROR ;
 5 P : ?PAIRS  ( n1 n2 --- ) = NOT 13 ?ERROR ;
 6 P : ?LOADING ( --- ) BLK @ 0= 16 ?ERROR ;
 7 P : ?CSP ( --- ) SP@ CSP @ - 14 ?ERROR ;
 8 P : !CSP ( --- ) SP@ CSP ! ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 54 " -->

55 LIST
 0 P ( ' VOCABULARY FORGET )
 1 P : ' ( --- pfa : execution ; --- : compiling ; ' <name> )
 2 P   FIND 2+ ( cfa -> pfa ) ?DUP 0= 0 ?ERROR [COMPILE] LITERAL ;
 3 P   IMMEDIATE
 4 P  
 5 P : VOCABULARY ( --- ; VOCABULARY <name> )
 6 P   CREATE A081 , ( "blank" word ) CURRENT @ CFA , HERE
 7 P   VOC-LINK @ , VOC-LINK ! DOES> 2+ CONTEXT ! ;
 8 P  
 9 P VOCABULARY FORTH IMMEDIATE
10 P  
11 P : FORGET ( --- ; FORGET <word> ) CURRENT @ CONTEXT @ -
12 P   18 ?ERROR [COMPILE] ' DUP FENCE @ < 15 ?ERROR
13 P   DUP NFA DP ! LFA @ CURRENT @ ! ;
14 P  
15 P ." 55 " -->

56 LIST
 0 P ( <MARK >MARK <RESOLVE >RESOLVE )
 1 P : <MARK ( --- a ) HERE ;
 2 P : >MARK ( --- a ) HERE 0 , ;
 3 P : <RESOLVE ( a --- ) HERE - , ;
 4 P : >RESOLVE ( a --- ) HERE OVER - SWAP ! ;
 5 P  
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 56 " -->

57 LIST
 0 P ( PICK RPICK DEPTH ROLL <ROLL )
 1 P : PICK ( n1 --- n2 ) 2* SP@ + @ ;
 2 P : RPICK ( n1 --- n2 ) 2* RP@ + @ ;
 3 P : DEPTH ( --- n ) S0 @ SP@ 2+ - 2/ ;
 4 P : ROLL ( n --- ) >R R@ PICK SP@ DUP 2+ R> 2* <CMOVE DROP ;
 5 P : <ROLL   ( n --- ) >R DUP SP@ DUP 2+ SWAP R@ 2* CMOVE
 6 P   SP@ R> 2* + ! ;
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 57 "-->

58 LIST
 0 P ( DIGIT )
 1 P : DIGIT ( c n1 --- n2 tf / ff )
 2 P   SWAP 30 ( '0' ) - DUP 0<
 3 P   IF 2DROP 0
 4 P   ELSE DUP 9 >
 5 P     IF 7 ( ":;<=>?@" ) - DUP 0A <
 6 P       IF 2DROP 0
 7 P       ELSE 2DUP > IF SWAP DROP 1 ELSE 2DROP 0 THEN
 8 P       THEN
 9 P     ELSE 2DUP > IF SWAP DROP 1 ELSE 2DROP 0 THEN
10 P     THEN
11 P   THEN ;
12 P  
13 P  
14 P  
15 P ." 58 " -->

59 LIST
 0 P ( CONVERT NUMBER )
 1 P : CONVERT ( d a --- d' a' )
 2 P   BEGIN 1+ DUP >R C@ BASE @ DIGIT
 3 P   WHILE SWAP BASE @ U* DROP ROT BASE @ U* D+
 4 P     DPL @ 1+ IF 1 DPL +! THEN R>
 5 P   REPEAT R> ;
 6 P  
 7 P : NUMBER ( a --- d ) 0.0 ROT DUP 1+ C@ 2D ( '-' )
 8 P   = DUP >R + -1
 9 P  BEGIN DPL ! CONVERT DUP C@ BL -
10 P  WHILE DUP C@ 2E ( '.' ) - 0 ?ERROR 0 REPEAT
11 P    DROP R> IF DNEGATE THEN ;
12 P  
13 P  
14 P  
15 P ." 59 " -->

60 LIST
 0 P ( INTERPRET )
 1 P : INTERPRET ( ---  )
 2 P   BEGIN FIND ?DUP
 3 P     IF ( found ) DUP 2+ ( cfa -> pfa ) NFA C@ STATE @ <
 4 P       IF , ELSE EXECUTE THEN ?STACK
 5 P     ELSE HERE NUMBER DPL @ 1+
 6 P       IF [COMPILE] DLITERAL
 7 P       ELSE DROP [COMPILE] LITERAL
 8 P       THEN ?STACK
 9 P     THEN
10 P   AGAIN ;
11 P  
12 P  
13 P  
14 P  
15 P ." 60 " -->

61 LIST
 0 P ( EXPECT )
 1 P : EXPECT ( a n --- ) OVER + OVER
 2 P   DO KEY DUP 8 ( backspace code ) =
 3 P     IF DROP UTF-8 @
 4 P       IF DUP I <
 5 P         IF 0
 6 P           BEGIN I 1- C@ 0C0 AND 80 =
 7 P           WHILE R> 1- >R DROP 1 REPEAT
 8 P           IF 8 EMIT BL EMIT 8 EMIT THEN
 9 P         THEN
10 P       THEN DUP I = DUP R> 2- + >R
11 P       IF 7 ( bell code ) ELSE 8 EMIT BL EMIT 8 THEN
12 P     ELSE DUP 0A ( LF code ) = IF LEAVE DROP BL 0 ELSE DUP THEN
13 P     I C! 0 I 1+ !
14 P     THEN ECHO @ IF EMIT ELSE DROP THEN
15 P   LOOP DROP ;  ." 61 "-->

62 LIST
 0 P ( QUERY QUIT ABORT )
 1 P : QUERY ( --- ) TIB @ TIBLEN EXPECT 0 >IN ! ;
 2 P  
 3 P : QUIT ( --- )
 4 P   0 BLK ! [COMPILE] [
 5 P   BEGIN RP! CR QUERY INTERPRET STATE @ 0= IF ." ok" THEN
 6 P   AGAIN ;
 7 P LATEST PFA CFA 5 th_patch @ !
 8 P  
 9 P : ABORT ( --- )
10 P   CR ." FORTH80 Version " UVR @ 0.0 D.R 2E ( '.' ) EMIT
11 P   UVR 2 + @ 0.0 D.R 2E EMIT
12 P   UVR 5 + C@ 0.0 D.R UVR 4 + C@ 41 ( 'A' ) + EMIT
13 P   SP! DECIMAL DR0 FORTH DEFINITIONS QUIT ;
14 P LATEST PFA CFA 4 th_patch @ !
15 P ." 62 " -->

63 LIST
 0 P ( ID. (CREATE )
 1 P : ID. ( nfa --- ; print word's name )
 2 P   DUP PFA LFA OVER - PAD SWAP CMOVE PAD COUNT 1F AND
 3 P   2DUP + 1- -80 SWAP +! TYPE SPACE ;
 4 P  
 5 P : (CREATE) ( --- ; (CREATE  <name> ) FIND ?DUP
 6 P   IF CR 2+ ( cfa -> pfa ) NFA ID. 4 MESSAGE THEN
 7 P   HERE DUP C@ WIDTH @ MIN 1+ ALLOT
 8 P   DUP 0A0 TOGGLE ( smudge ) HERE 1- 80 TOGGLE ( end of name )
 9 P   LATEST , CURRENT @ ! HERE 2+ , ;
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 63 " -->

64 LIST
 0 P ( ASSEMBLER CODE END-CODE ;CODE )
 1 P VOCABULARY ASSEMBLER IMMEDIATE
 2 P : CODE ( --- ; CODE <name> ) (CREATE) SMUDGE ASSEMBLER ;
 3 P : END-CODE ( --- ) CURRENT @ CONTEXT ! ;
 4 P : ;CODE ( --- ) ?CSP COMPILE (;CODE) [COMPILE] [ SMUDGE
 5 P   ASSEMBLER ; IMMEDIATE
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 64 " -->

65 LIST
 0 P ( .S LIST )
 1 P : .S ( --- ) SP@ S0 @ = 0=
 2 P   IF SP@ S0 @ 2- DO I @ . -2 +LOOP THEN ;
 3 P  
 4 P : LIST ( n --- ) BASE @ DECIMAL SWAP CR DUP DUP SCR !
 5 P   ." SCR # " . OFFSET @ + [ DRSIZE ] LITERAL /MOD
 6 P   ."  ( Drive " . ." # " . ." )" 10 0
 7 P   DO CR I 3 .R SPACE I SCR @ (LINE) TYPE 3C ( '<' ) EMIT LOOP
 8 P   CR BASE ! ;
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 65 " -->

66 LIST
 0 P ( Unique Words: "r" "w" "r+" )
 1 P VARIABLE "r" 1 "r" ! 72 C,
 2 P VARIABLE "w" 1 "w" ! 77 C,
 3 P VARIABLE "r+" 2 "r+" ! 72 C, 2B C,
 4 P DECIMAL
 5 P  
 6 P  
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 66 " -->

67 LIST
 0 P ( 79-STANDARD & Patch )
 1 P : 79-STANDARD ;
 2 P  
 3 P ( User Variables initial values )
 4 P     UP STACKSIZE -  UVR  6 + ( S0 ) !
 5 P                 UP  UVR  8 + ( R0 ) !
 6 P     UP STACKSIZE -  UVR 10 + ( TIB ) !
 7 P               HERE  UVR 16 + ( FENCE ) !
 8 P               HERE  UVR 18 + ( DP ) !
 9 P FIND ASSEMBLER 6 +  UVR 20 + ( VOC-LINK ) !
10 P     FIND FORTH 4 +  UVR 32 + ( CONTEXT ) !
11 P     FIND FORTH 4 +  UVR 34 + ( CURRENT ) !
12 P  
13 P  
14 P  
15 P ." 67 " -->

68 LIST
 0 P ( Umbilical Words' Parameter Fields Update #1 )
 1 P : WORD-UPDATE ( a --- ; pfa a DP-pfa CMOVE )
 2 P   LATEST PFA HERE OVER - ROT SWAP CMOVE ;
 3 P  
 4 P   ( Umbilical Words Re-compiling )
 5 P  
 6 P DECIMAL
 7 P ' :
 8 P : :  ?EXEC !CSP CURRENT @ CONTEXT ! (CREATE) ]
 9 P   ;CODE 50 C, END-CODE
10 P WORD-UPDATE FORGET :
11 P ' ;
12 P : ;  ?CSP COMPILE ;S SMUDGE [COMPILE] [ ;
13 P WORD-UPDATE FORGET ;
14 P  
15 P ." 68 " -->

69 LIST
 0 P ( Umbilical Words' Parameter Fields Update #2 )
 1 P ' (;CODE)
 2 P : (;CODE)  R> LATEST PFA CFA ! ;
 3 P WORD-UPDATE FORGET (;CODE)
 4 P ' DOES>
 5 P : DOES>  COMPILE (;CODE)
 6 P   [ JMP @ ] LITERAL C, [ HERE 8 + ] LITERAL , ;
 7 P WORD-UPDATE FORGET DOES>
 8 P DECIMAL
 9 P ' CREATE
10 P : CREATE  (CREATE) SMUDGE ;CODE 52 C, END-CODE
11 P WORD-UPDATE FORGET CREATE
12 P HEX
13 P ' IMMEDIATE
14 P : IMMEDIATE  LATEST 40 TOGGLE ;
15 P WORD-UPDATE FORGET IMMEDIATE  ." 69 " -->

70 LIST
 0 P ( Umbilical Words' Parameter Fields Update #3 )
 1 P ' COMPILE
 2 P : COMPILE  ?COMP R> DUP 2+ >R @ , ;
 3 P WORD-UPDATE FORGET COMPILE
 4 P ' [COMPILE]
 5 P : [COMPILE]  FIND ?DUP 0= 0 ?ERROR , ;
 6 P WORD-UPDATE FORGET [COMPILE]
 7 P ' [
 8 P : [  0 STATE ! ;
 9 P WORD-UPDATE FORGET [
10 P HEX
11 P ' ]
12 P : ]  0C0  STATE ! ;
13 P WORD-UPDATE FORGET ]
14 P  
15 P ." 70 " -->

71 LIST
 0 P ( Umbilical Words' Parameter Fields Update #4 )
 1 P ' IF
 2 P : IF  ?COMP COMPILE 0BRANCH >MARK 1 ;
 3 P WORD-UPDATE FORGET IF
 4 P ' ELSE
 5 P : ELSE  1 ?PAIRS COMPILE BRANCH >MARK SWAP >RESOLVE 1 ;
 6 P WORD-UPDATE FORGET ELSE
 7 P ' THEN
 8 P : THEN  1 ?PAIRS >RESOLVE ;
 9 P WORD-UPDATE FORGET THEN
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 71 " -->

72 LIST
 0 P ( Umbilical Words' Parameter Fields Update #5 )
 1 P HEX
 2 P ' DO
 3 P : DO  COMPILE (DO) <MARK 2 ;
 4 P WORD-UPDATE FORGET DO
 5 P ' LOOP
 6 P : LOOP  2 ?PAIRS COMPILE (LOOP) <RESOLVE ;
 7 P WORD-UPDATE FORGET LOOP
 8 P ' +LOOP
 9 P : +LOOP  2 ?PAIRS COMPILE (+LOOP) <RESOLVE ;
10 P WORD-UPDATE FORGET +LOOP
11 P ' EXIT
12 P : EXIT  ?COMP COMPILE ;S ;
13 P WORD-UPDATE FORGET EXIT
14 P  
15 P ." 72 " -->

73 LIST
 0 P ( Umbilical Words' Parameter Fields Update #6 )
 1 P ' BEGIN
 2 P : BEGIN  ?COMP <MARK 3 ;
 3 P WORD-UPDATE FORGET BEGIN
 4 P ' AGAIN
 5 P : AGAIN  3 ?PAIRS COMPILE BRANCH <RESOLVE ;
 6 P WORD-UPDATE FORGET AGAIN
 7 P ' UNTIL
 8 P : UNTIL  3 ?PAIRS COMPILE 0BRANCH <RESOLVE ;
 9 P WORD-UPDATE FORGET UNTIL
10 P ' WHILE
11 P : WHILE  3 ?PAIRS COMPILE 0BRANCH >MARK 4 ;
12 P WORD-UPDATE FORGET WHILE
13 P ' REPEAT
14 P : REPEAT  4 ?PAIRS COMPILE BRANCH SWAP <RESOLVE >RESOLVE ;
15 P WORD-UPDATE FORGET REPEAT   ." 73 " -->

74 LIST
 0 P ( Umbilical Words' Parameter Fields Update #7 )
 1 P ' (.")
 2 P : (.")  R@ COUNT DUP 1+ R> + >R TYPE ;
 3 P WORD-UPDATE FORGET (.")
 4 P HEX
 5 P ' ."
 6 P : ."  22 STATE @
 7 P   IF COMPILE (.") WORD C@ 1+ ALLOT ELSE WORD COUNT TYPE THEN ;
 8 P WORD-UPDATE FORGET ."
 9 P ' (
10 P : (  29 WORD DROP ;
11 P WORD-UPDATE FORGET (
12 P  
13 P  
15 P  
14 P  
15 P ." 74 " -->

75 LIST
 0 P ( Umbilical Words' Parameter Fields Update #8 )
 1 P ' LOAD
 2 P : LOAD  BLK @ >R >IN @ >R 0 >IN ! BLK !
 3 P   INTERPRET R> >IN ! R> BLK ! ;
 4 P WORD-UPDATE FORGET LOAD
 5 P ' -->
 6 P : -->  ?LOADING 0 >IN ! 1 BLK +! ;
 7 P WORD-UPDATE FORGET -->
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 75 " -->

76 LIST
 0 P ( Umbilical Words' Parameter Fields Update #9 )
 1 P ' COLD  : COLD  [ _UVR @ ] LITERAL UP [ UVREND @ _UVR @ - 2+ ]
 2 P           LITERAL CMOVE EMPTY-BUFFERS ABORT ;
 3 P WORD-UPDATE FORGET COLD
 4 P ' WARM  : WARM  EMPTY-BUFFERS ABORT ;
 5 P WORD-UPDATE FORGET WARM
 6 P ' KEY   : KEY  STDIN @
 7 P           IF STIN [ STIN @ HERE 2- ! ]
 8 P           ELSE CIN [ CIN @ HERE 2- ! ] THEN ;
 9 P WORD-UPDATE FORGET KEY
10 P ' EMIT  : EMIT  DUP COUT [ COUT @ HERE 2- ! ] PFLAG @
11 P           IF POUT [ POUT @ HERE 2- ! ] ELSE DROP THEN ;
12 P WORD-UPDATE FORGET EMIT
13 P  
14 P  
15 P ." 76 " -->

77 LIST
 0 P ( Umbilical Words' Parameter Fields Update #10 )
 1 P ' READ-REC  : READ-REC
 2 P   [ B/BUF BPS / ] LITERAL * [ B/BUF BPS / ] LITERAL 0
 3 P   DO 3 PICK 3 PICK 3 PICK READ [ READ @ HERE 2- ! ] DUP
 4 P     IF LEAVE ELSE I 1+ [ B/BUF BPS / ] LITERAL <
 5 P       IF DROP SWAP [ BPS ] LITERAL + SWAP 1+ THEN THEN
 6 P   LOOP >R 2DROP DROP R> ;
 7 P WORD-UPDATE FORGET READ-REC
 8 P ' WRITE-REC  : WRITE-REC
 9 P   [ B/BUF BPS / ] LITERAL * [ B/BUF BPS / ] LITERAL 0
10 P   DO 3 PICK 3 PICK 3 PICK WRITE [ WRITE @ HERE 2- ! ] DUP
11 P     IF LEAVE ELSE I 1+ [ B/BUF BPS / ] LITERAL <
12 P       IF DROP SWAP [ BPS ] LITERAL + SWAP 1+ THEN THEN
13 P   LOOP >R 2DROP DROP R> ;
14 P WORD-UPDATE FORGET WRITE-REC
15 P ." 77 " -->

78 LIST
 0 P ( Umbilical Words' Parameter Fields Update #11 )
 1 P null's_NFA @ PFA
 2 P : x  BLK @
 3 P   IF 1 BLK +! 0 >IN ! ?EXEC R> DROP
 4 P   ELSE R> DROP
 5 P   THEN ;
 6 P WORD-UPDATE FORGET WORD-UPDATE
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 78 " -->

79 LIST ( Vocabulary Patch )
 0 P ' 79-STANDARD NFA  FIND FORTH 4 +      !
 1 P                 0  FIND FORTH 6 +      !
 2 P ' 79-STANDARD NFA  FIND ASSEMBLER 4 +  !
 3 P    FIND FORTH 6 +  FIND ASSEMBLER 6 +  !
 4 P  
 5 P ( set the end of word search at LIT's lfa )
 6 P 0 ' LIT LFA !
 7 P  
 8 P  
 9 P  
10 P  
11 P  
12 P  
13 P  
14 P  
15 P ." 79 " DECIMAL ;S

80 LIST
 0 P ( ***** Memory Map *****                                      )
 1 P (                                                             )
 2 P (               |=======|                                     )
 3 P (        ORIG ->| 0000H |     program start                   )
 4 P (               |   .   |                                     )
 5 P (               |   .   |                                     )
 6 P (               |=======|                                     )
 7 P (       LIT-6 ->| 005DH |     start of dictionary             )
 8 P (               |   .   |                                     )
 9 P (               |   .   |                                     )
10 P (      INITDP ->| ????H |     initial position of DP          )
11 P (               |   .   |                                     )
12 P (               |   .   |                                     )
13 P (               | ----- |----                                 )
14 P (          DP  v|   x   |  |  dictionary pointer (go under)
15 P (               |   .   |  |  word buffer (70 bytes)

81 LIST
 0 P (               |   .   |  |                                  )
 1 P (               | ----- | temporary buffer area (151 bytes)
 2 P (         PAD  v| x+46H |  |                                  )
 3 P (               |   :   |  |  text buffer (81 bytes)
 4 P (               | x+96H |  |                                  )
 5 P (               | ----- |----                                 )
 6 P (               |   .   |                                     )
 7 P (               |   .   |                                     )
 8 P (               | ----- |                                     )
 9 P (          SP  ^|   .   |     stack pointer (go upper)
10 P (               |   .   |                                     )
11 P (               | 7716H |     bottom of stack                 )
12 P (               |=======|                                     )
13 P ( INITS0, TIB ->| 7718H |     terminal input buffer           )
14 P (               |   .   |                                     )
15 P (               |   .   |                                     )

82 LIST
 0 P (               | ----- |                                     )
 1 P (          BP  ^|   .   |     return stack pointer (go upper)
 2 P (               |   .   |                                     )
 3 P (               | 77B6H |     bottom of return stack          )
 4 P (               |=======|                                     )
 5 P (  INITR0, UP ->| 77B8H |     top of user variables area      )
 6 P (               |   :   |                                     )
 7 P (               |=======|                                     )
 8 P (       FIRST ->| 77F8H |     top of disk buffers             )
 9 P (               |   :   |                                     )
10 P (               | 7FFEH |     bottom of disk buffers          )
11 P (               |=======|                                     )
12 P (       LIMIT ->| 8000H |     out of area                     )
13 P (                                                             )
14 P (                                                             )
15 P (                                                             )

83 LIST
 0 P ( ***** Registers Set ******                                  )
 1 P (                                                             )
 2 P ( the 16 bit "virtual" registers used in this system :        )
 3 P (   Instruction Pointer         IP                            )
 4 P (   [parameter] Stack Pointer   SP                            )
 5 P (   Return stack Pointer        RP                            )
 6 P (   Working register            W                             )
 7 P (   other registers             AX, BX                        )
 8 P (   Program Counter             PC                            )
 9 P (                                                             )
10 P ( stack         : |[INITS0-2] ... [SI+4] [SI+2] [SI] ->       )
11 P ( return stack  : |[INITR0-2] ... [BP+4] [BP+2] [BP] ->       )
12 P (                                                             )
13 P (                                                             )
14 P (                                                             )
15 P (                                                             )

84 LIST
 0 P ( ***** Disk Buffer's Structure *****                         )
 1 P (                                                             )
 2 P (               |===========| f: update flag                  )
 3 P (       FIRST ->| f |   n   | n: block number (15 bit)
 4 P (               |-----------| -----                           )
 5 P (               |           |   |                             )
 6 P (               |           |   |                             )
 7 P (               |           |   |                             )
 8 P (       buffer1 |    DATA   |  1024 bytes                     )
 9 P (               ~           ~   |                             )
10 P (               ~           ~   |                             )
11 P (               |           |   |                             )
12 P (               |-----------| -----                           )
13 P (               | 00H | 00H | double null characters          )
14 P (               |===========|                                 )
15 P (               | f |   n   |                                 )

85 LIST
 0 P (               |-----------| -----                           )
 1 P (               |           |   |                             )
 2 P (               |           |   |                             )
 3 P (               |           |   |                             )
 4 P (       buffer2 |    DATA   |  1024 bytes                     )
 5 P (               ~           ~   |                             )
 6 P (               ~           ~   |                             )
 7 P (               |           |   |                             )
 8 P (               |-----------| -----                           )
 9 P (               | 00H | 00H |                                 )
10 P (               |===========|                                 )
11 P (       LIMIT ->                                              )
12 P (                                                             )
13 P (                                                             )
14 P (                                                             )
15 P (                                                             )
