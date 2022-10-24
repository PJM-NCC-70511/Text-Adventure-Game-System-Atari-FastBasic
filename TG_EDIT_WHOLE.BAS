

'***************************************************************************
'***                  Text Adventure Game File Editor                    ***
'***             IBM PC & Compatibles QuickBasic Version 1.00            ***
'***                 Program written by Peter J. Meyer                   ***
'***************************************************************************
' Modified (1.00-10/93) (1.01-06/94)
'
'     This program is used to edit text adventure game files, big text files,
' screen files,  and graphic files.  Most functions are self explanitory.  If
' you copy  these programs  to a different  sub-directory,  be sure to change
' Path$ in the "Initialize" module.
'
'        The Text Adventure Game System was originley written in BASIC XE for
' the Atari 8-bit computers with 128K or larger.  Program was created in 1987
' and was  upgraded several times.  The QuickBasic version  was imported from
' BASIC XE, by using a translating program, then uploading in ASCII, Removing
' the linenumbers with the "REMLINE.BAS" program, and then modifying the code
' to get the finished program.  Both  are  very similar  in  performance  and
' most of the Text Adventure codes are similar.
'

  PW_DIM = 32
  ItemTableSize = 128
  ComputerCharacterTableSize = 64
  RoomDataSize = 192
  DIM PW$(PW_DIM)
  
  
  
  DIM Ot(ItemTableSize) BYTE
  DIM OP(ItemTableSize) BYTE
  DIM OV(ItemTableSize) BYTE
  DIM OV2(ItemTableSize) BYTE
  DIM OV3(ItemTableSize) BYTE
  DIM OL(ItemTableSize) BYTE
  DIM ItemName$(ItemTableSize)
  DIM ItemDescription$(ItemTableSize)

  'Create Memory Tables for Item Objects
   
   'ItemTableBase = $8000
   'ItemTypeBase      = ItemTableBase + 0 * ItemTableSize 'OT(##) All Repleaced
   'ItemValueBase     = ItemTableBase + 1 * ItemTableSize 'OP(##) 
   'ItemVariable1Base = ItemTableBase + 2 * ItemTableSize 'OV(##)
   'ItemVariable2Base = ItemTableBase + 3 * ItemTableSize 'OV2(##)
   'ItemVariable3Base = ItemTableBase + 4 * ItemTableSize 'OV3(##)
   'ItemVariable4Base = ItemTableBase + 5 * ItemTableSize 'OV4(##)
   'ItemLocationBase  = ItemTableBase + 6 * ItemTableSize 'OL(##)
   

  
  DIM Ht(ComputerCharacterTableSize) BYTE
  DIM Hl(ComputerCharacterTableSize) BYTE
  DIM HS(ComputerCharacterTableSize) BYTE
  DIM HH(ComputerCharacterTableSize) BYTE
  DIM Hm(ComputerCharacterTableSize) BYTE
  DIM Hv(ComputerCharacterTableSize) BYTE
  DIM ComputerCharacterName$(ComputerCharacterTableSize)
  DIM HO(ComputerCharacterTableSize) BYTE
  
  
  'ComputerCharacterTableBase = $8800  
  'ComputerCharacterTypeBase       = ComputerCharacterTableBase + 0 * ComputerCharacterTableSize
  'ComputerCharacterValueBase      = ComputerCharacterTableBase + 1 * ComputerCharacterTableSize  
  'ComputerCharacterAttackAmount   = ComputerCharacterTableBase + 2 * ComputerCharacterTableSize
  'ComputerCharacterHitPoints      = ComputerCharacterTableBase + 3 * ComputerCharacterTableSize
  'ComputerCharacterMoveControl    = ComputerCharacterTableBase + 4 * ComputerCharacterTableSize
  'ComputerCharacterTotalHitPoints = ComputerCharacterTableBase + 5 * ComputerCharacterTableSize
  
  
  
  DIM RT(RoomDataSize)   BYTE
  DIM Rn(RoomDataSize)   BYTE
  DIM Rnl(RoomDataSize)  BYTE
  DIM Rnk(RoomDataSize)  BYTE
  DIM Rw(RoomDataSize)   BYTE
  DIM Rwl(RoomDataSize)  BYTE
  DIM Rwk(RoomDataSize)  BYTE
  DIM Re(RoomDataSize)   BYTE
  DIM Rel(RoomDataSize)  BYTE
  DIM Rek(RoomDataSize)  BYTE
  DIM Rs(RoomDataSize)   BYTE
  DIM Rsl(RoomDataSize)  BYTE
  DIM Rsk(RoomDataSize)  BYTE
  DIM Ru(RoomDataSize)   BYTE
  DIM Rul(RoomDataSize)  BYTE
  DIM Rd(RoomDataSize)   BYTE
  DIM Rdl(RoomDataSize)  BYTE
  DIM Ro(RoomDataSize)   BYTE
  DIM Rol(RoomDataSize)  BYTE
  DIM Rov(RoomDataSize)  BYTE
  DIM REN$(RoomDataSize)
  DIM RoomDescription$(RoomDataSize)

COMMON SHARED AC, CC, NC, K1, PD, pk, R, W, C, ADAPTER, NCOL, HIGHT, PATH$, TEMP1$
COMMON SHARED VX, VY, VC, VR, PCOL, CCOL, SIZE, PATTERN, SKIP, MODE, PC, PR
COMMON SHARED CHRHIGHT, CHRWIDTH, CSROW
GOSUB Initialize
DO
   CLS
   PRINT "*   Text Adventure Game File Editor    IBM PC Version 1.01 by Peter J. Meyer   *"
   PRINT TAB(40 - LEN(F$) / 2); F$
   PRINT
   PRINT USING "### Objects  ### hostilities  ### places  ## passwords/spells####,### Bytes Free"; OBJ; HOS; PLC; PWD; FRE(-1)
   PRINT
   PRINT "      A) Enter main information"
   PRINT "      B) Enter specifications"
   PRINT "      C) Enter passwords/spells"
   PRINT "      D) Enter object data"
   PRINT "      E) Enter hostility data"
   PRINT "      F) Enter place data"
   PRINT "      G) Create/edit text file"
   PRINT "      H) Create/edit screen file"
   PRINT "      I) Create/edit graphic file"
   PRINT "      J) New file name"
   PRINT "      K) Save game file"
   PRINT "      L) Load game file"
   PRINT "      M) Clear memory"
   PRINT "      ESCape for Main menu"
   CLOSE #1
   MODE = 1
   REPEAT
     KI$ = INKEY$
   UNTIL KI$ <> ""
   A = ASC(KI$)
   IF A > 96 THEN A = A - 32
   IF A - 64 < 5 THEN
     Modified = 1
   ENDIF

   SELECT CASE A - 64
    CASE IS = 1
      GOSUB Edit.main.info
    CASE IS = 2
      GOSUB Edit.Specifications
    CASE IS = 3
      GOSUB Edit.Passwords
    CASE IS = 4
      GOSUB Edit.Object.data
    CASE IS = 5
      GOSUB Edit.hostility.data
    CASE IS = 6
      GOSUB Edit.Place.data
    CASE IS = 7
      GOSUB Edit.text.file
    CASE IS = 8
      SCREENFILE
    CASE IS = 9
      GRAPHICFILE
    CASE IS = 10
      GOSUB Get.file.name
    CASE IS = 11
      GOSUB Save.file
    CASE IS = 12
      GOSUB SURE
      IF NOT SURE$ = "C" THEN GOSUB Load.file
    CASE IS = 13
      GOSUB SURE
      IF NOT SURE$ = "C" THEN RUN
    CASE A = 27
      GOSUB SURE
      IF SURE$ = "C" THEN A = 0
  END SELECT
LOOP UNTIL A = 27
RUN "TG_MAIN.BAS"

Initialize:
'  OPEN "TG.PIF" FOR INPUT AS #1
'    INPUT #1, ADAPTER
'    INPUT #1, PATH$
'  CLOSE #1
  ADAPTER = 1
  PATH$ = ""
  PW.DIM = 32
  Nf$ = ""
  Cu$ = ""
  Op$ = ""
  Cp$ = ""
  I$ = ""
  T$ = ""
  Modified = 0
  TEMP1$ = ""
  F$ = "(Untitled)"
  DD = 0
  O = 1
  H = 1
  R = 1
RETURN

Edit.main.info:
  CLS
  IF F$ = "(Untitled)" THEN
    GOSUB Get.file.name
    CLS
  ENDIF
  PRINT " Name of game"
  EDITSTRING T$
  PRINT
  PRINT "Author"
  EDITSTRING I$
  CLS
  PRINT "Opening paragraph."
  EDITSTRING Op$
  CLS
  PRINT "Closing paragraph."
  EDITSTRING Cp$
  CLS
  PRINT "Place number must be at to win"
  A = S1
  EDITVALUE A
  S1 = A
  CLS
  PRINT "Objects must have to win"
  PRINT "1st"
  EDITVALUE S2
  PRINT "2nd"
  EDITVALUE S3
  PRINT "3rd"
  EDITVALUE S4
  PRINT "4th"
  EDITVALUE S7
  PRINT "5th"
  EDITVALUE S30
  PRINT "6th"
  EDITVALUE S31
  PRINT "7th"
  EDITVALUE S32
  PRINT "8th"
  EDITVALUE S33
  PRINT "9th"
  EDITVALUE S34
  PRINT "10th"
  EDITVALUE S35
  CLS
  PRINT "Hostilities must defeat to win"
  PRINT "1st"
  EDITVALUE S5
  PRINT "2nd"
  EDITVALUE S36
  PRINT "3rd"
  EDITVALUE S37
  PRINT "4th"
  EDITVALUE S38
  PRINT "5th"
  EDITVALUE S39
  PRINT " Credit units must have to win."
  EDITVALUE S6
  PRINT " Starting place number"
  EDITVALUE S14
  PRINT " Starting number of shield points"
  EDITVALUE S8
  PRINT " How many credit units at start of game."
  EDITVALUE S22
  PRINT " Name of credit units (Dollars,tokens, gold pieces,etc.)"
  EDITSTRING Cu$
RETURN

Edit.Specifications:
  CLS
  PRINT "Enter 0 if all hostilities are to be stationary."
  PRINT "   1 if all to be at new loacation every move. "
  PRINT "   2 if all are to move at the begging of every turn."
  PRINT "   3 To have it controled by the hostility data."
  EDITVALUE S11
  PRINT "Number of turns to solve games."
  EDITVALUE S12
  PRINT "Number of turns before getting food or  water."
  EDITVALUE S13
    IF DD 
      PRINT "Place number for magician sanctuary"
      EDITVALUE S15
      PRINT "Place number revival or replacing party member"
      EDITVALUE S16
    ELSE
      PRINT "Number of lives you have to solve game."
      EDITVALUE S15
      PRINT "When player is killed. 1 Stay in same place. 2 Go to starting place. 3 Go to    last sanctuary."
      EDITVALUE S16
      PRINT "Maxinum number of objects carried"
      EDITVALUE S10
      PRINT "Number of shield points gain per turn"
      EDITVALUE S19
   ENDIF
    CLS
    PRINT "How many shield points are lost in a shield point losing trap."
    EDITVALUE S17
    PRINT "If hostilities are to be revived, enter amount hitpoints to be set at."
    PRINT "(-1 to restore to starting value)"
    EDITVALUE S23
    PRINT "How many credit units will be lost in a credit loosing trap"
    EDITVALUE S24
    PRINT "How hit points will be lost if hostilityis punched ?"
    EDITVALUE S25
    PRINT "How hit points will be lost if hostilityis kicked ?"
    EDITVALUE S26
    PRINT "Alternate name for shield points:"
    PRINT "0 Shield points(default)"
    PRINT "1 Health points"
    PRINT "2 Hit points"
    PRINT "3 Life units"
    PRINT "4 Power units"
    EDITVALUE S18
    CLS
    PRINT "Armor protection from hostility attack.             Damage ="
    PRINT "0 Armor is at highest percentage               (Highest Armor/100)"
    PRINT "1 Armor points are accumulated                 (Total Armor/50+Armor)"
    PRINT "2 Armor points are accumulated                 (Total Armor/1000)"
    PRINT "3 Armor+shields is at the highest of each part.(Total Armor/500)"
    EDITVALUE S21
    PRINT "Place display format:"
    PRINT "0 Normal, Exit, objects, and hostilities display in seperate paragraphs. "
    PRINT "1 Exit appended to place description, name of exit is in braces-{} at end."
    PRINT "2 Exits and Objects are appended to the opening paragraph."
    EDITVALUE S0
    PRINT "How many credit units do you get for killing each hostility ?"
    EDITVALUE S19
    PRINT "Objects sold at a market place."
    PRINT "0 Sold for the price of the object."
    PRINT "1 Sold at 90% of the price."
    EDITVALUE S27
    PRINT "Randomization:"
    PRINT "0 To randomize to timer, anyother to randomize to that number."
    EDITVALUE S28
    PRINT "Enter 1 to load another file when solve, 2 Must solve other file to access this one."
    EDITVALUE S20
    IF S20 = 1 THEN
      INPUT "Name of file:", P$
      MID$(Nf$, 1, LEN(P$)) = P$
    ENDIF
RETURN

Edit.Passwords:
  N = 1
  CLS
  IF NOT DD THEN
    PRINT "Passwords or Spells that are to be used in the game."
  ELSE
    PRINT "Spells or Passwords that are to be use in a Dungeons and Dragons game."
  ENDIF
  DO
    IF NOT DD THEN
      PRINT USING "WORD ## "; N
      TEMP1$ = PW$(N)
      EDITSTRING PW$(N)
      IF NOT TEMP1$ = "" THEN PW$(N) = TEMP1$
    ELSE
      IF N = 0 THEN
        CLS
        PRINT "Fighting spells"
      ENDIF
      IF N = 6 THEN
        CLS
        PRINT "Healing spells"
      ENDIF
      IF N = 12 THEN
         CLS
         PRINT "Protection spells"
       ENDIF
         IF N = 18 THEN
       CLS
         PRINT "General purpose spells/passwords."
       ENDIF
       IF N < 18 THEN
         PRINT USING "RING:#  WORD:## "; (N MOD 6) + 1; N
       ELSE
         PRINT USING "WORD:### "; N
       ENDIF
       TEMP1$ = PW$(N)
       EDITSTRING TEMP1$
       IF TEMP1$ = "" THEN PW$(N) = TEMP1$
     ENDIF
     IF TEMP1$ <> "" THEN
   N = N + 1
   IF N > PWD THEN
     PWD = N
     IF PWD > PW.DIM THEN
       PW.DIM = 8 + PW.DIM
       REDIM PW$(PW.DIM)
     ENDIF
   ENDIF
    ENDIF
  LOOP UNTIL TEMP1$ = ""
RETURN

Edit.Object.data:
 O = OBJ + 1
 DO
 CLS
 PRINT "Object number "; O; " type."
 PRINT " 1 Treasures     ", " 2 key(s)     ",
 IF DD = 0 AND S21 < 3 THEN PRINT " 3 Weapon        ",
 PRINT " 4 Ammunition    ",
 PRINT " 5 Food/Water    ",
 IF DD = 0 AND S21 < 3 THEN PRINT " 6 Armor/shield ",
 PRINT " 7 Explosive    ", " 8 Detonater  ",
 PRINT " 9 Transport    ", "10 Shovel     ",
 PRINT "11 Climbing tool", "12 Document   ",
 PRINT "13 Luminater    ", "14 Fuel       ",
 PRINT "15 Survival     ", "16 Shield points",
 PRINT "17 Poison       ", "18 More turns ",
 PRINT "19 Case/Container", "20 Extra life ",
 PRINT "21 More inventory", "22 Heavy container",
 PRINT "23 Heavy/pushable", "24 Burnable   ",
 PRINT "25 Breakable  ", "26 Heavy breakable",
 PRINT "27 Smasher/Hammer", "28 Immunity   ",
 PRINT "29 Need container", "30 Container to get",
 PRINT "31 Wound Medicine", "32 Poison Antidote",
 PRINT "33 Component/Parts", "34 Prints text file",
 PRINT "35 Healing{PW}  ", "36 Transport{PW}",
 PRINT "37 Heavy Container{PW}", "38 Luminater{PW}",
 PRINT "39 Kill near hostils{PW}", "40 Entrap near hostils{PW}",
 PRINT "41 Screen file    ", "42 Components/parts{PW}"
 IF S21 = 3 OR DD = 1 THEN
   PRINT "46 Armor/shield 1  ", "47 Armor/shield 2  ",
   PRINT "48 Head gear       ", "49 Chest Armor     ",
   PRINT "50 Gloves          ", "51 Leggings        ",
   PRINT "52 Boots           ", "53 Shields         "
 ENDIF
 LOCATE 18, 1
 PRINT "{72} GOTO OBJECT   {73} MAIN MENU   {74} SEARCH   {75} PREVIOUS";
 IF DD THEN PRINT "   {76} D&D"
 LOCATE 20, 1
 PRINT "Which ?"
 OT = OT(O)
 MODE = 1
 EDITVALUE OT
 IF OT = 72 THEN INPUT "What object number ? ", O
 IF OT = 74 THEN GOSUB SEARCH.OBJECTS
 IF OT = 75 OR pk = 1 THEN O = O - 1
 IF pk = 2 THEN O = O + 1
 IF pk = 3 THEN O = 1
 IF pk = 4 THEN O = OBJ + 1
 IF OT = 76 AND DD THEN GOSUB GET.DD.OBJECTS
 IF OT < 72 AND pk = -1 THEN
  OT(O) = OT
  IF OT > 47 AND OT < 72 AND DD THEN GOSUB EDIT.DD.OBJECT
  SELECT CASE OT
    CASE IS = 3
      P$ = "How many hitpoints damage will it cause to hostilities"
    CASE IS = 4
      P$ = "How many units of ammunition"
    CASE IS = 5
      P$ = "Addional turns added when consumed."
    CASE IS = 6, 46, 47, 48, 49, 50, 51, 52, 53
      P$ = "Number of armor points this object protects at."
    CASE IS = 7
      P$ = "Detonater object number must have to use."
    CASE IS = 20
      P$ = "How many extra lives will be given."
    CASE IS = 9
      P$ = "To what place number does it transport"
    CASE IS = 11
      P$ = "1-Can be used in any direction.2-Only go up.3-Only go down.4-Only in other exits"
    CASE IS = 13
      P$ = "How many turns will it last"
    CASE IS = 14
      P$ = "How many additional turns will it add"
    CASE IS = 25
      P$ = "What object is inside."
    CASE IS = 28
      P$ = "How many turns will be immune for"
    CASE IS = 30
      P$ = "Amount of effect."
    CASE IS = 28
      P$ = "When collected, becomes: 1-Food/water 2-Healing potion, 3-poison, 4-immunity"
    CASE IS = 16
      P$ = "How many shield points will be gained when swallowed"
    CASE IS = 17
      P$ = "1 to kill,2 loose shield points,3 Loose shield points per turn until cured."
    CASE IS = 18
      P$ = "How many more turn added if found"
    CASE IS = 19
      P$ = "What is the object number of object in  side?"
    CASE IS = 35
      P$ = "How many hit points will be added."
    CASE IS = 36
      P$ = "To what place will it transport to."
    CASE IS = 36
      P$ = "Enter next component/part number, or 0 if this is the last object."
    CASE IS = 21
      P$ = "How many more items can be carried."
    CASE IS = 22, 23, 24, 26, 37
      GOSUB Container.type
      P$ = "<SKIP>"
    CASE IS > 47
      P$ = "What is the first object varible"
    CASE ELSE
      P$ = "<SKIP>"
  END SELECT
  IF NOT P$ = "<SKIP>" THEN
    PRINT " Object "; O
    CLS
    PRINT P$
    EDITVALUE OV(O)
  ENDIF
  P$ = "<SKIP>"
  SELECT CASE OT
    CASE IS = 3
      P$ = "Enter 0-Used by itself, object number-ammunition, 500-must be thrown, 510-usedonly once."
    CASE IS = 17 AND OV(O) = 2
      P$ = "How many shield points lost."
    CASE IS = 17 AND OV(O) = 3
      P$ = "How many shield points lost per turn."
    CASE IS = 19
      P$ = "If it requires a key to open it, enter the object number, or else enter 0."
    CASE IS = 22, 23, 24, 26, 37
      IF OT = 37 THEN GOSUB Edit.password.number
      A = OV(O)
      IF A > 99 THEN
        A = A - (INT(A / 100) * 100)
      ENDIF
      IF A = 1 THEN
        P$ = "What object number is inside."
      ENDIF
      IF A > 1 AND A < 9 THEN P$ = "What place# will it lead to."
      IF OT = 24 AND A = 9 THEN
        P$ = "How many points of damage."
      ENDIF
      IF NOT OT = 24 AND A = 9 THEN
        P$ = "What hostile# is under or behind."
      ENDIF
      IF A = 10 THEN
        GOSUB Edit.range
      ENDIF
    CASE IS = 29
      P$ = "Number of times it can be collected."
    CASE IS = 34, 35, 36, 38, 39, 40
      P$ = "What Password/spell to activate it."
    CASE IS = 33 AND OV(O) = 0
    P$ = "Object number it will become when assembled. 1000 + Place number become other exit."
    CASE IS > 43
      P$ = "Enter 2nd object variable"
    CASE ELSE
      P$ = "<SKIP>"
  END SELECT
  IF NOT P$ = "<SKIP>" THEN
    CLS
    PRINT " Object "; O
    LOCATE 1, 1
    PRINT P$
    EDITVALUE OV2(O)
  ENDIF
  CLS
  IF OT < 72 THEN
    PRINT "Enter the place number of where this object is. Enter 0 for it to be in the"
    PRINT "inventory at the start of game. 500 for it to be inside of another object."
    PRINT "2000 + hostility number for it to be a reward for killing it."
    EDITVALUE OL(O)
    PRINT
    PRINT " What is the object credit value.Enter 0 if no value."
    EDITVALUE Op(O)
    PRINT
    PRINT " What is the name of this object"
    EDITSTRING ON$(O)
    PRINT
    IF (OT(O) = 23 OR OT(O) = 24 OR OT(O) = 26) AND OV(O) MOD 100 = 3 THEN
      PRINT "Name of other exit."
    ELSEIF (OT(O) = 23 OR OT(O) = 24 OR OT(O) = 26) AND OV(O) MOD 100 = 11 THEN
      PRINT "Enter information given."
    ELSEIF OT(O) = 12 THEN
      PRINT "What is written in this document"
    ELSEIF OT(O) = 34 THEN
      PRINT "Enter name of text file with extension."
    ELSEIF OT(O) = 41 THEN
      PRINT "Enter name of text file with extension."
    ELSE
      PRINT "Enter Description of the object."
    ENDIF
    EDITSTRING OD$(O)
    O = O + 1
    IF O > OB.DIM THEN
      OB.DIM = OB.DIM + 64
      REDIM OT(OB.DIM), Op(OB.DIM), OV(OB.DIM), OV2(OB.DIM), OV3(OB.DIM)
      REDIM OL(OB.DIM), ON$(OB.DIM), OD$(OB.DIM)
    ENDIF
  ENDIF
 ENDIF
LOOP UNTIL OT = 73
IF O - 1 > OBJ THEN OBJ = O - 1
RETURN

SEARCH.OBJECTS:
 IF O >= OBJ THEN
   OBJ = O
 ENDIF
 DO
  CLS
  PRINT " Object:"; O, ON$(O),
  IF OT = 1 THEN
    PRINT "Treasure"
  ENDIF
  IF OT = 2 THEN
    PRINT "Key "
  ENDIF
  IF OT = 3 THEN
    PRINT "Weapon "
    PRINT OV(O); " Damage"
    IF OV2(O) > 0 AND OV2(O) < 500 THEN
      PRINT "Ammunition:"; ON$(OV2(O))
    ENDIF
  ENDIF
  IF OT = 4 THEN
    PRINT "Ammunition "; OV(O); " Units."
  ENDIF
  IF OT = 5 THEN
    PRINT "Food/Water "; OV(O); " Turns."
  ENDIF
  IF OT = 6 THEN
    PRINT "Armor/Shield "; OV(O); "%."
  ENDIF
  IF OT = 7 THEN
    PRINT "Explosive "; ON$(OV(O)); "("; OV(O); ")"
  ENDIF
  IF OT = 8 THEN
    PRINT "Detonator "
  ENDIF
  IF OT = 9 THEN
    PRINT "Transport to place "; OV(O)
  ENDIF
  IF OT = 10 THEN
    PRINT "Shovel"
  ENDIF
  IF OT = 11 THEN
    PRINT "Climbing tool"
  ENDIF
  IF OT = 12 THEN
    PRINT "Document"
  ENDIF
  IF OT = 13 THEN
    PRINT "Lumination object for "; OV(O); " Turns."
  ENDIF
  IF OT = 14 THEN
    PRINT "Fuel for "; OV(O); " Turns."
  ENDIF
  IF OT = 15 THEN
    PRINT "Survival object "
  ENDIF
  IF OT = 16 THEN
    PRINT "Adds "; OV(O); " shield points."
  ENDIF
  IF OT = 17 THEN
    PRINT "Poison Food/water."
  ENDIF
  IF OT = 18 THEN
    PRINT "Adds "; OV(O); " turns."
  ENDIF
  IF OT = 19 THEN
    PRINT "Contains "; ON$(OV(O))
    IF OV2(O) > 0 THEN
      PRINT "opened by "; ON$(OV2(O))
    ENDIF
  ENDIF
  IF OT = 20 THEN
    PRINT "Adds "; OV(O); " life(s)."
  ENDIF
  IF OT = 21 THEN
    PRINT "Adds "; OV(N); " to inventory"
  ENDIF
  IF OT = 22 THEN
    PRINT "Heavy container, must be opened."
    PRINT "Codes:("; OV(O); "),("; OV2(O); ")."
  ENDIF
  IF OT = 23 THEN
    PRINT "Too heavy, must be push or pulled."
    PRINT "Codes:("; OV(O); "),("; OV2(O); ")."
  ENDIF
  IF OT = 24 THEN
    PRINT "Must be burned."
    PRINT "Codes:("; OV(O); "),("; OV2(O); ")."
  ENDIF
  IF OT = 25 THEN
    PRINT "Must be broken, contains object "; OV2(N)
  ENDIF
  IF OT = 26 THEN
    PRINT "Must be broken, but heavy or big."
    PRINT "Codes:("; OV(O); "),("; OV2(O); ")."
  ENDIF
  IF OT = 27 THEN
    PRINT "Hammer or smasher to break objects."
  ENDIF
  IF OT = 28 THEN
    PRINT "Immunity from hostilities for "; OV(O); " turns"
  ENDIF
  IF OT = 29 THEN
    PRINT "Need container to get."
    PRINT "Get only "; OV2(O); " times."
    PRINT "Code:"; OV(O)
  ENDIF
  IF OT = 30 THEN
    PRINT "Empty container to get."
    PRINT "Ammount of effect "; OV(O)
  ENDIF
  IF OT = 31 THEN
    PRINT "Medicine for infected wounds."
  ENDIF
  IF OT = 32 THEN
    PRINT "Antidote for poison."
  ENDIF
  IF OT = 33 THEN
    PRINT "Component/Part."
  ENDIF
  IF OT = 34 THEN
    PRINT "Displays text file."
  ENDIF
  IF OT = 35 THEN
    PRINT "Heals - Password/spell "; (OV2(O)); " by "; OV(O); " Points"
  ENDIF
  IF OT = 36 THEN
    PRINT "Transports - Password/spell "; OV2(O); " to place "; OV(O)
  ENDIF
  IF OT = 37 THEN
    PRINT "Container-Password/Spell "; OV2(O); "."; ON$(OV(O))
  ENDIF
  IF OT = 38 THEN
    PRINT "Luminating - Password/spell "; OV2(O); "."; OV(O); " Turns added"
  ENDIF
  IF OT = 39 THEN
    PRINT "Kills all hostilities in vicinity."
    PRINT "Password "; OV2(O)
  ENDIF
  IF OT = 40 THEN
    PRINT "Entraps all hostilities in vicinity."
    PRINT "Password "; OV2(O)
  ENDIF
  PRINT "Place"; OL(O), "Value"; Op(O); ; " "; Cu$
  PRINT
  PRINT OD$(O)
  KI$ = ""
  DO
    KI$ = UCASE$(INKEY$)
  LOOP UNTIL KI$ <> ""
  IF LEFT$(KI$, 1) = CHR$(0) THEN
    P = ASC(RIGHT$(KI$, 1))
  ELSE
    P = ASC(KI$)
  ENDIF
  IF (P = 77 OR P = 80) AND O <= OBJ THEN O = O + 1
  IF (P = 72 OR P = 75) AND O > 1 THEN O = O - 1
 LOOP UNTIL P = 13
RETURN

Edit.range:
  A = 0
  B = 0
  IF OV2(O) > 999 THEN
    F = INT(OV2(O) / 1000)
    E = OV2(O) - F * 1000
  ENDIF
  PRINT "What is the first object in the range."
  A = F
  EDITSTRING TEMP1$
  F = A
  PRINT
  PRINT "What is the last object in the range"
  A = E
  EDITSTRING TEMP1$
  E = A
  OV2(O) = E + F * 1000
RETURN

Edit.password.number:
  LOCATE 8, 1
  PRINT "What Password/spell will open it (Must enter password number)"
  A = 0
  B = OV(O)
  IF OV(O) > 99 THEN
    A = INT(OV(O) / 100)
    B = OV(O) - A * 100
  ENDIF
  LOCATE 10, 1
  PRINT A
  LOCATE 10, 1
  INPUT A
  IF A > 0 THEN
    OV(O) = OV(O) + 100 * A
  ENDIF
  IF B = 10 THEN
    GOSUB Edit.range
  ENDIF
RETURN

Container.type:
  CLS
  PRINT "Reveals:1-Object, 2-Down exit, 3-other exit, 4-North, 5-East, 6-West, 7-South,"
  PRINT "        8 Up, ";
  IF OT = 24 THEN
    PRINT " 9-Explosion, ";
  ELSE
    PRINT " 9-Hostility, ";
  ENDIF
  PRINT " 10-Range of objects, ";
  PRINT " 11-Information."
  PRINT
  B = 0
  A = OV(O)
  IF OV(O) > 99 THEN A = A MOD 100
  EDITVALUE A
  OV(O) = (100 * INT(OV(O) / 100)) + A
RETURN

GET.DD.OBJECTS:
  CLS
  PRINT "Object number "; O; " type"
  PRINT " Dungeons and Dragons game object list."
  PRINT "54 Magicians Robe   ", "55 Magicians Hat ",
  PRINT "56 Magicians Boots  ", "57 Magicians Weapon",
  PRINT "58 Weapon     ", "59 Casts Spell   ",
  PRINT "60 Adds Constitution", "61 Adds Dexterity",
  PRINT "62 Adds Intelligence", "63 Adds Strengh  ",
  PRINT "64 Adds Wisdom   ", "65 Adds Experience",
  PRINT "66 Adds Psychic Pts ", "67 Adds fire level",
  PRINT "68 Adds Heal level  ", "69 Adds Shield level",
  PRINT "70 Revives player   "
  PRINT "72 Back"
  LOCATE 20, 1
  PRINT "Which ?"
  OT = OT(O)
  EDITVALUE OT
  IF OT < 72 THEN GOSUB EDIT.DD.OBJECT
RETURN

EDIT.DD.OBJECT:
  OT(O) = OT
  IF OT > 47 AND OT < 57 THEN
    PRINT " How many shield points will be added?"
  ENDIF
  IF OT = 57 OR OT = 58 THEN
    PRINT " How many hitpoints of damage."
  ENDIF
  IF OT = 59 THEN
    PRINT " What spell is casted?"
  ENDIF
  IF OT > 59 AND OT < 70 THEN
    PRINT " How many points will be added?"
  ENDIF
  A = OV(O)
  EDITVALUE A
  OV(O) = A
  IF OT = 57 OR OT = 58 THEN
    PRINT "CODES:"
    PRINT "1-112 Ammunition"
    PRINT "500 Must be thrown"
    PRINT "510 Used once"
    E = OV2(O) AND 4095
    F = (OV2(O) AND 28672) / 4096
    A = E
    EDITVALUE A
    A = F
    PRINT "What Level must be at to use (1-8)?"
    EDITVALUE A
    F = A
    OV2(O) = E + 4096 * F
  ENDIF
  IF OT = 59 THEN
    PRINT "How many times it can be used?"
    EDITVALUE OV2(O)
  ENDIF
RETURN

Edit.hostility.data:
 H = HOS + 1
 DO
   CLS
   PRINT USING " Action of hostility ## does"; H
   PRINT "1-Shield point damage"
   PRINT "2-Kills player if not killed first."
   PRINT "3-Takes away all objects"
   PRINT "4-Takes away Treasures"
   PRINT "5-Takes away Keys"
   PRINT "6-Capable of moving objects around"
   IF DD = 0 THEN
     PRINT "7-Prevents from exiting"
     PRINT "8-Prevents from getting objects"
   ENDIF
   IF DD = 1 THEN
     PRINT "7-Causes armor dagage"
     PRINT "8-Causes experience damage"
   ENDIF
   PRINT "9-Puts player in other place."
   PRINT "10-Steals credit units"
   PRINT "11-Revives Hostilities."
   PRINT "12-Killed by certain weapon, causes shield damage."
   PRINT "13-Killed by password/spell, causes shield damage."
   PRINT "14-Removed bt password/spell, prevents from exiting."
   PRINT "15-Takes away all weapons."
   PRINT "16-Bites and infects."
   PRINT "17-Bites and poisons."
   PRINT
   PRINT "32 GOTO NUM   33 MAIN MENU   34 SEARCH"
   PRINT "Which ?"
   MODE = 1
   HT = HT(H)
   EDITVALUE HT
   IF pk = 1 AND H > 1 THEN H = H - 1
   IF pk = 2 AND H < HOS + 1 THEN H = H + 1
   IF pk = 3 THEN H = 1
   IF pk = 4 THEN H = HOS + 1
   IF HT = 32 THEN
     INPUT "Which Hostility number ", H
   ENDIF
   IF HT = 34 THEN GOSUB Search.hostility.data
    IF HT < 31 AND pk = -1 THEN
     HT(H) = HT
     CLS
     PRINT "Hostility "; H
     PRINT "Which place number is the hostility in"
     EDITVALUE HL(H)
     IF HT(H) = 1 OR HT(H) = 12 OR HT(H) = 13 OR HT(H) = 16 OR HT(H) = 17 OR (DD = 1 AND (HT(H) = 7 AND HT(H) = 8)) THEN
       PRINT "Amount of damage per attack/turn."
       EDITVALUE HS(H)
     ENDIF
     IF HT(H) <> 6 THEN
       IF HT(H) < 12 OR HT(H) > 14 THEN
         PRINT "How many hit points does it have"
       ENDIF
       IF HT(H) = 12 THEN
         PRINT "Object number of weapon to defeat it."
       ENDIF
       IF HT(H) = 13 OR HT(H) = 14 THEN
         PRINT "Password/Spell number to defeat it."
       ENDIF
       EDITVALUE HH(H)
     ENDIF
     IF S11 = 3 THEN
       CLS
       PRINT "1 hostility is stationary"
       PRINT "2 New location for every turn"
       PRINT "3 Moves every turn"
       PRINT "4 Increments place number"
       PRINT "5 Moves N,S,W,E only"
       PRINT "6 Appears anywhere"
       PRINT "7 Decrement place number"
       PRINT "8 Pursues player"
       EDITVALUE HM(H)
     ENDIF
     IF HT(H) = 9 THEN
       PRINT "What place number does it put player"
       EDITVALUE HS(H)
     ENDIF
     IF HT(H) = 10 THEN
       PRINT "How many credit units it takes per turn"
       EDITVALUE HS(H)
     ENDIF
     IF HT(H) = 16 OR HT(H) = 17 THEN
       PRINT "How many shields points lost per turn"
       EDITVALUE HS(H)
     ENDIF
     PRINT "What is the name of the hostility"
     EDITSTRING HN$(H)
     H = H + 1
     IF H > HS.DIM THEN
       HS.DIM = HS.DIM + 32
       REDIM HT(HS.DIM), HL(HS.DIM), HS(HS.DIM), HH(HS.DIM), HM(HS.DIM)
       REDIM HV(HS.DIM), HN$(HS.DIM)
     ENDIF
   ENDIF
  LOOP UNTIL HT = 33
  IF H - 1 > HOS THEN HOS = H - 1
RETURN

Search.hostility.data:
 IF H >= HOS THEN HOS = H
 DO
  CLS
  PRINT " Hostility:"; H; "   "; HN$(H)
  PRINT
  IF HT(H) = 1 OR HT(H) = 12 OR HT(H) = 13 THEN
    PRINT "Shield point damage:"; HS(H)
    PRINT
  ENDIF
  IF HT(H) = 12 THEN
    PRINT "Defeated by weapon "; ON$(HH(H)); "("; HH(H); ")"
  ENDIF
  IF HT(H) = 13 THEN
    PRINT "Defeated by Password "; HH(H)
  ENDIF
  IF HT(H) = 14 THEN
    PRINT "Removed by Password "; HH(H)
  ENDIF
  IF HT(H) = 15 THEN
    PRINT "Takes away all weapons"
  ENDIF
  IF HT(H) = 16 THEN
    PRINT "Bites and infects, lose "; HH(H); " per turn"
  ENDIF
  IF HT(H) = 17 THEN
    PRINT "Bites and poisons, lose "; HH(H); " per turn"
  ENDIF
  IF HT(H) = 2 THEN
    PRINT "Kills Player"
  ENDIF
  IF HT(H) = 3 THEN
    PRINT "Takes away objects"
  ENDIF
  IF HT(H) = 4 THEN
    PRINT "Takes away treasures"
  ENDIF
  IF HT(H) = 5 THEN
    PRINT "Takes away keys"
  ENDIF
  IF HT(H) = 6 THEN
    PRINT "Moves objects around"
  ENDIF
  IF HT(H) = 7 THEN
    PRINT "Prevents from exiting"
  ENDIF
  IF HT(H) = 8 THEN
    PRINT "Prevents from getting objects"
  ENDIF
  IF HT(H) = 9 THEN
    PRINT "Puts player in other place"
  ENDIF
  IF HT(H) = 10 THEN
    PRINT "Steals "; Cu$
  ENDIF
  IF HT(H) = 11 THEN
    PRINT "Revives other hostilities"
  ENDIF
  IF HT(H) < 12 OR HT(H) = 15 THEN
    PRINT "HP:"; HH(H),
  ENDIF
  PRINT "Place:"; HL(H), "Move Code:"; HM(H)
  KI$ = ""
  DO
    KI$ = UCASE$(INKEY$)
  LOOP UNTIL KI$ <> ""
  IF LEFT$(KI$, 1) = CHR$(0) THEN
    P = ASC(RIGHT$(KI$, 1))
  ELSE
    P = ASC(KI$)
  ENDIF
  IF (P = 77 OR P = 80) AND H <= HOS THEN H = H + 1
  IF (P = 72 OR P = 75) AND H > 1 THEN H = H - 1
 LOOP UNTIL P = 13
RETURN

Edit.Place.data:
 R = PLC + 1
 DO
  CLS
  PRINT " Place "; R
  PRINT
  PRINT " 1-Normal place."
  PRINT " 2-Game ending trap."
  PRINT " 3-Shield point loosing trap."
  PRINT " 4-Need luminating object for description."
  PRINT " 5-Need survival object."
  PRINT " 6-Have to kill hostility for object(s)."
  PRINT " 7-Trading place/market."
  PRINT " 8-(Question/Riddle)."
  PRINT " 9-Credit unit loosing trap."
  PRINT "10-Safety/Sanctuary."
  PRINT "11-Password to transport to other place."
  PRINT "12-Action to go to place."
  PRINT "13-Named teleport station."
  PRINT "14-Unlock doors lock when entered."
  PRINT "15-Return to place when killed."
  PRINT
  PRINT "{16} GOTO   {17} MAIN  {18} COMMON  {19} SEARCH"
  LOCATE 20, 1
  MODE = 1
  RT = RT(R)
  PRINT "Which ?"
  EDITVALUE RT
  IF RT = 8 AND pk = -1 THEN
    RT(R) = 8
    EXEC Edit_Riddle
  ENDIF
  IF pk > 0 THEN
    IF pk = 1 THEN R = R - 1
    IF pk = 2 THEN R = R + 1
    IF pk = 3 THEN R = 1
    IF pk = 4 THEN R = PLC + 1
  ELSE
    IF RT = 12 THEN GOSUB Command.phrase
    IF RT = 16 THEN INPUT "Which place number ", R
    IF RT = 18 THEN GOSUB Change.common
    IF RT = 19 THEN GOSUB Search.places
  ENDIF
  CLS
  IF RT < 16 AND RT <> 8 AND pk = -1 THEN
    RT(R) = RT
    PRINT " Place "; R
    PRINT "North exit. 0-No exit. 1-Open. 2-Unlocked. 3-Is locked. 4-Secret exit.          5-Password to open."
    EDITVALUE RN(R)
    IF RN(R) > 0 THEN
      PRINT " What place number it leads to"
      EDITVALUE RNL(R)
      IF RN(R) = 2 OR RN(R) = 3 THEN
        PRINT " What key object number"
        EDITVALUE RNK(R)
      ENDIF
      IF RN(R) = 5 THEN
        PRINT " What password/spell number."
        EDITVALUE RNK(R)
      ENDIF
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "West exit. 0-No exit. 1-Open. 2-Unlocked. 3-Is locked. 4-Secret exit.           5-Password to open."
    EDITVALUE RW(R)
    IF RW(R) > 0 THEN
      PRINT " What place number it leads to"
      EDITVALUE RWL(R)
      IF RW(R) = 2 OR RW(R) = 3 THEN
        PRINT " What key object number"
        EDITVALUE RWK(R)
      ENDIF
      IF RW(R) = 5 THEN
        PRINT " What password/spell number."
        EDITVALUE RWK(R)
      ENDIF
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "East exit. 0-No exit. 1-Open. 2-Unlocked. 3-Is locked. 4-Secret exit.           5-Password to open."
    EDITVALUE RE(R)
    IF RE(R) > 0 THEN
      PRINT "What place number it leads to"
      EDITVALUE REL(R)
      IF RE(R) = 2 OR RE(R) = 3 THEN
        PRINT " What key object number"
        EDITVALUE REK(R)
      ENDIF
      IF RE(R) = 5 THEN
        PRINT " What password/spell number."
        EDITVALUE REK(R)
      ENDIF
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "South exit. 0-No exit. 1-Open. 2-Unlocked. 3-Is locked. 4-Secret exit.          5-Password to open."
    EDITVALUE RS(R)
    IF RS(R) > 0 THEN
      PRINT "What place number it leads to"
      EDITVALUE RSL(R)
      IF RS(R) = 2 OR RS(R) = 3 THEN
        PRINT " What key object number"
        EDITVALUE RSK(R)
      ENDIF
      IF RS(R) = 5 THEN
        PRINT " What password/spell number."
        EDITVALUE RSK(R)
      ENDIF
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "Up exit.0-No exit.1-Open.2-Need climbing tool.4-Secret exit."
    EDITVALUE RU(R)
    IF RU(R) > 0 THEN
      PRINT " What place number it leads to"
      EDITVALUE RUL(R)
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "Down exit.0-No exit.1-Open.2-Need climbing tool.3-Need to be dugged up.4-Secret exit."
    EDITVALUE RD(R)
    IF RD(R) > 0 THEN
      PRINT " What place number it leads to"
      EDITVALUE RDL(R)
    ENDIF
    CLS
    IF RT(R) = 11 OR RT(R) = 13 THEN
      GOSUB Edit.teleport
    ELSE
      IF RT(R) <> 12 THEN
        PRINT " Place "; R, "Other exit."
        PRINT "0-No exit."
        PRINT "1-Open."
        PRINT "2-Needs to be dugged up."
        PRINT "3-Climbing tool needed."
        PRINT "4-Needs to blown open."
        PRINT "5-Password to open."
        PRINT "6-Password to appear."
        PRINT "7-Object/place that can be entered."
        PRINT "8-Must be assembled."
        EDITVALUE RO(R)
        IF RO(R) > 0 THEN
          PRINT " What place number it leads to"
          B = ROL(R) AND 4095
          EDITVALUE B
          ROL(R) = ROL(R) AND 61440 + B
          PRINT " Name of exit"
          EDITSTRING REN$(R)
          IF RO(R) = 5 OR RO(R) = 6 THEN
            PRINT " What Password/Spell will cause effect."
            B = INT(ROL(R) / 4096)
            EDITVALUE B
            ROL(R) = ROL(R) AND 4095 + 4096 * B
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    CLS
    PRINT " Place "; R
    PRINT "Enter description of room. Objects, Exits, and Hostilities automatically print."
    EDITSTRING RD$(R)
  ENDIF
  IF pk = -1 AND RT < 16 THEN
    R = R + 1
    IF R >= RM.DIM THEN
      RM.DIM = RM.DIM + 64
      REDIM RT(RM.DIM), RN(RM.DIM), RNL(RM.DIM), RNK(RM.DIM), RW(RM.DIM), RWL(RM.DIM)
      REDIM RWK(RM.DIM), RE(RM.DIM), REL(RM.DIM), REK(RM.DIM), RS(RM.DIM), RSL(RM.DIM)
      REDIM RSK(RM.DIM), RU(RM.DIM), RUL(RM.DIM), RD(RM.DIM), RDL(RM.DIM), RO(RM.DIM)
      REDIM ROL(RM.DIM), ROV(RM.DIM), REN$(RM.DIM), RD$(RM.DIM)
    ENDIF
  ENDIF
 LOOP UNTIL RT = 17
 IF R - 1 > PLC THEN PLC = R - 1
RETURN

Change.common:
  PRINT "Change Common start phrase to?"
  IF B$ <> "     " THEN
    B$ = ""
  ENDIF
  EDITSTRING B$
RETURN

Edit.teleport:
  CLS
  PRINT " Place "; R
  MID$(TEMP1$, 16) = " "
  IF RT(R) = 13 THEN
    TEMP1$ = "          "
    REN$(N) = TEMP1$
    LOCATE 1, 1
    PRINT "Name of this teleport station?"
    EDITSTRING TEMP1$
    REN$(N) = TEMP1$
  ENDIF
  IF RT(R) = 11 THEN
    PRINT "What password/spell to activate?"
    LOCATE 2, 1
    PRINT RO(R): EDITSTRING TEMP1$
    A = RO(R)
    LOCATE 4, 1
    PRINT "What place will be transported to?"
    LOCATE 5, 1
    PRINT ROL(R)
    LOCATE 5, 1
    INPUT ROL(R)
  ENDIF
RETURN

PROC Edit_Riddle
  CLS
  PRINT USING " Place ### -(Question/Riddle)"; R
  PRINT "Enter (Question/riddle).(3 lines)."
  EDITSTRING RD$(R)
  LOCATE 7, 1
  PRINT "What is the correct answer?"
  LOCATE 9, 1
  EDITSTRING REN$(R)
  RS(R) = 0
  LOCATE 11, 1
  PRINT "Go to what place number when done ?"
  LOCATE 12, 1
  PRINT ROL(R)
  LOCATE 12, 1
  EDITVALUE ROL(R)
  CLS
  PRINT "When answer is correct 1-Put in certain place      2-Gain shield units        "
  PRINT "                       3-Gain credit units         4-Gain turns to solve game"
  PRINT "                       5-Gain turns for food/water 6-Extra lives"
  EDITVALUE RN(R)
  IF RN(R) = 1 THEN
    PRINT "What place"
  ENDIF
  IF RN(R) > 1 THEN
    PRINT "How many"
  ENDIF
  EDITVALUE RNL(R)
  CLS
  PRINT "When answer is       1-Put in certain place       2-Shield unit lost"
  PRINT "incorrect            3-Loose credit units         4-Loose turns to solve game"
  PRINT "                     5-loose turns for food/water 6-loose lives"
  EDITVALUE RW(R)
  IF RW(R) = 1 THEN
    PRINT "What place"
  ENDIF
  IF RW(R) > 1 THEN
    PRINT "How many"
  ENDIF
  EDITVALUE RWL(R)
  R = R + 1
ENDPROC

Search.places:
 IF PLC <= R THEN
   PLC = R
 ENDIF
 DO
  CLS
  PRINT " PLACE:"; R; "   ";
  IF RT(R) = 1 THEN
    PRINT "Normal"
  ENDIF
  IF RT(R) = 2 THEN
    PRINT "Game ending trap"
  ENDIF
  IF RT(R) = 3 THEN
    PRINT "Loose "; S17; " shield points"
  ENDIF
  IF RT(R) = 4 THEN
    PRINT "Need light"
  ENDIF
  IF RT(R) = 5 THEN
    PRINT "Need surival object"
  ENDIF
  IF RT(R) = 6 THEN
    PRINT "Kill hostility to get"
  ENDIF
  IF RT(R) = 7 THEN
    PRINT "Trade/Market"
  ENDIF
  IF RT(R) = 8 THEN
    PRINT "(Question/Riddle)"
  ENDIF
  IF RT(R) = 9 THEN
    PRINT "Loose "; S24; " "; Cu$
  ENDIF
  IF RT(R) = 10 THEN
    PRINT "Safety from hostiles"
  ENDIF
  IF RT(R) = 11 THEN
    PRINT "Spell to transport"
  ENDIF
  IF RT(R) <> 8 THEN
    PRINT
    PRINT "Exit          Type      Leads to      Key/Password"
    TEMP1$ = "          "
    PRINT "North", RN(R), RNL(R), RNK(R)
    PRINT "West", RW(R), RWL(R), RWK(R)
    PRINT "East", RE(R), REL(R), REK(R)
    PRINT "South", RS(R), RSL(R), RSK(R)
    PRINT "Up", RU(R), RUL(R)
    PRINT "Down", RD(R), RDL(R)
    IF RO(R) > 0 THEN
      PRINT REN$(R); RO(R), ROL(R)
    ENDIF
    TEMP1$ = RD$(R)
    PRINT "  "; TEMP1$
  ELSE
    PRINT RD$(R)
    PRINT
    PRINT REN$(R)
  ENDIF
  KI$ = ""
  DO
    KI$ = UCASE$(INKEY$)
  LOOP UNTIL KI$ <> ""
  IF LEFT$(KI$, 1) = CHR$(0) THEN
    P = ASC(RIGHT$(KI$, 1))
    IF (P = 77 OR P = 80) AND R <= PLC THEN R = R + 1
    IF (P = 72 OR P = 75) AND R > 1 THEN R = R - 1
  ELSE
    P = ASC(KI$)
    IF P = 78 AND RNL(R) > 0 AND RN(R) > 0 THEN
      R = RNL(R)
    ENDIF
    IF P = 69 AND REL(R) > 0 AND RE(R) > 0 THEN
      R = REL(R)
    ENDIF
    IF P = 87 AND RWL(R) > 0 AND RW(R) > 0 THEN
      R = RWL(R)
    ENDIF
    IF P = 83 AND RSL(R) > 0 AND RS(R) > 0 THEN
      R = RSL(R)
    ENDIF
    IF P = 85 AND RUL(R) > 0 AND RU(R) > 0 THEN
      R = RUL(R)
    ENDIF
    IF P = 68 AND RDL(R) > 0 AND RD(R) > 0 THEN
      R = RDL(R)
    ENDIF
    IF P = 79 AND ROL(R) > 0 AND RO(R) > 0 THEN
      R = ROL(R)
    ENDIF
  ENDIF
 LOOP UNTIL P = 13
RETURN

Command.phrase:
  PRINT "Enter the correct command phrase."
  EDITSTRING REN$(R)
  PRINT "When entry is correct, go to what place number, or 0 to remain in same place?"
  EDITVALUE RO(R)
  PRINT "When entry is not correct, go to what place number, or 0 to remain in same    place?"
  EDITVALUE ROL(R)
RETURN

SURE:
  IF Modified = 1 THEN
    CLS
    PRINT "Current modified file not saved. Save it now ({Y}es  {N}o  {C}ancil)"
    DO
      SURE$ = INPUT$(1)
    LOOP UNTIL SURE$ = "Y" OR SURE$ = "N" OR SURE$ = "C"
    IF SURE$ = "Y" THEN
      GOSUB Save.file
    ENDIF
  ELSE
    SURE$ = "N"
  ENDIF
RETURN

Edit.text.file:
  CLS
  INPUT "File Name>", B$
  P$ = B$
  CLOSE #1
  IF P$ <> "" THEN
    OPEN P$ FOR INPUT AS #1
    INPUT #1, A
    TEMP1$ = INPUT$(A, 1)
    CLOSE #1
  ENDIF
  CLS
  LOCATE 1, 1
  PRINT "Enter the text to be in the file."
  EDITSTRING TEMP1$
  CLS
  PRINT "File Name"
  EDITSTRING P$
  IF P$ <> "" THEN
    CLOSE #1
    OPEN P$ FOR OUTPUT AS #1
    PRINT #1, LEN(TEMP1$)
    PRINT #1, TEMP1$
    CLOSE #1
  ENDIF
RETURN

Load.file:
  CLS
  GOSUB Get.directory
  GOSUB Get.file.name
  OPEN F$ FOR INPUT AS #1
  INPUT #1, A
  Op$ = INPUT$(A, 1)
  INPUT #1, A
  Cp$ = INPUT$(A, 1)
  LINE INPUT #1, T$
  LINE INPUT #1, I$
  LINE INPUT #1, Nf$
  LINE INPUT #1, Cu$
  LINE INPUT #1, TEMP1$
  INPUT #1, S0
  INPUT #1, S1
  INPUT #1, S2
  INPUT #1, S3
  INPUT #1, S4
  INPUT #1, S5
  INPUT #1, S6
  INPUT #1, S7
  INPUT #1, S8
  INPUT #1, S9
  INPUT #1, S10
  INPUT #1, S11
  INPUT #1, S12
  INPUT #1, S13
  INPUT #1, S14
  INPUT #1, S15
  INPUT #1, S16
  INPUT #1, S17
  INPUT #1, S18
  INPUT #1, S19
  INPUT #1, S20
  INPUT #1, S21
  INPUT #1, S22
  INPUT #1, S23
  INPUT #1, S24
  INPUT #1, S25
  INPUT #1, S26
  INPUT #1, S27
  INPUT #1, S28
  INPUT #1, S29
  INPUT #1, S30
  INPUT #1, S31
  INPUT #1, S32
  INPUT #1, S33
  INPUT #1, S34
  INPUT #1, S35
  INPUT #1, S36
  INPUT #1, S37
  INPUT #1, S38
  INPUT #1, S39
  INPUT #1, S40
  INPUT #1, S41
  INPUT #1, S42
  INPUT #1, S43
  INPUT #1, S44
  INPUT #1, S45
  INPUT #1, S46
  INPUT #1, S47
  INPUT #1, PWD
  IF PWD > PW.DIM THEN
    PW.DIM = INT((PWD + 8) / 8) * 8
    REDIM PW$(PW.DIM)
  ENDIF
  FOR N = 0 TO PWD
   INPUT #1, PW$(N)
  NEXT N
  INPUT #1, OBJ
  IF OBJ > OB.DIM THEN
    OB.DIM = INT((OBJ + 64) / 64) * 64
    REDIM OT(OB.DIM), Op(OB.DIM), OV(OB.DIM), OV2(OB.DIM), OV3(OB.DIM)
    REDIM OL(OB.DIM), ON$(OB.DIM), OD$(OB.DIM)
  ENDIF
  FOR N = 1 TO OBJ
   INPUT #1, OT(N)
   INPUT #1, Op(N)
   INPUT #1, OL(N)
   INPUT #1, OV(N)
   INPUT #1, OV2(N)
   INPUT #1, OV3(N)
   INPUT #1, ON$(N)
   LINE INPUT #1, OD$(N)
  NEXT N
  INPUT #1, HOS
  IF HOS > HS.DIM THEN
    HS.DIM = INT((HOS + 32) / 32) * 32
    REDIM HT(HS.DIM), HL(HS.DIM), HS(HS.DIM), HH(HS.DIM), HM(HS.DIM)
    REDIM HV(HS.DIM), HN$(HS.DIM)
  ENDIF
  FOR N = 1 TO HOS
   INPUT #1, HL(N)
   INPUT #1, HT(N)
   INPUT #1, HS(N)
   INPUT #1, HH(N)
   INPUT #1, HV(N)
   INPUT #1, HM(N)
   LINE INPUT #1, HN$(N)
  NEXT N
   INPUT #1, PLC
   IF PLC > RM.DIM THEN
     RM.DIM = INT((PLC + 64) / 64) * 64
     REDIM RT(RM.DIM), RN(RM.DIM), RNL(RM.DIM), RNK(RM.DIM), RW(RM.DIM), RWL(RM.DIM)
     REDIM RWK(RM.DIM), RE(RM.DIM), REL(RM.DIM), REK(RM.DIM), RS(RM.DIM), RSL(RM.DIM)
     REDIM RSK(RM.DIM), RU(RM.DIM), RUL(RM.DIM), RD(RM.DIM), RDL(RM.DIM), RO(RM.DIM)
     REDIM ROL(RM.DIM), ROV(RM.DIM), REN$(RM.DIM), RD$(RM.DIM)
   ENDIF
   FOR N = 1 TO PLC
   INPUT #1, RT(N)
   INPUT #1, RN(N)
   INPUT #1, RNL(N)
   INPUT #1, RNK(N)
   INPUT #1, RW(N)
   INPUT #1, RWL(N)
   INPUT #1, RWK(N)
   INPUT #1, RE(N)
   INPUT #1, REL(N)
   INPUT #1, REK(N)
   INPUT #1, RS(N)
   INPUT #1, RSL(N)
   INPUT #1, RSK(N)
   INPUT #1, RU(N)
   INPUT #1, RUL(N)
   INPUT #1, RD(N)
   INPUT #1, RDL(N)
   INPUT #1, RO(N)
   INPUT #1, ROL(N)
   LINE INPUT #1, REN$(N)
   INPUT #1, ROV(N)
   LINE INPUT #1, RD$(N)
  NEXT N
  CLOSE #1
  R = PLC
  O = OBJ
  H = HOS
  Modified = 0
RETURN

Save.file:
   CLOSE #1
   IF F$ = "(Untitled)" THEN
     GOSUB Get.file.name
   ENDIF
   Modified = 0
   OPEN PATH$ + "\" + F$ FOR OUTPUT AS #1
   PRINT #1, LEN(Op$)
   PRINT #1, Op$;
   PRINT #1, LEN(Cp$)
   PRINT #1, Cp$;
   PRINT #1, T$
   PRINT #1, I$
   PRINT #1, Nf$
   PRINT #1, Cu$
   PRINT #1, "(FE)"
   PRINT #1, S0
   PRINT #1, S1
   PRINT #1, S2
   PRINT #1, S3
   PRINT #1, S4
   PRINT #1, S5
   PRINT #1, S6
   PRINT #1, S7
   PRINT #1, S8
   PRINT #1, S9
   PRINT #1, S10
   PRINT #1, S11
   PRINT #1, S12
   PRINT #1, S13
   PRINT #1, S14
   PRINT #1, S15
   PRINT #1, S16
   PRINT #1, S17
   PRINT #1, S18
   PRINT #1, S19
   PRINT #1, S20
   PRINT #1, S21
   PRINT #1, S22
   PRINT #1, S23
   PRINT #1, S24
   PRINT #1, S25
   PRINT #1, S26
   PRINT #1, S27
   PRINT #1, S28
   PRINT #1, S29
   PRINT #1, S30
   PRINT #1, S31
   PRINT #1, S32
   PRINT #1, S33
   PRINT #1, S34
   PRINT #1, S35
   PRINT #1, S36
   PRINT #1, S37
   PRINT #1, S38
   PRINT #1, S39
   PRINT #1, S40
   PRINT #1, S41
   PRINT #1, S42
   PRINT #1, S43
   PRINT #1, S44
   PRINT #1, S45
   PRINT #1, S46
   PRINT #1, S47
   PRINT #1, PWD
 FOR N = 0 TO PWD
   PRINT #1, PW$(N)
 NEXT N
 PRINT #1, OBJ
 FOR N = 1 TO OBJ
   PRINT #1, OT(N)
   PRINT #1, Op(N)
   PRINT #1, OL(N)
   PRINT #1, OV(N)
   PRINT #1, OV2(N)
   PRINT #1, OV3(N)
   PRINT #1, ON$(N)
   PRINT #1, OD$(N)
 NEXT N

 PRINT #1, HOS
 FOR N = 1 TO HOS
   PRINT #1, HL(N)
   PRINT #1, HT(N)
   PRINT #1, HS(N)
   PRINT #1, HH(N)
   PRINT #1, HV(N)
   PRINT #1, HM(N)
   PRINT #1, HN$(N)
 NEXT N

   PRINT #1, PLC
   FOR N = 1 TO PLC
   PRINT #1, RT(N)
   PRINT #1, RN(N)
   PRINT #1, RNL(N)
   PRINT #1, RNK(N)
   PRINT #1, RW(N)
   PRINT #1, RWL(N)
   PRINT #1, RWK(N)
   PRINT #1, RE(N)
   PRINT #1, REL(N)
   PRINT #1, REK(N)
   PRINT #1, RS(N)
   PRINT #1, RSL(N)
   PRINT #1, RSK(N)
   PRINT #1, RU(N)
   PRINT #1, RUL(N)
   PRINT #1, RD(N)
   PRINT #1, RDL(N)
   PRINT #1, RO(N)
   PRINT #1, ROL(N)
   PRINT #1, REN$(N)
   PRINT #1, ROV(N)
   PRINT #1, RD$(N)
 NEXT N
 CLOSE #1
 Modified = 0
RETURN


Get.directory:
  ON ERROR GOTO CHECK.ERROR
  CHDIR PATH$
  FILES "*.TGF"
  FILES "*.TDD"
  ON ERROR GOTO 0
RETURN

CHECK.ERROR:
  ON ERROR GOTO CHECK.ERROR
RESUME NEXT

Get.file.name:
  INPUT "Name of File:", TEMP1$
  TEMP1$ = UCASE$(TEMP1$)
  IF INSTR(1, TEMP1$, ".TGF") > 0 THEN DD = 0
  IF INSTR(1, TEMP1$, ".TDD") > 0 THEN DD = 1
  IF INSTR(1, TEMP1$, ".TGF") = 0 AND DD = 0 THEN TEMP1$ = TEMP1$ + ".TGF"
  IF INSTR(1, TEMP1$, ".TDD") = 0 AND DD = 1 THEN TEMP1$ = TEMP1$ + ".TDD"
  F$ = TEMP1$
  PRINT F$
RETURN

SUB DRAWIT
  IF PATTERN = 1 THEN
    SELECT CASE MODE
      CASE 1, 2
        FOR N = 0 TO SIZE
          CIRCLE (C, CSROW), N, CCOL
        NEXT N
      CASE 3
        FOR N = 0 TO SIZE
          CIRCLE (C, CSROW), N, 0
        NEXT N
      CASE 4
        IF PC > 0 THEN
          LINE (PC, PR)-(C, CSROW), CCOL
          PC = -1
          PR = -1
        ELSE
          PC = C
          PR = CSROW
          PSET (C, CSROW), CCOL
        ENDIF
      CASE 5
          LINE (PC, PR)-(C, CSROW), CCOL
          PC = C
          PR = CSROW
          PSET (C, CSROW), CCOL
      CASE 6
        IF PC > 0 THEN
          LINE (PC, PR)-(C, CSROW), CCOL, B
          PC = -1
          PR = -1
        ELSE
          PC = C
          PR = CSROW
          PSET (C, CSROW), CCOL
        ENDIF
      CASE 7
        IF PC > 0 THEN
          LINE (PC, PR)-(C, CSROW), CCOL, BF
          PC = -1
          PR = -1
        ELSE
          PC = C
          PR = CSROW
          PSET (C, CSROW), CCOL
        ENDIF
      CASE 8
        IF PC > 0 THEN
          A = SQR(ABS(PC - C) ^ 2 + ABS((PR - CSROW) * 1.55) ^ 2)
          CIRCLE (PC, PR), A, CCOL
          PC = -1
          PR = -1
        ELSE
          PC = C
          PR = CSROW
        ENDIF
      CASE 9
        IF PC > 0 THEN
          A = SQR(ABS(PC - C) ^ 2 + ABS((PR - CSROW) * 1.55) ^ 2)
          CIRCLE (PC, PR), A, CCOL
          PAINT (PC, PR), CCOL
          PC = -1
          PR = -1
        ELSE
          PC = C
          PR = CSROW
        ENDIF
    END SELECT
  ELSEIF PATTERN = 2 THEN
    FOR N = 0 TO SIZE
      C1! = INT(RND * 314)
      R1! = INT(RND * SIZE)
      PSET (C + SIN(C1!) * R1!, CSROW + COS(C1!) * R1!), CCOL
    NEXT N
  ELSEIF PATTERN = 3 THEN
     FOR N = INT(RND * 3) TO INT(SIZE / 7)
       C1 = C - SIZE + RND * (SIZE * 2)
       R1 = CSROW - SIZE + RND * (SIZE * 2)
       CIRCLE (C1, R1), INT(RND * RND * 4), CCOL
       IF INT(RND * 3) = 1 THEN PAINT (C1, R1), CCOL
     NEXT N
  ELSEIF PATTERN > 3 AND PATTERN < 9 THEN
    C1 = C - SIZE + RND * (SIZE * 2)
    R1 = CSROW - SIZE + RND * (SIZE * 2)
    C2 = C - SIZE + RND * (SIZE * 2)
    R2 = CSROW - SIZE + RND * (SIZE * 2)
    C3 = C - SIZE + RND * (SIZE * 2)
    R3 = CSROW - SIZE + RND * (SIZE * 2)
    IF PATTERN = 4 THEN LINE (C1, R1)-(C2, R2), CCOL
    IF PATTERN = 7 AND INT(RND * 3) = 0 THEN LINE (C1, R1)-(C2, R2), CCOL, B
    IF PATTERN = 8 AND INT(RND * 3) = 0 THEN LINE (C1, R1)-(C2, R2), CCOL, BF
    IF (PATTERN = 5 OR PATTERN = 6) AND INT(RND * 4) = 0 THEN
      LINE (C1, R1)-(C2, R2), CCOL
      LINE (C2, R2)-(C3, R3), CCOL
      LINE (C3, R3)-(C1, R1), CCOL
      IF PATTERN = 6 THEN PAINT ((C1 + C2 + C3) / 3, (R1 + R2 + R3) / 3), CCOL
    ENDIF
  ELSEIF PATTERN = 9 THEN
    A = INT(SIZE / 2)
    IF A < 2 THEN A = 2
    D = 0
    IF CSROW MOD (A * 2) < A THEN D = 1
    FOR N = CSROW - SIZE TO CSROW + SIZE
      IF N MOD A = 0 THEN
        LINE (C - SIZE, N)-(C + SIZE, N), CCOL
        D = 1 - D
        FOR M = C - SIZE TO C + SIZE
          IF M MOD (A * 2) = D * A THEN LINE (M, N)-(M, N + A - 1), CCOL
        NEXT M
      ENDIF
    NEXT N
  ENDIF
END SUB

SUB EDITSTRING (TEMP1$)
  IF CSRLIN > 23 THEN
    PRINT
    PRINT
    PRINT
    PRINT
    PRINT
    LOCATE CSRLIN - 5, 1
  ENDIF
  TopRow = CSRLIN
  V$ = TEMP1$
  C = LEN(V$) + 1
  INS = 1
  LOCATE TopRow, 1
  PRINT V$
  DO
    KI$ = INKEY$
    IF KI$ <> "" AND LEN(KI$) = 1 THEN
      IF ASC(KI$) > 31 THEN
         IF LEN(V$) < C THEN
            V$ = V$ + " "
            MID$(V$, C) = KI$
         ELSE
            IF INS = 1 THEN
               TEMP1$ = MID$(V$, C)
               V$ = LEFT$(V$, C - 1) + KI$ + TEMP1$
            ELSE
               MID$(V$, C) = KI$
            ENDIF
         ENDIF
         C = C + 1
      ELSEIF ASC(KI$) = 8 AND C > 1 THEN
         IF C = LEN(C$) THEN
            KI$ = ""
         ELSE
            KI$ = MID$(V$, C)
         ENDIF
         C = C - 1
         V$ = LEFT$(V$, C - 1) + KI$
         LOCATE TopRow, 1
         PRINT SPACE$(LEN(V$) + 1)
      ENDIF
      LOCATE TopRow, 1, 0
      PRINT V$
    ELSEIF LEFT$(KI$, 1) = CHR$(0) THEN
      K = ASC(RIGHT$(KI$, 1))
      IF K = 71 THEN C = 1
      IF K = 72 THEN
         C = C - 80
         IF C < 1 THEN C = 1
      ENDIF
      IF K = 75 AND C > 1 THEN C = C - 1
      IF K = 77 AND C <= LEN(V$) THEN C = C + 1
      IF K = 79 THEN C = LEN(V$) + 1
      IF K = 80 THEN
         C = C + 80
         IF C > LEN(V$) THEN C = LEN(V$) + 1
      ENDIF
      IF K = 82 THEN INS = 1 - INS
      IF K = 83 AND C < LEN(V$) - 1 THEN
        KI$ = LEFT$(V$, C)
        V$ = KI$ + MID$(V$, C + 2)
        LOCATE TopRow, 1
        PRINT SPACE$(LEN(V$) + 1)
      ENDIF
      LOCATE TopRow, 1, 0
      PRINT V$
    ENDIF
    CLM = ((C - 1) MOD 80) + 1
    Row = TopRow + INT((C - 1) / 80)
    LOCATE Row, CLM, 1, INS * 11, 13
    LOOP UNTIL KI$ = CHR$(13)
  PRINT
  TEMP1$ = V$
END SUB

SUB EDITVALUE (A)
  pk = -1
  IF CSRLIN > 24 THEN
    PRINT
    PRINT
    LOCATE CSRLIN - 2, 1
  ENDIF
  Row = CSRLIN
  V$ = STR$(A)
  IF A = 0 THEN
    C = 2
  ELSE
    C = LEN(V$) + 1
  ENDIF
  LOCATE Row, 1, 0
  PRINT ">"; V$
  LOCATE Row, 1, 1
  DO
   KI$ = INKEY$
   IF KI$ <> "" AND LEFT$(KI$, 1) <> CHR$(0) THEN
     IF ASC(KI$) = 45 OR ASC(KI$) = 46 OR (ASC(KI$) > 47 AND ASC(KI$) < 59) THEN
       IF VAL(V$) = 0 AND NOT LEFT$(V$, 1) = "-" THEN
          V$ = ""
          IF ASC(KI$) > 45 THEN
            V$ = " " + KI$
            C = 3
          ELSE
            V$ = "-0"
            C = 2
          ENDIF
       ELSE
          IF LEN(V$) < C THEN V$ = V$ + " "
          MID$(V$, C) = KI$
          C = C + 1
       ENDIF
     ELSEIF KI$ = CHR$(8) AND C > 1 THEN
       C = C - 1
       V$ = LEFT$(V$, C - 1)
       LOCATE Row, 1, 0
       PRINT SPACE$(LEN(V$) + 2)
     ENDIF
   ELSEIF LEFT$(KI$, 1) = CHR$(0) THEN
     K1 = ASC(RIGHT$(KI$, 1))
     IF K1 = 83 THEN
       LOCATE Row, 1, 0
       PRINT SPACE$(LEN(V$) + 1)
       V$ = " 0"
       C = 2
     ENDIF
     IF MODE = 1 THEN
       IF K1 = 73 THEN pk = 1
       IF K1 = 81 THEN pk = 2
       IF K1 = 71 THEN pk = 3
       IF K1 = 79 THEN pk = 4
     ENDIF
   ENDIF
   LOCATE Row, 1, 0
   PRINT ">"; V$
   LOCATE Row, C + 1, 1
   FOR DELAY = 0 TO 499
   NEXT DELAY
  LOOP UNTIL KI$ = CHR$(13) OR pk > 0
  A = VAL(V$)
  MODE = 0
  PRINT
END SUB

SUB GRAPHICFILE
  CHDIR PATH$
  CLS
  INPUT "GRAPHIC FILE NAME>", F$
  IF F$ <> "" THEN
    IF INSTR(1, F$, ".") = 0 THEN F$ = F$ + ".GRF"
    LOADPICTURE F$
  ELSE
    PRINT
    INPUT "HIGHT>", HIGHT
    PCOL = 0
    IF HIGHT < 4 THEN HIGHT = 200
    IF ADAPTER > 1 THEN
      INPUT "Do you want to create a color file? (Y,N):", TEMP1$
      IF TEMP1$ = "Y" THEN PCOL = 1
    ENDIF
    SETSCREEN
  ENDIF
  CSROW = INT(HIGHT / 2)
  C = 320
  UC = POINT(C, CSROW)
  SIZE = 3
  CCOL = 1
  BCOL = 0
  MODE = 1
  SKIP = 1
  PC = -1
  PR = -1
  PATTERN = 1
  SHOWCURSER
  ??  s ???B  ?       ???    ELSE ????  RETURN  ???   NEXT N ???  RETURN  ???   NEXT N ???  RETURN  ???  END SUB ???    PRINT ???    ELSE  ???    DO  b ???  END SUB ???B  ?       ???  END SUB ???B  ?       ???S   
    KI$ = INKEY$
    PSET (C, CSROW), UC
    IF LEN(KI$) = 1 THEN
      K1 = ASC(KI$)
      IF K1 = 13 THEN DRAWIT
      IF (K1 > 0 AND K1 <> 7 AND K1 < 9) OR (K1 > 13 AND K1 < 27) OR (K1 > 31) THEN
        LOCATE INT(CSROW / CHRHIGHT) + 1, INT(C / CHRWIDTH) + 1
        C = C + CHRWIDTH
        IF C > 640 - CHRWIDTH THEN
          C = 0
          CSROW = CSROW + CHRHIGHT
        ENDIF
        PRINT CHR$(K1)
      ENDIF
    ELSE
      IF LEFT$(KI$, 1) = CHR$(0) THEN
          K1 = ASC(RIGHT$(KI$, 1))
          IF K1 = 75 OR K1 = 71 OR K1 = 79 THEN
           C = C - SKIP
           IF C < 1 THEN C = 1
          ENDIF
          IF K1 = 72 OR K1 = 71 OR K1 = 73 THEN
            CSROW = CSROW - SKIP
            IF CSROW < 1 THEN CSROW = 1
          ENDIF
          IF K1 = 80 OR K1 = 79 OR K1 = 81 THEN
            CSROW = CSROW + SKIP
            IF CSROW > HIGHT - SIZE / 2 THEN CSROW = HIGHT - SIZE / 2
          ENDIF
          IF K1 = 77 OR K1 = 73 OR K1 = 81 THEN
            C = C + SKIP
            IF C > 640 - SIZE / 2 THEN C = 640 - SIZE / 2
          ENDIF
          IF (MODE = 2 OR MODE = 3) AND K1 > 70 AND K1 < 82 THEN DRAWIT
          IF K1 = 59 THEN
            LOCATE VR + 1, 1
            PRINT "Use the curser curser to move the curser keys around. Press RETURN to"
            PRINT "draw point. Any other key prints it on screen. Press ESCape to when  "
            PRINT "done to save and exit.                                               "
            DO
            LOOP UNTIL INKEY$ <> ""
            SETSCREEN
          ENDIF
          IF K1 = 60 THEN MODE = 1
          IF K1 = 61 THEN MODE = 2
          IF K1 = 62 THEN MODE = 3
          IF K1 = 63 THEN MODE = 4
          IF K1 = 64 THEN MODE = 5
          IF K1 = 65 THEN MODE = 6
          IF K1 = 66 THEN MODE = 7
          IF K1 = 84 THEN MODE = 8
          IF K1 = 85 THEN MODE = 9
          IF K1 = 94 THEN PATTERN = 1
          IF K1 = 95 THEN PATTERN = 2
          IF K1 = 96 THEN PATTERN = 3
          IF K1 = 97 THEN PATTERN = 4
          IF K1 = 98 THEN PATTERN = 5
          IF K1 = 99 THEN PATTERN = 6
          IF K1 = 100 THEN PATTERN = 7
          IF K1 = 101 THEN PATTERN = 8
          IF K1 = 102 THEN PATTERN = 9
          IF K1 = 103 THEN PATTENR = 10
          IF K1 = 67 THEN
            CCOL = CCOL + 1
            IF CCOL > NCOL THEN CCOL = 0
            SHOWCURSER
          ENDIF
          IF K1 = 68 THEN
            SIZE = SIZE + 1
            IF SIZE > 10 THEN SIZE = 0
            SHOWCURSER
          ENDIF
          IF K1 = 87 THEN
            SKIP = SKIP + 1
            IF SKIP > 16 THEN SKIP = 1
            SHOWCURSER
          ENDIF
          IF K1 = 88 THEN
            PSET (C, CSROW), 0
            PAINT (C, CSROW), CCOL
          ENDIF
          IF K1 > 93 AND K1 < 104 THEN SHOWCURSER
          IF (K1 > 59 AND K1 < 67) OR (K1 > 83 AND K1 < 87) THEN SHOWCURSER
      ENDIF
    ENDIF
    UC = POINT(C, CSROW)
    CSRCOL = 1
    IF UC = CSRCOL THEN CSRCOL = 0
    PSET (C, CSROW), CSRCOL
  LOOP UNTIL KI$ = CHR$(27)
  PSET (C, CSROW), UC
  SAVEPICTURE
  SCREEN 0, 0, 0
END SUB

SUB LASTCHAR
  IF (PD = 0 AND K1 = 72 AND pk = 77) OR (PD = 1 AND K1 = 75 AND pk = 80) THEN NC = 217 'Ù
  IF (PD = 0 AND K1 = 80 AND pk = 77) OR (PD = 1 AND K1 = 75 AND pk = 72) THEN NC = 191 '¿
  IF (PD = 0 AND K1 = 72 AND pk = 75) OR (PD = 1 AND K1 = 77 AND pk = 80) THEN NC = 192 'À
  IF (PD = 0 AND K1 = 80 AND pk = 75) OR (PD = 1 AND K1 = 77 AND pk = 72) THEN NC = 218 'Ú
  IF PD = 2 AND K1 = pk THEN NC = 197
  IF PD = 3 AND K1 <> pk THEN NC = 197
  IF AC = 196 AND K1 = 72 AND AD = 0 THEN NC = 193
  IF AC = 196 AND K1 = 80 AND AD = 0 THEN NC = 194
  IF AC = 179 AND K1 = 75 AND AD = 1 THEN NC = 180
  IF AC = 179 AND K1 = 77 AND AD = 1 THEN NC = 195
  IF NC > 0 THEN
    MID$(TEMP1$, W, 1) = CHR$(NC)
    LOCATE Row, C, 0
    PRINT CHR$(NC);
  ENDIF
END SUB

SUB LOADPICTURE (F$)
  OPEN F$ FOR INPUT AS #1
  INPUT #1, A
  INPUT #1, PCOL
  INPUT #1, HIGHT
  INPUT #1, A
  SETSCREEN
  FOR N = 1 TO HIGHT
    LINE INPUT #1, TEMP1$
    O = 1
    FOR M = 1 TO LEN(TEMP1$)
      P = ASC(MID$(TEMP1$, M, 1))
      TR = INT(P / 16)
      LINE (O, N)-(O + TR - 1, N), P AND 15
      O = O + TR
    NEXT M
  NEXT N
  CLOSE #1
END SUB

SUB NEXTCHAR
  CC = ASC(MID$(TEMP1$, W, 1))
  NC = -1
  AC = -1
  IF K1 = 75 OR K1 = 77 THEN
     IF CC = 32 OR CC = 196 THEN
       NC = 196
       PD = 0
     ELSE
       IF K1 = 75 AND CC = 179 THEN
         NC = 195
         PD = 2
       ENDIF
       IF K1 = 77 AND CC = 179 THEN
         NC = 180
         PD = 2
       ENDIF
       IF CC = 191 OR CC = 218 THEN
         NC = 194
         PD = 3
       ENDIF
       IF CC = 192 OR CC = 217 THEN
         NC = 193
         PD = 3
       ENDIF
       IF CC = 180 OR CC = 195 THEN NC = 197
     ENDIF
     IF C > 1 AND C < 80 THEN
       IF K1 = 75 THEN AC = SCREEN(Row, C - 1, 0)
       IF K1 = 77 THEN AC = SCREEN(Row, C + 1, 0)
       AD = 0
     ENDIF
  ENDIF
  IF K1 = 72 OR K1 = 80 THEN
     IF CC = 32 OR CC = 179 THEN
       NC = 179
       PD = 1
     ELSE
       IF K1 = 72 AND CC = 196 THEN
          NC = 194
          PD = 2
       ENDIF
       IF K1 = 80 AND CC = 196 THEN
          NC = 193
          PD = 2
       ENDIF
       IF CC = 192 OR CC = 218 THEN
          NC = 195
          PD = 3
       ENDIF
       IF CC = 191 OR CC = 218 THEN
          NC = 180
          PD = 3
       ENDIF
       IF CC = 193 OR CC = 194 THEN NC = 197
     ENDIF
     IF Row > 1 AND Row < 24 THEN
       IF K1 = 72 THEN AC = SCREEN(Row - 1, C, 0)
       IF K1 = 80 THEN AC = SCREEN(Row + 1, C, 0)
       AD = 1
     ENDIF
  ENDIF
END SUB

SUB SAVEPICTURE
  IF Y > 23 THEN Y = 23
  LOCATE VR, 1
  PRINT SPACE$(240);
  LOCATE VR, 1
  INPUT "FILE NAME>", F$
  IF F$ <> "" THEN
   IF INSTR(1, F$, ".") = 0 THEN F$ = F$ + ".GRF"
   OPEN UCASE$(F$) FOR OUTPUT AS #1
   PRINT #1, ADAPTER
   PRINT #1, PCOL
   PRINT #1, HIGHT
   PRINT #1, 0
   FOR N = 1 TO HIGHT
    TEMP1$ = ""
    PP = POINT(1, N)
    TR = 1
    FOR M = 1 TO 640
      P = POINT(M, N)
      IF P = PP AND M < 640 AND TR < 15 THEN
        TR = TR + 1
      ELSE
        TEMP1$ = TEMP1$ + CHR$(PP + TR * 16)
        TR = 1
      ENDIF
      PP = P
    NEXT M
    PRINT #1, TEMP1$
   NEXT N
   CLOSE #1
  ENDIF
END SUB

SUB SCREENFILE
  TEMP1$ = SPACE$(1920)
  CLS
  LOCATE 1, 1
  CHDIR PATH$
  INPUT "Enter screen file name:", B$
  IF B$ <> "" THEN
     OPEN B$ FOR INPUT AS #1
     TEMP1$ = INPUT$(1920, 1)
     CLOSE #1
  ENDIF
  LOCATE 1, 1
  PRINT TEMP1$;
  CSROW = 1
  C = 1
  M = 0
  DO
    KI$ = INKEY$
    IF KI$ <> "" THEN
      LOCATE CSROW, C
      COLOR 7, 0
      PRINT CHR$(UC);
    ENDIF
    IF LEFT$(KI$, 1) = CHR$(0) THEN
      K1 = ASC(RIGHT$(KI$, 1))
      W = C + 80 * (CSROW - 1)
      IF M = 5 THEN LASTCHAR
      IF K1 = 72 THEN
        CSROW = CSROW - 1
        IF CSROW < 1 THEN CSROW = 1
      ELSEIF K1 = 75 THEN
        C = C - 1
        IF C < 1 THEN C = 1
      ELSEIF K1 = 77 THEN
        C = C + 1
        IF C > 80 THEN C = 80
      ELSEIF K1 = 80 THEN
        CSROW = CSROW + 1
        IF CSROW > 24 THEN CSROW = 24
      ENDIF
      pk = K1
      W = C + (CSROW - 1) * 80
      IF (K1 = 72 OR K1 = 75 OR K1 = 77 OR K1 = 80) AND M > 0 THEN
        NC = -1
        IF M = 1 THEN NC = 219
        IF M = 2 THEN NC = 176
        IF M = 3 THEN NC = 177
        IF M = 4 THEN NC = 178
        IF M = 6 THEN NC = 32
        IF M = 5 THEN NEXTCHAR
        IF NC > 0 THEN
          LOCATE CSROW, C
          PRINT CHR$(NC);
          MID$(TEMP1$, W, 1) = CHR$(NC)
        ENDIF
      ENDIF
      IF K1 = 59 THEN
        LOCATE 1, 1
        PRINT "                                               "
        PRINT " USE CURSER KEYS TO MOVE THE CURSER AROUND.    "
        PRINT " PRESSING OTHER CHARACTERS WILL IMMEDIATLY PUT "
        PRINT " THEM ON TO THE SCREEN.                        "
        PRINT " TO GET EXTENED CHARACTERS, HOLD DOWN 'ALT'    "
        PRINT " AND TYPE THE CHARACTER CODE ON THE KEYPAD.    "
        PRINT " FUNCTION KEYS:                                "
        PRINT " F2-NORMAL   F3-DRAW Û   F4 DRAW °   F5 DRAW ± "
        PRINT " F6-DRAW LINES           F7 DRAW SPACES        "
        PRINT " PRESS ESCAPE WHEN DONE.                       "
        PRINT "                                               "
        DO
        LOOP UNTIL INKEY$ <> ""
        LOCATE 1, 1
        PRINT TEMP1$;
      ENDIF
      IF K1 = 60 THEN M = 0
      IF K1 = 61 THEN M = 1
      IF K1 = 62 THEN M = 2
      IF K1 = 63 THEN M = 3
      IF K1 = 64 THEN M = 4
      IF K1 = 65 THEN M = 5
      IF K1 = 66 THEN M = 6
    ELSEIF KI$ <> "" THEN
      W = C + (CSROW - 1) * 80
      K1 = ASC(KI$)
      IF (K1 > 31 AND K1 < 127) OR (K1 > 127 AND K1 < 255) THEN
        MID$(TEMP1$, W, 1) = CHR$(K1)
        LOCATE CSROW, C
        PRINT CHR$(K1);
        C = C + 1
        IF C > 80 THEN
          C = 1
          CSROW = CSROW + 1
          IF CSROW > 24 THEN CSROW = 24
        ENDIF
      ENDIF
    ENDIF
    UC = SCREEN(CSROW, C, 0)
    LOCATE CSROW, C, 0
    COLOR 0, 7
    PRINT CHR$(UC);
  LOOP UNTIL KI$ = CHR$(27)
  LOCATE 3, 1
  COLOR 7, 0
  PRINT "________________________________________________________________________________";
  LOCATE 1, 1
  PRINT SPACE$(80);
  LOCATE 2, 1
  PRINT SPACE$(80);
  LOCATE 1, 1
  PRINT "Enter screen file name"
  EDITSTRING B$
  IF B$ <> "" THEN
    OPEN B$ FOR OUTPUT AS #1
    PRINT #1, TEMP1$
  ENDIF
END SUB

SUB SETSCREEN
  VX = -1
  VY = -1
  CHRWIDTH = 8
  IF ADAPTER = 1 THEN
    SCREEN 3
    NCOL = 1
    VX = 670
    VY = 10
    VC = 76
    VR = 1
    MAXHIGHT = 347
    CHRHIGHT = 14
    CHRWIDTH = 9
  ENDIF
  IF ADAPTER > 1 AND PCOL = 0 THEN
    SCREEN 2
    NCOL = 1
    MAXHIGHT = 199
    CHRHIGHT = 8
  ENDIF
  IF ADAPTER = 3 AND PCOL = 1 THEN
    SCREEN 9
    NCOL = 15
    MAXHIGHT = 347
    CHRHIGHT = 14
  ENDIF
  IF ADPATER = 4 AND PCOL = 1 THEN
    SCREEN 12
    NCOL = 15
    MAXHIGHT = 479
    CHRHIGHT = 16
  ENDIF
  EXHIGHT = 32
  IF HIGHT > MAXHIGHT - CHRHIGHT * 4 - EXHIGHT THEN HIGHT = MAXHIGHT - CHRHIGHT * 4 - EXHIGHT
  EXWIDTH = 0
  VX = 18
  VY = HIGHT + EXHIGHT - 31
  VC = 5
  VIEW (0, 0)-(639 + EXWIDTH, HIGHT + EXHIGHT)
  Y = INT((HIGHT + CHRHIGHT) / CHRHIGHT) + 1
  VR = Y
  LOCATE Y + 1, 1
  PRINT "        F1 HELP      F2 NORMAL    F3 DOWN       F4 ERASE    F5 LINE   "
  PRINT "        F6 LINES     F7 BOX       F8 BOXFILL    F9 COLOR    F10 SIZE  "
  IF Y < 20 THEN
    PRINT "SHIFT+  F2 CIRCLE    F3 FCIRCLE   F4 SKIP       F5 FILL     F9 COLOR2"
  ENDIF
  IF Y < 18 THEN
    PRINT "CTRL+   F1 NORMAL    F2 SPAY      F3 STONE      F4 STICKS   F5 TRIANGES"
    PRINT "PATTERN F6 FTRIAGLES F7 BLOCKS    F8 FBLOCKS    F9 BRICKS"
  ENDIF
  LINE (0, 0)-(640, HIGHT), 1, B
  IF VX > 1 THEN
    SHOWCURSER
  ENDIF
END SUB

SUB SHOWCURSER
  IF VY > 2 THEN
    LINE (VX - 13, VY)-(VX + 13, VY + 23), 0, BF
    LINE (VX - 14, VY)-(VX + 14, VY + 24), 1, B
    PSET (VX, VY + 12), CCOL
    IF SIZE > 1 THEN
      FOR N = 1 TO SIZE
        CIRCLE (VX, VY + 12), N, CCOL
      NEXT N
    ENDIF
    LOCATE VR, VC
    PRINT SKIP
    LOCATE VR + 1, VC
    PRINT MODE
    LOCATE VR + 2, VC
    PRINT PATTERN
    IF CCOL = 0 THEN CIRCLE (VX, VY + 12), SIZE + 1, 1
  ENDIF
END SUB

