'***************************************************************************
'***                  Text Adventure Game File Editor                    ***
'***             IBM PC & Compatibles QuickBasic Version 1.00            ***
'***                 Program written by Peter J. Meyer                   ***
'***************************************************************************
' Modified (1.00-10/93) (1.01-06/94) (1.50-10/2022)
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
' the  linenumbers.  Plans are  in progress  to port  to Visual Basic.NET  to 
' have the games  properly run in Winsows.  Plus this  Fast Basic port brings 
' the program  and games back  to the Atari 8-bit , and able to run on a wide 
' range computers with different memories available. 

  MemoryConfig = 48
  String_Store_Base = 38912
  String_Storage_Base_Last = 49152
  Place_Description_Base = 0
  Item_Name_Base = 0
  GameName_Store = 0
  Author_Store = 0
  Monetary_Unit_Store = 0
  Next_File_Store = 0
  Intro_Text_Store = 0
  Closing_Text_Store = 0  
  Item_Description_Base = 0
  Place_Exit_Base = 0
  Computer_Character_Base = 0
  Spell_Name_Base = 0
  Info_Bank = 255
  Place_BanK1 = 255
  Place_Bank2 = 255
  Place_Field_Size = 120
  Item_Bank = 255
  Name_Bank = 255                       
  ItemTableSize = 30
  ItemFieldSize = 16
  ComputerCharacterTableSize = 20
  PlaceTableSize = 38
  SureToProceed = 0
  FileName$=""
  OPEN #1,4,0,"D:TAGSETUP.TGD"
  IF err()=1 
     INPUT #1,MemoryConfig
  ELSE
     MemoryConfig = 48
  ENDIF  
  CLOSE #1
  IF MemoryConfig <= 64
      ItemTableMax = 72 
      ComputerCharacterTableMax = 36 
      PlaceTableMax = 84 
      SpellDataSize = 8
  ELIF MemoryConfig = 128
      ItemTableMax = 204
      ComputerCharacterTableMax = 256 
      PlaceTableMax = 256
      SpellDataSize = 16
  ENDIF
  
  DIM Ot(ItemTableMax) BYTE
  DIM OP(ItemTableMax) BYTE
  DIM OV(ItemTableMax) BYTE
  DIM OV2(ItemTableMax) BYTE
  DIM OV3(ItemTableMax) BYTE
  DIM OL(ItemTableMax) BYTE
     
  DIM Ht(ComputerCharacterTableMax) BYTE
  DIM Hl(ComputerCharacterTableMax) BYTE
  DIM HS(ComputerCharacterTableMax) BYTE
  DIM HH(ComputerCharacterTableMax) BYTE
  DIM Hm(ComputerCharacterTableMax) BYTE
  DIM Hv(ComputerCharacterTableMax) BYTE
  DIM HO(ComputerCharacterTableMax) BYTE
  
  DIM RT(PlaceTableMax)   BYTE
  DIM Rn(PlaceTableMax)   BYTE
  DIM Rnl(PlaceTableMax)  BYTE
  DIM Rnk(PlaceTableMax)  BYTE
  DIM Rw(PlaceTableMax)   BYTE
  DIM Rwl(PlaceTableMax)  BYTE
  DIM Rwk(PlaceTableMax)  BYTE
  DIM Re(PlaceTableMax)   BYTE
  DIM Rel(PlaceTableMax)  BYTE
  DIM Rek(PlaceTableMax)  BYTE
  DIM Rs(PlaceTableMax)   BYTE
  DIM Rsl(PlaceTableMax)  BYTE
  DIM Rsk(PlaceTableMax)  BYTE
  DIM Ru(PlaceTableMax)   BYTE
  DIM Rul(PlaceTableMax)  BYTE
  DIM Rd(PlaceTableMax)   BYTE
  DIM Rdl(PlaceTableMax)  BYTE
  DIM Ro(PlaceTableMax)   BYTE
  DIM Rol(PlaceTableMax)  BYTE
  DIM Rov(PlaceTableMax)  BYTE

  DIM EDIT$(3)
  EXEC Set_Memory_Configuration
  PLC = 0
  S0 = 0
  WIN_PLACE = 0 'S1
  WIN_ITEM_01 = 0
  WIN_ITEM_02 = 0
  WIN_ITEM_03 = 0
  WIN_DEFEAT_01 = 0
  WIN_MONETARY = 0
  WIN_ITEM_04 = 0
  Start_Shield_Points = 0
  S9 = 0
  Inventory_Limit = 0
  Computer_Character_Move_Set = 0
  Total_Turns = 0
  Food_Turn_Set = 0
  Starting_Place_Number = 0
  Start_lives = 0
  Player_Life_Resume = 0
  HitPoint_Trap_Lost = 0
  HitPoint_Name_Set = 0
  HitPoint_Heal_Gain = 0
  Next_File_After_Solve = 0
  Armor_High_Set = 0
  Start_Monetary_Units = 0
  Computer_Character_Revive_HP = 0
  Monetary_Trap_Lost = 0
  Punch_Damage = 0
  Kick_Damage = 0
  Sale_Back_Percentage = 0
  RANDOM_SEED = 0
  S29 = 0
  WIN_ITEM_05 = 0
  WIN_ITEM_06 = 0
  WIN_ITEM_07 = 0
  WIN_ITEM_08 = 0
  WIN_ITEM_09 = 0
  WIN_ITEM_10 = 0
  WIN_DEFEAT_02 = 0
  WIN_DEFEAT_03 = 0
  WIN_DEFEAT_04 = 0
  WIN_DEFEAT_05 = 0
  S40 = 0
  S41 = 0
  S42 = 0
  S43 = 0
  S44 = 0
  S45 = 0
  S46 = 0
  S47 = 0
  S48 = 0
  S49 = 0
  pk = 0  
  BIG_TEXT_XPOS = 0
  H=0
  I=0
  J=0
  FP0% = 0
  FP1% = 0
  Total_Places = 0
  Total_Items = 0
  Total_Computer_Characters = 0
  Total_Spells = 0
  Place_Index = 0
  Item_Index = 0
  Computer_Character_Index = 0
  Spell_Index = 0    
  EXEC CLEAR_GAME_MEMORY
  STRING_ADDRESS = 0
  EXEC Initialize
  
DO
   CLS
   POKE 82,0
   POKE 710,0
   POKE 709,12
 '    CLS
 '   EDIT$(0)="This is a test of editing big texts for the text adventure game system that Peter J. Meyer originally wrote in 1987. This Program was originally written in Basic XE then ported to the IBM PC. The Atari disks were lost. We need to test this for over 512 ch"
 '   EDIT$(1)="aracters and what is preventing it from displayling over 512 characters. There's your problem, right there. Let's see if this fixes the problem. This problem is still happening. Let's use sound to indicate which section of code is executing. "
 '   EDIT$(2)=""
   STRING_ADDRESS = ADR(EDIT$(0))
   CLS 
  'POKE 54286,0
  'POKE $D301,254
  'MOVE 40000,60000,200
  'POKE $D301,255 


   'POKE $D301,254
   'MOVE 60000,40000,200
   'POKE $D301,255 
   'POKE 54286,64
   
   PRINT "Text Adventure Game File Editor v1.50"
   PRINT "ATARI 8-BIT Version by Peter J. Meyer";CHR$(153)
   PRINT "ITM:";Total_Items;"\";ItemTableSize;
   PRINT "  CCC:";Total_Computer_Characters;"\";ComputerCharacterTableSize;
   PRINT "  PLC:";Total_Places;"\";PlaceTableSize;
   IF MemoryConfig <=64
        PRINT "   ";MemoryConfig;"K";CHR$(153) 
   ELIF MemoryConfig = 128
        PRINT " "
        POSITION 34,4
        PRINT MemoryConfig;"K"
   ENDIF
   'PRINT USING "### Objects  ### hostilities  ### places  ## passwords/spells####,### Bytes Free"; Total_Items; HOS; PLC; Total_Spells; FRE(-1)
   PRINT " A) Edit Main Information"
   PRINT " B) Edit Specifications"
   PRINT " C) Edit Passwords/spells"
   PRINT " D) Edit Item Data"
   PRINT " E) Edit Computer Controlled Characters"
   PRINT " F) Edit Place Data"
   PRINT " G) Save Game"
   PRINT " H) Load Game"
   PRINT " I) Config Memory"
   PRINT " J) Clear Memory"
   PRINT " ESCape to Exit To DOS."
   CLOSE #1
   MODE = 1
   A = 0
   REPEAT
     A = KEY()
   UNTIL A>0
   GET A   
   IF A > 96 THEN A = A - 32
   IF A > 63 AND A < 71
     Modified = 1
   ENDIF
   SELECTED = A & 63
   IF SELECTED  = 1
        EXEC Edit_Main_Info
    ELIF SELECTED  = 2
        EXEC  Edit_Specifications
    ELIF SELECTED  = 3
        EXEC  Edit_Passwords
    ELIF SELECTED  = 4
        EXEC  Edit_Item_Data
    ELIF SELECTED  = 5
        EXEC  Edit_Computer_Character_Data
    ELIF SELECTED  = 6
        EXEC  Edit_Place_Data
    ELIF SELECTED  = 7 
        PROMPT$ = "FILE TO SAVE "
        EXEC DISK_DIRECTORY
        IF LEN(FileName$)>2 then EXEC Save_file
    ELIF SELECTED  = 8 
        EXEC SURE
        IF SureToProceed = 1 
          PROMPT$ = "FILE TO LOAD "
          EXEC CLEAR_GAME_MEMORY
          EXEC DISK_DIRECTORY        
          IF LEN(FileName$)>2 then EXEC Load_File
        ENDIF
    ''  IF NOT SURE$ = "C" THEN GOSUB Load_file
    ELIF SELECTED  = 9
        EXEC SURE
        IF SureToProceed = 1
          EXEC Get_Memory_Configuration
          EXEC Set_Memory_Configuration
          OPEN #1,8,0,"D:TAGSETUP.TGD"
          IF err()=1
            PRINT #1,MemoryConfig
          ENDIF  
          CLOSE #1
        ENDIF
    ELIF SELECTED  = 10
        EXEC SURE
        IF SureToProceed = 1
            EXEC CLEAR_GAME_MEMORY
    ''  IF NOT SURE$ = "C" THEN RUN
        ENDIF
    ELIF A  = 27
        EXEC SURE
        IF SureToProceed = 1
            END
        ENDIF
    ''  IF SURE$ = "C" THEN A = 0
    ENDIF
LOOP
'------------------------------------------------------------------------------
  PROC Initialize
  '  OPEN "TG.PIF" FOR INPUT AS #1
  '    INPUT #1, ADAPTER
  '    INPUT #1, PATH$
  '  CLOSE #1
    ADAPTER = 1
    PATH$ = ""
    'Nf$ = ""
    'Cu$ = ""
    String_Length = 0
    Modified = 0
    TEMP1$ = ""
    EDIT$(0) = ""
    EDIT$(1) = ""
    EDIT$(2) = ""
    EDIT$(3) = ""
    PROMPT$ = ""
    FileName$=""
    Use_Advance_RPG = 0
    O = 1
    H = 1
    R = 1
  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Main_Info
    CLS
    Z=USR(@FIELD_GET, GameName_Store,ADR(EDIT$(0)), 40, Info_Bank)
    PRINT " Name of game"
    EXEC BIG_TEXT_EDIT 40,1 
    Z=USR(@FIELD_PUT, GameName_Store, ADR(EDIT$(0)), 40, LEN(EDIT$(0)), Info_Bank)

    Z=USR(@FIELD_GET, Author_Store, ADR(EDIT$(0)), 40, Info_Bank)
    POSITION 0,3
    PRINT "Author(s)"
    EXEC BIG_TEXT_EDIT 40,4 
    Z=USR(@FIELD_PUT,Author_Store,ADR(EDIT$(0)),40,LEN(EDIT$(0)),Info_Bank)
    CLS
    Z=USR(@FIELD_GET, Intro_Text_Store,ADR(EDIT$(0)),255,Info_Bank)
    Z=USR(@FIELD_GET, Intro_Text_Store+255,ADR(EDIT$(1)),255,Info_Bank)
    Z=USR(@FIELD_GET, Intro_Text_Store+510,ADR(EDIT$(2)),249,Info_Bank)  
    EXEC FIND_STORE_STRING_SIZE Intro_Text_Store,760
'   MOVE Intro_Text_Store,ADR(EDIT$(0))+1,255
'   MOVE Intro_Text_Store+255,ADR(EDIT$(1))+1,255
'   MOVE Intro_Text_Store+510,ADR(EDIT$(2))+1,255
    'IF String_Length > 512
       'POKE ADR(EDIT$(0)),255
       'POKE ADR(EDIT$(1)),255
       'POKE ADR(EDIT$(2)),String_Length - 510
    'ELIF String_Length > 255
       'POKE ADR(EDIT$(0)),255
       'POKE ADR(EDIT$(1)),String_Length - 255 
       'POKE ADR(EDIT$(2)),0
    'ELSE
       'POKE ADR(EDIT$(0)),String_Length 
       'POKE ADR(EDIT$(1)),0 
       'POKE ADR(EDIT$(2)),0
    'ENDIF
    PRINT "Opening Introduction Text."
    EXEC BIG_TEXT_EDIT 760,1
    'EXEC CLEAR_STRING_MEMORY Intro_Text_Store,759
    Z=USR(@FIELD_PUT,Intro_Text_Store,ADR(EDIT$(0)),255,LEN(EDIT$(0)),Info_Bank)
    Z=USR(@FIELD_PUT,Intro_Text_Store+255,ADR(EDIT$(1)),255,LEN(EDIT$(1)),Info_Bank)
    Z=USR(@FIELD_PUT,Intro_Text_Store+510,ADR(EDIT$(2)),249,LEN(EDIT$(2)),Info_Bank)
    EDIT$(1)=""
    EDIT$(2)=""
   ' MOVE ADR(EDIT$(0))+1,Intro_Text_Store,255
   ' MOVE ADR(EDIT$(1))+1,Intro_Text_Store+255,255
   ' MOVE ADR(EDIT$(2))+1,Intro_Text_Store+510,245
    CLS
    Z=USR(@FIELD_GET, Closing_Text_Store,ADR(EDIT$(0)),255,Info_Bank)
    Z=USR(@FIELD_GET, Closing_Text_Store+255,ADR(EDIT$(1)),255,Info_Bank)
    Z=USR(@FIELD_GET, Closing_Text_Store+510,ADR(EDIT$(2)),249,Info_Bank) 
    EXEC FIND_STORE_STRING_SIZE Closing_Text_Store,760
    'MOVE Closing_Text_Store,ADR(EDIT$(0))+1,255
    'MOVE Closing_Text_Store+255,ADR(EDIT$(1))+1,255
    'MOVE Closing_Text_Store+510,ADR(EDIT$(2))+1,255
    'IF String_Length > 512
    '   POKE ADR(EDIT$(0)),255
    '   POKE ADR(EDIT$(1)),255
    '   POKE ADR(EDIT$(2)),String_Length - 510
    'ELIF String_Length > 255
    '   POKE ADR(EDIT$(0)),255
    '   POKE ADR(EDIT$(1)),String_Length - 255 
    '   POKE ADR(EDIT$(2)),0
    'ELSE
    '   POKE ADR(EDIT$(0)),String_Length 
    '   POKE ADR(EDIT$(1)),0 
    '   POKE ADR(EDIT$(2)),0
    'ENDIF
    PRINT "Game Closing Text."
    EXEC BIG_TEXT_EDIT 760,1
    'EXEC CLEAR_STRING_MEMORY Closing_Text_Store,760
    Z=USR(@FIELD_PUT,Closing_Text_Store,ADR(EDIT$(0)),255,LEN(EDIT$(0)),Info_Bank)
    Z=USR(@FIELD_PUT,Closing_Text_Store+255,ADR(EDIT$(1)),255,LEN(EDIT$(1)),Info_Bank)
    Z=USR(@FIELD_PUT,Closing_Text_Store+510,ADR(EDIT$(2)),249,LEN(EDIT$(2)),Info_Bank)
    EDIT$(1)=""
    EDIT$(2)=""
    'MOVE ADR(EDIT$(0))+1,Closing_Text_Store,255
    'MOVE ADR(EDIT$(1))+1,Closing_Text_Store+255,255
    'MOVE ADR(EDIT$(2))+1,Closing_Text_Store+510,245
    CLS    
    PRINT "Place number must be at to win"
    EXEC EDITVALUE ADR(WIN_PLACE)
    CLS
    PRINT "Objects must have to win"
    PRINT "1st"
    EXEC EDITVALUE ADR(WIN_ITEM_01)
    PRINT "2nd"
    EXEC EDITVALUE ADR(WIN_ITEM_02)
    PRINT "3rd"
    EXEC EDITVALUE ADR(WIN_ITEM_03)
    PRINT "4th"
    EXEC EDITVALUE ADR(WIN_ITEM_04)
    PRINT "5th"
    EXEC EDITVALUE ADR(WIN_ITEM_05)
    PRINT "6th"
    EXEC EDITVALUE ADR(WIN_ITEM_06)
    PRINT "7th"
    EXEC EDITVALUE ADR(WIN_ITEM_07)
    PRINT "8th"
    EXEC EDITVALUE ADR(WIN_ITEM_08)
    PRINT "9th"
    EXEC EDITVALUE ADR(WIN_ITEM_09)
    PRINT "10th"
    EXEC EDITVALUE ADR(WIN_ITEM_10)
    CLS
    PRINT "Hostilities must defeat to win"
    PRINT "1st"
    EXEC EDITVALUE ADR(WIN_DEFEAT_01)
    PRINT "2nd"
    EXEC EDITVALUE ADR(WIN_DEFEAT_02)
    PRINT " ";WIN_DEFEAT_02;CHR$(28)
    EXEC EDITVALUE ADR(WIN_DEFEAT_03)
    PRINT " ";WIN_DEFEAT_03;CHR$(28)
    EXEC EDITVALUE ADR(WIN_DEFEAT_04)
    PRINT "5th"
    EXEC EDITVALUE ADR(WIN_DEFEAT_05)
    PRINT "Monetary units required to win."
    EXEC EDITVALUE ADR( WIN_MONETARY)
    PRINT "Starting place number"
    EXEC EDITVALUE ADR( Starting_Place_Number)
    PRINT "Starting number of shield points"
    EXEC EDITVALUE ADR( Start_Shield_Points)
    PRINT "Number of monetary units at game start."
    EXEC EDITVALUE ADR( Start_Monetary_Units)   
    Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(EDIT$(0)),12,Info_Bank)
    PRINT "Name of Monetary Units"
    EXEC BIG_TEXT_EDIT 12,20
    Z=USR(@FIELD_PUT,Monetary_Unit_Store,ADR(EDIT$(0)),12,LEN(EDIT$(0)),Info_Bank)
  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Specifications
        CLS
        PRINT "0 All hostilities Remain Stationary."
        PRINT "1 All at new loacation every move. "
        PRINT "2 All move start of each turn."
        PRINT "3 Controlled by the hostility data."
        EXEC EDITVALUE ADR(Computer_Character_Move_Set)
        PRINT "Number of turns to solve games."
        EXEC EDITVALUE ADR(Total_Turns)
        PRINT "Number of turns before getting food or  water."
        EXEC EDITVALUE ADR(Food_Turn_Set)
        IF Use_Advance_RPG 
            PRINT "Place number for magician sanctuary"
            EXEC EDITVALUE ADR(Start_lives)
        ELSE
            PRINT "Number of lives you have to solve game."
            EXEC EDITVALUE ADR(Start_lives)
            PRINT "When player is killed."
            PRINT "1 Stay in same place."
            PRINT "2 Go to starting place."
            PRINT "3 Go to last sanctuary."
            EXEC EDITVALUE ADR(Player_Life_Resume)
            PRINT "Maxinum number of objects carried"
            EXEC EDITVALUE ADR(Inventory_Limit)
            PRINT "Number of Hit Points gain per turn"
            EXEC EDITVALUE ADR(HitPoint_Heal_Gain)
       ENDIF
        CLS
        PRINT "How many shield points are lost in a"
        PRINT "shield point losing trap."        
        EXEC EDITVALUE ADR(HitPoint_Trap_Lost)
        PRINT "If hostilities are to be revived,"
        PRINT "enter amount hitpoints to be set at."
        PRINT "(-1 to restore to starting value)"
        EXEC EDITVALUE ADR(Computer_Character_Revive_HP)
        PRINT "How many credit units will be lost in a credit loosing trap"
        EXEC EDITVALUE ADR(Monetary_Trap_Lost)
        PRINT "How hit points will be lost if hostilityis punched ?"
        EXEC EDITVALUE ADR(Punch_Damage)
        PRINT "How hit points will be lost if hostilityis kicked ?"
        EXEC EDITVALUE ADR(Kick_Damage)
        PRINT "Alternate name for shield points:"
        PRINT "0 Hit points (default)"
        PRINT "1 Health points"
        PRINT "2 Shield points"
        PRINT "3 Life units"
        PRINT "4 Power units"
        EXEC EDITVALUE ADR(HitPoint_Name_Set)
        CLS
        PRINT "Armor protection from hostility attack."
        PRINT "0 Armor is at highest percentage"
        PRINT "1 Armor points are accumulated"
        PRINT "2 Armor points are accumulated"
        PRINT "3 Armor+shields is at the highest"
        EXEC EDITVALUE ADR(Armor_High_Set)
        PRINT "Place display format:"
        PRINT "0 Exit, Items, and hostility seperate display. "
        PRINT "1 Exits appended to place description."
        PRINT "2 Exits and Items are appended to place description."
        EXEC EDITVALUE ADR(S0)
        PRINT "How many credit units do you get for killing each hostility ?"
        EXEC EDITVALUE ADR(HitPoint_Heal_Gain)
        CLS
        PRINT "Objects sold at a market place."
        PRINT "0 Sold for the price of the object."
        PRINT "1 Sold at 90% of the price."
        EXEC EDITVALUE ADR(Sale_Back_Percentage)
        PRINT "Randomization:"
        PRINT "0 To randomize to timer, anyother to randomize to that number."
        EXEC EDITVALUE ADR(RANDOM_SEED)
        PRINT "Enter 1 to load next file when solved."
        PRINT "      2 Must solve other file first."
        EXEC EDITVALUE ADR( Next_File_After_Solve)
        IF Next_File_After_Solve = 1
          Z=USR(@FIELD_GET, Next_File_Store,ADR(EDIT$(0)),12,Info_Bank)
          PRINT "Name of file."
          EXEC BIG_TEXT_EDIT 12,12
          Z=USR(@FIELD_PUT,Next_File_Store,ADR(EDIT$(0)),12,LEN(EDIT$(0)),Info_Bank)
        ENDIF  
  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Passwords
    Spell_Index = 0
    CLS
    PRINT "Passwords or Spells that are to be used in the game."
    BIG_TEXT_XPOS = 8
    REPEAT  
        Z=USR(@FIELD_GET, Spell_Name_Base + 16 * Spell_Index,ADR(EDIT$(0)),16,Name_Bank)
        POSITION 0, 2 + Spell_Index
        PRINT "WORD ";Spell_Index
        POSITION 7, 2 + Spell_Index
        PRINT ":"
        EXEC BIG_TEXT_EDIT 16,2 + Spell_Index
        Z=USR(@FIELD_PUT,Spell_Name_Base + 16 * Spell_Index,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_BANK)
        Spell_Index = Spell_Index + 1 
        IF Spell_Index > Total_Spells THEN Total_Spells = Spell_Index
    UNTIL a = 27 or EDIT$(0) = "" or Spell_Index >= SpellDataSize
    BIG_TEXT_XPOS = 0

  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Item_Data
   ITEM_INDEX = Total_Items + 1
   REPEAT
       CLS
        POSITION 0,23
        PRINT "#:";ITEM_INDEX;
        Z=USR(@FIELD_GET,Item_Name_Base + ItemFieldSize * ITEM_INDEX,ADR(EDIT$(0)),ItemFieldSize,Name_Bank)
        POSITION 7,23
        PRINT EDIT$(0);
        POSITION 0,0
       PRINT " 1 Treasure         23 Heavy/pushable "
       PRINT " 2 Key              24 Burnable       "
       PRINT " 3 Weapon           25 Breakable      "
       PRINT " 4 Ammunition       26 Heavy breakable"
       PRINT " 5 Food/Water       27 Smasher/Hammer "
       PRINT " 6 Armor/Shield     28 Immunity       "
       PRINT " 7 Explosive        29 Needs container"
       PRINT " 8 Detonater        30 Container      " 
       PRINT " 9 Transport        31 Wound Medicine "
       PRINT "10 Shovel           32 Poison Antidote" 
       PRINT "11 Climbing tool    33 Component/Parts"
       PRINT "12 Document         34 Shows Text file" 
       PRINT "13 Luminater        35 Healing        "
       PRINT "14 Fuel             36 Transport      " 
       PRINT "15 Survival         37 Heavy Container"
       PRINT "16 Shield points    38 Luminater      " 
       PRINT "17 Poison           39 Kills All      "
       PRINT "18 More turns       40 Entraps All    " 
       PRINT "19 Case/Container   41 Screen file    " 
       PRINT "20 Extra life       42 Part of Item   "        
       PRINT "21 More inventory   "
       PRINT "22 Heavy container  "  
       IF Use_Advance_RPG = 1 
         PRINT "46 Armor/shield 1 "
         PRINT "47 Armor/shield 2 "
         PRINT "48 Head gear      "
         PRINT "49 Chest Armor    "
         PRINT "50 Gloves         "
         PRINT "51 Leggings       " 
         PRINT "52 Boots          "
         PRINT "53 Shields        "
       ENDIF
        IF OT(ITEM_INDEX) = 0 
            EDIT$(0) = ""
        ELSE    
            EDIT$(0)=STR$(OT(ITEM_INDEX)&127)
            IF OV3(ITEM_INDEX) >= 192
                B  = OV3(ITEM_INDEX)&63  
            ELSE
                B = 0
            ENDIF 
        ENDIF
        POSITION 29,23
        PRINT "TYPE:";
        EXEC EDIT_TYPE_FLAG 34,23
        IF a = 28 AND ITEM_INDEX > 1 THEN ITEM_INDEX = ITEM_INDEX - 1 
        IF a = 29 AND ITEM_INDEX < Total_Items + 1 THEN ITEM_INDEX = ITEM_INDEX + 1
        IF a = 127
            EXEC Goto_Index
            IF B>0 AND B < Total_Items + 1 
                ITEM_INDEX = B
            ELIF B > Total_Items
                ITEM_INDEX = Total_Items + 1
            ENDIF            
        ENDIF
        IF a = 125 
            ITEM_INDEX = 1
        ENDIF
        IF a = 27 or a = 28 or a = 29 or a=125 or a= 127
            Item_Type_In = 253
        ELSE
            Item_Type_In = VAL(EDIT$(0))
        ENDIF
        IF Item_Type_In >0 AND Item_Type_In <54
            OT(ITEM_INDEX) = Item_Type_In
            IF B > 1999 THEN OT(ITEM_INDEX) = OT(ITEM_INDEX)!128
            IF Item_Type_In = 3
              PROMPT$ = "How many hitpoints damage will it cause to hostilities"
            ELIF Item_Type_In = 4
              PROMPT$ = "How many units of ammunition"
            ELIF Item_Type_In = 5
              PROMPT$ = "Addional turns added when consumed."
            ELIF Item_Type_In = 6 or Item_Type_In = 46 or Item_Type_In = 47 or Item_Type_In = 48 
              PROMPT$ = "Number of armor points this item protects at."  
            ELIF Item_Type_In = 49 or Item_Type_In = 50 or Item_Type_In = 51 or Item_Type_In = 52 or Item_Type_In = 53
              PROMPT$ = "Number of armor points this item protects at."
            ELIF Item_Type_In = 7
              PROMPT$ = "Detonater item number must have to use."
            ELIF Item_Type_In = 20
              PROMPT$ = "How many extra lives will be given."
            ELIF Item_Type_In = 9
              PROMPT$ = "To what place number does it transport"
            ELIF Item_Type_In = 11
              PROMPT$ = "1-Can be used in any direction.2-Only go up.3-Only go down.4-Only in other exits"
            ELIF Item_Type_In = 13
              PROMPT$ = "How many turns will it last"
            ELIF Item_Type_In = 14
              PROMPT$ = "How many additional turns will it add"
            ELIF Item_Type_In = 25
              PROMPT$ = "What item is inside."
            ELIF Item_Type_In = 28
              PROMPT$ = "How many turns will be immune for"
            ELIF Item_Type_In = 30
              PROMPT$ = "Amount of effect."
            ELIF Item_Type_In = 28
              PROMPT$ = "When collected, becomes: 1-Food/water 2-Healing potion, 3-poison, 4-immunity"
            ELIF Item_Type_In = 16
              PROMPT$ = "How many shield points will be gained when swallowed"
            ELIF Item_Type_In = 17
              PROMPT$ = "1 to kill,2 loose shield points,3 Loose shield points per turn until cured."
            ELIF Item_Type_In = 18
              PROMPT$ = "How many more turn added if found"
            ELIF Item_Type_In = 19
              PROMPT$ = "What is the item number of item in  side?"
            ELIF Item_Type_In = 35
              PROMPT$ = "How many hit points will be added."
            ELIF Item_Type_In = 36
              PROMPT$ = "To what place will it transport to."
            ELIF Item_Type_In = 36
              PROMPT$ = "Enter next component/part number, or 0 if this is the last item."
            ELIF Item_Type_In = 21
              PROMPT$ = "How many more items can be carried." 
            ELIF Item_Type_In = 22 or Item_Type_In = 23 or Item_Type_In = 24 or Item_Type_In = 26 or Item_Type_In = 37
              'GOSUB Container.type
              PROMPT$ = "<SKIP>"
            ELIF Item_Type_In > 47
              PROMPT$ = "What is the first item varible"
            ELSE
              PROMPT$ = "<SKIP>"
            ENDIF 
            CLS
            POSITION 0, 0
            IF NOT PROMPT$ = "<SKIP>"
              PRINT PROMPT$
              EXEC EDITVALUE ADR(OV)+ITEM_INDEX
            ENDIF
            PROMPT$ = "<SKIP>"      
            IF Item_Type_In = 3
              PROMPT$ = "Amunition Item Number. 0 use by itself, 254 must be thrown, 255 for one use."
            ELIF Item_Type_In = 17 AND OV(ITEM_INDEX) = 2
              PROMPT$ = "How many shield points lost."
            ELIF Item_Type_In = 17 AND OV(ITEM_INDEX) = 3
              PROMPT$ = "How many shield points lost per turn."
            ELIF Item_Type_In = 19
              PROMPT$ = "If it requires a key to open it, enter  the item number, or else enter 0."
            ELIF Item_Type_In = 22 OR Item_Type_In = 23 OR Item_Type_In = 24 OR Item_Type_In = 26 OR Item_Type_In = 37
              'IF Item_Type_In = 37 THEN GOSUB Edit.password.number
              A = OV(ITEM_INDEX)
              IF A > 99
                A = A - (INT(A / 100) * 100)
              ENDIF
              IF A = 1
                PROMPT$ = "What item number is inside."
              ENDIF
              IF A > 1 AND A < 9 THEN PROMPT$ = "What place# will it lead to."
              IF Item_Type_In = 24 AND A = 9 
                PROMPT$ = "How many points of damage."
              ENDIF
              IF NOT Item_Type_In = 24 AND A = 9 
                PROMPT$ = "What hostile# is under or behind."
              ENDIF
              IF A = 10
                'GOSUB Edit.range
              ENDIF
            ELIF Item_Type_In = 29
              PROMPT$ = "Number of times it can be collected."
            ELIF Item_Type_In = 34 OR Item_Type_In = 35 OR Item_Type_In = 36 OR Item_Type_In = 38 OR Item_Type_In = 39 OR Item_Type_In = 40
              PROMPT$ = "What Password/spell to activate it."
            ELIF Item_Type_In = 33 AND OV(ITEM_INDEX) = 0
              PROMPT$ = "item number it will become when assembled. 1000 + Place number become other exit."
            ELIF Item_Type_In > 43
              PROMPT$ = "Enter 2nd item variable"
            ELSE
              PROMPT$ = "<SKIP>"
            ENDIF
            IF NOT PROMPT$ = "<SKIP>"
              PRINT PROMPT$
              EXEC EDITVALUE ADR(OV2) + ITEM_INDEX
            ENDIF
            IF Item_Type_In < 72 
                B = OL(ITEM_INDEX)
                PRINT "Enter the place number for this item."
                PRINT "0 to place in inventory at game start."
                PRINT "254 to be inside another item. Add 2000"
                PRINT "for defeating hostile character reward."
                EXEC EDITVALUE ADR(B)
                If B < 2000 
                    OL(ITEM_INDEX) = B
                    
                ELSE
                    OL(ITEM_INDEX) = B MOD 2000
                    OV3(ITEM_INDEX) = 192 + B / 2000
                ENDIF
                PRINT " What is the item credit value.Enter 0 if no value."
                EXEC EDITVALUE ADR(OP)+ITEM_INDEX
                PRINT
                Z=USR(@FIELD_GET,Item_Name_Base+ItemFieldSize*ITEM_INDEX,ADR(EDIT$(0)),ItemFieldSize,Name_Bank) 
                POSITION 0,17
                PRINT CHR$(156);CHR$(156)
                POSITION 0,17
                PRINT "What is the name of this item?"
                EXEC BIG_TEXT_EDIT ItemFieldSize,18 
                Z=USR(@FIELD_PUT,Item_Name_Base+ItemFieldSize*ITEM_INDEX,ADR(EDIT$(0)),ItemFieldSize,LEN(EDIT$(0)),Name_Bank)
                PRINT
                Z=USR(@FIELD_GET, Item_Description_Base + 80 * ITEM_INDEX,ADR(EDIT$(0)),80,Item_Bank)
                POSITION 0,19
                IF (OT(ITEM_INDEX) = 23 OR OT(ITEM_INDEX) = 24 OR OT(ITEM_INDEX) = 26) AND OV(ITEM_INDEX) MOD 100 = 3 
                  PRINT "Name of other exit."
                ELIF (OT(ITEM_INDEX) = 23 OR OT(ITEM_INDEX) = 24 OR OT(ITEM_INDEX) = 26) AND OV(ITEM_INDEX) MOD 100 = 11
                  PRINT "Enter information given."
                ELIF OT(ITEM_INDEX) = 12
                  PRINT "What is written in this document?"
                ELIF OT(ITEM_INDEX) = 34
                  PRINT "Enter name of text file with extension."
                ELIF OT(ITEM_INDEX) = 41
                  PRINT "Enter name of text file with extension."
                ELSE
                  PRINT "Enter Description of the item."
                ENDIF
                EXEC BIG_TEXT_EDIT 80,20
                Z=USR(@FIELD_PUT,Item_Description_Base + 80 * ITEM_INDEX,ADR(EDIT$(0)),80,LEN(EDIT$(0)),Item_BANK)
                ITEM_INDEX = ITEM_INDEX + 1
                IF ITEM_INDEX > Total_Items AND Total_Items < ItemTableSize THEN Total_Items = ITEM_INDEX
  

            'IF O > OB.DIM THEN
              'OB.DIM = OB.DIM + 64
              'REDIM OT(OB.DIM), Op(OB.DIM), OV(OB.DIM), OV2(OB.DIM), OV3(OB.DIM)
             '' REDIM OL(OB.DIM), ON$(OB.DIM), OD$(OB.DIM)
            'ENDIF
            ENDIF
        ENDIF
    UNTIL a = 27 or Item_Type_In = 0 
  
  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Computer_Character_Data
     Computer_Character_Index = Total_Computer_Characters + 1
     REPEAT 
       CLS
       CLS
        POSITION 0,23
        PRINT "#:";Computer_Character_Index;
        Z=USR(@FIELD_GET, Computer_Character_Base + 16 * Computer_Character_Index,ADR(EDIT$(0)),16,Name_Bank)
        POSITION 7,23
        PRINT EDIT$(0);
        POSITION 0,0
       'PRINT "Character:";Computer_Character_Index;" Type"
       PRINT " 1 Shield point damage"
       PRINT " 2 Kills player if not killed first."
       PRINT " 3 Takes away all items"
       PRINT " 4 Takes away Treasures"
       PRINT " 5 Takes away Keys"
       PRINT " 6 Capable of moving items around"
       PRINT " 7 Prevents from exiting"
       PRINT " 8 Prevents from getting items"
       PRINT " 9 Puts player in other place."
       PRINT "10 Steals credit units"
       PRINT "11 Revives Hostile Characters"
       PRINT "12 Special Weapon Kills."
       PRINT "13 Special Password/Spell Kills"
       PRINT "14 Special Password/Spell Removes"
       PRINT "15 Takes away all weapons."
       PRINT "16 Bites and infects."
       PRINT "17 Bites and poisons."
       PRINT "18 Causes armor dagage"
       PRINT "19 Causes experience damage"
        IF HT(Computer_Character_Index) = 0 
            EDIT$(0) = ""
        ELSE    
            EDIT$(0)=STR$(HT(Computer_Character_Index))
        ENDIF
        POSITION 29,23
        PRINT "TYPE:";
        EXEC EDIT_TYPE_FLAG 34,23
        IF a = 28  AND Computer_Character_Index > 1 THEN Computer_Character_Index = Computer_Character_Index - 1 
        IF a = 29  AND Computer_Character_Index < Total_Computer_Characters + 1 THEN Computer_Character_Index = Computer_Character_Index + 1
        IF a = 127
            EXEC Goto_Index
            IF B>0 AND B < Total_Computer_Characters + 1 
                Computer_Character_Index = B
            ELIF B > Total_Computer_Characters
                Computer_Character_Index =  Total_Computer_Characters + 1
            ENDIF            
        ENDIF
        IF a = 125 
            Computer_Character_Index = 1
        ENDIF
        IF a = 27 or a = 28 or a = 29 or a = 125 or a = 127
            Computer_Character_Type_In = 253
        ELSE
            Computer_Character_Type_In = VAL(EDIT$(0))
        ENDIF
       'IF Computer_Character_Type_In = 32 
       '  INPUT "Which Character Number ", Computer_Character_Index
       'ENDIF
       'IF Computer_Character_Type_In = 34 THEN GOSUB Search.hostility.data
       IF Computer_Character_Type_In > 0 and Computer_Character_Type_In < 20
         HT(Computer_Character_Index) = Computer_Character_Type_In
         CLS
         PRINT "Computer Character #"; Computer_Character_Index
         PRINT "Which place number is the hostility in"
         EXEC EDITVALUE ADR(HL)+Computer_Character_Index
         IF HT(Computer_Character_Index) = 1 OR HT(Computer_Character_Index) = 12 OR HT(Computer_Character_Index) = 13 OR HT(Computer_Character_Index) = 16
           PRINT "Amount of damage per attack/turn."
           EXEC EDITVALUE ADR(HS)+Computer_Character_Index
         ENDIF
         IF HT(Computer_Character_Index) = 17 OR (Use_Advance_RPG = 1 AND (HT(Computer_Character_Index) = 7 AND HT(Computer_Character_Index) = 8)) 
           PRINT "Amount of damage per attack/turn."
            EXEC EDITVALUE ADR(HS)+Computer_Character_Index
         ENDIF
         IF HT(Computer_Character_Index) <> 6
           IF HT(Computer_Character_Index) < 12 OR HT(Computer_Character_Index) > 14
             PRINT "How many hit points does it have"
           ENDIF
           IF HT(Computer_Character_Index) = 12
             PRINT "item number of weapon to defeat it."
           ENDIF
           IF HT(Computer_Character_Index) = 13 OR HT(Computer_Character_Index) = 14
             PRINT "Password/Spell number to defeat it."
           ENDIF
           EXEC EDITVALUE ADR(HH)+Computer_Character_Index
         ENDIF
         IF Computer_Character_Move_Set = 3
           CLS
           PRINT "1 Character is stationary"
           PRINT "2 New location for every turn"
           PRINT "3 Moves every turn"
           PRINT "4 Increments place number"
           PRINT "5 Moves N,S,W,E only"
           PRINT "6 Appears anywhere"
           PRINT "7 Decrement place number"
           PRINT "8 Pursues Player"
           EXEC EDITVALUE ADR(Hm)+Computer_Character_Index
         ENDIF
         IF HT(Computer_Character_Index) = 9
            PRINT "What place number does it put player?"
         ELIF HT(Computer_Character_Index) = 10
            PRINT "How many credit units it takes per turn?"
         ELIF HT(Computer_Character_Index) = 16 OR HT(Computer_Character_Index) = 17
            PRINT "How many shields points lost per turn?"
         ENDIF
         IF HT(Computer_Character_Index) = 9 OR HT(Computer_Character_Index) = 10 OR HT(Computer_Character_Index) = 16 OR HT(Computer_Character_Index) = 17            
            EXEC EDITVALUE ADR(HS)+Computer_Character_Index
         ENDIF

          Z=USR(@FIELD_GET, Computer_Character_Base + 16 * Computer_Character_Index,ADR(EDIT$(0)),16,Name_Bank)
          POSITION 0,16
          PRINT "What is the name of the character?"
          EXEC BIG_TEXT_EDIT 16,17
          Z=USR(@FIELD_PUT,Computer_Character_Base + 16 * Computer_Character_Index,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_BANK)

         Computer_Character_Index = Computer_Character_Index + 1
         IF Computer_Character_Index > Computer_Character_Index AND Total_Computer_Characters < ComputerCharacterTableSize THEN Total_Computer_Characters = Computer_Character_Index        
       ENDIF
    UNTIL a=27 or Computer_Character_Type_In = 0
  ENDPROC
'------------------------------------------------------------------------------  
  PROC Edit_Place_Data
   PLACE_INDEX = Total_Places + 1
   REPEAT
        CLS
        POSITION 0,23
        PRINT "#:";PLACE_INDEX;
        IF PLACE_INDEX < 128
          Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size*PLACE_INDEX,ADR(EDIT$(0)),Place_Field_Size,Place_BanK1)
        ELSE
          N = PLACE_INDEX & 127
          Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size*N,ADR(EDIT$(0)),Place_Field_Size,Place_Bank2)
        ENDIF
        N = 0
        REPEAT
            N = N + 1
        UNTIL EDIT$(0)[N,1] <> " "
        EDIT$(0) = EDIT$(0)[N]
        IF LEN(EDIT$(0)) > 8
            IF EDIT$(0)[1,8] = "You are " then EDIT$(0) = EDIT$(0)[9]
        ENDIF 
        IF LEN(EDIT$(0))>22 THEN EDIT$(0) = EDIT$(0)[1,22] 
        POSITION 6,23
        PRINT EDIT$(0);"..";
        POSITION 0,0
        PRINT " 1 Normal place."
        PRINT " 2 Game ending trap."
        PRINT " 3 Shield point loosing trap."
        PRINT " 4 Need light for description."
        PRINT " 5 Need survival item."
        PRINT " 6 Have to kill hostility for items."
        PRINT " 7 Trading place/market."
        PRINT " 8 (Question/Riddle)."
        PRINT " 9 Credit unit loosing trap."
        PRINT "10 Safety/Sanctuary."
        PRINT "11 Password to transport to place."
        PRINT "12 Action to go to place."
        PRINT "13 Named Teleport Station."
        PRINT "14 Unlock doors lock when entered."
        PRINT "15 Return to place when killed."
        IF RT(PLACE_INDEX) = 0 
            EDIT$(0) = ""
        ELSE    
            EDIT$(0)=STR$(RT(PLACE_INDEX))
        ENDIF
        POSITION 29,23
        PRINT "TYPE:";
        EXEC EDIT_TYPE_FLAG 34,23

        IF a = 28  AND PLACE_INDEX > 1 THEN PLACE_INDEX = PLACE_INDEX - 1 
        IF a = 29  AND PLACE_INDEX < Total_Places + 1 THEN PLACE_INDEX = PLACE_INDEX + 1
        IF a = 127
            EXEC Goto_Index
            IF B > 0 AND B <= Total_Places + 1 
                PLACE_INDEX = B
            ELIF B > Total_Places
                PLACE_INDEX =  Total_Places + 1
            ENDIF            
        ENDIF
        IF a = 125 
            PLACE_INDEX = 1
        ENDIF
        IF a = 27 or a = 28 or a = 29 or a = 125 or a = 127
            Room_Type_In = 253
        ELSE
            Room_Type_In = VAL(EDIT$(0))
        ENDIF
        IF Room_Type_In = 99 THEN INPUT "Which place number ", PLACE_INDEX
        CLS
        IF Room_Type_In > 0 and Room_Type_In < 16 
          IF Room_Type_In <> 8
              RT(PLACE_INDEX) = Room_Type_In
              PRINT " Place "; PLACE_INDEX; "    North Exit"
              EXEC Show_Exit_Options RN(PLACE_INDEX),RNK(PLACE_INDEX),RNL(PLACE_INDEX)
              RN(PLACE_INDEX) = H
              RNK(PLACE_INDEX) = I
              RNL(PLACE_INDEX) = J
              CLS
              PRINT " Place "; PLACE_INDEX ; "   West Exit."
              EXEC Show_Exit_Options RW(PLACE_INDEX),RWK(PLACE_INDEX),RWL(PLACE_INDEX)
              RW(PLACE_INDEX) = H
              RWK(PLACE_INDEX) = I
              RWL(PLACE_INDEX) = J
              CLS
              PRINT " Place "; R; "   East Exit."
              EXEC Show_Exit_Options Re(PLACE_INDEX),REK(PLACE_INDEX),REL(PLACE_INDEX)
              RE(PLACE_INDEX) = H
              REK(PLACE_INDEX) = I
              REL(PLACE_INDEX) = J
              CLS
              PRINT " Place "; PLACE_INDEX; "   South Exit."
              EXEC Show_Exit_Options RS(PLACE_INDEX),RSK(PLACE_INDEX),RSL(PLACE_INDEX)
              RS(PLACE_INDEX) = H               
              RSK(PLACE_INDEX) = I
              RSL(PLACE_INDEX) = J
              CLS
              PRINT " Place "; PLACE_INDEX; "   Up Exit."
              PRINT "0-No exit.1-Open.2-Need climbing tool.  4-Secret exit."
              EXEC EDITVALUE ADR(RU)+PLACE_INDEX
              IF RU(PLACE_INDEX) > 0
                PRINT " What place number it leads to"
                EXEC EDITVALUE ADR(RUL)+PLACE_INDEX                
              ENDIF
              CLS
              PRINT " Place "; PLACE_INDEX;"   Down Exit"
              PRINT "0-No exit.1-Open.2-Need climbing tool.  3-Need to be dugged up.   4-Secret exit."
                EXEC EDITVALUE ADR(RD)+PLACE_INDEX
              IF RD(PLACE_INDEX) > 0
                PRINT " What place number it leads to"
                EXEC EDITVALUE ADR(RDL)+PLACE_INDEX
              ENDIF
              CLS
              IF RT(PLACE_INDEX) = 11 OR RT(PLACE_INDEX) = 13
                'GOSUB Edit.teleport
              ELSE
                  IF RT(PLACE_INDEX) <> 12
                      PRINT " Place "; PLACE_INDEX, "Other exit."
                      PRINT "0-No exit."
                      PRINT "1-Open."
                      PRINT "2-Needs to be dugged up."
                      PRINT "3-Climbing tool needed."
                      PRINT "4-Needs to blown open."
                      PRINT "5-Password to open."
                      PRINT "6-Password to appear."
                      PRINT "7-item/place that can be entered."
                      PRINT "8-Must be assembled."
                      EXEC EDITVALUE ADR(RO)+PLACE_INDEX
                      IF RO(PLACE_INDEX) > 0
                          PRINT " What place number it leads to"
                          EXEC EDITVALUE ADR(ROL)+PLACE_INDEX
                          Z=USR(@FIELD_GET, Place_Exit_Base + 16 * PLACE_INDEX,ADR(EDIT$(0)),16,Name_Bank)
                          PRINT " Name of exit"  
                          EXEC BIG_TEXT_EDIT 16,14
                          Z=USR(@FIELD_PUT,Place_Exit_Base + 16 * PLACE_INDEX,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_Bank)
                          IF RO(PLACE_INDEX) = 5 OR RO(PLACE_INDEX) = 6
                              PRINT " What Password/Spell will cause effect."
                              B = RO(PLACE_INDEX)&240 / 16
                              PRINT " ";B;CHR$(28)
                              INPUT B
                              B = B * 16
                              RO(PLACE_INDEX) = ROL(PLACE_INDEX) AND 15 + B
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
          CLS
          IF PLACE_INDEX < 128
            Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size*PLACE_INDEX,ADR(EDIT$(0)),Place_Field_Size,Place_BanK1)
          ELSE
            N = PLACE_INDEX & 127
            Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size*N,ADR(EDIT$(0)),Place_Field_Size,Place_Bank2)
          ENDIF
          PRINT " Place "; PLACE_INDEX
          PRINT "Enter description of place."
          PRINT "(Exits, Items, and Characters"
          PRINT "display automatically.)"          
          EXEC BIG_TEXT_EDIT Place_Field_Size,4
          IF PLACE_INDEX < 128
                Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*PLACE_INDEX,ADR(EDIT$(0)),Place_Field_Size,LEN(EDIT$(0)),Place_Bank1)
          ELSE
                N = PLACE_INDEX & 127
                Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*N,ADR(EDIT$(0)),Place_Field_Size,LEN(EDIT$(0)),Place_bank2)
          ENDIF
      ENDIF
        IF Room_Type_In < 16
          PLACE_INDEX = PLACE_INDEX  + 1
          if PLACE_INDEX > Total_Places and Total_Places < PlaceTableSize then Total_Places = PLACE_INDEX
        ENDIF
    UNTIL a=27 or Room_Type_In = 0    
  ENDPROC
 '------------------------------------------------------------------------------ 
   PROC Show_Exit_Options H I J
        PRINT "0-No Exit. 1-Open. 2-Unlocked. 3-Locked.4-Secret Exit. 5-Need Password."
        PRINT " ";H;CHR$(28)
        INPUT H     
        IF H>=1
          PRINT "Place Number Leads To"
          PRINT " ";J;CHR$(28)
          INPUT J     
          IF H = 2 OR H = 3
            PRINT " What key item number"
          ENDIF
          IF H = 5
            PRINT " What password/spell number."
          ENDIF
          IF H = 2 OR H = 3 OR H = 5
              PRINT " ";I;CHR$(28)
              INPUT I     
          ELSE
              I = 0
          ENDIF
        ELSE
            I = 0
            J = 0
        ENDIF
   ENDPROC 
'------------------------------------------------------------------------------    
  
  PROC CLEAR_GAME_MEMORY
    IF MemoryConfig = 48 OR MemoryConfig = 52 OR MemoryConfig = 64
        POKE String_Store_Base,0
        MOVE String_Store_Base,String_Store_Base + 1,10232 
    ENDIF
    IF MemoryConfig = 52 
        POKE 54286,0
        POKE $D301,254
        POKE 49152,0
        MOVE 49152,49153,4095
        POKE $D301,255
        POKE 54286,64
    ENDIF
    IF MemoryConfig = 64
        Z=USR(@CLEAR_X_BANK,254)
    ELIF MemoryConfig = 128
        Z=USR(@CLEAR_X_BANK,227)
        Z=USR(@CLEAR_X_BANK,231)
        Z=USR(@CLEAR_X_BANK,235)
        Z=USR(@CLEAR_X_BANK,239)
    ENDIF
    POKE ADR(OT),0
    MOVE ADR(OT),ADR(OT)+1,5 * ItemTableMax 
    POKE ADR(HT),0
    MOVE ADR(HT),ADR(HT)+1,6 * ComputerCharacterTableMax
    POKE ADR(RT),0
    MOVE ADR(RT),ADR(RT)+1,20 * PlaceTableMax
    Total_Places = 0
    Total_Items = 0
    Total_Computer_Characters = 0
    Total_Spells = 0
    Place_Index = 0
    Item_Index = 0
    Computer_Character_Index = 0
    Spell_Index = 0    
    FileName$=""
  ENDPROC
'------------------------------------------------------------------------------    
  PROC Get_File_Name
  ENDPROC
'------------------------------------------------------------------------------
  PROC Save_file
       CLOSE #1
    ''  IF F$ = "(Untitled)"
    ''    GOSUB Get.file.name
    '' ENDIF
       Modified = 0
       OPEN #1,8,0,FileName$



         EXEC FIND_STORE_STRING_SIZE Intro_Text_Store,759
         PRINT #1, String_Length  
        IF MemoryConfig = 128
            CLS
            H=DPEEK(88)+40
            Z=USR(@FIELD_GET,Intro_Text_Store+510,H+509,255,Info_Bank)
            Z=USR(@FIELD_GET,Intro_Text_Store+255,H+254,255,Info_Bank)
            Z=USR(@FIELD_GET,Intro_Text_Store,H-1,255,Info_Bank)
            BPUT #1,H,String_Length
        ELSE
           BPUT #1,Intro_Text_Store,String_Length  
        ENDIF
        EXEC FIND_STORE_STRING_SIZE Closing_Text_Store,759
        PRINT #1, String_Length
        IF MemoryConfig = 128
            CLS
            H=DPEEK(88)+40
            Z=USR(@FIELD_GET,Closing_Text_Store+510,H+509,255,Info_Bank)
            Z=USR(@FIELD_GET,Closing_Text_Store+255,H+254,255,Info_Bank)
            Z=USR(@FIELD_GET,Closing_Text_Store,H-1,255,Info_Bank)
            BPUT #1,H,String_Length
            CLS
        ELSE
           BPUT #1,Closing_Text_Store,String_Length
        ENDIF
        Z=USR(@FIELD_GET, Gamename_Store,ADR(EDIT$(0)),40,Info_Bank)
        PRINT #1, EDIT$(0)  'TITLE_TEXT$
        Z=USR(@FIELD_GET, Author_Store,ADR(EDIT$(0)),40,Info_Bank)
        PRINT #1, EDIT$(0)  'AUTHOR_TEXT$
        Z=USR(@FIELD_GET, Monetary_Unit_Store,ADR(EDIT$(0)),12,Info_Bank)
        PRINT #1, EDIT$(0)  'Next Game File
        Z=USR(@FIELD_GET, Next_File_Store,ADR(EDIT$(0)),12,Info_Bank)
        PRINT #1, EDIT$(0)  'Monetary Unit Name
        PRINT #1, "(FE)"
        PRINT #1, S0
        PRINT #1, WIN_PLACE
        PRINT #1, WIN_ITEM_01
        PRINT #1, WIN_ITEM_02
        PRINT #1, WIN_ITEM_03
        PRINT #1, WIN_DEFEAT_01
        PRINT #1, WIN_MONETARY
        PRINT #1, WIN_ITEM_04
        PRINT #1, Start_Shield_Points
        PRINT #1, S9
        PRINT #1, Inventory_Limit
        PRINT #1, Computer_Character_Move_Set
        PRINT #1, Total_Turns
        PRINT #1, Food_Turn_Set
        PRINT #1, Starting_Place_Number
        PRINT #1, Start_lives
        PRINT #1, Player_Life_Resume
        PRINT #1, HitPoint_Trap_Lost
        PRINT #1, HitPoint_Name_Set
        PRINT #1, HitPoint_Heal_Gain
        PRINT #1, Next_File_After_Solve
        PRINT #1, Armor_High_Set
        PRINT #1, Start_Monetary_Units
        PRINT #1, Computer_Character_Revive_HP
        PRINT #1, Monetary_Trap_Lost
        PRINT #1, Punch_Damage
        PRINT #1, Kick_Damage
        PRINT #1, Sale_Back_Percentage
        PRINT #1, RANDOM_SEED
        PRINT #1, S29
        PRINT #1, WIN_ITEM_05
        PRINT #1, WIN_ITEM_06
        PRINT #1, WIN_ITEM_07
        PRINT #1, WIN_ITEM_08
        PRINT #1, WIN_ITEM_09
        PRINT #1, WIN_ITEM_10
        PRINT #1, WIN_DEFEAT_02
        PRINT #1, WIN_DEFEAT_03
        PRINT #1, WIN_DEFEAT_04
        PRINT #1, WIN_DEFEAT_05
        PRINT #1, S40
        PRINT #1, S41
        PRINT #1, S42
        PRINT #1, S43
        PRINT #1, S44
        PRINT #1, S45
        PRINT #1, S46
        PRINT #1, S47
        PRINT #1, Total_Spells
        FOR N = 0 TO Total_Spells
           Z=USR(@FIELD_GET, Spell_Name_Base + 16 * N,ADR(EDIT$(0)),16,Name_Bank)
           PRINT #1, EDIT$(0)
        NEXT N
        PRINT #1, Total_Items
        FOR N = 1 TO Total_Items
         PRINT #1, Ot(N)
         PRINT #1, Op(N)
         IF OV3(N) >= 1 
            I = OV3(N)&63
            FP0% = OL(N) + 2000 * I
            OV3(N) = 0 
         ELIF OL(N) = 254 
            FP0% = 500
         ELSE
            FP0% = OL(N)
         ENDIF 
         PRINT #1, FP0%
         PRINT #1, OV(N)
         PRINT #1, OV2(N)
         PRINT #1, OV3(N)   
         Z=USR(@FIELD_GET, Item_Name_Base+ItemFieldSize * N,ADR(EDIT$(0)),ItemFieldSize ,Name_Bank)
         PRINT #1, EDIT$(0) 'ON$(N)
         Z=USR(@FIELD_GET, Item_Description_Base + 80 * N,ADR(EDIT$(0)),80,Item_Bank)
         PRINT #1, EDIT$(0) 'OD$(N)
       NEXT N
    
       PRINT #1, Total_Computer_Characters
       FOR N = 1 TO Total_Computer_Characters
            PRINT #1, HL(N)
            PRINT #1, HT(N)
            PRINT #1, HS(N)
            PRINT #1, HH(N)
            PRINT #1, HV(N)
            PRINT #1, HM(N)
            IF N  < 204 
                Z=USR(@FIELD_GET, Computer_Character_Base + 16 * N,ADR(EDIT$(0)),16,Name_Bank)
            ELSE
                B = N - 204
                EDIT$(0) = ""
            ENDIF          
            PRINT #1, EDIT$(0) 'HN$(N)
       NEXT N    
       PRINT #1, Total_Places
       FOR N = 1 TO Total_Places
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
            Z=USR(@FIELD_GET, Place_Exit_Base + 16 * N,ADR(EDIT$(0)),16,Name_Bank)
            PRINT #1, EDIT$(0) 'REN$(N)
            PRINT #1, ROV(N)
            IF N < 128
                Z=USR(@FIELD_GET, Place_Description_Base + Place_Field_Size * N,ADR(EDIT$(0)),Place_Field_Size,Place_BanK1)
            ELSE
                Z=USR(@FIELD_GET, Place_Description_Base + Place_Field_Size * (N&127),ADR(EDIT$(0)),Place_Field_Size,Place_Bank2)
            ENDIF
            PRINT #1, EDIT$(0) 'RD$(N)   
       NEXT N
     CLOSE #1
     Modified = 0
  ENDPROC
'------------------------------------------------------------------------------
  PROC Load_File
    CLOSE #1
    OPEN #1,4,0,FileName$
    INPUT #1, A
    A = A & 4095
    IF MemoryConfig = 128
        CLS
        H=DPEEK(88)+40
        BGET #1,H,A
        Z=USR(@FIELD_PUT,Intro_Text_Store,H-1,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Intro_Text_Store+255,H+254,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Intro_Text_Store+510,H+509,255,255,Info_Bank)
    ELSE
        BGET #1,Intro_Text_Store,A
    ENDIF
    INPUT #1, A
    A = A & 4095
    IF MemoryConfig = 128
        CLS
        H=DPEEK(88)+40
        BGET #1,H,A
        Z=USR(@FIELD_PUT,Closing_Text_Store,H-1,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Closing_Text_Store+255,H+254,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Closing_Text_Store+510,H+509,255,255,Info_Bank)        
        CLS
    ELSE
        BGET #1,Closing_Text_Store,A
    ENDIF
    INPUT #1, EDIT$(0)  'TITLE_TEXT$
    'EXEC CLEAR_STRING_MEMORY GameName_Store,40
    Z=USR(@FIELD_PUT,GameName_Store,ADR(EDIT$(0)),40,LEN(EDIT$(0)),Info_Bank)
    INPUT #1, EDIT$(0)  'AUTHOR_TEXT$
    'EXEC CLEAR_STRING_MEMORY Author_Store,40    
    Z=USR(@FIELD_PUT,Author_Store,ADR(EDIT$(0)),40,LEN(EDIT$(0)),Info_Bank)
    INPUT #1, EDIT$(0)  'Next Game File
    'EXEC CLEAR_STRING_MEMORY Next_File_Store,12
    MOVE ADR(EDIT$(0))+1,Next_File_Store,LEN(EDIT$(0))
    Z=USR(@FIELD_PUT,Next_File_Store,ADR(EDIT$(0)),12,LEN(EDIT$(0)),Info_Bank)
    INPUT #1, EDIT$(0)  'Monetary Unit Name
    'EXEC CLEAR_STRING_MEMORY Monetary_Unit_Store,12
    Z=USR(@FIELD_PUT,Monetary_Unit_Store,ADR(EDIT$(0)),12,LEN(EDIT$(0)),Info_Bank)
    INPUT #1, EDIT$(0)  'Future Expansion
    INPUT #1, S0
    INPUT #1, WIN_PLACE 'S1
    INPUT #1, WIN_ITEM_01
    INPUT #1, WIN_ITEM_02
    INPUT #1, WIN_ITEM_03
    INPUT #1, WIN_DEFEAT_01
    INPUT #1, WIN_MONETARY
    INPUT #1, WIN_ITEM_04
    INPUT #1, Start_Shield_Points
    INPUT #1, S9
    INPUT #1, Inventory_Limit
    INPUT #1, Computer_Character_Move_Set
    INPUT #1, Total_Turns
    INPUT #1, Food_Turn_Set
    INPUT #1, Starting_Place_Number
    INPUT #1, Start_lives
    INPUT #1, Player_Life_Resume
    INPUT #1, HitPoint_Trap_Lost
    INPUT #1, HitPoint_Name_Set
    INPUT #1, HitPoint_Heal_Gain
    INPUT #1, Next_File_After_Solve
    INPUT #1, Armor_High_Set
    INPUT #1, Start_Monetary_Units
    INPUT #1, Computer_Character_Revive_HP
    INPUT #1, Monetary_Trap_Lost
    INPUT #1, Punch_Damage
    INPUT #1, Kick_Damage
    INPUT #1, Sale_Back_Percentage
    INPUT #1, RANDOM_SEED
    INPUT #1, S29
    INPUT #1, WIN_ITEM_05
    INPUT #1, WIN_ITEM_06
    INPUT #1, WIN_ITEM_07
    INPUT #1, WIN_ITEM_08
    INPUT #1, WIN_ITEM_09
    INPUT #1, WIN_ITEM_10
    INPUT #1, WIN_DEFEAT_02
    INPUT #1, WIN_DEFEAT_03
    INPUT #1, WIN_DEFEAT_04
    INPUT #1, WIN_DEFEAT_05
    INPUT #1, S40
    INPUT #1, S41
    INPUT #1, S42
    INPUT #1, S43
    INPUT #1, S44
    INPUT #1, S45
    INPUT #1, S46
    INPUT #1, S47
    INPUT #1, Total_Spells
    FOR N = 0 TO Total_Spells
        INPUT #1, EDIT$(0)
        Z=USR(@FIELD_PUT,Spell_Name_Base+16*N,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_Bank)
        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254    
       'EXEC CLEAR_STRING_MEMORY Spell_Name_Base+16*N,16
       'MOVE ADR(EDIT$(0))+1,Spell_Name_Base+16*N,LEN(EDIT$(0))
        'IF MemoryConfig = 64 THEN POKE 54286,64:POKE $D301,255
    NEXT N
    INPUT #1, Total_Items
    FOR N = 1 TO Total_Items
      INPUT #1, OT(N)
      INPUT #1, Op(N)
      INPUT #1, FP0%
      INPUT #1, OV(N)
      INPUT #1, OV2(N)
      INPUT #1, OV3(N)
      IF FP0%<256
        OL(N) = INT(FP0%)
      ELIF FP0%>255 AND FP0%<2000
        OL(N) = 254
      ELIF FP0% > 1999
        OV3(N) = 192 + INT(FP0% / 2000)
        I = OV3(N)&63
        OL(N) = INT(FP0% - (I * 2000))
      ENDIF
      INPUT #1, EDIT$(0) 'ON$(N)
      Z=USR(@FIELD_PUT,Item_Name_Base + ItemFieldSize * N,ADR(EDIT$(0)),ItemFieldSize ,LEN(EDIT$(0)),Name_Bank)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Item_Name_Base+16*N,16
      'MOVE ADR(EDIT$(0))+1,Item_Name_Base+16*N,LEN(EDIT$(0))
      'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
      INPUT #1, EDIT$(0) 'OD$(N)
      Z=USR(@FIELD_PUT,Item_Description_Base+80*N,ADR(EDIT$(0)),80,LEN(EDIT$(0)),Item_BANK)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Item_Description_Base+80*N,80
      'MOVE ADR(EDIT$(0))+1,Item_Description_Base+80*N,LEN(EDIT$(0))
      'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
    NEXT N
    INPUT #1, Total_Computer_Characters
    FOR N = 1 TO Total_Computer_Characters
      INPUT #1, HL(N)
      INPUT #1, HT(N)
      INPUT #1, HS(N)
      INPUT #1, HH(N)
      INPUT #1, HV(N)
      INPUT #1, HM(N)
      INPUT #1, EDIT$(0) 'HN$(N)
      Z=USR(@FIELD_PUT,Computer_Character_Base+16*N,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_BANK)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Computer_Character_Base+16*N,16
      'MOVE ADR(EDIT$(0))+1,Computer_Character_Base+16*N,LEN(EDIT$(0))
      'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
    NEXT N
    INPUT #1, Total_Places
    FOR N = 1 TO Total_Places
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
        INPUT #1, EDIT$(0) 'REN$(N)
        Z=USR(@FIELD_PUT,Place_Exit_Base+16*N,ADR(EDIT$(0)),16,LEN(EDIT$(0)),Name_BANK)
        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
        'EXEC CLEAR_STRING_MEMORY Place_Exit_Base+16*N,N
        'MOVE ADR(EDIT$(0))+1,Place_Exit_Base+16*N,LEN(EDIT$(0))
        'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
        INPUT #1, ROV(N)
        INPUT #1, EDIT$(0) 'RD$(N)
        IF N < 128
              Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*N,ADR(EDIT$(0)),Place_Field_Size,LEN(EDIT$(0)),Place_Bank1)
        ELSE
              Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*(N&127),ADR(EDIT$(0)),Place_Field_Size,LEN(EDIT$(0)),Place_Bank2)
        ENDIF

        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
        'EXEC CLEAR_STRING_MEMORY Place_Description_Base+Place_Field_Size*N,32
        'MOVE ADR(EDIT$(0))+1,Place_Description_Base+Place_Field_Size*N,LEN(EDIT$(0))
        'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
    NEXT N
    CLOSE #1
    Place_Index = Total_Places
    Item_Index = Total_Items
    Computer_Character_Index = Total_Computer_Characters
    Modified = 0
  ENDPROC
'------------------------------------------------------------------------------
  PROC BIG_TEXT_EDIT EDIT_STRING_SIZE Y_POS
      StringLength = len(EDIT$(0)) + len(EDIT$(1)) + len(EDIT$(2))
      CharPosition = StringLength 
      StringPosition = CharPosition
      StringIndex = 0
      ScreenMemoryLoc = DPEEK(88)+(40*Y_POS) + BIG_TEXT_XPOS 
      a  = 126
      POKE 752,255
      
      POSITION BIG_TEXT_XPOS,Y_POS  
      PRINT EDIT$(0);EDIT$(1);EDIT$(2);"  "
      REPEAT      
        IF CharPosition < 256
          StringIndex = 0
          StringPosition = CharPosition
        ELIF CharPosition > 255 AND CharPosition < 511
          StringIndex = 1
          StringPosition = CharPosition - 255
        ELIF CharPosition > 510  
          StringIndex = 2
          StringPosition = CharPosition - 510
        ENDIF
        K = PEEK(ScreenMemoryLoc + CharPosition)
        POKE ScreenMemoryLoc + CharPosition +0,K!128
        REPEAT
                k = KEY()
        UNTIL k>0      
        POSITION BIG_TEXT_XPOS + (CharPosition-1) MOD 40, Y_POS + (CharPosition-1) / 40
        K = PEEK(ScreenMemoryLoc + CharPosition)
        POKE ScreenMemoryLoc + CharPosition +0,K&127
      '' POSITION 0,0
        GET A
      '' PRINT k;"  ",A;"  "
        IF (a>0 and a<27) or (a>31 and a<125) 
              A = A&127
              IF A < 32  
                  K = A + 64
              ELIF A < 96 
                  K = A - 32
              ELSE
                  K = A
              ENDIF
              IF CharPosition < EDIT_STRING_SIZE - 1 THEN -MOVE ScreenMemoryLoc + CharPosition, ScreenMemoryLoc + CharPosition + 1, EDIT_STRING_SIZE - CharPosition - 1
              IF StringLength < EDIT_STRING_SIZE - 1 THEN StringLength = StringLength + 1
              POKE ScreenMemoryLoc + CharPosition +0,K
              IF CharPosition < EDIT_STRING_SIZE -1 then CharPosition = CharPosition + 1
              IF CharPosition > StringLength then StringLength = CharPosition  
        ELIF a = 126 OR a = 254
              IF a = 126 
                c  = CharPosition    
              ELIF a = 254
                if CharPosition >= EDIT_STRING_SIZE - 1
                    c  = -1
                else 
                    c  = CharPosition + 1
                endif
              ENDIF
              IF c >=0
                  MOVE ScreenMemoryLoc + C, ScreenMemoryLoc + C - 1, EDIT_STRING_SIZE - C 
                  POKE ScreenMemoryLoc + EDIT_STRING_SIZE - 1,0
                  IF CharPosition >= 1 and a = 126 THEN CharPosition = CharPosition - 1
                  IF CharPosition >= StringLength and a=254 THEN CharPosition = StringLength + 1
                  IF StringLength >=1 THEN StringLength = StringLength -1          
              ENDIF                
        ELIF A=28
            if CharPosition > 40 then CharPosition = CharPosition - 40
        ELIF A=29
             if StringLength>40 
               if CharPosition < StringLength - 38 then CharPosition = CharPosition + 40 
               if CharPosition >= StringLength THEN CharPosition = StringLength  
             endif
        ELIF A=30  
              if CharPosition > 0 then CharPosition = CharPosition - 1
        ELIF A=31 
              if CharPosition <= StringLength then CharPosition = CharPosition + 1
              if CharPosition > StringLength THEN CharPosition = StringLength 
        ENDIF
        ' StringLength = len(EDIT$(0)) + len(EDIT$(1)) + len(EDIT$(2))
      UNTIL A = 155
      EDIT$(0)=""
      EDIT$(1)=""
      EDIT$(2)=""      
      K = PEEK(ScreenMemoryLoc + CharPosition)
      POKE ScreenMemoryLoc + CharPosition +0,K&127
      StringLength=USR(@SCR2ATASCII,ScreenMemoryLoc,EDIT_STRING_SIZE - 1)    
      POKE 54286,0
      POKE 54272,0
      IF StringLength > 255 
            IF StringLength > 510 
                  M = StringLength - 510
                  MOVE ScreenMemoryLoc+510,ADR(EDIT$(2))+1,M  
                  POKE ADR(EDIT$(2)),M                  
                  M = 255
            ELSE
                  M = StringLength - 255                  
            ENDIF
            MOVE ScreenMemoryLoc+255,ADR(EDIT$(1))+1,M  
            POKE ADR(EDIT$(1)),M 
            M = 255
      ELSE
            M = StringLength  
      ENDIF
      MOVE ScreenMemoryLoc,ADR(EDIT$(0))+1,M  
      POKE ADR(EDIT$(0)),M 
      POSITION BIG_TEXT_XPOS,Y_POS
      PRINT EDIT$(0);EDIT$(1);EDIT$(2)
      POKE 54286,64
      POKE 752,0
      POSITION BIG_TEXT_XPOS,Y_POS 
  ENDPROC
'------------------------------------------------------------------------------  
  PROC EDITVALUE J
      I = PEEK(J)
      PRINT " ";I;CHR$(28)
      INPUT I     
      POKE J,I
  ENDPROC
 '------------------------------------------------------------------------------
  PROC Goto_Index
            POSITION 0,23
            PRINT CHR$(156);"GO#";
            EDIT$(0)=""
            EXEC EDIT_TYPE_FLAG 4,23
            B = VAL(EDIT$(0))
            A = 127
  ENDPROC
'------------------------------------------------------------------------------
  PROC EDIT_TYPE_FLAG X_POS Y_POS
    POKE 752,255
    StringLength = len(EDIT$(0))
    if CharPosition>=StringLength and StringLength < 3 THEN CharPosition = StringLength + 1
    REPEAT
      POSITION X_POS, Y_POS 
      PRINT EDIT$(0);"  ";
      IF CharPosition >= len(EDIT$(0))  
        a = 32
      ELSE
        a = ASC(EDIT$(0)[CharPosition,1])
      ENDIF
      POSITION X_POS + (CharPosition-1) MOD 40, Y_POS + (CharPosition-1) / 40
      PRINT CHR$(A!128);   
      REPEAT
              k = KEY()
      UNTIL k>0
      GET A
      IF a > 47 AND a < 58 and len(EDIT$(0)) < 3
          EDIT$(0) =+ CHR$(A)
          StringLength = len(EDIT$(0))
          if CharPosition>=StringLength THEN CharPosition = StringLength + 1
      ELIF a = 126 or a=254
          CharPosition = len(EDIT$(0)) + 1
          IF CharPosition > 1
              c = CharPosition  - 1     
              EDIT$(0) = EDIT$(0)[1,c-1]
              CharPosition = CharPosition - 1
          ELSE   
            EDIT$(0) = ""
          ENDIF
      ENDIF    
    UNTIL a = 155 OR a = 28 or a = 29 or a = 27 or a=127 or a = 125
    POSITION X_POS, Y_POS 
    PRINT EDIT$(0);"  ";
    POKE 752,0
  ENDPROC 
'------------------------------------------------------------------------------
   PROC FIND_STORE_STRING_SIZE String_Addr_Start String_Length_In
      String_Length = String_Length_In + 1 
      REPEAT
        String_Length = String_Length - 1
      UNTIL PEEK(String_Addr_Start + String_Length-1)>0 OR String_Length = 0  
   ENDPROC
'------------------------------------------------------------------------------
    PROC DISK_DIRECTORY  
        EDIT$(3) = "D1:*.TGF"
        REPEAT
              CLS
              A = 0
              B = 1        
              PRINT "TEXT GAMES ON THIS DISK. ";EDIT$(3)[1,2]
              OPEN #1,6,0,EDIT$(3)
              REPEAT
                  POSITION A,B 
                  INPUT #1,EDIT$(0)
                  IF ERR()=1 THEN PRINT EDIT$(0)
                  A = A + 20
                  IF A > 25 
                      A = 0
                      B = B + 1
                  ENDIF
              UNTIL ERR()>1
              CLOSE #1
              POSITION 0,22
              PRINT PROMPT$;">";FileName$
              POSITION 12,22
              INPUT ">>";FileName$
              IF FileName$<>""
                  J = LEN(FileName$) 
                  I = ASC(FileName$[J,1])
                  IF J>0 AND J<=2 AND I>47 AND I<58 
                      EDIT$(3) = "D"
                      EDIT$(3) =+ CHR$(I)
                      EDIT$(3) =+":*.TGF"
                  ELSE                    
                    IF J > 0 
                        IF FileName$[2,1]<>":" and FileName$[3,1]<>":" 
                              EDIT$(0) = FileName$
                              Filename$ = EDIT$(3)[1,3]
                              Filename$ =+EDIT$(0)
                        ENDIF
                        A = LEN(FileName$) - 4
                        IF A < 1 THEN A = 1
                        IF FileName$[A,4]<>".TGF"
                              Filename$ =+".TGF"
                        ENDIF
                    ENDIF
                    EDIT$(3) = "FILE OK"
                  ENDIF
              ENDIF
        UNTIL EDIT$(3) = "FILE OK"
    ENDPROC
'------------------------------------------------------------------------------    
    PROC Get_Memory_Configuration
        CLS
        PRINT "1)  48K ATARI 400/800."
        PRINT "2)  52K ATARI 400/800."
        PRINT "3)  64K ATARI 800XL/65XE."
        PRINT "4) 128K ATARI 130XE (MUST RESTART)."
        A = 0
        REPEAT
          A = KEY()
        UNTIL A>0
        GET A   
        IF A >48 AND A<53 
            A = A&15
            IF A = 1
                MemoryConfig = 48                 
            ELIF A = 2
                MemoryConfig = 52
            ELIF A = 3
                MemoryConfig = 64
            ELIF A = 4 
                MemoryConfig = 128
            ENDIF
        ENDIF
        
    ENDPROC
'------------------------------------------------------------------------------    
    PROC Set_Memory_Configuration
        Place_Bank2 = 255
        Place_Field_Size = 120
        IF MemoryConfig = 48
            ItemTableSize = 30
            ComputerCharacterTableSize = 20
            PlaceTableSize = 38
            SpellDataSize = 4
            SpellTableSize = 16
            ItemFieldSize = 16
            String_Store_Base = 38912
            GameName_Store = String_Store_Base
            Author_Store = String_Store_Base + 40
            Monetary_Unit_Store = String_Store_Base + 80
            Next_File_Store = String_Store_Base + 92
            Intro_Text_Store = String_Store_Base + 104
            Closing_Text_Store = Intro_Text_Store  + 760
            Place_Description_Base = Closing_Text_Store + 760
            Item_Description_Base = Place_Description_Base + 120 * PlaceTableSize 
            Place_Exit_Base = Item_Description_Base + 80 * ItemTableSize 
            Item_Name_Base = Place_Exit_Base + 16 * PlaceTableSize
            Computer_Character_Base = Item_Name_Base + 16 * ItemTableSize
            Spell_Name_Base = Computer_Character_Base + 16 * ComputerCharacterTableSize
            String_Storage_Base_Last = Spell_Name_Base + 16 * SpellDataSize
            Info_Bank = 255
            Place_BanK1 = 255
            Item_Bank = 255
            Name_Bank = 255
            DPOKE 2,$9200                       
        ELIF MemoryConfig = 52
            ItemTableSize = 51
            ComputerCharacterTableSize = 32
            PlaceTableSize = 52
            SpellTableSize = 16
            SpellDataSize = 4
            ItemFieldSize = 16
            String_Store_Base = 38912
            GameName_Store = String_Store_Base
            GameName_Store = String_Store_Base
            Author_Store = String_Store_Base + 40
            Monetary_Unit_Store = String_Store_Base + 80
            Next_File_Store = String_Store_Base + 92
            Intro_Text_Store = String_Store_Base + 104
            Closing_Text_Store = Intro_Text_Store  + 760  
            Place_Description_Base = Closing_Text_Store + 760
            Place_Exit_Base = Place_Description_Base + 120 * ItemTableSize 
            Item_Name_Base = Place_Exit_Base + 16 * PlaceTableSize
            Computer_Character_Base = Item_Name_Base + 16 * ItemTableSize
            Spell_Name_Base = Computer_Character_Base + 16 * ComputerCharacterTableSize
            String_Storage_Base_Last = Spell_Name_Base + 16 * SpellDataSize
            Item_Description_Base = 49152 
            Info_Bank = 255
            Place_BanK1 = 255
            Item_Bank = 255
            Name_Bank = 255
            DPOKE 2,$9200                       
        ELIF MemoryConfig = 64
            ItemTableSize = 72
            ComputerCharacterTableSize = 36
            PlaceTableSize = 84
            SpellTableSize = 16
            SpellDataSize = 16
            ItemFieldSize = 20
            Place_Description_Base = 55296 
            String_Store_Base = 38912
            GameName_Store = String_Store_Base
            Author_Store = String_Store_Base + 40
            Monetary_Unit_Store = String_Store_Base + 80
            Next_File_Store = String_Store_Base + 92
            Intro_Text_Store = String_Store_Base + 104
            Closing_Text_Store = Intro_Text_Store  + 760  
            Item_Description_Base = Closing_Text_Store + 760 
            Item_Name_Base = 49152
            Place_Exit_Base = Item_Name_Base + 20 * ItemTableSize
            Computer_Character_Base = Place_Exit_Base + 16 * PlaceTableSize
            Spell_Name_Base = Computer_Character_Base + 16 * ComputerCharacterTableSize
            String_Storage_Base_Last = Spell_Name_Base + 16 * SpellDataSize
            Info_Bank = 255
            Place_BanK1 = 254
            Item_Bank = 255
            Name_Bank = 254
            DPOKE 2,$BA00                                   
        ELIF MemoryConfig = 128
            ItemTableSize = 204
            ComputerCharacterTableSize = 250
            PlaceTableSize = 250
            SpellTableSize = 16
            SpellDataSize = 16
            ItemFieldSize = 20
            Place_Description_Base = 16384 
            Item_Description_Base = 16384 
            String_Store_Base = 16384
            GameName_Store = String_Store_Base
            Author_Store = String_Store_Base + 40
            Monetary_Unit_Store = String_Store_Base + 80
            Next_File_Store = String_Store_Base + 92
            Intro_Text_Store = String_Store_Base + 104
            Closing_Text_Store = Intro_Text_Store  + 800  
            Item_Name_Base = Closing_Text_Store + 800
            Place_Exit_Base = Item_Name_Base + 16 * PlaceTableSize
            Computer_Character_Base = Place_Exit_Base + 20 * ItemTableSize
            Spell_Name_Base = Computer_Character_Base + 16 * ComputerCharacterTableSize
            String_Storage_Base_Last = String_Store_Base + 16 * SpellDataSize        
            Place_BanK1 = 227
            Place_Bank2 = 231
            Place_Field_Size = 128
            Item_Bank = 235
            Info_Bank = 239
            Name_Bank = 239
            DPOKE 2,$BA00                                   
        ENDIF
        IF  MemoryConfig <=64  
            POKE 106,152
        ELSE
            POKE 106,192
        ENDIF
        GRAPHICS 0         
    ENDPROC
'------------------------------------------------------------------------------
  PROC SURE
    IF Modified = 1 
        SureToProceed = 0
        PRINT 
        PRINT "The Game Information has been modified,"
        PRINT "and not yet saved. Type YES to proceed."
        INPUT Edit$(3)
        IF Edit$(3)="YES" or Edit$(3)="yes" then SureToProceed = 1
    ELSE
        SureToProceed = 1
        Modified = 0
    ENDIF                                   
    'Z=USR(@FIELD_PUT,1,2,3,4,5)
    'Z=USR(@FIELD_GET,1,2,3,4,5)
  ENDPROC
'------------------------------------------------------------------------------  
END  

