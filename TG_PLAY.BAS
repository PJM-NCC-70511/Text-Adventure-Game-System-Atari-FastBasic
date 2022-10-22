'can we port to fast basic (Atari)?'
'can we port to visual basic.net (Windows PC)?'

  PW_DIM = 32
  ItemTableSize = 128
  RoomDataSize = 192
  OPEN #1,4,0,"D:TAGSETUP.TGD"
  IF err()=1 
     INPUT #1,MemoryConfig
  ELSE
     MemoryConfig = 48
  ENDIF  
  CLOSE #1 
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
  string_in$ = ""
  Last_Show_Number = 0
  seek$ = ""
  found_loc = 0
  filename$ = ""
  I = 0
  J = 0
  K = 0
  L = 0
  N = 0
  M = 0
  FP0% = 0
  FP1% = 0
  EXEC Set_Memory_Configuration
  DIM Ot(ItemTableSize) BYTE
  DIM OP(ItemTableSize) BYTE
  DIM OV(ItemTableSize) BYTE
  DIM OV2(ItemTableSize) BYTE
  DIM OV3(ItemTableSize) BYTE
  DIM OL(ItemTableSize) BYTE
  
  DIM Ht(ComputerCharacterTableSize) BYTE
  DIM Hl(ComputerCharacterTableSize) BYTE
  DIM HS(ComputerCharacterTableSize) BYTE
  DIM HH(ComputerCharacterTableSize) BYTE
  DIM Hm(ComputerCharacterTableSize) BYTE
  DIM Hv(ComputerCharacterTableSize) BYTE
  DIM ComputerCharacterName$(ComputerCharacterTableSize)
  DIM HO(ComputerCharacterTableSize) BYTE
  
  
  DIM RT(PlaceTableSize)   BYTE
  DIM Rn(PlaceTableSize)   BYTE
  DIM Rnl(PlaceTableSize)  BYTE
  DIM Rnk(PlaceTableSize)  BYTE
  DIM Rw(PlaceTableSize)   BYTE
  DIM Rwl(PlaceTableSize)  BYTE
  DIM Rwk(PlaceTableSize)  BYTE
  DIM Re(PlaceTableSize)   BYTE
  DIM Rel(PlaceTableSize)  BYTE
  DIM Rek(PlaceTableSize)  BYTE
  DIM Rs(PlaceTableSize)   BYTE
  DIM Rsl(PlaceTableSize)  BYTE
  DIM Rsk(PlaceTableSize)  BYTE
  DIM Ru(PlaceTableSize)   BYTE
  DIM Rul(PlaceTableSize)  BYTE
  DIM Rd(PlaceTableSize)   BYTE
  DIM Rdl(PlaceTableSize)  BYTE
  DIM Ro(PlaceTableSize)   BYTE
  DIM Rol(PlaceTableSize)  BYTE
  DIM Rov(PlaceTableSize)  BYTE

  DIM Show$(3)  
  Show$(0) = "  "
  Show$(1) = "  "
  Show$(2) = "  "
  Show$(3) = "  "
  Text_Justify = 1  
  CHOICE = 0
  EXEC Initialize
  REPEAT
      CLS
      POKE 82,0
      POKE 710,0
      POKE 709,12
      EXEC Get_game
      EXEC Main_line
      EXEC Game_over
  UNTIL CHOICE = 81
  END
'------------------------------------------------------------------------------
PROC Initialize
  'CHDIR "TEXTGAME"
'  OPEN "TG.PIF" FOR INPUT AS #1
'    INPUT #1, ADAPTER
'    INPUT #1, PATH$
'  CLOSE #1
  I=0
  J=0
  K=0
  L=0
  M=0
  N=0
  O=0
  Q=0
  Current_Place=0
  CV=0
  LOC_TO = 0
  LOC_THE = 0
  Turn_Number = 0
  P$=" "
  Verb$ = ""
  PriorCommand$ = ""
  PriorRoom=0  
  PlayerHitPoints =0
  PlayerFoodWaterNeed = 0
  PlayerTurn= 0
  PlayerLifeStatus = 0 
  PlayerWinStatus = 0 
  PlayerImmunityTurns = 0 
  PlayerPoisonStatus = 0
  PlayerInfectionStatus = 0
  TotalComputerCharactersLoaded = 0
  Armor_Points = 0     
  Place_Index = 0
  Item_Index = 0
  Items_Carrying = 0
  Computer_Character_Index = 0
  Total_Items = 0
  Total_Places = 0
  Total_Computer_Characters = 0
  Total_Spells = 0
  


   S0 = 0
   WIN_PLACE = 0
   S2 = 0
   S3 = 0
   S4 = 0
   S5 = 0
   S6 = 0
   S7 = 0
   S8 = 0
   S9 = 0
   Inventory_Limit = 0
   S11 = 0
   S12 = 0
   S13 = 0
   S14 = 0
   S15 = 0
   S16 = 0
   S17 = 0
   S18 = 0
   S19 = 0
   S20 = 0
   S21 = 0
   S22 = 0
   S23 = 0
   S24 = 0
   S25 = 0
   S26 = 0
   S27 = 0
   S28 = 0
   S29 = 0
   S30 = 0
   S31 = 0
   S32 = 0
   S33 = 0
   S34 = 0
   S35 = 0
   S36 = 0
   S37 = 0
   S38 = 0
   S39 = 0
   S40 = 0
   C=0
   found_loc = 0 
   a$ = " "
   CHOICE = 0 
ENDPROC
'------------------------------------------------------------------------------
PROC Main_line
 REPEAT  
   REPEAT     
     PriorRoom = Current_Place
     CLS
     FirstMove = 1
     IF RT(Current_Place) = 8 
       '***   EXEC Answer_riddle
     ELSE
       EXEC New_place
       IF PlayerHitPoints < 0 OR PlayerFoodWaterNeed <= 0 THEN PlayerLifeStatus = 1
       Ft = 1
       IF PlayerLifeStatus = 0 
         WHILE Current_Place = PriorRoom AND PlayerHitPoints >= 0 AND PlayerFoodWaterNeed >= 1 AND PlayerTurn<= S12 AND PlayerLifeStatus = 0 AND PlayerWinStatus = 0
           PlayerTurn = PlayerTurn + 1
           PlayerFoodWaterNeed = PlayerFoodWaterNeed - 1
           IF Ft = 0 
             PRINT "---------------------------------------"
           ELSE
             PRINT
             Ft = 0
           ENDIF
           H = 0
           IF FirstMove = 1
             EXEC Check_for_Computer_Controlled_Characters
           ELSE
             EXEC Move_Computer_Controlled_Characters
           ENDIF
           EXEC Enter_command
           IF LEN(a$) >= 2 
             EXEC Perform_action
           ELSE
             EXEC Go_direction
           ENDIF
           IF PlayerImmunityTurns > 0 
             PlayerImmunityTurns = PlayerImmunityTurns - 1
           ELSE
             IF PlayerInfectionStatus > 0 
               PlayerHitPoints = PlayerHitPoints - PlayerInfectionStatus
             ENDIF
             IF PlayerPoisonStatus > 0 
               PlayerHitPoints = PlayerHitPoints - PlayerPoisonStatus
             ENDIF
             IF H > 0 
               EXEC Computer_Character_Attack
             ENDIF
           ENDIF
         WEND
       ENDIF
       N = 0
       IF Current_Place = WIN_PLACE OR WIN_PLACE = 0 THEN N = N + 1
       IF OL(S2) = 0 OR S2 = 0 THEN N = N + 1
       IF OL(S3) = 0 OR S3 = 0 THEN N = N + 1
       IF OL(S4) = 0 OR S4 = 0 THEN N = N + 1
       IF OL(S7) = 0 OR S7 = 0 THEN N = N + 1
       IF OL(S30) = 0 OR S30 = 0 THEN N = N + 1
       IF OL(S31) = 0 OR S31 = 0 THEN N = N + 1
       IF OL(S32) = 0 OR S32 = 0 THEN N = N + 1
       IF OL(S33) = 0 OR S33 = 0 THEN N = N + 1
       IF OL(S34) = 0 OR S34 = 0 THEN N = N + 1
       IF OL(S35) = 0 OR S35 = 0 THEN N = N + 1
       IF HH(S5) = 0 OR S5 = 0 THEN N = N + 1
       IF HH(S36) = 0 OR S36 = 0 THEN N = N + 1
       IF HH(S37) = 0 OR S37 = 0 THEN N = N + 1
       IF HH(S38) = 0 OR S38 = 0 THEN N = N + 1
       IF HH(S39) = 0 OR S39 = 0 THEN N = N + 1
       IF C >= S6 OR S6 = 0 THEN N = N + 1
       IF N = 17 THEN PlayerWinStatus= 1
     ENDIF
   UNTIL PlayerLifeStatus = 1 OR PlayerWinStatus= 1 OR PlayerTurn> S12
   IF PlayerTurn> S12  
      LIVES = -1
   ENDIF 
   IF PlayerHitPoints < 0 OR PlayerFoodWaterNeed < 0 OR PlayerLifeStatus 
     PlayerLifeStatus = 0
     LIVES = LIVES - 1
     IF LIVES > 0 
       PRINT "You have "; LIVES; " lives left."
       PRINT "Press any key to continue."
       N = 0
       REPEAT
         N = KEY()
       UNTIL N > 0
     ENDIF
     IF PlayerHitPoints < 10 THEN PlayerHitPoints = S8
     IF PlayerFoodWaterNeed < 10 THEN PlayerFoodWaterNeed = S13
     IF S16 = 2 OR S16 = 3 OR RT(Current_Place) = 2 OR RT(Current_Place) = 5 OR RT(Current_Place) = 9 
       PriorRoom = S14
       Current_Place = S14
     ENDIF
     IF S16 = 1 
       PriorRoom = 0
       H = 0
     ENDIF
   ENDIF
 UNTIL LIVES <= 0 OR PlayerWinStatus = 1
ENDPROC 
'------------------------------------------------------------------------------
'******************** Computer Controlled Character Section *******************
'------------------------------------------------------------------------------
PROC Check_for_Computer_Controlled_Characters
    FOR N = 1 TO Total_Computer_Characters
        IF Hl(N) = Current_Place
            EXEC Computer_Controlled_Character_Present
        ENDIF
    NEXT N
    FirstMove = 0
ENDPROC  
'------------------------------------------------------------------------------
PROC Computer_Controlled_Character_Present
          M = Ht(N) AND 127
          IF HH(N) >= 0 
                Computer_Character_Index = N
                Z=USR(@FIELD_GET, Computer_Character_Base + 16 * Computer_Character_Index,ADR(SHOW$(3)),16,Name_Bank)
                Z=USR(@TRIM,ADR(Show$(3)))
                Z=USR(@CHANGE_CASE,2,ADR(Show$(3)))
                IF M = 1 OR M = 2 OR M = 12 OR M = 13 
                  PRINT "The "; Show$(3); " is attacking you."
                  H = N
                ENDIF
                IF (M > 2 AND RT(N) < 12) OR M > 13 
                  PRINT "The "; Show$(3); " is in here."
                  H = N
                ENDIF
                IF HH(N) > 0 AND M < 12 
                  PRINT "It has "; HH(N); " hit points."
                ENDIF
          ELSE
                PRINT "There is a Dead "; Show$(3); " in here."
          ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Fight_Computer_Controlled_Character 
     IF Computer_Character_Index > 0 AND (Ht(Computer_Character_Index) < 13 OR Ht(Computer_Character_Index) = 15) 
        Z=USR(@FIELD_GET, Computer_Character_Base + 16 * Computer_Character_Index,ADR(SHOW$(2)),16,Name_Bank)
        Z=USR(@TRIM,ADR(Show$(2)))
        Z=USR(@CHANGE_CASE,2,ADR(Show$(2)))         
         IF LEN(a$) >= 3 
               Hd = 0
               EXEC Find_Item_Number
               IF N >= 1 AND Found_Loc >=1
                     t1 = OV2(N)
                     IF OL(N) = 0 AND Ot(N) = 3 
                           IF t1 > Total_Items OR t1 = 0 
                             t1 = N
                           ENDIF            
                           IF OV2(N) = 0 
                             Hd = OV(N)
                           ENDIF
                           IF OV2(N) > 0 AND OV2(N) < 500 
                             IF OV(OV2(N)) > 0 AND OL(OV2(N)) = 0 
                               OV(OV2(N)) = OV(OV2(N)) - 1
                               Hd = OV(N)
                             ENDIF
                             IF OV(OV2(N)) <= 0 AND OL(OV2(N)) = 0 
                               Hd = 0
                               '*** 'PRINT "You are out of "; Show$(3)ItemName$(OV2(N)); ". "
                             ENDIF
                           ENDIF
                           IF OV2(N) = 500 
                             IF a$[1, 2] = "K1" 
                               OL(N) = Current_Place
                               Hd = OV(N)
                             ELSE
                               PRINT "It must be thrown."
                             ENDIF
                           ENDIF
                           IF OV2(N) = 510 
                             Hd = OV(N)
                             OL(N) = -1
                           ENDIF
                           IF Ht(Computer_Character_Index) = 12 AND Ot(N) = HH(Computer_Character_Index) 
                             HH(Computer_Character_Index) = 0
                             Hd = 1
                           ELIF Ht(Computer_Character_Index) = 0 
                             Hd = 0
                           ENDIF
                     ELIF Ot(N) <> 3 AND OL(N) = 0 
                         PRINT "It is not a weapon."
                     ELIF  OL(N) >= 0 
                         PRINT "It is not in your inventory."
                     ENDIF
               ENDIF 
               IF a$[1, 2] = "K2" 
                 Hd = S26
               ENDIF
               IF a$[1, 2] = "K3" 
                 Hd = S25
               ENDIF
               IF Hd > 0 
                 HH(Computer_Character_Index) = HH(Computer_Character_Index) - Hd
                 IF HH(Computer_Character_Index) >= 0 
                     Show$(0) = "You just damaged the "
                     Show$(0) =+ Show$(2) 
                     Show$(0) =+ " by " 
                     Show$(0) =+ STR$(Hd) 
                     Show$(0) =+ " hitpoints."
                     ADJ = 1
                     EXEC Display
                 ELSE
                     PRINT "You killed the "; Show$(2); ". "
                     IF Ht(Computer_Character_Index) = 6 AND HS(Computer_Character_Index) > 0 
                       
                       '*** PRINT "Found "; ItemName$(HS(Computer_Character_Index)); ". "
                       OL(HS(Computer_Character_Index)) = Current_Place
                       HS(Computer_Character_Index) = 0
                     ENDIF
                     EXEC Search_Dead_Computer_Character_Inventory
                     Ht(Computer_Character_Index) = Ht(Computer_Character_Index) OR 128
                     Hm(Computer_Character_Index) = Hm(Computer_Character_Index) OR 128
                     Computer_Character_Index = 0
                     IF S19 
                       Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
                       PRINT "You have been awarded "; S19; " "; P$; ". "
                       C = C + S19
                     ENDIF
                 ENDIF
            ENDIF
        ENDIF
     ELSE
       IF Computer_Character_Index = 0 
         PRINT "There is nothing here."
       ENDIF
       IF Ht(Computer_Character_Index) >= 13 AND Ht(Computer_Character_Index) <= 14 
         PRINT "You caused no damage."
       ENDIF
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Search_Dead_Computer_Character_Inventory
     FOR N = 1 TO Total_Items
       IF OV3(N) > 192 
         IF OV3(N)&63 = Computer_Character_Index 
            Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
            Z=USR(@CHANGE_CASE,0,ADR(Show$(3)))
           
           OL(N) = 0
           PRINT "Now found the "; Show$(3); ". "
         ENDIF
       ENDIF
     NEXT N
ENDPROC
'------------------------------------------------------------------------------
PROC Computer_Character_Attack
      Show$(0) = "NO ATTACK " 
      Z=USR(@FIELD_GET, Computer_Character_Base + 16 * Computer_Character_Index,ADR(SHOW$(2)),16,Name_Bank)
      Z=USR(@TRIM,ADR(Show$(2)))
      Z=USR(@CHANGE_CASE,2,ADR(Show$(2)))              
     IF Ht(Computer_Character_Index) = 1 OR Ht(Computer_Character_Index) = 12 OR Ht(Computer_Character_Index) = 13 
       IF S21 = 0 THEN FP0% = ((100 - Armor_Points) / 100) * HS(Computer_Character_Index)
       IF S21 > 0 THEN FP0% = (1 - Armor_Points / (50 + Armor_Points)) * HS(Computer_Character_Index)
       FP0% = INT(((.5 + FP0%) * 100) / 100)
       PlayerHitPoints = PlayerHitPoints - INT(FP0%)
       IF PlayerHitPoints > 0 
         EXEC Get_Hitpoint_Name
         Show$(0) = "The "
         Show$(0) =+ Show$(2) 
         Show$(0) =+ " attacked, and lost " 
         Show$(0) =+ STR$(FP0%) 
         Show$(0) =+ P$
         Show$(0) =+ ". "
       ENDIF
     ENDIF
     IF Ht(Computer_Character_Index) = 2 OR PlayerHitPoints < 0 
       PlayerLifeStatus = 1
       Show$(0) = "The " 
       Show$(0) =+ Show$(2) 
       Show$(0) =+ " killed you."
       IF PriorRoom > 0 THEN Current_Place = PriorRoom
     ENDIF
     IF Ht(Computer_Character_Index) = 3 OR Ht(Computer_Character_Index) = 4 OR Ht(Computer_Character_Index) = 5 
       FOR N = 1 TO Total_Items
         IF OL(N) = 0 
           IF Ht(Computer_Character_Index) = 3 
             OL(N) = Current_Place
           ENDIF
           IF Ht(Computer_Character_Index) = 4 AND Ot(N) = 1 
             OL(N) = Current_Place
             Show$(0) = "The " 
             Show$(0) =+ Show$(2) 
             Show$(0) =+ " attacked and you lost all treasures."
           ENDIF
           IF Ht(Computer_Character_Index) = 5 AND Ot(N) = 2 
             OL(N) = Current_Place
             Show$(0) = "The " 
             Show$(0) =+ Show$(2) 
             Show$(0) =+ " attacked and you lost all keys."
           ENDIF
           IF Ht(Computer_Character_Index) = 15 AND Ot(N) = 3 OR Ot(N) = 4 
             OL(N) = Current_Place
             Show$(0) = "The " 
             Show$(0) =+ Show$(2) 
             Show$(0) =+ " attacked and you lost all weapons."
           ENDIF
         ENDIF
       NEXT N
     ENDIF
     IF Ht(Computer_Character_Index) = 9 
       Current_Place = HS(Computer_Character_Index)
     ENDIF
     IF Ht(Computer_Character_Index) = 10 AND C > HS(Computer_Character_Index) 
       C = C - HS(Computer_Character_Index)
       Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
       Show$(0) = "The " 
       Show$(0) =+ Show$(2) 
       Show$(0) =+ " just took " 
       Show$(0) =+ STR$(HS(Computer_Character_Index)) 
       Show$(0) =+ " " 
       Show$(0) =+ P$
       Show$(0) =+ ". "
     ENDIF
     IF Ht(Computer_Character_Index) = 16 
       Show$(0) = "The " 
       Show$(0) =+ Show$(2) 
       Show$(0) =+ " just bit you. The wounds are PlayerInfectionStatusected."
       PlayerInfectionStatus = HS(Computer_Character_Index)
     ENDIF
     IF Ht(Computer_Character_Index) = 17 
       Show$(0) = "The " 
       Show$(0) =+ Show$(2) 
       Show$(0) =+ " just bit you and injected poison."
       PlayerPoisonStatus = HS(Computer_Character_Index)
     ENDIF
     IF Show$(0) <> "NO ATTACK " 
       ADJ = 1
       EXEC Display
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Move_Computer_Controlled_Characters
     IF S11 = 1 
       FOR N = 1 TO Total_Computer_Characters
         IF HH(N) >= 0 
           EXEC Random_place
         ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ENDIF
         IF Ht(N) = 11 
           EXEC Heal_Computer_Controlled_Character
         ENDIF
         IF Hl(N) = Current_Place 
           EXEC Computer_Controlled_Character_Present
         ENDIF
       NEXT N
     ENDIF
     IF S11 = 2 
       FOR N = 1 TO Total_Computer_Characters
         IF HH(N) >= 0 
           EXEC Random_place
         ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ENDIF
         IF Ht(N) = 11 
           EXEC Heal_Computer_Controlled_Character
         ENDIF
         IF Hl(N) = Current_Place 
           EXEC Computer_Controlled_Character_Present
         ENDIF
       NEXT N
     ENDIF
     IF S11 = 3 
       FOR N = 1 TO Total_Computer_Characters         
           IF Hm(N) = 2
             EXEC Random_place
           ELIF Hm(N) = 3
             K = RAND(7)
             EXEC Move_Computer_Character_To_New_Place
           ELIF Hm(N) = 4
             EXEC Inc_place_number
           ELIF Hm(N) = 5
             K = RAND(4)
             EXEC Move_Computer_Character_To_New_Place
           ELIF Hm(N) = 6
             EXEC Random_place
           ELIF Hm(N) = 7
             EXEC Dec_place_number
           ELIF Hm(N) = 8
             EXEC Persue_player
           ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ELIF Ht(N) = 11 
           EXEC Heal_Computer_Controlled_Character
         ENDIF
         IF Hl(N) = Current_Place THEN EXEC Computer_Controlled_Character_Present
       NEXT N
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Move_Computer_Character_To_New_Place
     L = Hl(N) 
     IF K = 0 AND Rn(L) = 1 OR Rn(L) = 2 OR Rn(L) = 4 THEN Hl(N) = Rnl(L)
     IF K = 1 AND Re(L) = 1 OR Re(L) = 2 OR Re(L) = 4 THEN Hl(N) = Rel(L)
     IF K = 2 AND Rw(L) = 1 OR Rw(L) = 2 OR Rw(L) = 4 THEN Hl(N) = Rwl(L)
     IF K = 3 AND Rs(L) = 1 OR Rs(L) = 2 OR Rs(L) = 4 THEN Hl(N) = Rsl(L)
     IF K = 4 AND Ru(L) = 1 OR Ru(L) = 2 OR Ru(L) = 4 THEN Hl(N) = Rul(L)
     IF K = 5 AND Rd(L) = 1 OR Rd(L) = 2 OR Rd(L) = 4 THEN Hl(N) = Rdl(L)
     IF K = 6 AND Ro(L) = 1 OR Ro(L) = 3 THEN Hl(N) = Rol(L)
     M = RT(Hl(N))
     IF M > 1 AND M < 4 OR M > 6 AND M < 11 OR M > 14 THEN Hl(N) = L
ENDPROC
'------------------------------------------------------------------------------
PROC Random_place
     L = 1 + RAND(Total_Places)
     M = RT(L)
     IF (M > 1 AND M < 4) OR (M > 6 AND M < 11) OR M > 14 AND Hm(N) = 2 
       Hl(N) = L
     ENDIF
     IF M <> 2 AND M <> 8 AND Hm(N) = 6 
       Hl(N) = L
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Inc_place_number
     REPEAT
       Hl(N) = Hl(N) + 1
       IF Hl(N) > Total_Places THEN Hl(N) = 1
       M = RT(Hl(N))
     UNTIL M = 1 OR M = 4 OR M = 5 OR M = 6 OR M > 10
ENDPROC
'------------------------------------------------------------------------------
PROC Dec_place_number
     REPEAT
       Hl(N) = Hl(N) - 1
       IF Hl(N) < 1 THEN Hl(N) = Total_Places
       M = RT(Hl(N))
     UNTIL M = 1 OR M = 4 OR M = 5 OR M = 6 OR M > 10
ENDPROC
'------------------------------------------------------------------------------
PROC Persue_player
       L = Hl(N)     
       IF Current_Place = Rnl(L)
         Hl(N) = Rnl(N)
       ELIF Current_Place = Rel(L)
         Hl(N) = Rel(N)
       ELIF Current_Place = Rwl(L)
         Hl(N) = Rwl(N)
       ELIF Current_Place = Rsl(L)
         Hl(N) = Rsl(N)
       ELIF Current_Place = Rul(L)
         Hl(N) = Rul(N)
       ELIF Current_Place = Rdl(L)
         Hl(N) = Rdl(N)
       ELIF Current_Place = Rol(L)
         Hl(N) = Rol(N)
       ELSE
         K = RAND(7)
         EXEC Move_Computer_Character_To_New_Place
        ENDIF
     IF M > 1 AND M < 4 OR M > 6 AND M < 11 OR M > 14 THEN Hl(N) = L
ENDPROC
'------------------------------------------------------------------------------
PROC Move_objects_around
     M = 1 + RAND(2)
     IF HS(N) > 0 AND M = 1 
       OL(HS(N)) = Hl(N)
       HS(N) = 0
     ELSE
       IF HS(N) = 0 
         WHILE M < Total_Items AND NOT OL(M) = Hl(N)
           M = M + 1 + RAND(3)
         WEND
         IF OL(M) = Hl(N) AND OT(M) <> 23 AND OT(M) <> 24 AND OT(M) <> 26 
           OL(M) = 252
           HS(N) = M
         ENDIF
       ENDIF
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Heal_Computer_Controlled_Character
     L = Hl(N)
     M = 1 + RAND(2)
     WHILE M < Total_Computer_Characters
       IF Hl(M) = L AND HH(M) < 0 AND (Ht(M) < 12 OR Ht(M) > 14) 
         IF S23 > 0 THEN HH(M) = S23
         IF S23 < 0 THEN HH(M) = HO(M)
         Ht(M) = Ht(M) AND 127
         Hm(M) = Hm(M) AND 127
       ENDIF
       M = M + RAND(3)
     WEND
ENDPROC
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
PROC Place_Information    
        IF Current_Place < 128
            Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size * Current_Place,ADR(Show$(0)),Place_Field_Size,Place_BanK1)
        ELSE
            N = Current_Place & 127
            Z=USR(@FIELD_GET, Place_Description_Base+Place_Field_Size * N,ADR(Show$(0)),Place_Field_Size,Place_Bank2)
        ENDIF

''****    Show$(0) = RoomDescription$(Current_Place)
''****    RoomOtherEntranceName$ = REN$(Current_Place)
    CLS
    IF Show$(0)[1,1] = "(" 
''****      Show$(0) = Show$(0)[2, INSTR(1, Show$(0), ")"]
      EXEC Read_text_file
    ENDIF
''****    I = INSTR(1, Show$(0), "|")
''****     IF I > 0 
''****       F$ = Show$(0)[I + 1]
''****      Show$(0) = Show$(0)[1, I - 1]
''****      LOADPICTURE F$
''****     ENDIF
''****    I = INSTR(1, Show$(0), "{")
''****     IF I > 0 AND (S0 = 1 OR S0 = 2) 
''****      IF INSTR(1, Show$(0), "}") > 0 
''****         J = INSTR(1, Show$(0), "}") - I - 1
''****      ENDIF
      
''****       EXIT_NAME$ = Show$(0)[I + 1, J]
''****      Show$(0) = Show$(0)[1, I - 1]
''****      IF INSTR(1, EXIT_NAME$, ",") > 0 
''****        J = INSTR(1, EXIT_NAME$, ",")
''****        EXIT_ADJECTIVE$ = EXIT_NAME$[J + 1]
''****        EXIT_NAME$ = EXIT_NAME$[J- 1]
''****      ENDIF
''****     ENDIF
    IF S0 = 0 OR RT(Current_Place) = 8 OR RT(Current_Place) = 2 
      Text_Justify = 1
      EXEC Display
      Show$(0) = ""
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Read_text_file
''***    ON ERROR GOTO 314
    P$ = a$
''****    OPEN P$ FOR INPUT AS #1
''****    INPUT #1, a
''****    a$ = INPUT$(a, 1)
''****    CLOSE #1
ENDPROC
'------------------------------------------------------------------------------
PROC New_place  
        IF RT(Current_Place) <> 4 OR L > 0 
      ''****     IF RT(Current_Place) = 14 AND a$ <> "L" THEN EXEC Lock_exits
          EXEC Place_Information
          IF S0 = 1 OR S0 = 2 THEN Show$(0) =+ " "
          IF RT(Current_Place) <> 2 
            Exit_Count = 0  
            B$ = ""
            IF Rn(Current_Place) > 0 AND Rn(Current_Place) <> 4 
              B$ =+ "North"
              Exit_Count = Exit_Count + 1
              IF Rn(Current_Place) = 3 OR Rn(Current_Place) = 5 THEN B$ =+ "(locked)"
              IF Rn(Current_Place) = 2 THEN B$ =+ "(unlocked)"
            ENDIF
            IF (Re(Current_Place) > 0 AND Re(Current_Place) <> 4) AND Exit_Count > 0 
              B$ =+ ", "
              T2 = LEN(B$)
            ENDIF
            IF Re(Current_Place) > 0 AND Re(Current_Place) <> 4 
              B$ =+ "East"
              Exit_Count = Exit_Count + 1
              IF Re(Current_Place) = 3 OR Re(Current_Place) = 5 THEN B$ =+ "(locked)"
              IF Re(Current_Place) = 2 THEN B$ =+ "(unlocked)"
            ENDIF
            IF (Rw(Current_Place) > 0 AND Rw(Current_Place) <> 4) AND Exit_Count 
              B$ =+ ", "
              T2 = LEN(B$)
            ENDIF
            IF Rw(Current_Place) > 0 AND Rw(Current_Place) <> 4 
              B$ =+ "West"
              Exit_Count = Exit_Count + 1
              IF Rw(Current_Place) = 3 OR Rw(Current_Place) = 5 THEN B$ =+ "(locked)"
              IF Rw(Current_Place) = 2 THEN B$ =+ "(unlocked)"
            ENDIF
            IF (Rs(Current_Place) > 0 AND Rs(Current_Place) <> 4) AND Exit_Count 
              B$ =+ ", "
              T2 = LEN(B$)
            ENDIF
            IF Rs(Current_Place) > 0 AND Rs(Current_Place) <> 4 
              B$ =+ "South"
              Exit_Count = Exit_Count + 1
              IF Rs(Current_Place) = 3 OR Rs(Current_Place) = 5 THEN B$ =+ "(locked)"
              IF Rs(Current_Place) = 2 THEN B$ =+ "(unlocked)"
            ENDIF
            IF (Ru(Current_Place) > 0 AND Ru(Current_Place) <> 4) AND Exit_Count 
              B$ =+ ", "
              T2 = LEN(B$)
            ENDIF
            IF Ru(Current_Place) > 0 AND Ru(Current_Place) <> 4 
              B$ =+ "Up"
              Exit_Count = Exit_Count + 1
            ENDIF
            IF (Rd(Current_Place) > 0 AND Rd(Current_Place) <> 3 AND Rd(Current_Place) <> 4) AND Exit_Count 
              B$ =+ ", "
              T2 = LEN(B$)
            ENDIF
            IF Rd(Current_Place) > 0 AND Rd(Current_Place) <> 4 AND Rd(Current_Place) <> 3 
              B$ =+ "Down"
              Exit_Count = Exit_Count + 1
            ENDIF
            IF Ro(Current_Place) > 0 AND Ro(Current_Place) < 6 
              '** RoomOtherEntranceName$ = REN$(Current_Place)
              B$ =+ ", "
              T2 = LEN(B$)
              IF S0 = 1 OR S0 = 2 
                B$ =+ "There is a"
                '** CV=ASC(RoomOtherEntranceName$[1,1])
                IF CV > 96 AND CV<124 THEN CV = CV - 64
                P$=CHR$(CV)
                Z=USR(@CHANGE_CASE,0,ADR(P$))
                Exit_Count = Exit_Count + 1
                IF CV = 65 OR CV = 69 OR CV = 73 OR CV = 79 OR CV = 85 THEN B$ =+ "n"
              ENDIF
              T2 = LEN(B$)
              B$ =+ " "
              '** B$=+RoomOtherEntranceName$
              Z=USR(@FIELD_GET, Place_Exit_Base + 16 * Current_Place,ADR(B$),16,Name_Bank)
              IF Ro(Current_Place) = 5 THEN B$ =+ "(locked)"
            ELSE
              IF Ro(Current_Place) = 4 THEN B$ =+ "(blocked)"
            ENDIF
            B$ =+ ". "
            IF Exit_Count > 1 
              IF S0 = 0 
                Show$(0) = "EXITS:"
              ELSE
                Show$(0)=+"There are exits going "
                'B$ = LCASE$(B$)         
              ENDIF
              IF Exit_Count >= 3 
                P$ = B$[1, T2 - 1]:P$=+" and":P$=+B$[T2]
              ELIF Exit_Count = 2
                P$ = B$[1, T2 - 2]:P$=+ " and":P$=+ B$[T2]
              ENDIF
              Show$(0) =+P$
            ELIF Exit_Count = 1 
              IF S0 = 0 
                 Show$(0) = "EXIT:"
              ELSE
                 Show$(0)=+ "There is an exit going "
              ENDIF
              Show$(0) =+ B$
            ENDIF
            IF S0 = 0 OR S0 = 1 
              Text_Justify = 1
              EXEC Display
              Show$(0)=""
            ENDIF
            IF RT(Current_Place) <> 7 
              B$ = ""
              Exit_Count = 0
              T2 = 0
              FOR N = 1 TO Total_Items
                IF OL(N) = Current_Place AND Exit_Count 
                  B$ =+ ", "
                  T2 = LEN(B$)
                ENDIF
                IF OL(N) = Current_Place 
                  Exit_Count = Exit_Count + 1
                  Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
                  Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
                  Z=USR(@TRIM,ADR(Show$(3)))                                                      
                  B$ =+Show$(3)
                ENDIF
              NEXT N
              IF Ro(Current_Place) = 7 
                IF Exit_Count THEN B$ =+ ", "
                T2 = LEN(B$)
                '** B$ =+ RoomOtherEntranceName$
                Exit_Count = Exit_Count + 1
              ENDIF
              IF Exit_Count > 0 
                'B$ =+ "_ "
                P$ = B$
                IF Exit_Count = 2
                    P$ = B$[1, T2 - 2]
                    P$ =+ " and"
                    P$ =+ B$[T2]
                ENDIF
                IF Exit_Count > 2
                    P$ = B$[1, T2 - 1]
                    P$ =+ " and"
                    P$ =+ B$[T2]
                ENDIF
                IF S0 = 0 OR S0 = 1 
                  Show$(0) = "ITEM"
                  IF Exit_Count > 1 THEN Show$(0) =+ "S"
                  Show$(0) =+ ":"
                  Show$(0) =+ P$
                ELIF S0 = 2 
                  '***  IF UCASE$(RIGHT$(B$, 1)) = "S" AND Exit_Count = 1 THEN Exit_Count = 99
                  IF Exit_Count < 3 
                    Show$(0) =+ "There is "
                    CV=ASC(P$[1,1])
                    IF CV > 96 AND CV<124 THEN CV = CV - 64             
                    IF CV = 65 OR CV = 69 OR CV = 73 OR CV = 79 OR CV = 85  
                      Show$(0) =+ "an "
                    ELSE
                      Show$(0) =+ "a "
                    ENDIF
                    Show$(0) =+ P$
                    Show$(0) =+ " in here. "
                  ELSE
                    Show$(0) =+ "There are "
                    Show$(0) =+ P$
                    Show$(0) =+ " in here. "
                  ENDIF
                ENDIF
              ENDIF
              IF Show$(0) <> "" OR S0 = 2 
                Text_Justify = 1
                EXEC Display
                Show$(0) = ""
             ENDIF
            ELSE
              Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
              Z=USR(@CHANGE_CASE,0,ADR(P$))
              PRINT "ITEM"; TAB(30); P$
              FOR N = 1 TO Total_Places
                IF OL(N) = Current_Place 
                    Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
                    Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
                    PRINT Show$(3); TAB(30); OP(N)
                ENDIF
              NEXT N
            ENDIF
          ELIF RT(Current_Place) = 2 
            PlayerLifeStatus = 1
          ENDIF
        ELSE
          CLS
          PRINT "Too dark, can not see."
        ENDIF
        IF RT(Current_Place) = 3 
          PlayerHitPoints = PlayerHitPoints - S17
          EXEC Get_Hitpoint_Name
          PRINT "You lost "; S17; P$
          IF PlayerHitPoints < 0 
            PlayerHitPoints = 0
            PlayerLifeStatus = 1
            PRINT "Died because did not have enough "; P$
          ENDIF
        ENDIF
        IF RT(Current_Place) = 5 
          '*** EXEC Find_survival_equipment
        ENDIF
        IF RT(Current_Place) = 9 
          C = C - S24
          IF C < 0 
            C = 0
          ENDIF
        ENDIF
        IF RT(Current_Place) = 15 OR (RT(Current_Place) = 10 AND S16 = 3) THEN S14 = Current_Place
ENDPROC
'------------------------------------------------------------------------------
PROC Go_Direction
    B$ = "Can not go that way, it is locked. "
    P$ = "Can not go that way, need password. "
    IF a$ = "N" 
      N = Rn(Current_Place)
      M = Rnl(Current_Place)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "E" 
      N = Re(Current_Place)
      M = Rel(Current_Place)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "W" 
      N = Rw(Current_Place)
      M = Rwl(Current_Place)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "S" 
      N = Rs(Current_Place)
      M = Rsl(Current_Place)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "D" 
      IF Rd(Current_Place) = 1 OR Rd(Current_Place) = 4 
        Current_Place = Rdl(Current_Place)
      ENDIF
      IF Rd(Current_Place) = 2 
        M = Rdl(Current_Place)
        EXEC Ok_to_climb
      ENDIF
    ENDIF
    IF a$ = "U" 
      IF Ru(Current_Place) = 1 OR Ru(Current_Place) = 4 
        Current_Place = Rul(Current_Place)
      ENDIF
      IF Ru(Current_Place) = 2 
        M = Rul(Current_Place)
        EXEC Ok_to_climb
      ENDIF
    ENDIF
    IF a$ = "I" 
      EXEC Examine_Inventory
    ENDIF
    IF Ht(H) = 7 OR Ht(H) = 14 OR (RT(Current_Place) = 6 AND H > 0) 
      IF PriorRoom > 0 THEN Current_Place = PriorRoom
      PRINT "The "; ComputerCharacterName$(H); " prevents you from exiting."
    ENDIF
    IF A$ = "L" 
      PriorRoom = 0
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Ok_to_move
    IF N = 3 
      PRINT B$
    ENDIF
    IF N = 5 
      PRINT P$
    ENDIF
    IF N = 1 OR N = 2 OR N = 4 
      Current_Place = M
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Ok_to_climb
    FOR N = 1 TO Total_Places
      IF OT(N) = 11 AND OL(N) = 0 THEN Current_Place = M
    NEXT N
    IF PriorRoom = Current_Place 
      PRINT "Need something to climb with."
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Examine_Inventory
    Show$(0) = "Inventory:"
    t1 = 0
    FOR N = 1 TO Total_Items
      IF OL(N) = 0 
        IF t1>0 then Show$(0) =+ ", "
        Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
        Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
        Z=USR(@TRIM,ADR(Show$(3)))                
        Show$(0) =+Show$(3)
        t1 = 1
      ENDIF
    NEXT N
    Show$(0) =+ ". "
    EXEC Display
ENDPROC
'------------------------------------------------------------------------------
PROC Enter_command
    EXEC Show_Status
    REPEAT
      PRINT
      PRINT "Enter command."
      INPUT ">"; a$
    UNTIL a$ <> ""
    IF a$ = "*" 
      a$ = PriorCommand$
      PRINT CHR$(28);">";PriorCommand$
    ELSE
      '***  a$ = UCASE$(a$)
    ENDIF
    EXEC Break_down
ENDPROC
'------------------------------------------------------------------------------
PROC Break_down
    Z=USR(@CHANGE_CASE,0,ADR(a$))
    PriorCommand$ = a$
    N = len(a$)
    IF a$[N, 1] = "." OR a$[N, 1] = "!" 
      a$ = a$[1, LEN(a$) - 1]
    ENDIF
    Show$(3) = a$
    Seek$ = "THE"
    Z=USR(@REMOVE_SUBSTRING,ADR(A$),ADR(SEEK$))
    Seek$ = "TO"
    Z=USR(@REMOVE_SUBSTRING,ADR(A$),ADR(SEEK$))                       
    EXEC SEPERATE_VERB_NOUN
    IF Verb$ = "JUMP" OR Verb$ = "LEAP" OR Verb$ = "CLIMB" OR Verb$ = "JOG" 
      a$ = P$
      EXEC SEPERATE_VERB_NOUN
    ENDIF
    IF Verb$ = "GO" OR Verb$ = "RUN" OR Verb$ = "WALK" OR Verb$ = "TURN" 
      a$ = P$
      EXEC SEPERATE_VERB_NOUN
    ENDIF
    PriorCommand$ = a$
    IF Verb$ = "SOUTH" THEN a$ = "S"
    IF Verb$ = "NORTH" THEN a$ = "N"
    IF Verb$ = "EAST" THEN a$ = "E"
    IF Verb$ = "WEST" THEN a$ = "W"
    IF Verb$ = "UP" THEN a$ = "U"
    IF Verb$ = "DOWN" THEN a$ = "D"
    IF Verb$ = "LOOK" OR a$ = "GAZE" OR a$[1, 3] = "SEE" OR a$ = "GLANCE" OR a$[1, 4] = "VIEW" THEN a$ = "L"
    IF Verb$ = "INVENTORY" OR a$ = "INV" THEN a$ = "I"
    IF Verb$ = "KICK" THEN a$ = "K2"
    IF Verb$ = "PUNCH" OR Verb$ = "HIT" OR Verb$ = "SLAP" THEN a$ = "K3"
    IF Verb$ = "EXAMINE" OR Verb$ = "READ" OR Verb$ = "LOOK" OR Verb$ = "CHECK" 
      IF P$[1, 3] = " AT" 
        P$ = P$[4]
      ENDIF
      IF P$ <> "" 
        a$ = "X"
        a$ =+ P$
      ELSE
        a$ = "L"
      ENDIF
    ENDIF    
    IF Verb$ = "DIG" OR Verb$ = "SHOVEL" OR Verb$ = "UNCOVER" THEN a$ = "DIG"
    IF Verb$ = "SHOUT" OR Verb$ = "SAY" OR Verb$ = "CAST" 
        a$ = "S"
        A$ =+ P$
    ENDIF
    IF Verb$ = "MAKE" OR Verb$ = "ASSEMBLE" OR Verb$ = "BUILD" 
        a$ = "M"
        A$ =+ P$
    ENDIF
    IF Verb$ = "GET" OR Verb$ = "BUY" OR Verb$ = "TAKE" OR Verb$ = "STEEL" OR Verb$ = "GRAB" OR Verb$ = "OBTAIN" OR Verb$ = "POSSESS" 
      a$ = "G"
      a$ =+ P$
    ENDIF
    IF Verb$ = "SELL" OR Verb$ = "DROP" OR Verb$ = "LEAVE" 
        a$ = "D"
        a$ =+ P$
    ENDIF
    IF Verb$ = "ENTER" OR Verb$ = "CROSS" OR Verb$ = "IN" 
        a$ = "R"
        a$ =+ P$
    ENDIF        
    IF Verb$ = "USE" OR Verb$ = "ACTIVATE" OR Verb$ = "DETONATE" 
        a$ = "U"
        a$ =+ P$
    ENDIF        
    IF Verb$ = "EAT" OR Verb$ = "SWALLOW" OR Verb$ = "DRINK" OR Verb$ = "BITE" OR Verb$ = "CONSUME" 
        a$ = "E"
        a$ =+ P$
    ENDIF
    IF Verb$ = "ZAP" OR Verb$ = "TELEPORT" OR B$ = "TRANSPORT" OR B$ = "BEAM" 
        a$ = "T"
        a$ =+ P$
    ENDIF
    IF Verb$ = "SHOOT" OR Verb$ = "SWING" OR Verb$ = "POKE" OR Verb$ = "JAB" OR Verb$ = "STAB" 
      a$ = "K" 
      a$ =+ P$
    ENDIF
    IF Verb$ = "THROW" OR Verb$ = "HURL" OR Verb$ = "FLING" OR Verb$ = "PITCH" 
      a$ = "K1" 
      a$ =+ P$
    ENDIF
    IF Verb$ = "ON" OR Verb$ = "LIGHT" THEN a$ = "ON"
    IF Verb$ = "OFF" OR Verb$ = "UNLIGHT" THEN a$ = "OFF"
    IF Verb$ = "OPEN" OR Verb$ = "UNLOCK"  
        a$ = "O"
        A$ =+ P$
    ENDIF
    IF Verb$ = "LOCK" OR Verb$ = "CLOSE" 
        a$ = "C"
        a$ =+ P$
    ENDIF
    IF Verb$ = "PUSH" OR Verb$ = "PULL" OR Verb$ = "YANK" OR Verb$ = "MOVE" 
        a$ = "P"
        A$ =+ P$
    ENDIF
    IF Verb$ = "FRY" OR Verb$ = "BURN"
        a$ = "B"
        a$ =+ P$
    ENDIF
    IF Verb$ = "BREAK" OR Verb$ = "BUST" OR Verb$ = "HIT" OR Verb$ = "SLAM"
        a$ = "H"
        a$ =+ P$
    ENDIF    
ENDPROC
'------------------------------------------------------------------------------
PROC Show_Status 
   Show$(0) = "You "
   IF S12 >= 1 
     Show$(0) =+ "are on turn "
     Show$(0) =+ STR$(Turn_Number) 
     Show$(0) =+ " of " 
     Show$(0) =+ STR$(S12) 
     Show$(0) =+ ". "
   ENDIF
   B$ = ""
   T2 = 1
   LC = 1
   IF S15 > 0 
     B$ =+ STR$(LIVES)
     IF LIVES = 1 
       B$ =+ " life"
     ELSE
       B$ =+ " lives"
     ENDIF
     B$ =+ " left, "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S8 > 0 
     EXEC Get_Hitpoint_Name
     B$ =+ STR$(PlayerHitPoints)
     B$ =+ P$
     B$ =+ ", "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S22 > 0 
     Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
     Z=USR(@CHANGE_CASE,1,ADR(P$))
     B$ =+ " "
     B$ =+ STR$(C)
     B$ =+ P$
     B$ =+ ", "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S21 = 1 
     B$ =+ STR$(Armor_Points)
     B$ =+ " armor point"
     IF NOT Armor_Points = 1 THEN B$ =+ "s"
     LC = T2
     B$ =+ ", "
     T2 = LEN(B$)
   ENDIF
   IF B$ <> "" 
     IF LC > 1 
       P$ = B$[1, LC]
       P$ =+ "and"
       P$ =+ B$[LC]
       B$ = P$[1,T2+2]
       B$ =+ "."
     ENDIF
     Show$(0) =+ "You have "
     Show$(0) =+ B$
   ENDIF
   IF S13 > 0 AND PlayerFoodWaterNeed < 13 
     Show$(0) =+ "You need food or water, have "
     Show$(0) =+ STR$(PlayerFoodWaterNeed)
     Show$(0) =+ " turns left. "
   ENDIF
   IF LEN(Show$(0)) > 9 
     Text_Justify = 1
     EXEC Display
   ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC SEPERATE_VERB_NOUN
  N = 0
  L = LEN(a$)
  Repeat
    N = N + 1
  Until a$[N,1]=" " OR N >= L
  IF N >= 2 AND N < L 
      Verb$ = a$[1, N - 1]
      P$ = a$[N]
  ELSE
      IF N = 1 AND L >=3  
        P$ = A$[2]
        A$ = P$
      ENDIF
      Verb$ = a$
      P$ = ""
  ENDIF
ENDPROC
'------------------------------------------------------------------------------
' *************************** Player Actions Section **************************
'------------------------------------------------------------------------------
PROC Perform_action    
    IF RT(Current_Place) = 12 THEN EXEC Correct_action
    P$ = a$[1, 2]
    Z=USR(@CHANGE_CASE,0,ADR(a$))
    Seek$ = a$[3, LEN(a$)]
    IF P$ = "G " THEN EXEC Get_Objects
    IF P$ = "D " THEN EXEC Drop_Objects
    IF P$ = "X " THEN EXEC Examine_object
    IF P$ = "E " THEN EXEC Consume_object
    IF a$ = "ON" THEN EXEC Light_object
    IF P$ = "P " THEN EXEC Push_Item
    IF P$ = "O " THEN EXEC Open_Item
    IF P$ = "C " THEN EXEC Close_Exit
    IF a$ = "DIG" THEN EXEC Dig_Up_Item
    IF P$ = "B " THEN EXEC Burn_object
    IF P$ = "R " THEN EXEC Enter_other_exit
    IF P$ = "H " THEN EXEC Break_object
    IF P$ = "U " THEN EXEC Blow_open_exit
    IF P$ = "K " OR P$ = "K1" OR P$ = "K2" OR P$ = "K3" THEN EXEC Fight_Computer_Controlled_Character
    IF P$ = "S " THEN EXEC Check_password
    IF P$ = "T " THEN EXEC Transport
    IF P$ = "M " THEN EXEC Search_For_Components
    IF a$ = "OFF" AND L = 1 
        L = 0
        PriorRoom = 0
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Correct_action
     '** IF INSTR(1, RoomOtherEntranceName$, PC$) OR INSTR(1, PC$, RoomOtherEntranceName$) 
       Current_Place = Ro(Current_Place)
     '** ELSE
       Current_Place = Rol(Current_Place)
     '** ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Get_Objects
    Show$(0) = ""
    IF Items_Carrying < Inventory_Limit AND Ht(H) <> 8     
        EXEC Find_Item_Number
        IF OL(N) = Current_Place AND found_loc >= 1 AND N >= 0 
            IF RT(Current_Place) <> 7 OR (RT(Current_Place) = 7 AND C > OP(N)) 
            Show$(0) = "Ok,"
              IF RT(Current_Place) = 7 
                Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
                Z=USR(@CHANGE_CASE,1,ADR(P$))
                Show$(0) =+ " brought it for " 
                Show$(0) =+ STR$(OP(N)) 
                Show$(0) =+ " " 
                Show$(0) =+ P$
                C = C - OP(N)
                OL(N) = 0
              ELSE
                Show$(0) =+ " got it."
              ENDIF
              IF NOT (OT(N) = 18 OR OT(N) = 20 OR OT(N) = 23 OR OT(N) = 24 OR OT(N) = 26 OR OT(N) = 28) 
                OL(N) = 0
                IF S21 = 0 THEN Items_Carrying = Items_Carrying + 1
                '***   IF OT(N) = 6 THEN EXEC Measure_armor
              ELSE
                IF OT(N) = 18 
                  Show$(0) =+ "Now have more turns."
                  OL(N) = 255
                  S12 = S12 + OV(N)
                ENDIF
                IF OT(N) = 20 
                  Show$(0) =+ "Gained " 
                  Show$(0) =+ STR$(OV(N)) 
                  Show$(0) =+ " lives."
                  OL(N) = 255
                  LIVES = LIVES + OV(N)
                ENDIF
                IF OT(N) = 22 
                  Show$(0) =+ "Can carry " 
                  Show$(0) =+ STR$(OV(N)) 
                  Show$(0) =+ " more objects."
                  Inventory_Limit = Inventory_Limit + OV(N)
                  OL(N) = 255
                ENDIF
                IF OT(N) = 23 OR OT(N) = 24 OR OT(N) = 26 
                  Show$(0) = "Can't get it, too big and heavy."
                  OL(N) = Current_Place
                ENDIF
                IF OT(N) = 28 
                  '***   EXEC Collect_with_container
                ENDIF
              ENDIF
            ELSE
              Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
              Z=USR(@CHANGE_CASE,1,ADR(P$))
              Show$(0) = "Do not have enougth " 
              Show$(0) =+ P$
              Show$(0) =+ ". "
            ENDIF
        ENDIF
        Text_Justify = 1
    ENDIF
    IF Ht(H) = 8 
      Show$(0) = "The " 
      Show$(0) =+ ComputerCharacterName$(H) 
      Show$(0) =+ " stops you from getting anything."
    ENDIF
    IF Items_Carrying >= Inventory_Limit 
      Show$(0) =+ "Can't carry anymore objects."
    ENDIF
    Text_Justify = 1
    EXEC Display
ENDPROC
'------------------------------------------------------------------------------
PROC Drop_Objects
      EXEC Find_Item_Number  
      IF OL(N) = 0 AND found_loc >= 1 
            OL(N) = Current_Place
            IF RT(Current_Place) <> 7 
                PRINT "Ok, dropping "; Show$(3); ". "
                Items_Carrying = Items_Carrying - 1
            ELSE
                Z=USR(@FIELD_GET,Monetary_Unit_Store,ADR(P$),12,Info_Bank)
                Z=USR(@CHANGE_CASE,1,ADR(P$))
                IF S27 = 0 THEN M = OP(N)
                IF S27 = 1 THEN M = INT((OP(N) * .9) / 5) * 5
                PRINT "Ok, sold "; Show$(3); " for "; M; " "; P$
                Items_Carrying = Items_Carrying - 1
                C = C + M
            ENDIF
            IF OT(N) = 14 OR OT(N) = 27 
                L = 0
            ENDIF
      ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Examine_Object
    EXEC Find_Item_Number
    IF found_loc >= 1 AND N >=1
        IF OL(N) = 0  
            IF Show$(3)[1,1]= "("
              Seek$ = ")"
              found_loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))    
              Show$(3) = Show$(3)[2, found_loc - 3]
              EXEC Read_text_file
            ENDIF
            IF ot(N) = 34 THEN EXEC Read_text_file
            '*** IF ot(N) = 41 THEN EXEC Show_screen_file
            ADJ = 2
            EXEC Display
        ELSE 
            PRINT "Object not in inventory."
        ENDIF
    ENDIF    
ENDPROC
'------------------------------------------------------------------------------
PROC Consume_Object
      EXEC Find_Item_Number     
      IF OL(N) = 0 AND found_loc >= 1 AND N >= 1
        IF ot(N) = 5 
          Show$(0) = "That's better, now have "
          Show$(0) =+ STR$(OV(N)) 
          Show$(0) =+ " more turns before getting more food or water."
          PlayerFoodWaterNeed = PlayerFoodWaterNeed + OV(N)
        ENDIF
        IF ot(N) = 17 
          IF OV(N) = 2 
            EXEC Get_Hitpoint_Name
            Show$(0) = "Lost " 
            Show$(0) =+ STR$(OV2(N)) 
            Show$(0) =+ " " 
            Show$(0) =+ P$
            Show$(0) =+ " because it was poisoned."
            PlayerHitPoints = PlayerHitPoints - OV2(N)
            OL(N) = 255
          ENDIF
          IF OV(N) = 1 
            Z=USR(@CHANGE_CASE,2,ADR(Show$(3)))
            Show$(0) = "The " 
            Show$(0) =+ Show$(3) 
            Show$(0) =+ " was a lethal poison, you are Dead."
            PlayerHitPoints = -1
            OL(N) = 255
            PlayerLifeStatus = 1
          ENDIF
          IF OV(N) = 3 
            Show$(0) = "Member was poisoned, must find an antidote or will die."
            OL(N) = 255
            PlayerPoisonStatus = OV2(N)
          ENDIF
        ENDIF
        IF ot(N) = 16           
          PlayerHitPoints = PlayerHitPoints + OV(N)
          EXEC Get_Hitpoint_Name
          Show$(0) = "That's better, now have " 
          Show$(0) =+ STR$(PlayerHitPoints) 
          Show$(0) =+ " " 
          Show$(0) =+ P$
          Show$(0) =+ ". "
          OL(N) = 255
        ENDIF
        IF ot(N) = 28 
          PlayerImmunityTurns = PlayerImmunityTurns + (OV(N))
          Show$(0) = "Have PlayerImmunityTurnsune power for " 
          Show$(0) =+ STR$(PlayerImmunityTurns) 
          Show$(0) =+ " turns."
          OL(N) = 255
        ENDIF
        IF ot(N) = 31 AND PlayerInfectionStatus > 0 
          OL(N) = 255
          Show$(0) = "It cured the PlayerInfectionStatusection."
          PlayerInfectionStatus = 0
        ENDIF
        IF ot(N) = 32 AND PlayerPoisonStatus > 0 
          OL(N) = 255
          Show$(0) = "It stopped the poison."
          PlayerPoisonStatus = 0
        ENDIF
        IF (ot(N) = 16 OR ot(N) = 17 OR ot(N) = 28 OR ot(N) = 31 OR ot(N) = 32) AND OL(N) = -1 
          ADJ = 1
          EXEC Display
          seek$ = " OF"
          Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
          IF Found_Loc >= 1  
            B$ = Show$(3)[1, found_loc]
            Show$(3) = "EMPTY "
            Show$(3) =+ B$
            Z=USR(@CHANGE_CASE,0,ADR(Show$(3)))
            OL(N) = 0
            ot(N) = 28
            Z=USR(@FIELD_PUT,Item_Name_Base + ItemFieldSize * N,ADR(SHOW$(3)),ItemFieldSize ,LEN(SHOW$(3)),Name_Bank)
          ENDIF
        ENDIF
        IF OL(N) = -1 
          Oc = Oc - 1
        ENDIF
      ENDIF
    
ENDPROC
'------------------------------------------------------------------------------
PROC Light_object
     FOR N = 1 TO Total_Items
       IF OL(N) = 0 
         IF ot(N) = 13 
           Lt = Lt + OV(N)
           PriorRoom = 0
           L = 1
         ENDIF
         IF ot(N) = 14 
           Lt = Lt + OV(N)
           OL(N) = 255
           Oc = Oc - 1
         ENDIF
       ENDIF
     NEXT N
     PriorRoom = 0
ENDPROC
'------------------------------------------------------------------------------
  PROC Push_Item
    EXEC Find_Item_Number
    IF OL(N) = Current_Place AND Ot(n) = 23 AND found_loc >0 AND N >= 1 
       IF OV(N) > 0
         EXEC Reveal
       ENDIF
    ELSE
       PRINT "Can't push it."
    ENDIF
  ENDPROC
'------------------------------------------------------------------------------
  PROC Open_Item
     IF Rn(Current_Place) = 3 AND OL(Rnk(Current_Place)) = 0 
       Rn(Current_Place) = 2
       PriorRoom = 0
     ENDIF
     IF Re(Current_Place) = 3 AND OL(Rek(Current_Place)) = 0 
       Re(Current_Place) = 2
       PriorRoom = 0
     ENDIF
     IF Rw(Current_Place) = 3 AND OL(Rwk(Current_Place)) = 0 
       Rw(Current_Place) = 2
       PriorRoom = 0
     ENDIF
     IF Rs(Current_Place) = 3 AND OL(Rsk(Current_Place)) = 0 
       Rs(Current_Place) = 2
       PriorRoom = 0
     ENDIF
     IF Rn(Current_Place) = 3 AND OL(Rnk(Current_Place)) >0 AND PriorRoom = Current_Place 
       N = Rnk(Current_Place)
       EXEC Cannot_Open_Exit
     ENDIF
     IF Re(Current_Place) = 3 AND OL(Rek(Current_Place)) >0 AND PriorRoom = Current_Place 
       N = Rek(Current_Place)
       EXEC Cannot_Open_Exit
     ENDIF
     IF Rw(Current_Place) = 3 AND OL(Rwk(Current_Place)) >0 AND PriorRoom = Current_Place
        N = Rwk(Current_Place)
        EXEC Cannot_Open_Exit 
     ENDIF
     IF Rs(Current_Place) = 3 AND OL(Rsk(Current_Place)) >0 AND PriorRoom = Current_Place
        N = Rsk(Current_Place)
        EXEC Cannot_Open_Exit
     ENDIF
     IF PriorRoom = Current_Place 
         EXEC Find_Item_Number
         IF OL(N) = 0 AND found_loc > 0 AND N > 0 
               IF Ot(n) = 19 
                     IF OV2(N) = 0 OR OL(OV2(N)) = 0 
                        Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * OV(N), ADR(Show$(3)), ItemFieldSize, Name_Bank)
                        Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
                        Z=USR(@TRIM,ADR(Show$(3)))
                        PRINT "Ok, found "; Show$(3); ". "
                       OL(OV(N)) = 0
                     ENDIF
                     IF OV2(N) > 0 AND OL(OV2(N)) > 0 
                        Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * OV2(N), ADR(Show$(3)), ItemFieldSize, Name_Bank)
                        Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
                        Z=USR(@TRIM,ADR(Show$(3)))
                       PRINT "Cannot open it, need "; Show$(3); ". "
                     ENDIF
               ENDIF
               IF Ot(n) = 22 
                     M = 0
                     IF OV(N) > 99 
                       M = INT(OV(N) / 100)
                       OV(N) = OV(N) - M * 100
                     ENDIF
                     IF M = 0 OR OL(M) = 0 
                       EXEC Reveal
                     ENDIF
               ENDIF
               IF Ot(n) <> 19 AND Ot(n) <> 22 
                  PRINT "It will not open."
               ENDIF
         ENDIF
     ENDIF
  ENDPROC
'------------------------------------------------------------------------------
  PROC Cannot_Open_Exit
       Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
       Z=USR(@CHANGE_CASE,2,ADR(Show$(3)))
       Z=USR(@TRIM,ADR(Show$(3)))
       PRINT "Can't open, need ";Show$(3);"."
  ENDPROC
'------------------------------------------------------------------------------
PROC Close_Exit
     IF Rn(Current_Place) = 2 AND OL(Rnk(Current_Place)) = 0 
       Rn(Current_Place) = 3
       PriorRoom = 0
     ENDIF
     IF Re(Current_Place) = 2 AND OL(Rek(Current_Place)) = 0 
       Re(Current_Place) = 3
       PriorRoom = 0
     ENDIF
     IF Rw(Current_Place) = 2 AND OL(Rwk(Current_Place)) = 0 
       Rw(Current_Place) = 3
       PriorRoom = 0
     ENDIF
     IF Rs(Current_Place) = 2 AND OL(Rsk(Current_Place)) = 0 
       Rs(Current_Place) = 3
       PriorRoom = 0
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Dig_Up_Item
     FOR N = 1 TO Total_Items
       IF Ot(n) = 10 AND OL(N) = 0 
         IF Rd(Current_Place) = 3 
           Rd(Current_Place) = 1
           PriorRoom = 0
         ENDIF
         IF Ro(Current_Place) = 2 
           Ro(Current_Place) = 1
           PriorRoom = 0
         ENDIF
       ENDIF
     NEXT N
ENDPROC
'------------------------------------------------------------------------------
PROC Burn_object  
     IF L > 0 AND Lt > 0 
       EXEC Find_Item_Number
       IF N >= 0 AND Ot(N) = 24 AND OL(N) = Current_Place AND found_loc > 0 
           IF OV(N) > 0 
             EXEC Reveal
           ENDIF
           OL(N) = 255
           IF PlayerHitPoints < 0 
             PlayerLifeStatus = 1
           ENDIF
         ENDIF
     ELSE
       PRINT "Need something to burn it with."
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Find_Item_Number
      N = 0
      Found_Loc = 0
      REPEAT
        N = N + 1 
        Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
        Z=USR(@CHANGE_CASE,0,ADR(Show$(3)))
        Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
      UNTIL Found_Loc >= 1 or N > Total_Items
      IF N > Total_Items 
        N = 0
        found_loc = 0
      ENDIF
ENDPROC           
'------------------------------------------------------------------------------
PROC Enter_other_exit
     Z=USR(@FIELD_GET, Place_Exit_Base + 16 * PLACE_INDEX,ADR(SHOW$(3)),16,Name_Bank)
     Z=USR(@CHANGE_CASE,0,ADR(Show$(3)))
     Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
     IF Found_Loc >= 0
       IF Ro(Current_Place) = 1 OR Ro(Current_Place) = 7 
         Current_Place = Rol(Current_Place)
       ENDIF
       IF Ro(Current_Place) = 4 
         PRINT "Can't go that way, it is blocked."
       ENDIF
       IF Ro(Current_Place) = 8 
         PRINT "Can't go that way yet."
       ENDIF
       IF Ro(Current_Place) = 3 
         M = Rol(Current_Place)
         EXEC Ok_to_climb
       ENDIF
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Break_object
     M = 1
     WHILE M < Total_Items AND OT(M) <> 27
       M = M + 1
     WEND
     IF OT(M) = 27 
       FOR N = 1 TO Total_Items
         IF OL(N) = Current_Place AND Ot(N) = 26 
           IF OV(N) > 1 
             EXEC Reveal
           ENDIF
         ELSE
           IF Ot(N) = 25 AND OL(N) = 0 
             OL(OV(N)) = 0
             OL(N) = 255
           ENDIF
         ENDIF
       NEXT N
     ELSE
       PRINT "Need something to break it."
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Blow_open_exit
    EXEC Find_Item_Number
    IF OL(N) = 0 AND (Ot(N) = 7 OR Ot(N) = 8) AND Found_Loc >=1 
          IF (OL(OV(N)) = 0 OR OV(N) = 0) AND Ro(Current_Place) = 4 
             Ro(Current_Place) = 1
             PriorRoom = 0
           ENDIF
    ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Check_password
     PW = 0
     PRINT CHR$(34); Seek$; CHR$(34)
     FOR N = 0 TO Total_Spells
       Z=USR(@FIELD_GET, Spell_Name_Base + 16 * N,ADR(SHOW$(3)),16,Name_Bank)
       Z=USR(@CHANGE_CASE,0,ADR(SHOW$(3)))
       Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
       IF found_loc >= 1 
         PW = N + 1
       ENDIF
     NEXT N
     IF Rn(Current_Place) = 5 AND PW = Rnk(Current_Place) 
       Rn(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF Rw(Current_Place) = 5 AND PW = Rwk(Current_Place) 
       Rw(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF Re(Current_Place) = 5 AND PW = Rek(Current_Place) 
       Re(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF Rs(Current_Place) = 5 AND PW = Rsk(Current_Place) 
       Rs(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF Ht(H) = 13 AND PW = HH(H) 
     
       '***  PRINT "You defeated the "; ComputerCharacterName$(H); ". "
       HH(H) = -1
     ENDIF
     IF Ht(H) = 14 AND PW = HH(H)
      
       '***  PRINT "You got rid of the "; ComputerCharacterName$(H); ". "
       N = H
       WHILE Hl(N) = Current_Place
       '***  EXEC Random_place
       WEND
     ENDIF
     IF Ro(Current_Place) = 5 AND OV3(Current_Place) = PW 
       Ro(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF Ro(Current_Place) = 6 AND OV3(Current_Place) = PW 
       Ro(Current_Place) = 1
       PriorRoom = 0
     ENDIF
     IF RT(Current_Place) = 11 AND Ro(Current_Place) = PW 
       Current_Place = Rol(Current_Place)
     ENDIF
     FOR N = 1 TO Total_Items
       IF Ot(N) > 34 AND Ot(N) < 47 AND OV2(N) = PW AND OL(N) = 0 
         IF Ot(N) = 35 
           OL(N) = -1
           PlayerHitPoints = PlayerHitPoints + OV(N)
         ENDIF
         IF Ot(N) = 36 
           Current_Place = OV(N)
         ENDIF
         IF Ot(N) = 38 
           Lt = Lt + OV(N)
           OV(N) = 0
           EXEC Light_object
           L = 1
         ENDIF
         IF Ot(N) = 39 
           '***  EXEC Kill_Computer_Controlled_Characters
         ENDIF
         IF Ot(N) = 40  
            '***  EXEC Entrap_Computer_Controlled_Characters
         ENDIF
         IF Ot(N) = 42 
           EXEC Search_For_Components
           OL(N) = -1
         ENDIF
       ENDIF
       IF Ot(N) = 37 
         M = 0
         IF OV(N) > 99 
           M = INT(OV(N) / 100)
           OV(N) = OV(N) - M * 100
         ENDIF
         IF PW = M 
           EXEC Reveal
         ENDIF
       ENDIF
     NEXT N
ENDPROC
'------------------------------------------------------------------------------
PROC Search_For_Components
     B$ = ""
     M = 1
     J = -1
     t1 = 1
     FOR N = 1 TO Total_Items
       IF Ot(N) = 33 THEN EXEC Make_object
     NEXT N
ENDPROC
'------------------------------------------------------------------------------
PROC Make_object 
     I = N
     B$ = ""
     REPEAT
       J = OV(I)
       IF OL(I) > 0 THEN t1 = 0
       IF J > 0 
         B$ =+ CHR$(I)
         M =+ 1
         I = J
       ENDIF
     UNTIL J = 0 
     J = OV2(I)
     Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * J, ADR(Show$(3)), ItemFieldSize, Name_Bank)
     Z=USR(@CHANGE_CASE,0,ADR(SHOW$(3)))
     Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
     IF found_loc >= 1 
         IF t1 = 1 
              Z=USR(@CHANGE_CASE,1,ADR(SHOW$(3)))
              PRINT "Ok, made "; Show$(3); ". "
              PriorRoom = 0
              IF J < 250 
                 OL(J) = Current_Place
              ELSE
                  Ro(Current_Place) = 8
              ENDIF
              FOR N = 1 TO LEN(B$)
                 I = ASC(B$[N,1])
                 IF I>0 AND I<192 
                     OL(I) = 255
                     Oc = Oc - 1
                 ENDIF
              NEXT N
         ELSE
            PRINT "Don't have everything to make "; Show$(3); ". "
         ENDIF
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Transport
    N = 1
    REPEAT
        IF N < 128
            Z=USR(@FIELD_GET, Place_Description_Base + Place_Field_Size * N,ADR(Show$(3)),Place_Field_Size,Place_BanK1)
        ELSE
            N = N & 127
            Z=USR(@FIELD_GET, Place_Description_Base + Place_Field_Size * N,ADR(Show$(3)),Place_Field_Size,Place_Bank2)
        ENDIF
       Z=USR(@CHANGE_CASE,ADR(SHOW$(3)))
       Found_Loc = USR(@FIND_SUBSTRING,ADR(Show$(3)),ADR(SEEK$))
       IF found_loc >= 1 AND RT(N) = 13
         Current_Place = N
       ENDIF
       N = N + 1
    UNTIL N >Total_Places OR found_loc > 0
ENDPROC
'------------------------------------------------------------------------------
PROC Reveal
     IF OV(N) = 1 
       OL(OV2(N)) = Current_Place
       PriorRoom = 0
     ENDIF
     IF OV(N) = 2 
       Rd(N) = 1
       Rdl(N) = OV2(N)
     ENDIF
     IF OV(N) = 3 
       Ro(Current_Place) = 1
       Rol(Current_Place) = OV2(N)
       Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
       Z=USR(@CHANGE_CASE,1,ADR(Show$(3)))
       Z=USR(@TRIM,ADR(Show$(3)))       
       IF LEN(SHOW$(3)) > 16 THEN SHOW$(3) = SHOW$(3)[1,16]
       Z=USR(@FIELD_PUT,Place_Exit_Base+16*Current_Place,ADR(SHOW$(3)),16,LEN(SHOW$(3)),Name_BANK)       
     ENDIF
     IF OV(N) = 4 
       Rn(N) = 1
       Rnl(N) = OV2(N)
     ENDIF
     IF OV(N) = 5 
       Re(N) = 1
       Rel(N) = OV2(N)
     ENDIF
     IF OV(N) = 6 
       Rw(N) = 1
       Rwl(N) = OV2(N)
     ENDIF
     IF OV(N) = 7 
       Rs(N) = 1
       Rsl(N) = OV2(N)
     ENDIF
     IF OV(N) = 8 
       Ru(N) = 1
       Rul(N) = OV2(N)
     ENDIF
     IF OV(N) = 10 
       J = OV(N) & 63
       I = OV(N) & 192 / 4 + J + 1
       FOR M = I TO I+J
         OL(M) = 0
         Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * M, ADR(Show$(3)), ItemFieldSize, Name_Bank)
         Z=USR(@CHANGE_CASE,2,ADR(Show$(3)))
         Z=USR(@TRIM,ADR(Show$(3)))       
         PRINT "Ok, Found "; Show$(3);". "
       NEXT M
     ENDIF
     IF OV(N) = 11 THEN EXEC Examine_object
     IF OV(N) = 9 AND (Ot(N) = 22 OR Ot(N) = 23 OR Ot(N) = 24 OR Ot(N) = 26 OR Ot(N) = 36) 
       Hl(OV2(N)) = Current_Place
       PriorRoom = 0
     ENDIF
     IF OV(N) = 9 AND Ot(N) = 24 
       EXEC Get_Hitpoint_Name
       Z=USR(@FIELD_GET, Item_Name_Base + ItemFieldSize * N, ADR(Show$(3)), ItemFieldSize, Name_Bank)
       Z=USR(@CHANGE_CASE,2,ADR(Show$(3)))
       Z=USR(@TRIM,ADR(Show$(3)))              
       Show$(0) = "The "
       Show$(0) =+ Show$(3) 
       Show$(0) =+ " exploded. Damaged by " 
       Show$(0) =+ STR$(OV2(N)) 
       Show$(0) =+ P$
       Show$(0) =+ "."
       PlayerHitPoints = PlayerHitPoints - OV2(N)
       EXEC Display
     ENDIF
     PriorRoom = 0
     OV(N) = 0
ENDPROC
'------------------------------------------------------------------------------
PROC Display
    J = 1  
    I = 40
    N = 0
    P$ = ""
    IF Last_Show_Number = 2 
        Z=USR(@TRIM,ADR(Show$(2)))
    ENDIF
    IF I > LEN(Show$(N)) THEN I = LEN(Show$(N))
    WHILE I <= LEN(Show$(N)) AND J <= LEN(Show$(N)) AND N <= Last_Show_Number  
        IF I < LEN(Show$(N)) 
            IF Show$(N)[I + 1, 1] = " " THEN I = I - 1
              A$ = Show$(N)[I, 1]
              WHILE A$ <> " " AND A$ <> "." AND A$ <> "-" AND A$ <> "," AND A$ <> "!" AND A$ <> "?" AND A$ <> ";"
                A$ = Show$(N)[I, 1]
                I = I - 1
              WEND
            ENDIF
            IF Show$(N)[J, 1] = " " THEN J = J + 1
            IF Show$(N)[I, 1] = " " THEN I = I - 1
            IF I - J > 1 THEN P$ =+ Show$(N)[J, I - J + 1]
            IF Text_Justify = 2 
              T = INT(19.5 - LEN(P$) / 2)
            ENDIF
            IF Text_Justify = 1 OR T < 0 THEN T = 0
            IF T>0 THEN PRINT TAB(T);
            PRINT P$;
            IF LEN(P$) < 40 THEN PRINT
            J = I + 1
            I = I + 40
            IF N >= Last_Show_Number OR Show$(N)=""
              IF I > LEN(Show$(N)) AND J < LEN(Show$(N)) 
                I = LEN(Show$(N))
                Text_Justify = 1
              ENDIF
              P$ = ""
            ELSE
              IF I > LEN(Show$(N)) AND J < LEN(Show$(N)) 
                  P$ = Show$(N)[J]
                  I = 40 - LEN(P$)
                  J = 1
                  N = N + 1
              ELSE
                P$ = ""
              ENDIF
        ENDIF
    WEND
    Text_Justify = 1
    Last_Show_Number = 0
ENDPROC
'------------------------------------------------------------------------------
PROC Get_HitPoint_Name
     IF S18 = 0 OR S8 > 0        
       P$ = " hit points" 
     ENDIF
     IF S18 = 1 
       P$ = " health points"
     ENDIF
     IF S18 = 2 
       P$ = " shield points"  
     ENDIF
     IF S18 = 3 
       P$ = " life points"
     ENDIF
ENDPROC
'------------------------------------------------------------------------------
PROC Get_game



     CLS
     PRINT "       TEXT ADVENTURE GAME PLAYER      "
     PRINT "        ATARI 8-BIT Version 1.50       "
     PRINT "           BY PETER J. MEYER           "
     PRINT FRE()  
     EXEC Load_file
     
    Z=USR(@FIELD_GET, GameName_Store,ADR(SHOW$(2)),40,Info_Bank)
    Z=USR(@FIELD_GET, Author_Store,ADR(SHOW$(3)),40,Info_Bank)
     
     Items_Carrying = 0
     CLS
     IF Show$(3)[1, 1] <> "|"
       PRINT TAB(20 - LEN(Show$(2)) / 2); Show$(2)
     ELSE
       Show$(0) = Show$(3)[2]
       'LOADPICTURE Show$(0)
     ENDIF
     IF Show$(3) <> "" 
       PRINT TAB(19); "By"
       PRINT TAB(20 - LEN(Show$(3)) / 2); Show$(3)
     ENDIF
     Z=USR(@FIELD_GET, Intro_Text_Store, ADR(SHOW$(0)),255,Info_Bank)
     Z=USR(@FIELD_GET, Intro_Text_Store+255, ADR(SHOW$(1)),255,Info_Bank)
     Z=USR(@FIELD_GET, Intro_Text_Store+510, ADR(SHOW$(2)),249,Info_Bank)  
     String_Length = LEN(SHOW$(0))+LEN(SHOW$(1))+LEN(SHOW$(2))
     IF String_Length <645 
        PRINT
     ENDIF
     'PRINT SHOW$(0);SHOW$(1);SHOW$(2)
     Last_Show_Number = 2
     Text_Justify = 2
     
     EXEC Display 
     IF String_Length <685 
       PRINT
       PRINT "Press any key to begin program.";
     ENDIF
     FOR N = 1 TO Total_Items
       IF OL(N) = 0 and OT(N) >=1
         Items_Carrying = Items_Carrying + 1
       ENDIF
     NEXT N
     IF S15 > 0 
       LIVES = S15
     ELSE
       LIVES = 1
     ENDIF
     IF S23 < 0 
       FOR N = 1 TO Total_Computer_Characters
         HO(N) = HH(N)
       NEXT N
     ENDIF
     C = S22
     PlayerHitPoints = S8
     PlayerImmunityTurns = 0
     PlayerPoisonStatus = 0
     PlayerInfectionStatus = 0
     PlayerFoodWaterNeed = S13
     Current_Place = 1
     PlayerLifeStatus = 0
     PlayerWinStatus= 0
     N = 0
     REPEAT
       N = KEY()
     UNTIL N > 0
ENDPROC
'------------------------------------------------------------------------------
PROC Game_Over
     IF PlayerWinStatus= 1 
       Items_Carrying = 0
       PRINT " ";
       Text_Justify = 2
       EXEC Display
       PRINT
       PRINT "GAME SOLVED."
     ELSE
       PRINT "GAME OVER."
     ENDIF
     PRINT "(P)lay other, (M)ain menu, or (Q)uit."
     CHOICE = 0
     
     REPEAT
       N = 0
       REPEAT
         N = KEY()
       UNTIL N>0
       CLS
       GET CHOICE
       IF CHOICE > 96 THEN CHOICE = CHOICE - 32
       Show$(0) = CHR$(N)
       IF LEN(Show$(0)) = 1 THEN CHOICE = ASC(Show$(0))
     UNTIL CHOICE = 77 OR CHOICE = 80 OR CHOICE = 81
ENDPROC
'------------------------------------------------------------------------------
'PROC Find_Substring
''  String_Length = len(Show$(3)) - len(Seek$)
''  L = len(Seek$)
''  found_loc = 0
''  repeat
''    found_loc =+1
''  until Seek$ = Show$(3)[found_loc,L] OR found_loc >= String_Length
''  if found_loc >= m then found_loc = 0
'ENDPROC
'------------------------------------------------------------------------------
PROC Load_file
   
   
   
   
    EXEC Disk_Directory
    CLOSE #1
    OPEN #1,4,0,FileName$
    INPUT #1, L
    L = L & 4095
    IF MemoryConfig = 128
        CLS
        H=DPEEK(88)+40
        BGET #1,H,L
        Z=USR(@FIELD_PUT,Intro_Text_Store,H-1,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Intro_Text_Store+255,H+254,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Intro_Text_Store+510,H+509,255,255,Info_Bank)
    ELSE
        BGET #1,Intro_Text_Store,L
    ENDIF
    INPUT #1, L
    L = L & 4095
    IF MemoryConfig = 128
        CLS
        H=DPEEK(88)+40
        BGET #1,H,L
        Z=USR(@FIELD_PUT,Closing_Text_Store,H-1,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Closing_Text_Store+255,H+254,255,255,Info_Bank)
        Z=USR(@FIELD_PUT,Closing_Text_Store+510,H+509,255,255,Info_Bank)        
        CLS
    ELSE
        BGET #1,Closing_Text_Store,L
    ENDIF
    INPUT #1, SHOW$(0)  'TITLE_TEXT$
    'EXEC CLEAR_STRING_MEMORY GameName_Store,40
    Z=USR(@FIELD_PUT,GameName_Store,ADR(SHOW$(0)),40,LEN(SHOW$(0)),Info_Bank)
    INPUT #1, SHOW$(0)  'AUTHOR_TEXT$
    'EXEC CLEAR_STRING_MEMORY Author_Store,40    
    Z=USR(@FIELD_PUT,Author_Store,ADR(SHOW$(0)),40,LEN(SHOW$(0)),Info_Bank)
    INPUT #1, SHOW$(0)  'Next Game File
    'EXEC CLEAR_STRING_MEMORY Next_File_Store,12
    MOVE ADR(SHOW$(0))+1,Next_File_Store,LEN(SHOW$(0))
    Z=USR(@FIELD_PUT,Next_File_Store,ADR(SHOW$(0)),12,LEN(SHOW$(0)),Info_Bank)
    INPUT #1, SHOW$(0)  'Monetary Unit Name
    'EXEC CLEAR_STRING_MEMORY Monetary_Unit_Store,12
    Z=USR(@FIELD_PUT,Monetary_Unit_Store,ADR(SHOW$(0)),12,LEN(SHOW$(0)),Info_Bank)
    INPUT #1, SHOW$(0)  'Future Expansion
    INPUT #1, S0
    INPUT #1, WIN_PLACE 'S1
    INPUT #1, S2
    INPUT #1, S3
    INPUT #1, S4
    INPUT #1, S5
    INPUT #1, S6
    INPUT #1, S7
    INPUT #1, S8
    INPUT #1, S9
    INPUT #1, Inventory_Limit
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
    INPUT #1, Total_Spells
    FOR N = 0 TO Total_Spells
        INPUT #1, SHOW$(0)
        Z=USR(@FIELD_PUT,Spell_Name_Base+16*N,ADR(SHOW$(0)),16,LEN(SHOW$(0)),Name_Bank)
        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254    
       'EXEC CLEAR_STRING_MEMORY Spell_Name_Base+16*N,16
       'MOVE ADR(SHOW$(0))+1,Spell_Name_Base+16*N,LEN(SHOW$(0))
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
      ELIF FP0%>1999 
        OV3(N) = 192 + INT(FP0% / 2000)
        OL(N) = INT(FP0% - OV3(N) * 2000)
      ENDIF
      INPUT #1, SHOW$(0) 'ON$(N)
      Z=USR(@FIELD_PUT,Item_Name_Base + ItemFieldSize * N,ADR(SHOW$(0)),ItemFieldSize ,LEN(SHOW$(0)),Name_Bank)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Item_Name_Base+16*N,16
      'MOVE ADR(SHOW$(0))+1,Item_Name_Base+16*N,LEN(SHOW$(0))
      'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
      INPUT #1, SHOW$(0) 'OD$(N)
      Z=USR(@FIELD_PUT,Item_Description_Base+80*N,ADR(SHOW$(0)),80,LEN(SHOW$(0)),Item_BANK)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Item_Description_Base+80*N,80
      'MOVE ADR(SHOW$(0))+1,Item_Description_Base+80*N,LEN(SHOW$(0))
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
      INPUT #1, SHOW$(0) 'HN$(N)
      Z=USR(@FIELD_PUT,Computer_Character_Base+16*N,ADR(SHOW$(0)),16,LEN(SHOW$(0)),Name_BANK)
      'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
      'EXEC CLEAR_STRING_MEMORY Computer_Character_Base+16*N,16
      'MOVE ADR(SHOW$(0))+1,Computer_Character_Base+16*N,LEN(SHOW$(0))
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
        INPUT #1, SHOW$(0) 'REN$(N)
        Z=USR(@FIELD_PUT,Place_Exit_Base+16*N,ADR(SHOW$(0)),16,LEN(SHOW$(0)),Name_BANK)
        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
        'EXEC CLEAR_STRING_MEMORY Place_Exit_Base+16*N,N
        'MOVE ADR(SHOW$(0))+1,Place_Exit_Base+16*N,LEN(SHOW$(0))
        'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
        INPUT #1, ROV(N)
        INPUT #1, SHOW$(0) 'RD$(N)
        IF N < 128
              Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*N,ADR(SHOW$(0)),Place_Field_Size,LEN(SHOW$(0)),Place_Bank1)
        ELSE
              Z=USR(@FIELD_PUT,Place_Description_Base+Place_Field_Size*(N&127),ADR(SHOW$(0)),Place_Field_Size,LEN(SHOW$(0)),Place_Bank2)
        ENDIF

        'IF MemoryConfig = 64 THEN POKE 54286,0:POKE $D301,254
        'EXEC CLEAR_STRING_MEMORY Place_Description_Base+Place_Field_Size*N,32
        'MOVE ADR(SHOW$(0))+1,Place_Description_Base+Place_Field_Size*N,LEN(SHOW$(0))
        'IF MemoryConfig = 64 THEN POKE $D301,255:POKE 54286,64
    NEXT N
    CLOSE #1
ENDPROC
'------------------------------------------------------------------------------
   PROC DISK_DIRECTORY
        I = 0
        J = 6
        PRINT
        PRINT "TEXT GAMES ON THIS DISK."
        OPEN #1,6,0,"D:*.TGF*"
        REPEAT
            POSITION I,J 
            INPUT #1,SHOW$(0)
            IF ERR()=1 THEN PRINT SHOW$(0)
            I = I + 20
            IF I > 25 
                I = 0
                J = J + 1
            ENDIF
        UNTIL ERR()>1
        POSITION 0,22
        INPUT "GAME>";FileName$
        IF FileName$<>""
            IF FileName$[2,1]<>":" and FileName$[3,1]<>":" 
                  SHOW$(0) = FileName$
                  Filename$ = "D:"
                  Filename$ =+SHOW$(0)
            ENDIF
            L = LEN(FileName$) - 4
            IF L < 1 THEN L = 1
            IF FileName$[L,4]<>".TGF"
                  Filename$ =+".TGF"
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
        ELIF MemoryConfig = 128
            ItemTableSize = 204
            ComputerCharacterTableSize = 256
            PlaceTableSize = 256
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



