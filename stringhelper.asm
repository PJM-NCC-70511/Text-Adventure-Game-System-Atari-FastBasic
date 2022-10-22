        .segment "DATA"
        .include "Atari 8-bit Equates.asm"
NDX0									                  =$CA
NDX1									                  =$CB                    
NDX2                                                      =$CC
NDX3                                                      =$CD
TRIM:
      .export TRIM
      PLA
      STA NDX1
      PLA
      STA NDX0
      LDY #0
      LDA (NDX0),Y
      TAY 
Find_Last_Non_Space_Loop:
     LDA (NDX0),Y
     AND #95
     CMP #0
     BNE Set_Trim_Size
     DEY
     BNE Find_Last_Non_Space_Loop
Set_Trim_Size:      
     TYA      
     LDY #0
     STA (NDX0),Y
     RTS
      
CHANGE_CASE:
      .export CHANGE_CASE
            
      PLA
      STA NDX1
      PLA
      STA NDX0
      
      PLA
      PLA 
      TAX


      LDY #0
      LDA (NDX0),Y
      TAY 

      CPX #0
      BNE No_Change_Upper
Upper_Case_Loop:
      LDA (NDX0),Y
      CMP #97
      BCC No_Sub32_For_Upper
      AND #95
No_Sub32_For_Upper:
      STA (NDX0),Y
      DEY
      BNE Upper_Case_Loop
      RTS 
           
No_Change_Upper:      
                
Lower_Case_Loop:
      LDA (NDX0),Y
      CMP #65
      BCC No_Add32_For_Lower
      CMP #91
      BCS No_Add32_For_Lower
      ORA #32
No_Add32_For_Lower:
      STA (NDX0),Y
      CPY #2
      BCS No_Caps_Go
      CPX #2
      BEQ Upper_Case_Loop
No_Caps_Go:
      DEY
      BNE Lower_Case_Loop


No_Change_Lower:      
      
      RTS
REMOVE_SUBSTRING:
      .export REMOVE_SUBSTRING
      LDA #$FF
      .BYTE $2C
FIND_SUBSTRING:
      LDA #0
      STA NDX3
      .export FIND_SUBSTRING
      PLA
      STA Search_For      + 1
      STA NDX1
      PLA
      STA Search_For      + 0
      STA NDX0
      LDY #0
      LDA (NDX0),Y
      STA Search_For_Size
      

      PLA
      STA Search_String   + 1
      STA NDX1
      PLA
      STA Search_String   + 0
      STA NDX0
      LDA (NDX0),Y
      CMP Search_For_Size
      BCC Smaller_Than_Seek_String        
      STA Search_Sub_String_End
      LDX #1
      LDY #1
Search_String_Loop:
Search_String = *+1      
      LDA $FFFF,Y
Search_For =*+1
      CMP $FFFF,X
      BNE No_Match_Yet
      INX
Search_For_Size =*+1
      CPX #$FF
      BCC Search_Next_Character
      BEQ Search_Next_Character
Match_Found:
      DEX
      STX NDX2
      BIT NDX3
      BMI Remove_Found_String 
      LDX #0
      INY
      TYA
      SEC
      SBC NDX2
      RTS      
No_Match_Yet:
      CPX #2 
      BCC Search_Next_Character
Do_Back_Match_Reset:
      DEY
      DEX
      CPX #1
      BNE Do_Back_Match_Reset 
Search_Next_Character:
      INY
Search_Sub_String_End =*+1
      CPY #$FF
      BCC Search_String_Loop
      BEQ Search_String_Loop
Smaller_Than_Seek_String:      
      LDA #0
      LDX #0
      RTS

Remove_Found_String:
      TAY 
      STA NDX2
      SEC
      ADC Search_For_Size
      STA NDX3 
      LDA Search_Sub_String_End
      STA Remove_Size_End
      SEC
      SBC #1
      LDY #0
      STA (NDX0),Y      
Remove_SubString_Loop:      
      LDY NDX3
      LDA (NDX0),Y
      LDY NDX2
      STA (NDX0),Y
      INC NDX3
      INY
      STY NDX2
Remove_Size_End = *+1
      CPY Remove_Size_End
      BCC Remove_SubString_Loop
      RTS

Replace_Found_String:
      RTS
CompactText:

ExpandText:  
