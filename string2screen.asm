        .segment "DATA"
        .include "Atari 8-bit Equates.asm"
NDX0									                  =$CA
NDX1									                  =$CB                    
NDX2                                                      =$CC
NDX3                                                      =$CD
SIZH                                                      =$CE
LSTN                                                      =$CF
ATASCII2SCR:
      .export ATASCII2SCR
            
      PLA
      STA SIZH
      PLA
      TAY

      PLA
      CLC
      ADC SIZH
      STA NDX1
      PLA
      STA NDX0
STR2LOOP: 
      LDA (NDX0),Y
      AND #127
      CMP #32
      BCS No_Scr_Add_32
      CLC
      ADC #64
      STA (NDX0),Y
      JMP Put2Scr
No_Scr_Add_32:
      CMP #96
      BCS Put2Scr
      SEC
      SBC #32
Put2Scr:
      STA (NDX0),Y
      DEY
      CPY #255
      BNE STR2LOOP
      DEC NDX1
      DEC SIZH
      BPL STR2LOOP
      RTS
SCR2ATASCII:
      .export SCR2ATASCII
      PLA
      STA SIZH
      PLA
      TAY

      PLA
      CLC
      ADC SIZH
      STA NDX1
      PLA
      STA NDX0

      LDA #0
      STA LSTN
WAIT_END_SPACES_LOOP:
      LDA (NDX0),Y
      AND #127
      BNE SAVE_NON_SPACE_SIZE
      DEY
      CPY #255
      BNE WAIT_END_SPACES_LOOP
      DEC NDX1
      DEC SIZH
      BPL WAIT_END_SPACES_LOOP
      RTS
SAVE_NON_SPACE_SIZE:
      TYA
      CLC
      ADC #1
      PHA
      LDA SIZH
      ADC #0
      PHA
SCR2LOOP: 
      LDA (NDX0),Y
      AND #127

      CMP #64
      BCS No_Str_Add_32
      CLC
      ADC #32
      STA (NDX0),Y
      JMP Put2Str
No_Str_Add_32:
      CMP #96
      BCS Put2Str
      SEC
      SBC #64
Put2Str:
      STA (NDX0),Y
NoConv2Str:
      DEY
      CPY #255
      BNE SCR2LOOP
      DEC NDX1
      DEC SIZH
      BPL SCR2LOOP
      PLA
      TAX
      PLA      
      RTS
      
