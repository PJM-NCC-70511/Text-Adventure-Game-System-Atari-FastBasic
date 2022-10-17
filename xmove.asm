        .segment "PAGE6"
        .include "Atari 8-bit Equates.asm"            
STRING_BUFFER                                             =$02
NDX0									                  =$CA
NDX1									                  =$CB
STRSIZE      							                  =$CC
FLDSIZE                                                   =$CD
LAST_NON0                                                 =$CE
CASE_SET                                                  =$CE
FIELD_GET:
;;;   FIELD_PUT 
;;;;  Field_Addr_Start String_Addr Field_Size String_Size Field_Bank
      .export FIELD_GET
      PLA      
      PLA
      STA GET_PORTB_BANK
;     PLA
;     PLA 
;     STA STRSIZE
      PLA
      PLA 
      STA FLDSIZE
      PLA
      STA NDX1
      PLA
      STA NDX0
      PLA
      STA GET_FLD_ADDR + 1
      PLA
      STA GET_FLD_ADDR + 0
      LDY FLDSIZE
      LDA #0
      STA NMIEN
      LDA #2
      STA COLPF1
GET_PORTB_BANK =*+1
      LDA #$FF
      STA PORTB 
GET_TO_BUFFER_LOOP:
      DEY  
GET_FLD_ADDR = *+1
      LDA $FFFF,Y
      STA (STRING_BUFFER),Y         
      CPY #0
      BNE GET_TO_BUFFER_LOOP
      LDA #$FF
      STA PORTB
      LDA #64
      STA NMIEN
      LDA #4
      STA COLPF1     

      LDY FLDSIZE
COPY_TO_STRING_LOOP:
      DEY ;2 ;1 ;0
      LDA (STRING_BUFFER),Y
      INY ;3 ;2; 1
      STA (NDX0),Y
      DEY ;2 ;1; 0
      BNE COPY_TO_STRING_LOOP
      LDY FLDSIZE
Truncate_Loop:
      DEY
      BEQ EndTrunchate
      LDA (STRING_BUFFER),Y
      BEQ Truncate_Loop
      INY
EndTrunchate:
      TYA     
      LDY #0
      STA (NDX0),Y
      RTS
;;;;  PROC FIELD_GET
;;;;  Field_Addr_Start String_Addr Field_Size Field_Bank          
FIELD_PUT:
      .export FIELD_PUT

      PLA      
      PLA
      STA PUT_PORTB_BANK

      PLA
      PLA 
      STA STRSIZE

      PLA
      PLA 
      STA FLDSIZE

      PLA
      STA PUT_STR_ADDR + 1
      PLA
      STA PUT_STR_ADDR + 0

      PLA
      STA NDX1
      PLA
      STA NDX0


      LDY STRSIZE
COPY_TO_PUT_BUFFER_LOOP:
PUT_STR_ADDR =*+1
      LDA $FFFF,Y
      DEY        
STRING_BUFFER_3 =*+1      
      STA (STRING_BUFFER),Y
      BNE COPY_TO_PUT_BUFFER_LOOP

      LDA #0
      STA NMIEN
      LDA #2
      STA COLPF1
PUT_PORTB_BANK =*+1
      LDA #$FF
      STA PORTB 

      LDY STRSIZE
COPY_PUT_FIELD_LOOP:      
      LDA (STRING_BUFFER),Y
      STA (NDX0),Y
      DEY
      CPY #255
      BNE COPY_PUT_FIELD_LOOP

      LDY FLDSIZE
      LDA #0
      INC STRSIZE
      BEQ NO_CLEAR_FIELD 
CLEAR_FIELD_LOOP:     
      CPY STRSIZE
      BCC NO_CLEAR_FIELD      
      DEY
      STA (NDX0),Y
      BNE CLEAR_FIELD_LOOP
NO_CLEAR_FIELD:
      LDA #$FF
      STA PORTB
      LDA #64
      STA NMIEN
      LDA #4
      STA COLPF1     
      RTS

CLEAR_X_BANK:
      .export CLEAR_X_BANK
      LDA #0
      STA NMIEN
      PLA      
      PLA
      STA PORTB
      CMP #254
      BNE No_Clear_Under_OS
      LDX #$FF
      LDA #$D8
      BNE Set_X_Clear
No_Clear_Under_OS:
      LDX #$7F
      LDA #$40
Set_X_Clear:
      STX XClearHI
      STA XClearLow 
XClearNextPage:
      LDA #$00
      TAY
XClearLoop:
       
XClearHI = *+2
      STA $FF00,Y
      INY
      BNE XClearLoop
      DEX
      STX XClearHI
XClearLow =*+1
      CPX #$FF
      BCS XClearNextPage
      CPX #$D7
      BNE No_2nd_Under_OS_Section
      LDX #$CF
      LDA #$C0
      BNE Set_X_Clear
No_2nd_Under_OS_Section:     
      LDA #255
      STA PORTB
      LDA #64
      STA NMIEN
      RTS
      
      
      