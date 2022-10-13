SIZE_STRING    					                          =$C8
SIZE_FIELD                                                =$C9
BANK                                                      =$CA
NDX0                                                      =$CC
NDX1                                                      =$CD
;  Field_Addr_Start String_Addr Field_Size String_Size Field_Bank
      .export PUT_FIELD
        PLA 
        PLA 
        STA PUT_BANK
        PLA
        PLA 
        STA SIZE_STRING        
        PLA 
        PLA 
        STA SIZE_FIELD
        PLA
        STA PUT_STRING+1
        PLA
        STA PUT_STRING+0
        PLA 
        STA NDX1
        PLA 
        STA NDX0 ;Address of Field

        LDX SIZE_STRING
        INX
COPY_TO_PUT_BUFFER_LOOP

PUT_STRING =*+1
        LDA $FFFF,X
        STA TEMP_BUFFER-1,X
        DEX
        BNE COPY_TO_PUT_BUFFER_LOOP
PUT_BANK = *+1        
        LDA #$FF
        STA PORTB
        
        LDY SIZE_FIELD
        LDA #0
CLEAR_PUT_FIELD_LOOP
        DEY
        STA (NDX0),Y
        BNE CLEAR_PUT_FIELD_LOOP
        LDY SIZE_FIELD
        DEY
COPY_PUT_BUFFER_LOOP
        LDA TEMP_BUFFER,Y
PUT_STRING_ADDR = *+1
        STA (NDX0),Y
        DEY
        CPY #255
        BNE COPY_PUT_BUFFER_LOOP
        LDA #$FF
        STA PORTB
        RTS        
GET_FIELD:
;    Field_Addr_Start   String_Addr   Field_Size   Field_Bank

      .export GET_FIELD
        PLA 
        PLA 
        STA GET_BANK
        PLA 
        PLA 
        STA SIZE_FIELD
        PLA 
        STA NDX1
        PLA 
        STA NDX0 ;Address of String
        PLA
        STA GET_FIELD+1
        PLA
        STA GET_FIELD+0

GET_BANK = *+1        
        LDA #$FF
        STA PORTB
        LDX SIZE_FIELD
COPY_TO_GET_BUFFER_LOOP
        
        DEX
GET_FIELD =*+1
        LDA $FFFF,X
        STA TEMP_BUFFER,X
        CPX #255
        BNE COPY_TO_GET_BUFFER_LOOP
        
        LDA #$FF
        STA PORTB
        
        LDX SIZE_FIELD
GET_SIZE_LOOP
        DEX
        LDA TEMP_BUFFER,X
        BEQ GET_SIZE_LOOP
        CPX #255
        BNE GET_SIZE_LOOP
        INX
        TXA
        LDY #$00
        STA (NDX0),Y
        TAY
        INY
COPY_GET_BUFFER_LOOP
        LDA TEMP_BUFFER,X
        STA (NDX0),Y
        DEX
        DEY
        BNE COPY_GET_BUFFER_LOOP
        RTS

GET_BIG_STRING_SIZE:
        .export GET_BIG_STRING_SIZE
        PLA 
        PLA 
        STA PORTB

        PLA 
        STA NDX1
        PLA 
        STA NDX0

        PLA
        STA CHECK_FIELD+1
        PLA
        STA CHECK_FIELD+0
        
                 

              
        
        
       
       
            