//////////////////////////////////////////////////////////////////////////////
// numdisplay.asm
// Copyright(c) 2023 Jonathan Masters.
// License: MIT. See LICENSE file in root directory.
/////////////////////////////////////////////////////////////////////////////
// This sample shows how to convert numbers to decimal and display 
// them on the screen.


*=$0801 "BASIC Start"  // location to put a 1 line basic program so we can just
        // type run to execute the assembled program.
        // will just call assembled program at correct location
        //    10 SYS (4096)

        // These bytes are a one line basic program that will 
        // do a sys call to assembly language portion of
        // of the program which will be at $1000 or 4096 decimal
        // basic line is: 
        // 10 SYS (4096)
        .byte $0E, $08           // Forward address to next basic line
        .byte $0A, $00           // this will be line 10 ($0A)
        .byte $9E                // basic token for SYS
        .byte $20, $28, $34, $30, $39, $36, $29 // ASCII for " (4096)"
        .byte $00, $00, $00      // end of basic program (addr $080E from above)


        // assembler constants for special memory locations
        .const CLEAR_SCREEN_KERNAL_ADDR = $E544     // Kernal routine to clear screen
        .const PLOT_KERNAL_ADDR = $FFF0             // Kernal routine to get set cursor position
        .const SCREEN_MEM_START = $0400             // start of screen memory

        // zero page locations
        .const VALUE_L = $10
        .const VALUE_H = $11

        // character set constants
        .const ZERO = $30                           // screen character code for zero

*=$0900 "Data start"
        
    HELLO_TEXT: .text @"hello 123\$00"
    VALUE_16:   .word $0010                         // 16 decimal


// our assembly code will goto this address
*=$1000 "Main Start"

        //////////////////////////////////////////////////////////////////////
        // clear screeen leave cursor upper left
        jsr CLEAR_SCREEN_KERNAL_ADDR 

        // load the value into zero page
        lda VALUE_16
        sta VALUE_L
        lda (VALUE_16 + 1)
        sta VALUE_H

    done:
        rts


    mod_10:
        