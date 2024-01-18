//////////////////////////////////////////////////////////////////////////////
// scoredisplay.asm
// This is an experiment in using BCD to store and display numbers that will
// be displayed to players.
/////////////////////////////////////////////////////////////////////////////

BasicUpstart2(main)

#importonce
#import "./include/vic.asm"

    // assembler constants for special memory locations
    .const CLEAR_SCREEN_KERNAL_ADDR = $E544     // Kernal routine to clear screen
    .const SCREEN_MEM_START = $0400             // start of screen memory

    // constants
    .const ZERO = $30                           // screen character code for zero
    .const SCORE_LEN = 4                        // number of BCD bytes to use (2 digits per byte)
    .const SCORE_INCREMENTS = 25                // the amount we're adding to the score (0 - 99)


*=$1000 "Main Start"
main:
    // clear screeen leave cursor upper left
    jsr CLEAR_SCREEN_KERNAL_ADDR 

    // some background text
    ldx #0
!:
    lda SCORE_TEXT, x
    beq !+
    sta SCREEN_MEM_START, x
    inx
    jmp !-
!:

main_loop:
    wait_for_raster(250)

// adds an increment to the score value using BCD
update_score:
    sed
    lda SCORE
    clc
    adc #SCORE_INCREMENTS 
    sta SCORE

    .for (var i = 1; i < SCORE_LEN; i++) {
        lda SCORE + i
        adc #0
        sta SCORE + i
    }

    cld

// displays the BCD value
display_score:

    ldx #7              // screen location offset
    ldy #SCORE_LEN - 1  // BCD byte of score
!:
    lda SCORE, y
    lsr
    lsr 
    lsr
    lsr
    clc
    adc #ZERO
    sta SCREEN_MEM_START, x
    inx

    lda SCORE, y
    and #$0f
    clc 
    adc #ZERO 
    sta SCREEN_MEM_START, x
    inx

    dey
    cpy #$ff
    bne !-
    
done:
    jmp main_loop

// bytes holding the BCD score value - note the least significant bytes are on the left
// e.g. a value of 12345678 is laid out as .byte $78, $56, $34, $12
SCORE: .byte $00, $00, $00, $00
SCORE_TEXT: .text @"score: \$00"

///////////////////////////////////////////////////////////////////
// name: wait_for_raster
// Parameters: 
//      line - a single byte raster line to wait for
// Description: Delays execution until the specified raster line
//              has started drawing
////////////////////////////////////////////////////////////////////
.macro wait_for_raster(line) {
!: 
    lda vic.raster_scan_line
    cmp #line
    bne !-
}

