/*****************************************************
Experiments with removing top and bottom borders.
*****************************************************/

BasicUpstart2(main)

#importonce
#import "./include/vic.asm"

.const RasterFlag = $10

*=$1000 "Main Program"
main:

    // set border color to black
    lda #$0
    sta $d020

    sei         // Disable interrupts

    lda #$7f    // Disable CIA timers
    sta $dc0d
    sta $dd0d

    lda #$fa    // Set raster line to trigger interrupt (adjust as needed)
    sta $d012

    // init the raster flag to 25 rows
    lda #0
    sta RasterFlag

    lda #<irq              // Load low  .byte of interrupt handler address
    sta $0314              // Store it in the interrupt vector low  .byte
    lda #>irq              // Load high     .byte of interrupt handler address
    sta $0315              // Store it in the interrupt vector high     .byte

    lda #$81
    sta $d01a
    lda #$1b
    sta $d011

    lda $dc0d
    lda $dd0d
    asl $d019

    cli     // Enable interrupts

    // draw the sprite

    // point sprite 0 at data in the cassette buffer
    lda #14
    sta $07f8

    // set sprite 0 colour to white
    lda #1
    sta $d027

    // turn on sprite 0
    lda #01
    sta $d015

    // expand sprite 0 in the X direction
    lda $d01d
    ora #1
    sta $d01d

    // position the sprite
    lda #25
    sta vic.sprite0.xpos
    lda #255
    sta vic.sprite0.ypos


    rts     // Return from subroutine
    
irq:

    // display raster time in the border
    inc $d020

    // Your interrupt code goes here
    // Use registers A, X, and Y for temporary storage if needed

    // test the raster toggle flag
    // 0 means 25 rows, 1 means 24 rows
    lda RasterFlag
    bne !+

    // set the screen to 24 rows
    lda vic.controlRegister  
    and #$f7    //%11110111
    sta vic.controlRegister
    // update the flag
    inc RasterFlag
    // next interrupt to happen at 260 lines
    lda #10
    sta $d012
    jmp !done+

!:  // set the screen to 25 rows
    lda vic.controlRegister  
    ora #$08    //%00001000
    sta vic.controlRegister
    // unset the flag
    dec RasterFlag
    // set up the interrupt for the next frame
    lda #250
    sta $d012

!done:

    // unset raster time in the border
    dec $d020
        
    // set irq handled flag and end
    asl $d019
    
    // restore the registers and return from the interrupt handler
    pla
    tay
    pla
    tax
    pla
    rti 
    


// Sprite #1
// Single color mode, BG color: 0, Sprite color: 1
*=$0380
spriteDataScoreLabel:
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $FF, $FF, $FF
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $71, $88, $E7
    .byte $82, $14, $94
    .byte $84, $22, $94
    .byte $64, $22, $E7
    .byte $14, $22, $94
    .byte $12, $14, $94
    .byte $E1, $88, $97
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $FF, $FF, $FF
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00



