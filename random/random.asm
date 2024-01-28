/*
    This programming challenge was to take the classic 'maze' BASIC
    program and turn it into an assembly version. 
    It uses the random number generator in the SID chip to generate
    random numbers which it picks up from the $D41B register. The bottom
    (LSB) bit is then used to choose either a left or right 'slash' char
    (77 or 78) by adding 77 to the masked value. This is then effectively
    POKE'd into the next screen location.
*/

BasicUpstart2(main)

#importonce
#import "include/sid.asm"

.const SCREEN_CHAR_BUFFER_1 = $0400
.const SCREEN_ADDR_LO = $f7
.const SCREEN_ADDR_HI = $f8
.const MAZE_CHAR_1 = 77

*=$1000 "Code"
main:
    sid_init_random()

    // the screen address is held in zero page so we can 
    // update it during the blit
    lda #<SCREEN_CHAR_BUFFER_1
    sta SCREEN_ADDR_LO
    lda #>SCREEN_CHAR_BUFFER_1
    sta SCREEN_ADDR_HI

    // set up the index registers so we do 4 batchess of
    // 250 characters (== 1000 chars == 40x25)
    // the x register holds the batch index and counts down
    ldx #4
next_batch:
    // the y register holds the character index and counts up
    ldy #0
next_char:

    // get a random number from the SID
    lda $d41b

    // mask off the lsb to give us a 1 or 0
    and #$01

    // select a character to display
    clc
    adc #MAZE_CHAR_1

    // display it
    sta (SCREEN_ADDR_LO), y

    // if we're still in a batch get the next char
    iny
    cpy #250
    bne next_char

    // if we've ended a batch, update the screen pointer
    // to the scfreen starting point for the next batch
    lda SCREEN_ADDR_LO
    clc
    adc #250
    sta SCREEN_ADDR_LO
    bcc !l+
    inc SCREEN_ADDR_HI
!l:
    
    // if this wasn't the last batch, go round again
    dex
    bne next_batch

done:
    rts

