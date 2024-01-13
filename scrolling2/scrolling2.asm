/*
    This program builds on the scrolling1 example with the objectives to:
    1. Use the hardware scrolling features of the VIC II chip to achieve smoother scrolling in X & Y
    2. Speed up the character scrolling by only referencing the map when new rows or columns need to be
       added to the character / colour memory.

    Zero Page:
    ==========

    The addresses below are used as working memory to allow the blitting of character
    and colour data to the VIC II memory bank. It's currently assuming that the VIC
    is pointing at bank 0 
    $f7:$f8  00d8 : $d800 : Colour RAM Ptr
    $f9:$fa  0030 : $4000 : map color data Ptr
    $fb:$fc  0028 : $3000 : map character data Ptr
    $fd:$fe  0004 : $0400 : screen memory Ptr    
*/

// dimensions
.const MAP_WIDTH = 80
.const MAP_HEIGHT = 50
.const SCREEN_WIDTH = 40
.const SCREEN_HEIGHT = 25

// video memory locations are defined here as they are application specific
// note that we define 2 locations so that we can use double buffering to 
// try to smooth out the scrolling
.const SCREEN_CHAR_BUFFER_1 = $0400
.const SCREEN_CHAR_BUFFER_2 = $0800


// keyboard scan codes
.const SC_A = 10
.const SC_S = 13
.const SC_D = 18
.const SC_W = 9
.const SC_NONE = 64

// kernal routine locations
.const SCNKEY = $ff9f

BasicUpstart2(main)

#importonce
#import "./include/vic.asm"

*=$1000
// The scroll offset is a 16 bit offset into both the character and colour data and
// holds the starting point for blits into character and colour video RAM. To caluclate
// the offset we need to use the equation (ypos * MAP_WIDTH) + xpos.
scroll_offset_lo: .byte 0
scroll_offset_hi: .byte 0

background_x_pos_fine: .byte 0
background_x_pos:      .byte 35

backgound_y_pos_fine: .byte 7
background_y_pos:     .byte 0

// these pre-calculated offsets make it easier to find rows in the map to display
// TODO: Either turn these into lo hi tables or remove them and use the map_row_offsets
//       tables instead (just add the base address where needed)
map_row_char_start_addresses: .fillword MAP_HEIGHT, map_character_codes + (i * MAP_WIDTH)
map_row_colour_start_addresses: .fillword MAP_HEIGHT, map_colour_data + (i * MAP_WIDTH)

map_row_offets: .lohifill MAP_HEIGHT, i * MAP_WIDTH

scroll_temp: .byte 0

main:

    // set to 25 line extended color text mode and turn on the screen
    lda #[vic.CR_EXTENDED_COLOUR_TEXT + vic.CR_BLANK_SCREEN_TO_BORDER_COLOUR + 7]
    sta vic.control_register

    // disable SHIFT-Commodore
    lda #$80
    sta $0291

    // set screen memory ($0400) and charset bitmap offset ($2000)
    lda #$18
    sta vic.memory_pointers

    // set border and screen colours
    lda #BLACK
    sta vic.border_colour
    sta vic.background_0_colour

    // set extended bg color 1
    lda #WHITE
    sta vic.background_1_colour

    // set extended bg color 2
    lda #RED
    sta vic.background_2_colour

    // set extended bg color 3
    lda #CYAN
    sta vic.background_3_colour

    // start by drawing the whole screen
    jsr draw_entire_screen


// main game loop
main_loop:

    // wait for a good raster position
    wait_for_raster(251)

    // scan for a key down
    jsr SCNKEY
    lda 203
    cmp #SC_NONE
	bne handle_down

    // lda #BLACK
    // sta vic.border_colour
    jmp loop_done

    // there should be a key value in the accumulator

handle_down:
    // check for scroll down
    cmp #SC_S
    beq !+
    jmp handle_up
!:

    // Check bounds
    lda background_y_pos
    cmp #MAP_HEIGHT - SCREEN_HEIGHT - 2
    bne !+ 
    jmp loop_done
!:
    ///////////////////////////////////////////
    // scroll down
    ///////////////////////////////////////////

    // start by decrementing the hardware scroll
    // value whic is in the 3 least significant
    // bits of the vic control register
    lda backgound_y_pos_fine
    sec
    sbc #1
    sta backgound_y_pos_fine
    and #7
    sta scroll_temp
    lda vic.control_register
    and #$f8
    ora scroll_temp
    sta vic.control_register
    
    // if the hardware scroll has reached it's
    // maximum value, we need to shift the screen
    // up a row before it cycles back to 0
    lda backgound_y_pos_fine
    and #7
    beq !+
    jmp end_down
!:
    // if we get here, it's because we need to shift
    // the whole screen up by one row.

    // update our 'coarse' background y position
    inc background_y_pos

    // We're double buffering to keep the scroll smooth
    // the vic memory pointers register holds a value
    // indicating the offset of the screen buffer.
    // We're going to assemble the new screen in a 
    // different buffer and then flip the vic over to
    // to that buffer when we're done.
    lda vic.memory_pointers
    and #$10
    beq !+
    shift_screen_up(SCREEN_CHAR_BUFFER_1, SCREEN_CHAR_BUFFER_2)
    jmp !++
!:
    shift_screen_up(SCREEN_CHAR_BUFFER_2, SCREEN_CHAR_BUFFER_1)
!:
    
    // wait for a good raster position
    wait_for_raster(251)

    // toggle the char buffer
    lda vic.memory_pointers
    eor #$30
    sta vic.memory_pointers

end_down:
    jmp loop_done

handle_up:
    // check for scroll up
    cmp #SC_W
    beq !+
    jmp handle_right
!:

    // Check bounds!
    lda background_y_pos
    cmp #0
    bne !+ 
    jmp loop_done
!:
    ///////////////////////////////////////////
    // scroll up
    ///////////////////////////////////////////

    // start by incrementing the hardware scroll
    // value whic is in the 3 least significant
    // bits of the vic control register
    lda backgound_y_pos_fine
    clc
    adc #1
    sta backgound_y_pos_fine
    and #7
    sta scroll_temp
    lda vic.control_register
    and #$f8
    ora scroll_temp
    sta vic.control_register
    
    // if the hardware scroll has reached it's
    // maximum value, we need to shift the screen
    // down a row before it cycles back to 0
    lda backgound_y_pos_fine
    and #7
    cmp #7
    beq !+ 
    jmp end_up
!:
    // if we get here, it's because we need to shift
    // the whole screen down by one row.

    // update our 'coarse' background y position
    dec background_y_pos

    // We're double buffering to keep the scroll smooth
    // the vic memory pointers register holds a value
    // indicating the offset of the screen buffer.
    // We're going to assemble the new screen in a 
    // different buffer and then flip the vic over to
    // to that buffer when we're done.
    lda vic.memory_pointers
    and #$10
    beq !+
    shift_screen_down(SCREEN_CHAR_BUFFER_1, SCREEN_CHAR_BUFFER_2)
    jmp !++
!:
    shift_screen_down(SCREEN_CHAR_BUFFER_2, SCREEN_CHAR_BUFFER_1)
!:

    // wait for a good raster position
    wait_for_raster(251)

    // toggle the char buffer
    lda vic.memory_pointers
    eor #$30
    sta vic.memory_pointers
    
end_up:
    jmp loop_done

handle_right:
    // check for scroll right
    cmp #SC_D
    bne handle_left

    // TODO: Check bounds!

    // scroll right
    // lda #BLUE
    // sta vic.border_colour
    
    jmp loop_done

handle_left:
    // check for scroll left
    cmp #SC_A
    bne loop_done

    // TODO: Check bounds!

    // scroll left
    lda #GREEN
    sta vic.border_colour
    
loop_done:
    // raster time
    //dec vic.border_colour

    jmp main_loop

////////////////////////////////////////////////////////////////
// Subroutines
////////////////////////////////////////////////////////////////

    // performs a blit from the character and colour maps into video memory
draw_entire_screen:
    
    // reset the zero page locations we're
    // going to be using
    reset_zp()

    // calculate the scroll address
    x_y_to_scroll_offset()

    // apply xpos to the map character data ptr
    lda $fb
    clc
    adc scroll_offset_lo
    sta $fb
    bcc !+
    inc $fc
!:
    lda $fc
    clc // there's nothing we can do about the carry here so we can probably remove this
    adc scroll_offset_hi
    sta $fc

    // apply scroll pos to the map colour data ptr
    lda $f9
    clc
    adc scroll_offset_lo
    sta $f9
    bcc !+
    inc $fa
!:
    lda $fa
    clc // there's nothing we can do about the carry here so we can probably remove this
    adc scroll_offset_hi
    sta $fa

    // draw a row of the screen
    ldx #$00
l1: ldy #$00

l2: lda ($fb),y
    sta ($fd),y
    lda ($f9),y
    sta ($f7),y
    iny
    cpy #SCREEN_WIDTH
    bne l2

    // add 40 to the screen ptr
    lda $fd
    clc
    adc #SCREEN_WIDTH
    sta $fd
    bcc !+
    inc $fe
!:

    // add 40 to the colour ram ptr
    lda $f7
    clc
    adc #SCREEN_WIDTH
    sta $f7
    bcc !+
    inc $f8
!:

    // add 80 to the map character data ptr
    lda $fb
    clc
    adc #MAP_WIDTH
    sta $fb
    bcc !+
    inc $fc
!:

    // add 80 to the map colour data ptr
    lda $f9
    clc
    adc #MAP_WIDTH
    sta $f9
    bcc !+
    inc $fa
!:

    // incrememnt the row counter and check for last row
    inx
    cpx #SCREEN_HEIGHT
    beq done_frame

    // draw the next line of the frame
    jmp l1

done_frame:
    rts

///////////////////////////////////////////////////////////////////
// name: wait_for_raster
// Parameters: 
//      line - a single byte raster line to wait for
// Description: Delays execution until the specified raster lime
//              has started drawing
////////////////////////////////////////////////////////////////////
.macro wait_for_raster(line) {
!: 
    lda vic.raster_scan_line
    cmp #line
    bne !-
}

///////////////////////////////////////////////////////////////////
// name: x_y_to_scroll_offset
// Description: converts the background x y positions into an offset
//              for use with the character or colour maps.
////////////////////////////////////////////////////////////////////
.macro x_y_to_scroll_offset() {

    // start with y
    ldx background_y_pos
    lda map_row_offets.lo, x 
    sta scroll_offset_lo
    lda map_row_offets.hi, x 
    sta scroll_offset_hi

    // add x
    lda scroll_offset_lo
    clc
    adc background_x_pos
    sta scroll_offset_lo
    bcc !+ 
    inc scroll_offset_hi
!:
}

///////////////////////////////////////////////////////////////////
// name: shift_screen_up
// Parameters:
//      screen_src: address of the buffer holding the source data
//      screen_dst: address of the buffer that will receive the
//                  data
// Descriptoin: Shifts the screen up and populate the new row from 
//              the screen map
////////////////////////////////////////////////////////////////////
.macro shift_screen_up(screen_src, screen_dst) {

    // lda #RED
    // sta vic.border_colour

    ldx #0
!:
    lda screen_src + SCREEN_WIDTH, x
    sta screen_dst, x 
    inx 
    bne !-

    ldx #0
!:
    lda screen_src + SCREEN_WIDTH + 256, x
    sta screen_dst + 256, x 
    inx 
    bne !-

    ldx #0
!:
    lda screen_src + SCREEN_WIDTH + 512, x
    sta screen_dst + 512, x 
    inx 
    bne !-

    ldx #0
!:
    lda screen_src + SCREEN_WIDTH + [256 * 3], x
    sta screen_dst +  [256 * 3], x 
    inx 
    cpx #235
    bne !-

    // locate the map char start address based on background y pos
    lda background_y_pos
    adc #25
    asl 
    tax
    lda map_row_char_start_addresses, x 
    sta $fb 
    inx 
    lda map_row_char_start_addresses, x
    sta $fc

    // add in the x pos
    lda $fb
    clc
    adc background_x_pos
    sta $fb
    bcc !+ 
    inc $fc 
!:

    ldy #0
!:
    lda ($fb), y
    sta screen_dst + [24 * SCREEN_WIDTH], y 
    iny 
    cpy #SCREEN_WIDTH
    bne !- 
}

///////////////////////////////////////////////////////////////////
// name: shift_screen_down
// Descriptoin: Shifts the screen down and populate the new row from 
//              the screen map
////////////////////////////////////////////////////////////////////
.macro shift_screen_down(screen_src, screen_dst) {
    .const SCREEN_CHAR_BUFFER_1_END_SRC = screen_src + $03e7
    .const SCREEN_CHAR_BUFFER_1_END_DST = screen_dst + $03e7

    ldx #$ff
!:
    lda SCREEN_CHAR_BUFFER_1_END_SRC - SCREEN_WIDTH - $ff, x
    sta SCREEN_CHAR_BUFFER_1_END_DST - $ff, x 
    dex 
    bne !-

    ldx #$ff
!:
    lda SCREEN_CHAR_BUFFER_1_END_SRC - $ff - SCREEN_WIDTH - $ff, x
    sta SCREEN_CHAR_BUFFER_1_END_DST - $ff - $ff,  x 
    dex 
    bne !-

    ldx #$ff
!:
    lda SCREEN_CHAR_BUFFER_1_END_SRC - $1fe - SCREEN_WIDTH - $ff, x
    sta SCREEN_CHAR_BUFFER_1_END_DST - $1fe - $ff,  x 
    dex 
    bne !-

    ldx #$ff
!:
    lda SCREEN_CHAR_BUFFER_1_END_SRC - $2fd - SCREEN_WIDTH - $ff, x
    sta SCREEN_CHAR_BUFFER_1_END_DST - $2fd - $ff,  x 
    dex 
    cpx #20
    bne !-

    // get an index of the start of the correct character data row
    lda background_y_pos
    asl 
    tax
    lda map_row_char_start_addresses, x 
    sta $fb 
    inx 
    lda map_row_char_start_addresses, x
    sta $fc

    // add in the x pos
    lda $fb
    clc
    adc background_x_pos
    sta $fb
    bcc !+ 
    inc $fc 
!:

    // copy the row to the start of the screen
    ldy #0
!:
    lda ($fb), y
    sta screen_dst, y 
    iny 
    cpy #SCREEN_WIDTH
    bne !- 
}

    // resets the zp to the original values so they can be re-used
    // for calculations
.macro reset_zp() {
    lda #<vic.colour_ram
    sta $f7
    lda #>vic.colour_ram
    sta $f8

    lda #<map_colour_data
    sta $f9
    lda #>map_colour_data
    sta $fa

    lda #<map_character_codes
    sta $fb
    lda #>map_character_codes
    sta $fc

    lda #<SCREEN_CHAR_BUFFER_1
    sta $fd
    lda #>SCREEN_CHAR_BUFFER_1
    sta $fe
}

// Character bitmap definitions 2k
*=$2000
	.byte	$3C, $66, $6E, $6E, $60, $62, $3C, $00
	.byte	$18, $3C, $66, $7E, $66, $66, $66, $00
	.byte	$7C, $66, $66, $7C, $66, $66, $7C, $00
	.byte	$3C, $66, $60, $60, $60, $66, $3C, $00
	.byte	$78, $6C, $66, $66, $66, $6C, $78, $00
	.byte	$7E, $60, $60, $78, $60, $60, $7E, $00
	.byte	$7E, $60, $60, $78, $60, $60, $60, $00
	.byte	$3C, $66, $60, $6E, $66, $66, $3C, $00
	.byte	$66, $66, $66, $7E, $66, $66, $66, $00
	.byte	$3C, $18, $18, $18, $18, $18, $3C, $00
	.byte	$1E, $0C, $0C, $0C, $0C, $6C, $38, $00
	.byte	$66, $6C, $78, $70, $78, $6C, $66, $00
	.byte	$60, $60, $60, $60, $60, $60, $7E, $00
	.byte	$63, $77, $7F, $6B, $63, $63, $63, $00
	.byte	$66, $76, $7E, $7E, $6E, $66, $66, $00
	.byte	$3C, $66, $66, $66, $66, $66, $3C, $00
	.byte	$7C, $66, $66, $7C, $60, $60, $60, $00
	.byte	$3C, $66, $66, $66, $66, $3C, $0E, $00
	.byte	$7C, $66, $66, $7C, $78, $6C, $66, $00
	.byte	$3C, $66, $60, $3C, $06, $66, $3C, $00
	.byte	$7E, $18, $18, $18, $18, $18, $18, $00
	.byte	$66, $66, $66, $66, $66, $66, $3C, $00
	.byte	$66, $66, $66, $66, $66, $3C, $18, $00
	.byte	$63, $63, $63, $6B, $7F, $77, $63, $00
	.byte	$66, $66, $3C, $18, $3C, $66, $66, $00
	.byte	$66, $66, $66, $3C, $18, $18, $18, $00
	.byte	$7E, $06, $0C, $18, $30, $60, $7E, $00
	.byte	$3C, $30, $30, $30, $30, $30, $3C, $00
	.byte	$0C, $12, $30, $7C, $30, $62, $FC, $00
	.byte	$3C, $0C, $0C, $0C, $0C, $0C, $3C, $00
	.byte	$00, $18, $3C, $7E, $18, $18, $18, $18
	.byte	$00, $10, $30, $7F, $7F, $30, $10, $00
	.byte	$00, $00, $00, $00, $00, $00, $00, $00
	.byte	$18, $18, $18, $18, $00, $00, $18, $00
	.byte	$66, $66, $66, $00, $00, $00, $00, $00
	.byte	$66, $66, $FF, $66, $FF, $66, $66, $00
	.byte	$18, $3E, $60, $3C, $06, $7C, $18, $00
	.byte	$62, $66, $0C, $18, $30, $66, $46, $00
	.byte	$3C, $66, $3C, $38, $67, $66, $3F, $00
	.byte	$06, $0C, $18, $00, $00, $00, $00, $00
	.byte	$0C, $18, $30, $30, $30, $18, $0C, $00
	.byte	$30, $18, $0C, $0C, $0C, $18, $30, $00
	.byte	$00, $66, $3C, $FF, $3C, $66, $00, $00
	.byte	$00, $18, $18, $7E, $18, $18, $00, $00
	.byte	$00, $00, $00, $00, $00, $18, $18, $30
	.byte	$00, $00, $00, $7E, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $18, $18, $00
	.byte	$00, $03, $06, $0C, $18, $30, $60, $00
	.byte	$3C, $66, $6E, $76, $66, $66, $3C, $00
	.byte	$18, $18, $38, $18, $18, $18, $7E, $00
	.byte	$3C, $66, $06, $0C, $30, $60, $7E, $00
	.byte	$3C, $66, $06, $1C, $06, $66, $3C, $00
	.byte	$06, $0E, $1E, $66, $7F, $06, $06, $00
	.byte	$7E, $60, $7C, $06, $06, $66, $3C, $00
	.byte	$3C, $66, $60, $7C, $66, $66, $3C, $00
	.byte	$7E, $66, $0C, $18, $18, $18, $18, $00
	.byte	$3C, $66, $66, $3C, $66, $66, $3C, $00
	.byte	$3C, $66, $66, $3E, $06, $66, $3C, $00
	.byte	$00, $00, $18, $00, $00, $18, $00, $00
	.byte	$00, $00, $18, $00, $00, $18, $18, $30
	.byte	$0E, $18, $30, $60, $30, $18, $0E, $00
	.byte	$00, $00, $7E, $00, $7E, $00, $00, $00
	.byte	$70, $18, $0C, $06, $0C, $18, $70, $00
	.byte	$3C, $66, $06, $0C, $18, $00, $18, $00
	.byte	$00, $00, $00, $FF, $FF, $00, $00, $00
	.byte	$08, $1C, $3E, $7F, $7F, $1C, $3E, $00
	.byte	$18, $18, $18, $18, $18, $18, $18, $18
	.byte	$00, $00, $00, $FF, $FF, $00, $00, $00
	.byte	$00, $00, $FF, $FF, $00, $00, $00, $00
	.byte	$00, $FF, $FF, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $FF, $FF, $00, $00
	.byte	$30, $30, $30, $30, $30, $30, $30, $30
	.byte	$0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C
	.byte	$00, $00, $00, $E0, $F0, $38, $18, $18
	.byte	$18, $18, $1C, $0F, $07, $00, $00, $00
	.byte	$18, $18, $38, $F0, $E0, $00, $00, $00
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $FF, $FF
	.byte	$C0, $E0, $70, $38, $1C, $0E, $07, $03
	.byte	$03, $07, $0E, $1C, $38, $70, $E0, $C0
	.byte	$FF, $FF, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$FF, $FF, $03, $03, $03, $03, $03, $03
	.byte	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00
	.byte	$00, $00, $00, $00, $00, $FF, $FF, $00
	.byte	$36, $7F, $7F, $7F, $3E, $1C, $08, $00
	.byte	$60, $60, $60, $60, $60, $60, $60, $60
	.byte	$00, $00, $00, $07, $0F, $1C, $18, $18
	.byte	$C3, $E7, $7E, $3C, $3C, $7E, $E7, $C3
	.byte	$00, $3C, $7E, $66, $66, $7E, $3C, $00
	.byte	$18, $18, $66, $66, $18, $18, $3C, $00
	.byte	$06, $06, $06, $06, $06, $06, $06, $06
	.byte	$08, $1C, $3E, $7F, $3E, $1C, $08, $00
	.byte	$18, $18, $18, $FF, $FF, $18, $18, $18
	.byte	$C0, $C0, $30, $30, $C0, $C0, $30, $30
	.byte	$18, $18, $18, $18, $18, $18, $18, $18
	.byte	$00, $00, $03, $3E, $76, $36, $36, $00
	.byte	$FF, $7F, $3F, $1F, $0F, $07, $03, $01
	.byte	$00, $00, $00, $00, $00, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
	.byte	$00, $00, $00, $00, $FF, $FF, $FF, $FF
	.byte	$FF, $00, $00, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $00, $00, $FF
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$CC, $CC, $33, $33, $CC, $CC, $33, $33
	.byte	$03, $03, $03, $03, $03, $03, $03, $03
	.byte	$00, $00, $00, $00, $CC, $CC, $33, $33
	.byte	$FF, $FE, $FC, $F8, $F0, $E0, $C0, $80
	.byte	$03, $03, $03, $03, $03, $03, $03, $03
	.byte	$18, $18, $18, $1F, $1F, $18, $18, $18
	.byte	$00, $00, $00, $00, $0F, $0F, $0F, $0F
	.byte	$18, $18, $18, $1F, $1F, $00, $00, $00
	.byte	$00, $00, $00, $F8, $F8, $18, $18, $18
	.byte	$00, $00, $00, $00, $00, $00, $FF, $FF
	.byte	$00, $00, $00, $1F, $1F, $18, $18, $18
	.byte	$18, $18, $18, $FF, $FF, $00, $00, $00
	.byte	$00, $00, $00, $FF, $FF, $18, $18, $18
	.byte	$18, $18, $18, $F8, $F8, $18, $18, $18
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0
	.byte	$07, $07, $07, $07, $07, $07, $07, $07
	.byte	$FF, $FF, $00, $00, $00, $00, $00, $00
	.byte	$FF, $FF, $FF, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $FF, $FF, $FF
	.byte	$03, $03, $03, $03, $03, $03, $FF, $FF
	.byte	$00, $00, $00, $00, $F0, $F0, $F0, $F0
	.byte	$0F, $0F, $0F, $0F, $00, $00, $00, $00
	.byte	$18, $18, $18, $F8, $F8, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $00, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F
	.byte	$C3, $99, $91, $91, $9F, $99, $C3, $FF
	.byte	$E7, $C3, $99, $81, $99, $99, $99, $FF
	.byte	$83, $99, $99, $83, $99, $99, $83, $FF
	.byte	$C3, $99, $9F, $9F, $9F, $99, $C3, $FF
	.byte	$87, $93, $99, $99, $99, $93, $87, $FF
	.byte	$81, $9F, $9F, $87, $9F, $9F, $81, $FF
	.byte	$81, $9F, $9F, $87, $9F, $9F, $9F, $FF
	.byte	$C3, $99, $9F, $91, $99, $99, $C3, $FF
	.byte	$99, $99, $99, $81, $99, $99, $99, $FF
	.byte	$C3, $E7, $E7, $E7, $E7, $E7, $C3, $FF
	.byte	$E1, $F3, $F3, $F3, $F3, $93, $C7, $FF
	.byte	$99, $93, $87, $8F, $87, $93, $99, $FF
	.byte	$9F, $9F, $9F, $9F, $9F, $9F, $81, $FF
	.byte	$9C, $88, $80, $94, $9C, $9C, $9C, $FF
	.byte	$99, $89, $81, $81, $91, $99, $99, $FF
	.byte	$C3, $99, $99, $99, $99, $99, $C3, $FF
	.byte	$83, $99, $99, $83, $9F, $9F, $9F, $FF
	.byte	$C3, $99, $99, $99, $99, $C3, $F1, $FF
	.byte	$83, $99, $99, $83, $87, $93, $99, $FF
	.byte	$C3, $99, $9F, $C3, $F9, $99, $C3, $FF
	.byte	$81, $E7, $E7, $E7, $E7, $E7, $E7, $FF
	.byte	$99, $99, $99, $99, $99, $99, $C3, $FF
	.byte	$99, $99, $99, $99, $99, $C3, $E7, $FF
	.byte	$9C, $9C, $9C, $94, $80, $88, $9C, $FF
	.byte	$99, $99, $C3, $E7, $C3, $99, $99, $FF
	.byte	$99, $99, $99, $C3, $E7, $E7, $E7, $FF
	.byte	$81, $F9, $F3, $E7, $CF, $9F, $81, $FF
	.byte	$C3, $CF, $CF, $CF, $CF, $CF, $C3, $FF
	.byte	$F3, $ED, $CF, $83, $CF, $9D, $03, $FF
	.byte	$C3, $F3, $F3, $F3, $F3, $F3, $C3, $FF
	.byte	$FF, $E7, $C3, $81, $E7, $E7, $E7, $E7
	.byte	$FF, $EF, $CF, $80, $80, $CF, $EF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$E7, $E7, $E7, $E7, $FF, $FF, $E7, $FF
	.byte	$99, $99, $99, $FF, $FF, $FF, $FF, $FF
	.byte	$99, $99, $00, $99, $00, $99, $99, $FF
	.byte	$E7, $C1, $9F, $C3, $F9, $83, $E7, $FF
	.byte	$9D, $99, $F3, $E7, $CF, $99, $B9, $FF
	.byte	$C3, $99, $C3, $C7, $98, $99, $C0, $FF
	.byte	$F9, $F3, $E7, $FF, $FF, $FF, $FF, $FF
	.byte	$F3, $E7, $CF, $CF, $CF, $E7, $F3, $FF
	.byte	$CF, $E7, $F3, $F3, $F3, $E7, $CF, $FF
	.byte	$FF, $99, $C3, $00, $C3, $99, $FF, $FF
	.byte	$FF, $E7, $E7, $81, $E7, $E7, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $CF
	.byte	$FF, $FF, $FF, $81, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $FF
	.byte	$FF, $FC, $F9, $F3, $E7, $CF, $9F, $FF
	.byte	$C3, $99, $91, $89, $99, $99, $C3, $FF
	.byte	$E7, $E7, $C7, $E7, $E7, $E7, $81, $FF
	.byte	$C3, $99, $F9, $F3, $CF, $9F, $81, $FF
	.byte	$C3, $99, $F9, $E3, $F9, $99, $C3, $FF
	.byte	$F9, $F1, $E1, $99, $80, $F9, $F9, $FF
	.byte	$81, $9F, $83, $F9, $F9, $99, $C3, $FF
	.byte	$C3, $99, $9F, $83, $99, $99, $C3, $FF
	.byte	$81, $99, $F3, $E7, $E7, $E7, $E7, $FF
	.byte	$C3, $99, $99, $C3, $99, $99, $C3, $FF
	.byte	$C3, $99, $99, $C1, $F9, $99, $C3, $FF
	.byte	$FF, $FF, $E7, $FF, $FF, $E7, $FF, $FF
	.byte	$FF, $FF, $E7, $FF, $FF, $E7, $E7, $CF
	.byte	$F1, $E7, $CF, $9F, $CF, $E7, $F1, $FF
	.byte	$FF, $FF, $81, $FF, $81, $FF, $FF, $FF
	.byte	$8F, $E7, $F3, $F9, $F3, $E7, $8F, $FF
	.byte	$C3, $99, $F9, $F3, $E7, $FF, $E7, $FF
	.byte	$FF, $FF, $FF, $00, $00, $FF, $FF, $FF
	.byte	$F7, $E3, $C1, $80, $80, $E3, $C1, $FF
	.byte	$E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $00, $00, $FF, $FF, $FF
	.byte	$FF, $FF, $00, $00, $FF, $FF, $FF, $FF
	.byte	$FF, $00, $00, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $00, $00, $FF, $FF
	.byte	$CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
	.byte	$F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3
	.byte	$FF, $FF, $FF, $1F, $0F, $C7, $E7, $E7
	.byte	$E7, $E7, $E3, $F0, $F8, $FF, $FF, $FF
	.byte	$E7, $E7, $C7, $0F, $1F, $FF, $FF, $FF
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $00, $00
	.byte	$3F, $1F, $8F, $C7, $E3, $F1, $F8, $FC
	.byte	$FC, $F8, $F1, $E3, $C7, $8F, $1F, $3F
	.byte	$00, $00, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$00, $00, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$FF, $C3, $81, $81, $81, $81, $C3, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $00, $00, $FF
	.byte	$C9, $80, $80, $80, $C1, $E3, $F7, $FF
	.byte	$9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F
	.byte	$FF, $FF, $FF, $F8, $F0, $E3, $E7, $E7
	.byte	$3C, $18, $81, $C3, $C3, $81, $18, $3C
	.byte	$FF, $C3, $81, $99, $99, $81, $C3, $FF
	.byte	$E7, $E7, $99, $99, $E7, $E7, $C3, $FF
	.byte	$F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9
	.byte	$F7, $E3, $C1, $80, $C1, $E3, $F7, $FF
	.byte	$E7, $E7, $E7, $00, $00, $E7, $E7, $E7
	.byte	$3F, $3F, $CF, $CF, $3F, $3F, $CF, $CF
	.byte	$E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7
	.byte	$FF, $FF, $FC, $C1, $89, $C9, $C9, $FF
	.byte	$00, $80, $C0, $E0, $F0, $F8, $FC, $FE
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
	.byte	$FF, $FF, $FF, $FF, $00, $00, $00, $00
	.byte	$00, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $00
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$33, $33, $CC, $CC, $33, $33, $CC, $CC
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$FF, $FF, $FF, $FF, $33, $33, $CC, $CC
	.byte	$00, $01, $03, $07, $0F, $1F, $3F, $7F
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$E7, $E7, $E7, $E0, $E0, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0
	.byte	$E7, $E7, $E7, $E0, $E0, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $07, $07, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $00, $00
	.byte	$FF, $FF, $FF, $E0, $E0, $E7, $E7, $E7
	.byte	$E7, $E7, $E7, $00, $00, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $00, $00, $E7, $E7, $E7
	.byte	$E7, $E7, $E7, $07, $07, $E7, $E7, $E7
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
	.byte	$F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	.byte	$00, $00, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$00, $00, $00, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $00, $00, $00
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $00, $00
	.byte	$FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F
	.byte	$F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF
	.byte	$E7, $E7, $E7, $07, $07, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0

// screen character data
*=$3000
map_character_codes:
	// character codes (4000 bytes)
    .byte $20,$20,$20,$20,$95,$95,$95,$95,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59
    .byte $20,$20,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20
    .byte $20,$95,$95,$95,$95,$95,$95,$95,$20,$95,$20,$20,$20,$95,$20,$20,$20,$20,$95,$95,$20,$20,$95,$95,$95,$20,$20,$67,$20,$20,$20,$20,$67,$20,$20,$67,$20,$67,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$95,$95,$95,$95,$95,$95,$20,$95,$20,$20,$20,$95,$20,$20,$95,$95,$20,$20,$20,$20,$20,$20,$95,$20,$20,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$95,$95,$95,$95,$95,$95,$20,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$95,$20,$20,$20,$95,$95,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$67,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$95,$95,$95,$95,$95,$95,$20,$20,$20,$20,$95,$20,$20,$20,$20,$20,$20,$95,$95,$95,$95,$20,$20,$20,$20,$67,$20,$20,$20,$20,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$EB,$EB,$EB,$20,$EB,$EB,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$95,$95,$95,$95,$95,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$95,$95,$95,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$67,$67,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$95,$95,$20,$20,$20,$20,$6A,$6A,$6A,$6A,$6A,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$EB,$20,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$6A,$6A,$20,$20,$20,$20,$6A,$6A,$6A,$6A,$6A,$6A,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$6A,$6A,$6A,$6A,$6A,$20,$20,$20,$20,$20,$20,$6A,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$EB,$20,$20,$EB,$EB,$EB,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$6A,$6A,$6A,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$6A,$6A,$20,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$EB,$EB,$20,$EB,$EB,$EB,$EB,$EB,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$6A,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$80,$20,$20,$20,$80,$80,$80,$20,$20,$20,$EB,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$20,$EB,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$20,$20,$6A,$6A,$20,$6A,$6A,$20,$20,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$EB,$EB,$EB,$20,$EB,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$6A,$20,$20,$20,$6A,$6A,$6A,$6A,$6A,$20,$20,$20,$20,$20,$20,$6A,$6A,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$6A,$6A,$6A,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$EB,$EB,$EB,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$6A,$20,$20,$20,$20,$20,$20,$20,$6A,$20,$20,$6A,$6A,$20,$20,$20,$80,$80,$20,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$6A,$6A,$6A,$20,$20,$20,$20,$6A,$20,$6A,$6A,$20,$20,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$20,$20,$80,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$6A,$6A,$6A,$6A,$6A,$6A,$6A,$20,$20,$80,$80,$80,$80,$80,$80,$20,$80,$80,$80,$80,$80,$80,$20,$80,$80,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$80,$20,$20,$80,$80,$80,$80,$80,$20,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$20,$20,$20,$80,$20,$80,$80,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$80,$20,$20,$20,$80,$20,$20,$80,$20,$20,$20,$80,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$80,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$80,$20,$20,$20,$80,$20,$20,$20,$80,$20,$20,$20,$20,$80,$80,$20,$20,$20,$80,$80,$20,$80,$80,$20,$80,$80,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$80,$20,$20,$20,$20,$20,$80,$20,$20,$20,$80,$80,$20,$20,$80,$80,$20,$80,$20,$20,$80,$80,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$80,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$80,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$59,$59,$20,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$80,$80,$80,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$59,$59,$59,$20,$20,$20,$59,$59,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$80,$20,$80,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$59,$59,$59,$59,$20,$20,$59,$59,$59,$59,$59,$20,$20,$59,$59,$59,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$EB,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$80,$80,$20,$80,$80,$80,$80,$80,$80,$80,$80,$80,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$59,$20,$59,$59,$59,$59,$59,$59,$20,$20,$59,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$59,$20,$59,$59,$59,$59,$20,$59,$20,$20,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$59,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$EB,$20,$EB,$EB,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$59,$59,$59,$59,$20,$20,$59,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$59,$59,$59,$20,$59,$59,$20,$20,$20,$20,$20,$59,$59,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$EB,$EB,$EB,$EB,$EB,$EB,$20,$20,$20,$20,$20,$EB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$59,$59,$59,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$59,$59,$20,$59,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$59,$59,$59,$59,$59,$59,$59,$59,$59,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

// screen color data
*=$4000
map_colour_data:
    // color codes (4000 bytes)
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    .byte $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
