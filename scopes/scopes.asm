
BasicUpstart2(main)

/*
This might be an interesting way of mapping important port registers and memory
locations so that the code is easier to understand.
*/
*=$0400 "Screen Memory" virtual
screen: {
    zone1: .fill 80,  0
    zone2: .fill 80,  0
    zone3: .fill 80,  0
}

*=$d000 "VIC II Registers" virtual 
vic: {
    sprite0: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite1: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite2: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite3: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite4: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite5: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite6: {
        xpos: .byte 0
        ypos: .byte 0
    }
    sprite7: {
        xpos: .byte 0
        ypos: .byte 0
    }
    spriteXMSB: .byte 0
}

*=$1000 "Main Program"
main:

    ldx #0
    lda #1
!:  
    sta screen.zone1, x
    sta screen.zone3, x
    inx
    pha
    txa
    cmp #80
    beq done
    tax 
    pla
    jmp !-

done:
    pla
    rts


