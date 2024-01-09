/*****************************************************
Map of the VIC II Registers
*****************************************************/

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

    // Bits:
    //  7: Raster Compare   
    //  6: Extended Colour Text   
    //  5: Bitmap Mode   
    //  4: Blank Screen to Border Colour
    //  3: Select 24/25 Row Text Display 0: 24 Rows 1: 25 rows
    //  2 - 0: Smooth Y Scroll
    controlRegister: .byte 0    
}
