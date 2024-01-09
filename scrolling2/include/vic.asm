/*****************************************************
Map of the VIC II Registers
*****************************************************/

// *=$d000 "VIC II Registers" virtual 
// vic_registers: {
//     sprite0: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite1: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite2: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite3: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite4: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite5: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite6: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     sprite7: {
//         xpos: .byte 0
//         ypos: .byte 0
//     }
//     spriteXMSB: .byte 0

//     // Bits:
//     //  7: Raster Compare   
//     //  6: Extended Colour Text   
//     //  5: Bitmap Mode   
//     //  4: Blank Screen to Border Colour
//     //  3: Select 24/25 Row Text Display 0: 24 Rows 1: 25 rows
//     //  2 - 0: Smooth Y Scroll
//     controlRegister: .byte 0    
// }



.namespace vic {
    
    .label CR_RASTER_COMPARE                = $80
    .label CR_EXTENDED_COLOUR_TEXT          = $40
    .label CR_BITMAP_MODE                   = $20
    .label CR_BLANK_SCREEN_TO_BORDER_COLOUR = $10
    .label CR_24_25_ROW_SELECT              = $08
    .label control_register     = $d011

    .label raster_scan_line     = $d012

    // Bits 0:3 Character Definition Base, Bits 4:8 Screen Base Address
    .label memory_pointers      = $d018

    // set to 0 to clear the current interrupt (use asl as this is faster)
    .label interrupt_flag      = $d019

    // 1 = enable, 0 = disable
    .label interrupt_enable     = $d01a
    
    .label border_colour        = $d020
    .label background_0_colour  = $d021
    .label background_1_colour  = $d022
    .label background_2_colour  = $d023
    .label background_3_colour  = $d024
    .label colour_ram           = $d800
}
