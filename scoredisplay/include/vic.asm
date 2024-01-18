/*****************************************************
Map of the VIC II Registers
*****************************************************/

.namespace vic {
    
    .label CR_RASTER_COMPARE                = $80
    .label CR_EXTENDED_COLOUR_TEXT          = $40
    .label CR_BITMAP_MODE                   = $20
    .label CR_BLANK_SCREEN_TO_BORDER_COLOUR = $10
    .label CR_24_25_ROW_SELECT              = $08
    .label control_register                 = $d011
    .label raster_scan_line                 = $d012

    .label CR2_38_40_COL_SELECT             = $08
    .label control_register_2               = $d016

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
