    LIST p=16F628A          ; Tell assembler what micro is in use
    include "P16F628A.inc"  ; Include definitions for this device

	__config _CP_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT & _MCLRE_ON

    RADIX   DEC ; Default base for numbers is Decimal
    ERRORLEVEL -1302; Disable the assembler messages for RAM bank check

; Data addresses definition  ---------------------
cblock H'20'     ; Start of general purpose registers
    ; Framework data ----------
	vmem:8		; "Video" memory

    dd_addr     ; Data Address used for display data transmission
    dd_data     ; Data used for display data transmission
    dd_byte     ; Temp byte to send
    dd_cnt      ; Bit counter for display data transmission

    last_bttns  ; Keeps track of pressed buttons, and those
    down_bttns  ; that changed from last reading.
    new_bttn_dwn; 

    scroll_strhi; Scrolling text parameter: HIGH address of string table
    scroll_strlo; Scrolling text parameter: LOW address of string table
    scroll_strsz; Scrolling text parameter: String size (length)

    st_str_off  ; Scrolling text, string offset
    st_max_off  ; Scrolling text, max offset
    st_char_ndx ; Scrolling text char index
    st_col_ref  ; Scrolling text column reference

    font_char   ; font character
    font_ndx    ; Actual font char index in table
    char_col    ; font column

	shift_cnt	; Variables for bit shifting
	shift_val	; operation

    vmem_col	; Variables for some VRAM 
	vmem_row	; routines and operations
	vmem_data	;

    ; Misc data ---------------
    math_x      ; X value for math functions
    math_y      ; Y value for math functions
    math_tmp    ; Temporary variable used internally by math functions

    ; Game data ---------------
    gravity_cd  ; Gravity count-down
    tetris_lvl  ; Current tetris level
    tetris_score; 
    score_digits; 4 bits per digit of score
    tetro_n     ; Current tetromino index
    tetro_d0    ; Current tetromino graphics data 0
    tetro_d1    ; Current tetromino graphics data 0
    tetro_r     ; Tetromino rotation
    tetro_x     ; Tetromino X position
    tetro_y     ; Tetromino Y position
    tetro_draw_r; Tetromino draw row counter
    tetro_data  ; Tetro pixel data to draw
    tetro_maxxp ; Max X pixel drawn
    tetro_flags ; Tetro flags when drawing it. Will check whether we are next to the walls or if we will collide with the background
    tetro_oldx  ; Backup of tetro_x before checking if movement is valid
    tetro_oldy  ; Backup of tetro_y before checking if movement is valid
    tetro_oldr  ; Backup of tetro_r before checking if movement is valid
    tetro_linem ; Mask of lines made when reducing lines
    tetro_tmpi  ; Temp variable for tetro processing
    tetro_tmpj  ; Temp variable for tetro processing
    tetro_tmpk  ; Temp variable for tetro processing
    ttr_field:8 ; Tetris field
endc

; Make sure the following registers are at the bottom area of the RAM,
; so they can be accessed from any page.
w_bkup         equ     H'7f'
status_bkup    equ     H'7e'
eeprom_addr    equ     H'7d'
frame_tick     equ     H'7c'
frame_cntdown  equ     H'7b'

; Constants
FIRST_FONT_CHAR   equ     ' '
LAST_FONT_CHAR    equ     '`'
BUTTONS_MASK      equ     B'00001111' ; We want to read all four buttons
                                      ; Connected to RA3:RA0

BUTTON_UP         equ   0
BUTTON_DOWN       equ   1
BUTTON_RIGHT      equ   2
BUTTON_LEFT       equ   3

; ---- I/O PINS ---------------
M7219_PORT        equ     PORTA
M7219_DIN         equ     6
M7219_CLK         equ     7
M7219_NOTCS       equ     4

BUTTONS_PORT      equ     PORTA

LED_PIN           equ     7


; Game flags ------------------
TETRO_FLAG_WALL_COLLISION           equ     1
TETRO_FLAG_FIELD_COLLISION          equ     2   ; Collision with the floor, or with other pieces.
TETRO_FLAG_BIT_BG_COLLISION         equ     3
; These are execution flags that should not be cleared when drawing
TETRO_FLAG_CLEARMASK                equ     B'11000000'
TETRO_EXFLAG_BIT_TEST_DONTDRAW      equ     7

SPAWN_TETRO_Y                       equ     -1


; --- Other Settings ----------
DISPL_BRIGHTNESS  equ     0     ; A value from 0 to 0xF -- 0 is already bright enough.
                                ; Check max7219_init to see how you can change this at runtime, if desired.

; Display driver (MAX7219) commands
M7219_DECODE      equ     H'9'
M7219_INTENSITY   equ     H'A'
M7219_SCANLIMIT   equ     H'B'
M7219_SHUTDOWN    equ     H'C'
M7219_TEST        equ     H'F'

; #######################################################################
; ##                                                                   ##
; ##                            M A C R O S                            ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; [MACRO] scroll_text
;   Scrolls text defined at PGM address string_table, with (const) length
;   str_len_c
; -----------------------------------------------------------------------
scroll_text macro string_table, str_len_c
    movlw   HIGH(string_table)
    movwf   scroll_strhi

    movlw   LOW(string_table)
    movwf   scroll_strlo

    movlw   str_len_c
    movwf   scroll_strsz
    
    call    scroll_text_fx
    endm

; -----------------------------------------------------------------------
; [MACRO] shift_right_w_f
;   Shifts the value of W, file_f places to the right. Returns with
;   value on W. Not really efficient but it works.
; -----------------------------------------------------------------------
shift_right_w_f macro file_f
	local	_rsh_loop
	local	_rsh_end
	movwf	shift_val
	movf	file_f, W
	movwf	shift_cnt

_rsh_loop
    movf	shift_cnt, F; Move shift_cnt onto itself. this should update the z flag
    btfsc   STATUS, Z   ; End the loop if shift_cnt is 0.
    goto _rsh_end

    bcf		STATUS, C
	rrf		shift_val, F

	decf    shift_cnt, F
    goto	_rsh_loop

_rsh_end
	movf	shift_val, W
    endm

; -----------------------------------------------------------------------
; [MACRO] max7219_send_cc
;   Sends a (constant) byte to a (constant) address in the MAX7219
;   controller
; -----------------------------------------------------------------------
max7219_send_cc   macro   const_addr, const_data
    movlw   const_addr
    movwf   dd_addr
    movlw   const_data
    movwf   dd_data
    call    max7219_send
    endm

; #######################################################################
; ##                                                                   ##
; ##                       E N T R Y   P O I N T                       ##
; ##                                                                   ##
; #######################################################################
    org     H'0000' ; Reset vector
    goto    init
    
    org     H'0004' ; Interrupt vector
; -----------------------------------------------------------------------
; interrupt_handler
;   Handlin' dem interrupts
; -----------------------------------------------------------------------
interrupt_handler:
    ; Backup W and STATUS
    movwf   w_bkup
    swapf   STATUS, W   ; Will be a swapped version of STATUS, but this instruction
                        ; affects no flags
    movwf   status_bkup

    ; Check that we are here because of a Timer1 interrupt
    bsf     STATUS, RP0
    btfss   PIR1, TMR1IF
    goto    interrupt_handler_end

timer1_int_handler:
    ; Increase our refresh "tick"
    decf    frame_tick, F
    ; Decrease a countdown. Useful for waiting X frames
    decf    frame_cntdown, F

    ; Back to Page 0
    bcf     STATUS, RP0

    ; --------- TEST -----------
    btfss   tetro_flags, TETRO_FLAG_WALL_COLLISION
    bcf     PORTB, LED_PIN
    btfsc   tetro_flags, TETRO_FLAG_WALL_COLLISION
    bsf     PORTB, LED_PIN
    ; --------- /TEST ----------

    ; Update the display
display_update:
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

_disp_upd_loop:
	; Send the contents of the INDF register to the MAX7219, address vmem_col
	movf    vmem_col, W
    movwf   dd_addr
    movf    INDF, W
    movwf   dd_data
    call    max7219_send

	incf	FSR				; Increase FSR to move to next byte in VMEM
	decfsz	vmem_col, F		; Decrease our byte counter and test if we've reached zero.
	goto	_disp_upd_loop

    ; Restart the interrupt timer
    call    start_refrsh_timer

interrupt_handler_end:
    bcf     STATUS, RP0
    ; Clear all interrupt flags in INTCON
    movlw   B'11111000'
    andlw   INTCON
    ; Also clear peripheral interrupt flags
    clrf    PIR1

    ; Restore W and STATUS
    swapf   status_bkup, W
    movwf   STATUS
    ; Similar SWAP trick to restore W without affecting STATUS
    swapf   w_bkup, F
    swapf   w_bkup, W
    retfie

; -----------------------------------------------------------------------
; init
;   Initialize stuff
; -----------------------------------------------------------------------
init:
    ; Setup -----------
    ; Select bank 0
    bcf     STATUS, RP1
    bcf     STATUS, RP0 

    ; Disable interrupts
    bcf     INTCON, GIE
    ;turn comparators off
    movlw   H'07'
    movwf   CMCON
 
    ; Since we do a lot of computed jumps in this code it doesn't hurt
    ; to make sure PCLATH is clear on reset.
    clrf    PCLATH

    ; I/O PORT direction
    bsf     STATUS, RP0 ; Select bank 1
    clrf    TRISB       ; All PORTB as output
    clrf    TRISA       ; And also all PORTA. We will change the pins we need later
    movlw   BUTTONS_MASK
    movwf   BUTTONS_PORT; Buttons as input

    ; Back to bank 0
    bcf     STATUS, RP0

    ; Initialize display
    call    max7219_init
    
    ; Configure and start Timer1
    call    init_timer    
    call    start_refrsh_timer
    
    ; Initialize the serial port
    movlw   USART_BAUD_38400
    call    USART_init

; -----------------------------------------------------------------------
; main_loop
;   Self-explanatory.
; -----------------------------------------------------------------------
main_loop:
    clrf   tetro_n
    clrf   tetro_r

    movlw   1
    movwf   tetris_lvl
    
    clrf    tetris_score

    call    set_gravity
    call    clear_tetris_field

tetro_test:
    ; Get tetromino from frame_tick
    movf    frame_tick, W
    andlw   B'111'
    movwf   tetro_n
    ; Check limit. If tetro_n is 7 we set it to 0
    sublw   7
    btfsc   STATUS, Z
    clrf    tetro_n
    

    clrf    tetro_r

    movlw   2
    movwf   tetro_x
    
    movlw   SPAWN_TETRO_Y
    movwf   tetro_y

game_loop:
    ; Wait 1 frames before processing (drawing, checking collisions, reading buttons, etc)
    movlw   1
    call    delay_w_frames
    
    call    vmem_clear
    call    render_tetris_field

    ; Before we start checking anything let's backup our position and rotation in case the movement we make
    ; this frame is illegal
    movf    tetro_x, W
    movwf   tetro_oldx
    movf    tetro_y, W
    movwf   tetro_oldy
    movf    tetro_r, W
    movwf   tetro_oldr

    ; Reduce gravity countdown
    decfsz  gravity_cd, F
    goto    gravity_done

    ; Let's apply gravity
    incf    tetro_y, F

    ; Reset gravity countdown
    call    set_gravity

gravity_done:
    ; Call this function for drawing
    bsf     tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    btfsc   frame_tick, 2   ; This will give us a blinking piece, which helps set it apart from the field
    bcf     tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    call    draw_tetromino
    
    ; Before checking buttons let's check flags. In theory the only possible flag we can have here
    ; is "field" (floor or tetris piece collision) due to gravity being applied or collision on spawn.
    btfss   tetro_flags, TETRO_FLAG_FIELD_COLLISION
    goto    no_gravity_collisions

    ; In case of collision we want to revert our Y position
    movf    tetro_oldy, W
    movwf   tetro_y

    ;  Was tetro_y = the spawn position?
    sublw   SPAWN_TETRO_Y
    btfss   STATUS, Z
    goto    no_gameover_yet

gameover:
    ; Make sure we draw the piece without gravity
    ; Clear everything
    call    vmem_clear
    call    render_tetris_field
    ; Draw the piece again in the position it had before the collision
    bcf     tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    call    draw_tetromino

    ; Small animation filling the field from the bottom
    ; Using tetro_tmpi as a counter
    movlw   8
    movwf   tetro_tmpi
    ; And linem as a mask
    clrf    tetro_linem

gameover_fill_loop:
    bsf     STATUS, C
    rlf     tetro_linem, F 
    call    vmem_ior_linem

    ; Wait several frames
    movlw   8
    call    delay_w_frames

    decfsz  tetro_tmpi, F
    goto    gameover_fill_loop

    call    prepare_score_digits

    ; Now scroll a message on screen
    scroll_text     game_over_msg_table, GAME_OVER_MSG_LEN

    ; And then the score message    
    scroll_text     score_msg, SCORE_MSG_LEN
    goto            main_loop

no_gameover_yet:
    ; Clear everything
    call    vmem_clear
    call    render_tetris_field
    ; Draw the piece again in the position it had before the collision
    bcf     tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    call    draw_tetromino

    ; Check for lines made and save that into a mask
    call    check_lines_on_vmem
    movwf   tetro_linem

    ; If no line was made we can skip the whole line process
    btfsc   STATUS, Z
    goto    finished_proc_lines

    ; Let's re-use tetro_draw_r as a counter
    movlw   16
    movwf   tetro_draw_r

blink_lines:
    ; Apply the mask to the whole buffer
    call    vmem_xor_linem
    movlw   4
    call    delay_w_frames
    decfsz  tetro_draw_r, F
    goto    blink_lines
    
    ; We need to reduce now the lines that were made. Since our screen data is
    ; organized in columns instead of rows, this is going to be a bit of a pain.

    ; We will store in tmpi a mask of the pixels we want to keep starting from
    ; the bottom row. We will build this mask iteratively while examining the
    ; mask of "lines that were made" that we got from "check_lines_on_vmem"
    clrf    tetro_tmpi
reduce_lines_cycle:
    ; Compute the complement of tmpi into tmpj
    comf    tetro_tmpi, W
    movwf   tetro_tmpj

    ; We will always check the LSb of the line mask. If no line needs to be reduced
    ; we will keep going, growing our "mask of rows we want to keep" 
    btfss   tetro_linem, 0
    goto    prepare_reduce_next

    ; The process to remove a line using the masks is as follow:
    ; * For each column of blocks, we will compute the right shifted version of it,
    ;   which is basically the same as pushing ALL the blocks 1 row to the bottom.
    ; * Then we will AND (bitwise) the current column data with the tmpi mask we've
    ;   been building (which contains "1" only for "the rows below the cut") to
    ;   obtain the data that we will keep from that column.
    ; * Then we will also filter the shifted column, performing an AND against the
    ;   complement of tmpi. This will give us the row data ABOVE the line we are
    ;   removing.
    ; * Then we we will OR both values.
    ; An example:
    ; 
    ;    TETRIS FIELD        LINEM
    ; | . . . . . . . . |      0
    ; | . . . . . . . . |      0
    ; | . . . . . . . . |      0
    ; | . # . . . . . . |      0
    ; | # # # . . . . # |      0
    ; | # # # # # # # # |      1
    ; | # . # # # . # # |      0
    ; | # # # # # . # # |      0
    ; +-----------------+
    ;   A B C D E F G H
    ;
    ; After 2 iterations checking the LSb of linem, and shifting it "downwards",
    ; building our mask of rows to keep in the meantime, we get to the "1" that
    ; is set. At that point our mask of rows to keep is: 0000 0011
    ;
    ; We will do the procedure above for all columns.
    ; Let's take as an example column B:
    ; tmpi (bottom rows to keep)    : 00000011
    ; tmpj (top rows to keep)       : 11111100
    ; OB = ORIGINAL B COLUMN DATA   : 00011101
    ; SB = SHIFTED  B SHIFTED DATA  : 00001110
    
    ; X = (OB AND tmpi )            : 00000001
    ; Y = (SB AND tmpj )            : 00001100
    ; ----------------------------------------
    ; X OR B (END COLUMN RESULT)    : 00001101
    ;
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col
    
    ; Increase score
    incf    tetris_score, F

reduce_lines_cols_loop:
    ; Read original column AND it with the tmpi mask
    movf    INDF, W
    andwf   tetro_tmpi, W
    ; Save that to vmem_data, temporarily
    movwf   vmem_data

    ; Read the original data again shifted one bit, AND that with the
    ; complement of tmpi (tmpj)
    bcf     STATUS, C
    rrf     INDF, W
    andwf   tetro_tmpj, W

    ; OR that with vmem_data
    iorwf   vmem_data, W
    
    ; Save it into the column
    movwf   INDF
    
    ; Next column
	incf	FSR				; Move to next byte in VMEM
	decfsz	vmem_col, F	    ; Decrease our byte counter and check if done
	goto	reduce_lines_cols_loop

    ; When we are done with the columns, we want to skip shifting the mask,
    ; as the data has 1 row less.
    goto    done_with_this_line

prepare_reduce_next:
    ; Shift-in another 1 to the tmpi mask
    bsf     STATUS, C
    rlf     tetro_tmpi, F

done_with_this_line:
    ; Shift the line mask one place to the right
    bcf     STATUS, C
    rrf     tetro_linem, F
    ; Check whether the mask is 0
    movf    tetro_linem, F ; RRF doesn't affect Z so we need to do this
    btfss   STATUS, Z
    goto    reduce_lines_cycle
    
finished_proc_lines:
    ; And copy the whole screen to the tetris field
    call    tetris_field_from_screen

    ; Skip checking for buttons and draw a new piece
    goto    tetro_test

no_gravity_collisions:
    ; Update tetro_oldy since it may have been affected by gravity
    movf    tetro_y, W
    movwf   tetro_oldy

    ; Only do buttons and logic every 8 refreshes (at 60hz)
    ; btfss   frame_tick, 3
    ; goto    end_of_processing

    ; Buttons
    call    read_buttons
   
    
bttn_check_left:
    btfsc   new_bttn_dwn, BUTTON_LEFT
    decf    tetro_x, F

bttn_check_right:
    btfsc   new_bttn_dwn, BUTTON_RIGHT
    incf    tetro_x, F

bttn_check_down:
    btfsc   new_bttn_dwn, BUTTON_DOWN
    incf    tetro_y, F

bttn_check_up:
    btfsc   new_bttn_dwn, BUTTON_UP
    incf    tetro_r, F
    ; --- Check limit
    movf    tetro_r, W
    sublw   3
    btfss   STATUS, C
    clrf    tetro_r

    ; Let's check if we actually changed (or tried to change) the X position or
    ; the rotation. If so, we will set back gravity for a few frames.
    movf    tetro_oldx, W
    subwf   tetro_x, W
    btfss   STATUS, Z
    goto    setback_gravity

    movf    tetro_oldr, W
    subwf   tetro_r, W
    btfss   STATUS, Z
    goto    setback_gravity
    
    goto    do_tetro_checks

setback_gravity:
    ; We are going to be rather generous with the number of frames, considering
    ; that the game is already quite tight due to the small field size.
    movf    gravity_cd, W
    addlw   8
    ; Don't save the result to gravity_cd if we have overflown the counter with
    ; our modification.
    btfss   STATUS, C
    movwf   gravity_cd

; Check if the movement is actually valid, and restrict accordingly.
; Adjusting will be an iterative process; We will make an adjustment and then
; will check all flags again. This is because we are only adjusting position
; (for wall collision for instance) or nullifying all changes (when floor
; collision would happen due to a newly triggered rotation).
; A rotation that only causes wall collision should be easily fixed adjusting X.
; But if a rotation causes floor collision AND wall collision, then fixing the
; X coordinate will only solve the wall collision. We want to solve that first,
; and if floor collision still happens, then we will restore the old state of
; the piece.
do_tetro_checks:
    ; After we are done processing buttons, call the "draw" function again but
    ; without drawing; Just to check for collisions.
    bsf     tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    call    draw_tetromino
    
    ; Are we colliding with walls?
    btfss   tetro_flags, TETRO_FLAG_WALL_COLLISION
    goto    wall_checks_done
    
    ; Ok, we need to adjust the position. Let's substract tetro_maxxp from 7.
    movlw   7
    subwf   tetro_maxxp, W ; W = tetro_maxxp - 7
    ; If is > 3, we probably ran through the left wall
    sublw   3           ; W = 3 - W
    btfsc   STATUS, C
    goto    adjust_right_wall
adjust_left_wall:
    ; Let's adjust for the left wall, we need to complement tetro_maxxp, and add
    ; it to tetro_x
    incf    tetro_x, F ; Single-block adjustement
    ; After a position adjustment we want to iterate again, to check if the
    ; adjustment fixes the position
    goto    do_tetro_checks

adjust_right_wall:
    decf    tetro_x, F  ; Single-block adjustement
    ; After a position adjustment we want to iterate again, to check if the
    ; adjustment fixes the position
    goto    do_tetro_checks

wall_checks_done:
    ; Check for floor collision (considered a field collision)
    btfss   tetro_flags, TETRO_FLAG_FIELD_COLLISION
    goto    end_tetro_checks
    
    ; If after solving wall collisions we are still colliding with the floor or
    ; with a piece we can't allow the movement. Let's restore tetro_x, tetro_y
    ; and tetro_r
    movf    tetro_oldx, W
    movwf   tetro_x
    movf    tetro_oldy, W
    movwf   tetro_y
    movf    tetro_oldr, W
    movwf   tetro_r

end_tetro_checks:
    ; Loop
    goto    game_loop


; -----------------------------------------------------------------------
; prepare_score_digits
;   Sets the score_digits variable from the current tetris_score
; -----------------------------------------------------------------------    
prepare_score_digits:
    movlw   10
    movwf   math_x
    movf   tetris_score, W
    call    div_8bit

    movwf   math_x      ; Save first digit to math_x and shift it
    rlf     math_x,F
    rlf     math_x,F
    rlf     math_x,F
    rlf     math_x,F
    movlw   H'F0'
    andwf   math_x, W

    iorwf   math_y, W    ; Add second digit
    movwf   score_digits
    return

; -----------------------------------------------------------------------
; set_gravity
;   Sets count-down for applying gravity
; -----------------------------------------------------------------------
set_gravity:
    ; GRAVITY = 32 - level
    movf    tetris_lvl, W
    sublw   32
    movwf   gravity_cd
    return

; -----------------------------------------------------------------------
; check_lines_on_vmem
;   Checks completed lines in vmem. Since data is stored in columns, this
;   function will return a mask with 1's where lines are.
; -----------------------------------------------------------------------
check_lines_on_vmem:
    ; No need for a loop I guess
    movf    vmem, W
    andwf   vmem+1, W
    andwf   vmem+2, W
    andwf   vmem+3, W
    andwf   vmem+4, W
    andwf   vmem+5, W
    andwf   vmem+6, W
    andwf   vmem+7, W
    return

; -----------------------------------------------------------------------
; vmem_ior_linem
;   (Inclusive) OR tetro_linem into the whole screen. 
; -----------------------------------------------------------------------
vmem_ior_linem:
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

vmem_ior_linem_loop:
    movf	tetro_linem, W
    iorwf   INDF, F   
	incf	FSR				; Increase FSR to move to next byte in VMEM
	decfsz	vmem_col, F	    ; Decrease our byte counter and test if done.
	goto	vmem_ior_linem_loop
	return
; -----------------------------------------------------------------------
; vmem_xor_linem
;   XORs tetro_linem into the whole screen. 
; -----------------------------------------------------------------------
vmem_xor_linem:
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

vmem_xor_linem_loop:
    movf	tetro_linem, W
    xorwf   INDF, F   
	incf	FSR				; Increase FSR to move to next byte in VMEM
	decfsz	vmem_col, F	    ; Decrease our byte counter and test if done.
	goto	vmem_xor_linem_loop
	return

; -----------------------------------------------------------------------
; game_over_msg_table
;   Contains the string we are going to print in this example.
;   Call with desired char index in W.
; -----------------------------------------------------------------------
; IF YOU UPDATE THIS STRING ALSO UPDATE 'GAME_OVER_MSG_LEN'
GAME_OVER_MSG_LEN     equ     11
game_over_msg_table:
    retlw ' '
    retlw ' '
    retlw 'G'
    retlw 'A'
    retlw 'M'
    retlw 'E'
    retlw ' '
    retlw 'O'
    retlw 'V'
    retlw 'E'
    retlw 'R'
    
; -----------------------------------------------------------------------
; score_msg
;   Contains the string for the score, with the last 2 digits being a
;   sort of trick to return an actual number from memory
; -----------------------------------------------------------------------
; IF YOU UPDATE THIS STRING ALSO UPDATE 'SCORE_MSG_LEN'
SCORE_MSG_LEN     equ     11
score_msg:
    retlw ' '
    retlw ' '
    retlw 'L'
    retlw 'I'
    retlw 'N'
    retlw 'E'
    retlw 'S'
    retlw ':'
    retlw ' '
    goto  score_digit_1
    goto  score_digit_2
    retlw  0
    
score_digit_1:
    ; Get the highest part of the score
    swapf   score_digits, W
    andlw   H'F'
    addlw   '0' ; Add the base character 0
    return

score_digit_2:
    ; Get the lowest part of the score
    movf   score_digits, W
    andlw   H'F'
    addlw   '0' ; Add the base character 0
    return


; Call with a byte from the tetromino in W, and vmem_col, vmem_row already set
; to the beginning X and Y position.
tetro_draw_row_and_check:
    movwf   tetro_data
    movlw   8
    movwf   tetro_draw_r

tetro_draw_check_loop:
    ; We only need to draw the pixels with a value of 1 in the tetro data
    btfss   tetro_data, 7
    goto    end_tetro_block_draw

    ; Update tetro_maxxp if vmem_col > tetro_maxxp (current)
    movf    vmem_col, W
    subwf   tetro_maxxp, W
    btfsc   STATUS, C

    goto    tetro_maxxp_unchanged
    movf    vmem_col, W
    movwf   tetro_maxxp

tetro_maxxp_unchanged:
    ; Check for collisions and update tetro_flags accordingly
    ; Outside-boundaries. We normally need to check for > 7, as negative
    ; coords will underflow the register
    ;  [ X > 7 test ]
    movf    vmem_col, W
    sublw   7
    btfss   STATUS, C
    bsf     tetro_flags, TETRO_FLAG_WALL_COLLISION
    
    ; [ Y > 7 test ] Let's test for Y == 8 instead, so we won't flag
    ; upper-bound collisions, and (considering all blocks in a tetromino
    ; are connected) any > 7 will obviously go through the 8th row
    movf    vmem_row, W
    sublw   8
    btfsc   STATUS, Z
    bsf     tetro_flags, TETRO_FLAG_FIELD_COLLISION
    
    ; Collision against the tetris field
    call    tetris_field_get_block
    addlw   0; Literally just making this so we can check STATUS, Z
    btfss   STATUS, Z
    bsf     tetro_flags, TETRO_FLAG_FIELD_COLLISION

    ; Draw the pixel
    btfss   tetro_flags, TETRO_EXFLAG_BIT_TEST_DONTDRAW
    call    vmem_set_pixel

end_tetro_block_draw:
    incf    vmem_col, F

    ; Test if we need to move into next line
    decf    tetro_draw_r, W
    andlw   B'11'
    btfss   STATUS, Z
    goto    next_tetro_pixel

    ; Increase Y, reset X
    movf    tetro_x, W
    movwf   vmem_col
    incf    vmem_row, F

next_tetro_pixel:
    ; Shift data
    rlf     tetro_data, F
    ; Next iteration
    decfsz  tetro_draw_r, F
    goto    tetro_draw_check_loop
    return

; Call with tetromino index in tetro_n, rotation in tetro_r, and position in
; tetro_x, tetro_y
draw_tetromino:
    ; Clear tetro test flags
    movlw   TETRO_FLAG_CLEARMASK
    andwf    tetro_flags, F
    ; Also the computed max X pixel drawn
    clrf    tetro_maxxp

    ; Set initial drawing values
    movf    tetro_y, W
    movwf   vmem_row
    movf    tetro_x, W
    movwf   vmem_col

    ; Get tetro data
    movf    tetro_n, W
    addwf   tetro_n, W
    addwf   tetro_n, W
    addwf   tetro_n, W
    addwf   tetro_r, W
    movwf   tetro_d1 ; Temporarily use tetro_d1 to store the index
    addwf   tetro_d1, F

    movlw   HIGH(tetrominoes_table)
    movwf   PCLATH
    movf    tetro_d1, W
    call    tetrominoes_table
    movwf   tetro_d0

    movlw   HIGH(tetrominoes_table)
    movwf   PCLATH
    incf    tetro_d1, W
    call    tetrominoes_table
    movwf   tetro_d1
    
    ; Draw the tetromino
    movf    tetro_d0, W
    call    tetro_draw_row_and_check
    
    movf    tetro_d1, W
    call    tetro_draw_row_and_check

    return

; -----------------------------------------------------------------------
; clear_tetris_field
;   Clears ttr_field.
; -----------------------------------------------------------------------  
clear_tetris_field:
	movlw	ttr_field       ; Move the address of ttr_field (as constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

_ttr_mem_clr_loop:
    clrf	INDF
	incf	FSR				; Increase FSR to move to next byte in ttr_field
	decfsz	vmem_col, F		; Decrease our byte counter and test if done.
	goto	_ttr_mem_clr_loop
	return

; -----------------------------------------------------------------------
; tetris_field_get_block
;  Checks vmem_col, vmem_row in the tetris buffer. Returns with W=0 if no block
;  is there. 0xff otherwise
; -----------------------------------------------------------------------
tetris_field_get_block:
     ; Return if col > 7
    btfsc   vmem_col, 3
    retlw   0

    movf    vmem_col, W
	addlw	ttr_field       ; Adds the addr of the field (as constant) to W
	movwf	FSR				; Pass that to FSR
    movf    INDF, W
    movwf   vmem_data

    movlw   B'10000000'         ; Create a bit mask with a 1, shifted to
    shift_right_w_f vmem_row    ; vmem_row position

    andwf   vmem_data, W
    btfss   STATUS, Z
    retlw	H'FF'
    retlw   0

; -----------------------------------------------------------------------
; tetris_field_from_screen
;   Copies the whole screen into the tetris field array
; -----------------------------------------------------------------------  
tetris_field_from_screen:
    ; Very dumb code but it works. Better than dealing with the only pointer we
    ; have I guess.
    movf    vmem, W
    movwf   ttr_field
    
    movf    vmem+1, W
    movwf   ttr_field+1
    
    movf    vmem+2, W
    movwf   ttr_field+2
    
    movf    vmem+3, W
    movwf   ttr_field+3
    
    movf    vmem+4, W
    movwf   ttr_field+4
    
    movf    vmem+5, W
    movwf   ttr_field+5
    
    movf    vmem+6, W
    movwf   ttr_field+6
    
    movf    vmem+7, W
    movwf   ttr_field+7
	return
    
; -----------------------------------------------------------------------
; render_tetris_field
;   Copies ttr_field into vmem.
; -----------------------------------------------------------------------                             
render_tetris_field:
	movlw	8
	movwf	vmem_col

_ttr_mem_render_loop:
    decf    vmem_col, F
    movf    vmem_col, W     ; W = vmem_col
    addlw   ttr_field       ; Add the address of ttr_field (as constant) to W
	movwf	FSR				; Pass that to FSR
    movf	INDF, W         ; Read from mem and save it to vmem_data
    movwf   vmem_data

	call    vmem_set_col

    movf    vmem_col, F
	btfss	STATUS, Z
	goto	_ttr_mem_render_loop
    
	return

; #######################################################################
; ##                                                                   ##
; ##                     B U T T O N   I N P U T                       ##
; ##                                                                   ##
; #######################################################################
read_buttons:
    ; Read buttons and compare to previous state
    comf    BUTTONS_PORT, W
    andlw   BUTTONS_MASK
    movwf   down_bttns

    ; Compare against last_bttns
    xorwf   last_bttns, W
    movwf   new_bttn_dwn

    ; Update last_bttns
    movf    down_bttns, W
    movwf   last_bttns

    ; Let's finish combining down_bttns with the information about the
    ; buttons that changed, to get the buttons we need to take care of.
    andwf   new_bttn_dwn, F
    return


; #######################################################################
; ##                                                                   ##
; ##            " V I D E O   M E M O R Y "   R O U T I N E S          ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; vmem_draw_char_col
;   Draws column 'char_col' from character 'font_char' at position
;   vmem_col, vmem_row
; -----------------------------------------------------------------------
vmem_draw_char_col:
    movlw   FIRST_FONT_CHAR
    subwf   font_char, W
    movwf   font_ndx

    ; -- Get character data
    call    _3x6_font_data

    ; -- Shift it vmem_row places
    shift_right_w_f vmem_row
    movwf   vmem_data
    ; -- Get current data at that position
    call    vmem_get_col
    ; Merge it with the data we got for the character
    iorwf   vmem_data, F
    ; Overwrite the column
    call    vmem_set_col
    return

; -----------------------------------------------------------------------
; vmem_draw_char
;   Draws character 'font_char' at position vmem_col, vmem_row
; -----------------------------------------------------------------------
vmem_draw_char:
    movlw   3
    movwf   char_col ; Should go from 1 to 3

_vmem_draw_char_loop:
    call    vmem_draw_char_col

    ; Increment vmem_col
    incf    vmem_col, F

    ; Decrement char_col
    decfsz  char_col, F
    goto    _vmem_draw_char_loop
    return

; -----------------------------------------------------------------------
; vmem_get_col
;   Gets column vmem_col from the VMEM buffer. Returns with value in W.
; -----------------------------------------------------------------------
vmem_get_col:
    ; Return if col > 7
    btfsc   vmem_col, 3
    retlw   0

    movf    vmem_col, W
	addlw	vmem			; Adds the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
    movf    INDF, W
    return

; -----------------------------------------------------------------------
; vmem_set_col
;   Sets column vmem_col in the VMEM buffer with vmem_data data
; -----------------------------------------------------------------------
vmem_set_col:
    ; Return if col > 7
    btfsc   vmem_col, 3
    return

    movf    vmem_col, W
	addlw	vmem			; Adds the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
    movf    vmem_data, W
    movwf   INDF
    return

; -----------------------------------------------------------------------
; vmem_set_pixel
;   Sets pixel at vmem_col, vmem_row
; -----------------------------------------------------------------------
vmem_set_pixel:
    movlw   B'10000000'         ; Create a bit mask with a 1, shifted to
    shift_right_w_f vmem_row    ; vmem_row position
    movwf   vmem_data           ; Store it in vm_data

    call    vmem_get_col        ; Now get the current value of the column vmem_col
    iorwf   vmem_data, F        ; Combine the mask and the retrieved column

    call    vmem_set_col        ; Replace the column data
    return
    
; -----------------------------------------------------------------------
; vmem_clear_pixel
;   Clears pixel at vmem_col, vmem_row
; -----------------------------------------------------------------------
vmem_clear_pixel:
    movlw   B'10000000'         ; Create a bit mask with a 1, shifted to
    shift_right_w_f vmem_row    ; vmem_row position
    movwf   vmem_data           ; Store it in vm_data
    comf    vmem_data, F        ; Invert its bits
    decf    vmem_data, F

    call    vmem_get_col        ; Now get the value of the column vmem_col
    andwf   vmem_data, F        ; Combine the mask and the retrieved column

    call    vmem_set_col        ; Replace the column data
    return

; -----------------------------------------------------------------------
; vmem_fill
;   Completely fills the VMEM buffer (sets pixels to 1).
; -----------------------------------------------------------------------
vmem_fill:
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

_vmem_fill_loop:
    movlw	H'FF'
	movwf	INDF
	incf	FSR				; Increase FSR to move to next byte in VMEM
	decfsz	vmem_col, F	    ; Decrease our byte counter and test if done
	goto	_vmem_fill_loop
	return

; -----------------------------------------------------------------------
; vmem_clear
;   Completely clears the VMEM buffer (sets pixels to 0)
; -----------------------------------------------------------------------
vmem_clear:
	movlw	vmem			; Move the address of VMEM (as a constant) to W
	movwf	FSR				; Pass that to FSR
	movlw	8
	movwf	vmem_col

_vmem_clr_loop:
    clrf	INDF
	incf	FSR				; Increase FSR to move to next byte in VMEM
	decfsz	vmem_col, F		; Decrease our byte counter and test if done
	goto	_vmem_clr_loop
	return

; #######################################################################
; ##                                                                   ##
; ##                   H A R D W A R E   U S A R T                     ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; USART_init
;   Initializes serial port with a baud rate constant in W.
;   For a 4Mhz oscillator and using USART in Asynchronous Mode
;   the following values can be used for baud rate:
; -----------------------------------------------------------------------
USART_BAUD_9600     equ         12
USART_BAUD_19200    equ         25
USART_BAUD_38400    equ         6
USART_BAUD_57600    equ         3
USART_BAUD_115200   equ         1
USART_BAUD_250000   equ         0

USART_init:
	bsf		STATUS, RP0		; -- Bank 1
    movwf	SPBRG           ; Set baud rate
    movlw	b'00100100'		; TX9=0, SYNC=0, TXEN=1, BRGH=1 (High Speed, Async)
    movwf	TXSTA
    bsf		TRISB, 1		; RX Pin (RB1) as input
    bsf		TRISB, 2		; TX Pin (RB2) as input. USART will control this.
	bcf		STATUS, RP0		; -- Bank 0

	bsf		PORTB, 2		; Set TX Pin (RB2) to High (inactive)
	movlw	b'10010000'		; Enables port (configs pins). Enables Async Recv
	movwf	RCSTA
	return

; -----------------------------------------------------------------------
; USART_block_send
;   Won't return until data is moved out of the TSR buffer.
;   Call with data in W.
; -----------------------------------------------------------------------
USART_block_send:
    ; Wait until TXIF is empty. We can't just overwrite TXREG with data
    ; if it hasn't even moved the previous byte to the TSR.
    btfss	PIR1, TXIF
	goto	$-1

	movwf	TXREG			; move data to TXREG

	bsf		STATUS, RP0		; -- Bank 1
_USART_tx_wait:
	btfss	TXSTA, TRMT		; Transmission completes on TRMT=1
	goto	_USART_tx_wait

	bcf 	STATUS,RP0		; -- Bank 0
	return

; -----------------------------------------------------------------------
; USART_send
;   Call with data in W. Asynchronous; Won't wait a thing.
; -----------------------------------------------------------------------
USART_send:
    ; We still need to wait for TXIF.
    btfss	PIR1, TXIF
	goto	$-1

	movwf	TXREG			; move data to TXREG
	return

; -----------------------------------------------------------------------
; USART_recv
;   Will return 0 if no data was pending to be read, otherwise will read
;   Any pending data and will save it in W.
; -----------------------------------------------------------------------
USART_recv:
	call	USART_checkRecvError
	btfss	PIR1, RCIF
    retlw	0

	movf	RCREG, W		; Save received data in W
	return

; -----------------------------------------------------------------------
; USART_recv_pending
;   Will return W = 1 if data is pending to be read, 0 otherwise.
; -----------------------------------------------------------------------
USART_recv_pending:
	call	USART_checkRecvError
	btfsc	PIR1, RCIF
    retlw	1
	retlw   0

; -----------------------------------------------------------------------
; USART_checkRecvError
;   Checks and clears if an error ocurred in the RX module.
; -----------------------------------------------------------------------
USART_checkRecvError:
	;Check if RECV overrun has occurred and resets RX module if needed.
	btfss	RCSTA, OERR		; Clear overrun bit if set
	return
	bcf		RCSTA, CREN
	bsf		RCSTA, CREN
	return

; #######################################################################
; ##                                                                   ##
; ##                      E E P R O M   C O D E                        ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; eeprom_read
;   Reads data from eeprom into W register. Call with address in
;   eeprom_addr.
; -----------------------------------------------------------------------
eeprom_read:
    movf    eeprom_addr, W

    bsf     STATUS, RP0 ; Bank 1
    movwf   EEADR
    bsf     EECON1, RD  ; EE Read
    movf    EEDATA, W   ; W = EEDATA
    bcf     STATUS, RP0 ; Bank 0
    return

; -----------------------------------------------------------------------
; eeprom_write
;   Writes data in EEPROM. Call with address in eeprom_addr and data in W
;   It is recommended that interrupts are disabled before doing an EEPROM
;    write.
; -----------------------------------------------------------------------
eeprom_write:
    bsf     STATUS, RP0     ; Bank 1
    bsf     EECON1, WREN    ; Enable write
    
    movwf   EEDATA          ; Set data for write
    movf    eeprom_addr, W
    movwf   EEADR

    ; Begin write sequence
    movlw   H'55'
    movwf   EECON2          ;Write 55h
    movlw   H'AA'
    movwf   EECON2          ;Write AAh

    bsf     EECON1, WR      ;Set WR bit

    ; Wait until done
    btfsc   EECON1, WR
    goto    $-1

    bcf     STATUS, RP0     ;Bank 0
    return

; #######################################################################
; ##                                                                   ##
; ##             A U X I L I A R Y   F U N C T I O N S                 ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; div_8bit
;   Simple but slow 8 bit division implementation. Call with dividend in
;   W and divisor in math_x. Quotient is returned in W and remainder in
;   math_y
; -----------------------------------------------------------------------
div_8bit
    movwf   math_y
    clrf    math_tmp

_div_WK_loop:
    subwf   math_x, W ; Is W < divisor?
    btfss   STATUS, C
    goto    _do_WK_div
    btfsc   STATUS, Z
    goto    _do_WK_div

    ; Return current result
    goto    _div_WK_end

_do_WK_div:
    ; No? Good, substract divisor from W and increase quot
    movf    math_x, W
    subwf   math_y, F
    incf    math_tmp, F
    movf    math_y, W
    goto _div_WK_loop
    
_div_WK_end
    movf    math_tmp, W
    return

; -----------------------------------------------------------------------
; delay_w_frames
;   Does nothing for 'W' number of frames.
; -----------------------------------------------------------------------
delay_w_frames:
    movwf   frame_cntdown

    movf    frame_cntdown, W
    btfss   STATUS, Z
    goto    $-2
    return

; #######################################################################
; ##                                                                   ##
; ##             T I M E R 1   S E T U P   R O U T I N E S             ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; init_timer
;   Initializes timer and enables peripheral interrupts.
; -----------------------------------------------------------------------
init_timer:
    ; Most settings are good for us if set to 0
    clrf    T1CON
    
    ; Bank 1
    bsf     STATUS, RP0
    
    ; Enable Timer1 interrupt
    bsf     PIE1, TMR1IE

    ; Enable only Peripheral interrupts (and turn ON interrupts
    ; by setting the Global Interrupt Enable bit)
    clrf    INTCON
    bsf     INTCON, PEIE
    bsf     INTCON, GIE

    ; Bank 0
    bcf     STATUS, RP0

    return

; -----------------------------------------------------------------------
; start_refrsh_timer
;   Configures Timer1 for a fixed refresh rate, and starts counting
; -----------------------------------------------------------------------
start_refrsh_timer:
; T1 configured as a 16-bit timer will cause a pair of registers (TMR1H and
; TMR1L)to increment automatically every CPU cycle, and will trigger an
; interrupt when it overflows to 0x0000 from 0xFFFF.
; To make the interrupt trigger at a specific interval of "N" cycles, we need
; to re-start the counter at (0xFFFF - N) instead of zero, so N cycles wills
; pass before the counter resets and triggers an interrupt.
;
; Computing the start value for TMR1H:TMR1L
; ------------------------------------------
; The input frequency for the timer will be FOsc/4: With the internal 4 Mhz
; source that would be 1 Mhz. We need to calculate the number of increments
; that would result in a 60 Hz interrupt, and that is N = 1000000 / 60 = 16667
;
; Considering that 0xFFFF is 65535, we need to set TMR1H:TMR1L to:
;   65535 - 16667 = 48868 .... which in hex is 0xBEE4
; 
; So, we need to make 
;    TMR1H = 0xBE, and TMR1L = 0xE4
;
; Example timer settings:
;   60Hz          0xBEE4
;   120Hz         0xDF72
;   240Hz         0xEFB9
;   480Hz         0xF7DC
;   960Hz         0xFBEE
;
    ; First, let's stop the timer. Just in case.
    bcf     T1CON, TMR1ON   ; Disable timer
    ; Now set the values for TMR1H:TMR1L
    movlw   H'BE'
    movwf   TMR1H
    movlw   H'E4'
    movwf   TMR1L
    ; We can re-enable the timer now
    bsf     T1CON, TMR1ON   ; Enable timer
    return


; #######################################################################
; ##                                                                   ##
; ##   M A X 7 2 1 9   D I S P L A Y   D R I V E R   R O U T I N E S   ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; max7219_init
;   Initializes the max7219 controller.
; -----------------------------------------------------------------------
max7219_init:
    ; Initial state for the communication lines
    bcf     M7219_PORT, M7219_CLK
    bsf     M7219_PORT, M7219_NOTCS

    ; Disable display test mode, shutdown mode and decoding
    max7219_send_cc   M7219_TEST,       0
    max7219_send_cc   M7219_SHUTDOWN,   1
    max7219_send_cc   M7219_DECODE,     0
    ; Set display intensity to user-defined value and # of display lines to 8
    max7219_send_cc   M7219_INTENSITY,  DISPL_BRIGHTNESS
    max7219_send_cc   M7219_SCANLIMIT,  7
    return

; -----------------------------------------------------------------------
; max7219_send
;   Sends 'dd_data' to the address 'dd_addr' of the max7219.
; -----------------------------------------------------------------------
max7219_send:
; This sub assumes that the CS line has been taken care of before calling
; this function.
    bcf     M7219_PORT, M7219_NOTCS
    nop
    movf    dd_addr, W
    call    max7219_send_W
    movf    dd_data, W
    call    max7219_send_W
    bsf     M7219_PORT, M7219_NOTCS
    return

; -----------------------------------------------------------------------
; max7219_send_W
;   Low level function that sends the 'W' register to the max7219 IC.
; -----------------------------------------------------------------------
max7219_send_W:
    movwf   dd_byte
    bcf     M7219_PORT, M7219_CLK
    movlw   8
    movwf   dd_cnt
send_byte_loop:
    ; Raise or lower DATA IN line, depending on the value of
    ; the bit we are checking
    btfss   dd_byte, 7
    bcf     M7219_PORT, M7219_DIN
    btfsc   dd_byte, 7
    bsf     M7219_PORT, M7219_DIN
    nop
    ; clock HIGH
    bsf     M7219_PORT, M7219_CLK
    nop
    ; clock LOW
    bcf     M7219_PORT, M7219_CLK
    nop
    rlf     dd_byte, F
    decfsz  dd_cnt, F
    goto    send_byte_loop
    return


; #######################################################################
; ##                                                                   ##
; ##      I N T E R N A L   A U X I L I A R Y   F U N C T I O N S      ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; [INTERNAL] scroll_text_char_table
;   Checks the table [scroll_strhi:scroll_strlo] for st_char_ndx. Returns
;   space if the requested character exceeds scroll_strsz.
; -----------------------------------------------------------------------
scroll_text_char_table:
    ; Check if the requested character exceeds the length of the string
    incf    st_char_ndx, W  ; Start at 1, so we can compare if > to len easily
    subwf   scroll_strsz, W
    btfss   STATUS, C
    goto    _end_of_string

    ; Set the address for the jump
    movf    scroll_strhi, w
    movwf   PCLATH
    
    movf    st_char_ndx, W
    addwf   scroll_strlo, W
    
    ; Call the table directly.
    movwf   PCL

_end_of_string:
    retlw   ' '

; -----------------------------------------------------------------------
; scroll_text_fx
;   Scrolls string at PGM address scroll_strhi: scroll_strlo, with length
;   scroll_strsz
; -----------------------------------------------------------------------
scroll_text_fx:
    ; Compute max scroll
    ; MAX_SCROLL = (scroll_strsz + 2) * 4
    movlw   2
    addwf   scroll_strsz, W
    movwf   st_max_off
    bcf     STATUS, C
    rlf     st_max_off, F
    bcf     STATUS, C
    rlf     st_max_off, F

    clrf    st_str_off

_scroll_text_loop:
    ; Draw values
    movf    0
    movwf   vmem_col
    
    movlw   1
    movwf   vmem_row
    
    movf   st_str_off, W
    movwf  st_col_ref
    
  	; Clear virtual memory
	call	vmem_clear

_draw_col_loop:
    ; char col = 4 - (column reference % 4)
    movlw   B'11'
    andwf   st_col_ref, W
    sublw   4
    movwf   char_col

    ; Current char = column reference / 4
    movf    st_col_ref, W
    movwf   st_char_ndx

    bcf     STATUS, C
    rrf     st_char_ndx, F
    bcf     STATUS, C
    rrf     st_char_ndx, F
    
    ; --Get the character from the string table.
    call    scroll_text_char_table
    movwf   font_char

    ; Set info for character display
    
    ; Draw char column
    call    vmem_draw_char_col

    ; Increase column counters
    incf    vmem_col, F
    incf    st_col_ref, F

    ; Check if we have reached column 8. If not, keep looping
    btfss   vmem_col, 3
    goto    _draw_col_loop

    ; No need to call "update_display" because that's
    ; being done in our 60 Hz interrupt.
    movlw   7
    call    delay_w_frames
    
    ; Increase scroll offset
    incf    st_str_off, F

    ; Check if st_str_off > st_max_off
    movf    st_max_off, W
    subwf   st_str_off, W
    btfss   STATUS, C

    ; Still not exceeded? good, simply loop
    goto    _scroll_text_loop
    
    return

; #######################################################################
; ##                                                                   ##
; ##                       D A T A   T A B L E S                       ##
; ##                                                                   ##
; #######################################################################
;More than enough space for code
    ORG H'300'
; Call with index in W:
; Each tetromino is 2 bytes:
; BYTE 0  is (TETROMINO_ID*4 + ROTATION)*2
; BYTE 1  is (TETROMINO_ID*4 + ROTATION)*2 + 1
tetrominoes_table:
    addwf   PCL, F
    ; ::: Z ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. . . .]
    retlw   B'00001100'      ; [# # . .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]

    ; Rotation 1             ; [. . # .]
    retlw   B'00100110'      ; [. # # .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00001100'      ; [# # . .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]

    ; Rotation 3             ; [. . # .]
    retlw   B'00100110'      ; [. # # .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]

    ; ::: S ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'11000000'      ; [# # . .]
                             ; [. . . .]

    ; Rotation 1             ; [. #   .]
    retlw   B'01000110'      ; [. # # .]
    retlw   B'00100000'      ; [. . # .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'11000000'      ; [# # . .]
                             ; [. . . .]

    ; Rotation 3             ; [. #   .]
    retlw   B'01000110'      ; [. # # .]
    retlw   B'00100000'      ; [. . # .]
                             ; [. . . .]
                             
    ; ::: J ::::::::::::::::::::::::::::
    ; Rotation 0             ; [# . . .]
    retlw   B'10001110'      ; [# # # .]
    retlw   B'00000000'      ; [. . . .]
                             ; [. . . .]

    ; Rotation 1             ; [. # # .]
    retlw   B'01100100'      ; [. # . .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00001110'      ; [# # # .]
    retlw   B'00100000'      ; [. . # .]
                             ; [. . . .]

    ; Rotation 3             ; [. # . .]
    retlw   B'01000100'      ; [. # . .]
    retlw   B'11000000'      ; [# # . .]
                             ; [. . . .]
                             
    ; ::: T ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. # . .]
    retlw   B'01001110'      ; [# # # .]
    retlw   B'00000000'      ; [. . . .]
                             ; [. . . .]

    ; Rotation 1             ; [. # . .]
    retlw   B'01000110'      ; [. # # .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00001110'      ; [# # # .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]

    ; Rotation 3             ; [. # . .]
    retlw   B'01001100'      ; [# # . .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]
                             
    ; ::: L ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. . . #]
    retlw   B'00101110'      ; [. # # #]
    retlw   B'00000000'      ; [. . . .]
                             ; [. . . .]

    ; Rotation 1             ; [. # . .]
    retlw   B'01000100'      ; [. # . .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00001110'      ; [. . . .]
    retlw   B'10000000'      ; [# # # .]
                             ; [# . . .]

    ; Rotation 3             ; [# # . .]
    retlw   B'11000100'      ; [. # . .]
    retlw   B'01000000'      ; [. # . .]
                             ; [. . . .]

    ; ::: I ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. . . .]
    retlw   B'00001111'      ; [# # # #]
    retlw   B'00000000'      ; [. . . .]
                             ; [. . . .]

    ; Rotation 1             ; [. . # .]
    retlw   B'00100010'      ; [. . # .]
    retlw   B'00100010'      ; [. . # .]
                             ; [. . # .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00001111'      ; [# # # #]
    retlw   B'00000000'      ; [. . . .]
                             ; [. . . .]

    ; Rotation 3             ; [. . # .]
    retlw   B'00100010'      ; [. . # .]
    retlw   B'00100010'      ; [. . # .]
                             ; [. . # .]

    ; ::: O ::::::::::::::::::::::::::::
    ; Rotation 0             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]

    ; Rotation 1             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]
                            
    ; Rotation 2             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]

    ; Rotation 3             ; [. . . .]
    retlw   B'00000110'      ; [. # # .]
    retlw   B'01100000'      ; [. # # .]
                             ; [. . . .]

                             

; #######################################################################
; ##                                                                   ##
; ##        A L P H A N U M E R I C   C H A R   D A T A                ##
; ##                                                                   ##
; #######################################################################
; -----------------------------------------------------------------------
; _3x6_font_data
;  Retrieve data for a particular char from our charset.
;  Requires 'font_ndx' (0-64) and 'char_col' (1-3)
; -----------------------------------------------------------------------
_3x6_font_data:
    ; Return if char_col > 3
    movlw   B'11111100'
    andwf   char_col, W
    btfss   STATUS, Z
    retlw   0

    ; Load high bits of _3x6_char_table in PCLATH
    ; Preparing for the jump to the table later
    movlw   HIGH(_3x6_char_table)
    movwf   PCLATH

    decf    char_col, W  ; Move char_col to the (0-2) range
    addwf   font_ndx, W
    addwf   font_ndx, W
    addwf   font_ndx, W
    
    call    _3x6_char_table

_3x6_font_data_end:
    return

; -----------------------------------------------------------------------
; _3x6_char_table
;  Simple 3x6 font. Call with data index in W.
;  The data index for a particular "font_character" (0-64) and a row
;  from that char (0-3) is calculated as follows:
;  data_index = (font_character * 3) + (character_row)
; Characters implemented: ASCII 32 to 96
;
; -----------------------------------------------------------------------
; Make sure we have this table at the begining of a 256-word block. In this
; case we will put it at the very bottom of the addressable memory space.
; It's important that it fits within a 256-word region for the computed-GOTO
; to work, as we are just adding to PCL, which is a 8-bit register.
    org H'0700'

_3x6_char_table:
    addwf   PCL, F
; (space)
    retlw   B'000000' << 2
    retlw   B'000000' << 2
    retlw   B'000000' << 2
; !
    retlw   B'000000' << 2
    retlw   B'111001' << 2
    retlw   B'000000' << 2
; " 
    retlw   B'110000' << 2
    retlw   B'000000' << 2
    retlw   B'110000' << 2
; # 
    retlw   B'001111' << 2
    retlw   B'001010' << 2
    retlw   B'011110' << 2   
; $
    retlw   B'010111' << 2
    retlw   B'110101' << 2
    retlw   B'011101' << 2
; %
    retlw   B'110001' << 2
    retlw   B'001100' << 2
    retlw   B'100011' << 2
; &
    retlw   B'010001' << 2
    retlw   B'110101' << 2
    retlw   B'001010' << 2
; '
    retlw   B'000000' << 2
    retlw   B'111000' << 2
    retlw   B'110100' << 2
; (
    retlw   B'100001' << 2
    retlw   B'110011' << 2
    retlw   B'011110' << 2
; )
    retlw   B'011110' << 2
    retlw   B'110011' << 2
    retlw   B'100001' << 2
; *
    retlw   B'001010' << 2
    retlw   B'000100' << 2
    retlw   B'001010' << 2
; +
    retlw   B'000100' << 2
    retlw   B'001110' << 2
    retlw   B'000100' << 2
; '
    retlw   B'000000' << 2
    retlw   B'110000' << 2
    retlw   B'000000' << 2
; -
    retlw   B'000100' << 2
    retlw   B'000100' << 2
    retlw   B'000100' << 2
; .
    retlw   B'000000' << 2
    retlw   B'000001' << 2
    retlw   B'000000' << 2
; /
    retlw   B'110000' << 2
    retlw   B'001100' << 2
    retlw   B'000011' << 2
; 0
    retlw   B'011110' << 2
    retlw   B'100001' << 2
    retlw   B'011110' << 2
; 1
    retlw   B'000000' << 2
    retlw   B'111111' << 2
    retlw   B'010000' << 2
; 2
    retlw   B'011001' << 2
    retlw   B'100101' << 2
    retlw   B'010011' << 2
; 3
    retlw   B'110110' << 2
    retlw   B'101001' << 2
    retlw   B'100001' << 2
; 4
    retlw   B'111111' << 2
    retlw   B'010100' << 2
    retlw   B'001100' << 2
; 5
    retlw   B'100110' << 2
    retlw   B'101001' << 2
    retlw   B'111001' << 2
; 6
    retlw   B'100110' << 2
    retlw   B'101001' << 2
    retlw   B'011110' << 2
; 7
    retlw   B'110000' << 2
    retlw   B'101111' << 2
    retlw   B'100000' << 2
; 8
    retlw   B'010110' << 2
    retlw   B'101001' << 2
    retlw   B'010110' << 2
; 9
    retlw   B'011110' << 2
    retlw   B'100101' << 2
    retlw   B'011001' << 2
; :
    retlw   B'000000' << 2
    retlw   B'001010' << 2
    retlw   B'000000' << 2
; ;
    retlw   B'000000' << 2
    retlw   B'001010' << 2
    retlw   B'000001' << 2
; <
    retlw   B'010001' << 2
    retlw   B'001010' << 2
    retlw   B'000100' << 2
; =
    retlw   B'001010' << 2
    retlw   B'001010' << 2
    retlw   B'001010' << 2
; >
    retlw   B'000100' << 2
    retlw   B'001010' << 2
    retlw   B'010001' << 2
; ?
    retlw   B'011100' << 2
    retlw   B'100111' << 2
    retlw   B'010000' << 2
; @
    retlw   B'111101' << 2
    retlw   B'101001' << 2
    retlw   B'011111' << 2
; A
    retlw   B'011111' << 2
    retlw   B'100100' << 2
    retlw   B'011111' << 2
; B
    retlw   B'010110' << 2
    retlw   B'101001' << 2
    retlw   B'111111' << 2
; C
    retlw   B'100001' << 2
    retlw   B'100001' << 2
    retlw   B'011110' << 2
; D
    retlw   B'011110' << 2
    retlw   B'100001' << 2
    retlw   B'111111' << 2
; E
    retlw   B'101001' << 2
    retlw   B'101001' << 2
    retlw   B'111111' << 2
; F
    retlw   B'101000' << 2
    retlw   B'101000' << 2
    retlw   B'111111' << 2
; G
    retlw   B'101111' << 2
    retlw   B'100001' << 2
    retlw   B'011111' << 2
; H
    retlw   B'111111' << 2
    retlw   B'001000' << 2
    retlw   B'111111' << 2
; I
    retlw   B'000000' << 2
    retlw   B'111111' << 2
    retlw   B'000000' << 2
; J
    retlw   B'111110' << 2
    retlw   B'000001' << 2
    retlw   B'000001' << 2
; K
    retlw   B'110111' << 2
    retlw   B'001000' << 2
    retlw   B'111111' << 2
; L
    retlw   B'000001' << 2
    retlw   B'000001' << 2
    retlw   B'111111' << 2
; M
    retlw   B'111111' << 2
    retlw   B'010000' << 2
    retlw   B'111111' << 2
; N
    retlw   B'001111' << 2
    retlw   B'011000' << 2
    retlw   B'111111' << 2
; O
    retlw   B'011110' << 2
    retlw   B'100001' << 2
    retlw   B'011110' << 2
; P
    retlw   B'011000' << 2
    retlw   B'100100' << 2
    retlw   B'111111' << 2
; Q
    retlw   B'111111' << 2
    retlw   B'100011' << 2
    retlw   B'111110' << 2
; R
    retlw   B'111011' << 2
    retlw   B'100100' << 2
    retlw   B'111111' << 2
; S
    retlw   B'100110' << 2
    retlw   B'101101' << 2
    retlw   B'011001' << 2
; T
    retlw   B'100000' << 2
    retlw   B'111111' << 2
    retlw   B'100000' << 2
; U
    retlw   B'111110' << 2
    retlw   B'000001' << 2
    retlw   B'111110' << 2
; V
    retlw   B'111100' << 2
    retlw   B'000011' << 2
    retlw   B'111100' << 2
; W
    retlw   B'111111' << 2
    retlw   B'000010' << 2
    retlw   B'111111' << 2
; X
    retlw   B'110011' << 2
    retlw   B'001100' << 2
    retlw   B'110011' << 2
; Y
    retlw   B'111000' << 2
    retlw   B'001111' << 2
    retlw   B'111000' << 2
; Z
    retlw   B'110001' << 2
    retlw   B'101101' << 2
    retlw   B'100011' << 2
; [
    retlw   B'100001' << 2
    retlw   B'111111' << 2
    retlw   B'000000' << 2
; \
    retlw   B'000011' << 2
    retlw   B'001100' << 2
    retlw   B'110000' << 2
; ]
    retlw   B'000000' << 2
    retlw   B'111111' << 2
    retlw   B'100001' << 2
; ^
    retlw   B'010000' << 2
    retlw   B'100000' << 2
    retlw   B'010000' << 2
; _
    retlw   B'000001' << 2
    retlw   B'000001' << 2
    retlw   B'000001' << 2
; `
    retlw   B'110100' << 2
    retlw   B'111000' << 2
    retlw   B'000000' << 2

    retlw   0
 end
