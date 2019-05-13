; #############################################################################
; 
; File   : boilerplate.asm
; Author : Elias Zacarias
; 
; #############################################################################
; This file contains all the code I wrote for the OWO PIC DEMO Board Workshop,
; and includes snippets of code for the following:
;
; Buttons ---------
; There's a subroutine for reading the hardware buttons (read_buttons). This
; will maintain the following information in file registers:
;   - down_bttns   : Buttons currently pressed
;   - last_bttns   : Buttons that were pressed previously
;   - new_bttn_dwn : Buttons that were not previously pressed, but are pressed
; now.
; The BUTTON_XXXX constants have the bit from those bytes that correspond to
; each physical button.
;
; 8x8 LED Matrix --
; The data for the screen is held in a framebuffer (vmem) from which it can be
; read/written at any time. The physical screen is controlled by a MAX7219 IC
; and is updated via a 60Hz interrupt (this frequency can be changed; check
; start_refrsh_timer). There  are some subroutines to control the driver IC
; directly, but the main subs you will probably want to use are:
;   - vmem_set_pixel   : Turns on a single pixel on the screen.
;   - vmem_clear_pixel : Turns off a single pixel from the screen.
;   - vmem_get_col     : Gets a whole column (8 bits) of pixel data at once.
;   - vmem_set_col     : Sets a whole column of pixels at once.
;   - vmem_fill        : Completely fills the screen (sets all pixels to 1)
;   - vmem_clear       : Completely clears the screen (sets all pixels to 0)
;
; Serial Port -----
; There´s several functions for the hardware serial port.
;   - USART_init         : Initializes the Hardware USART controller.
;   - USART_block_send   : Sends a byte of data. Waits until it has been sent.
;   - USART_send         : Sends a byte of data asynchronously.
;   - USART_recv         : Gets one byte of data if available.
;   - USART_recv_pending : Checks whether there's unread data in the IN buffer
;
; EEPROM ----------
; There´s also a few basic functions to read and write to the onboard EEPROM
; and store information like hi-scores, settings, etc.
;   - eeprom_read  : Reads one byte from EEPROM
;   - eeprom_write : Writes one byte to EEPROM
;
; Font / Text -----
; Additionally I wrote code to handle printing texts on screen based on PGM 
; tables. Tge following subroutines are the most useful:
;   - vmem_draw_char : Draws a char from the font table
;   - scroll_text_fx : Scrolls a text string on the screen.
; I also included a macro (scroll_text) and a demo of how to use it on this
; file. It greatly simplifies displaying text strings.
;
; Frame control ---
; The screen refresh interrupt will always decrease a counter that you can use
; to time your game logic. There is an auxiliary function (delay_w_frames) that
; uses that to pause the execution for a given number of frames.

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

; --- Other Settings -----------
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
    ; Turn the LED ON or OFF depending on the 4th bit (starting from
    ; zero) of the frame counter. That bit will change every 16 increments
    ; of the counter. If we turn the LED ON or OFF depending on its status,
    ; we'll have a complete cycle after it has gone through 1 and 0... which
    ; should happen every 32 iterations... creating a blinking speed of 1/32th
    ; of the interrupt frequency, or 1.875 Hz.
    bcf     STATUS, RP0
    btfss   frame_tick, 4
    bcf     PORTB, LED_PIN
    btfsc   frame_tick, 4
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
    ; SCROLLING TEXT EXAMPLE -----------------------
    scroll_text     hello_world_table, HELLO_WORLD_LEN

    ; Loop
    goto            main_loop

; -----------------------------------------------------------------------
; hello_world_table
;   Contains the string we are going to print in this example.
;   Call with desired char index in W.
; -----------------------------------------------------------------------
; IF YOU UPDATE THIS STRING ALSO UPDATE 'HELLO_WORLD_LEN'
HELLO_WORLD_LEN     equ     14
hello_world_table:
    retlw ' '
    retlw ' '
    retlw 'H'
    retlw 'E'
    retlw 'L'
    retlw 'L'
    retlw 'O'
    retlw ' '
    retlw 'W'
    retlw 'O'
    retlw 'R'
    retlw 'L'
    retlw 'D'
    retlw '!'

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
