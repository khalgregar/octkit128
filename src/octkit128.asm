; GAME CONSTANTS

__ROOM_START_X		equ	9
__ROOM_START_Y		equ	7
PLAYER_START_X		equ 	1*8
PLAYER_START_Y		equ 	11*8
GRAVITY			equ	14
JUMP_VELOCITY		equ	$10000-450
PLAYER_SPEED_H		equ	$100
PLAYER_STUN_VELOCITY	equ	$0280
PLAYER_STUN_DURATION	equ	100
NUM_DEATH_FRAMES	equ	28

; Memory map
;
; $7000 GAME CODE 1 (to $7bff)
; $8000	GAME CODE 2
; $9000	SPRITE TABLE		- 4 bytes per entry, max 8 entries.
; $9020	SPRITE DATA		- sprite bitmaps (and masks)
; $a400 TILE PROPERTIES		- properties per tile
; $a680 ROOM OBJECTS
; $a6a0	[SPARE]
; $a800 ATTRIBUTES		- tile colours
; $aa80	[128]
; $ab00	SCRATCH MEM		- multi-purpose storage
; $ac00 ROOM TILES		- tile IDs
; $ae80 [128]
; $af00	TILE ATTRIBUTES		- tile colours (may become scratch memory)
; $b000	TILE DATA		- tile bitmaps (256 * 8 bytes)
; $b800	SPRITES			- sprite instances (8 bytes, max 32)
; $b900	[1280]
; $bdbd	IM2 HANDLER
; $be02	LOCAL VARS
; $bf00	STACK			- top is $c000

; Display RAM map
;
; $c000 SCANLINES
; $d800 ATTRIBUTES
; ...
; $ffe0 DISPLAY VARS
; $ff00 RECT STACK

__GAMECODE_1		equ	$8004
__GAMECODE_2		equ	$6b00

__BANKM			equ	$5b5c

__BANK_ROOMS		equ	0
__BANK_TILES		equ	1
__BANK_SPRITES		equ	3
__BANK_STATICS		equ	4
__BANK_DISPLAY		equ	5
__BANK_INFO		equ	6
__BANK_SHADOW		equ	7

__PLAYER_FRAMES		equ	$6000
__STATIC_GFX_BUFFER	equ	$6400
__EVALUATORS		equ	$6800
__GAME_VARS_DEFAULT	equ	$6900	; 256 bytes
__GAME_VARS		equ	$6a00	; 256 bytes

__CHARS			equ	$3c00
__CHARS_TITLE		equ	$7a00
__CHAR_MASKS		equ	$7e00

__SPRITE_TABLE		equ	$9000
__SPRITE_DATA		equ	$9020


__Y_LOOKUP		equ	$a000
__FLIP_BITS_TABLE	equ	$a200
__TILE_PROPS		equ	$a400	; 640 bytes (32 x 20) 	$a400 - $a67f
__ROOM_OBJECTS		equ	$a680	; 32 bytes
__ROOM_TITLE		equ	$a6a0	; 32 bytes
__OBJECT_MASKS		equ	$a700	; 256 bytes (1 byte per room)
__ATTRIBUTES		equ	$a800	; 640 bytes (32 x 20)	$a800 - $aa7f
__ROOM_DATA		equ	$ac00	; 640 bytes (32 x 20)	$ac00 - $ae7f
__SCRATCHMEM		equ	$ab00	; 256 bytes of scratch memory
__TILE_ATTR		equ	$af00
__TILE_DATA		equ	$b000	; 2048 bytes (256 tiles x 8 bytes)
__SPRITES		equ	$bb00
__SPRITES_TOP		equ	$bbff
__SWITCHES		equ	$bc00	; 48 bytes (max 8 switches)
__SWITCH_VARS		equ	$bc80	; 32 bytes (1 bit per switch)
; interrupt routine 		$bdbd
; interrupt table 		$be00-$bf00
__LOCAL_VARS		equ	$bf02	; space for local variables

; DISPLAY VARIABLES AND CONSTANTS

__DISPLAY_PIXELS	equ	$c000
__DISPLAY_ATTRIBUTES	equ	$d800
__DISPLAY_DIRTY_FLAGS	equ	$fefe
__RECTS_COUNT		equ	$feff
__RECTS			equ	$ff00


__ROOM_TABLE		equ	$c000
__STATICS_TABLE		equ	$c002

DISPLAY_FLAG_LIVES		equ 0
DISPLAY_FLAG_COLLECTABLES	equ 1
DISPLAY_FLAG_INFO_BOX		equ 2

colBlack 	equ 0
colBlue		equ 1
colRed		equ 2
colMagenta	equ 3
colGreen	equ 4
colCyan		equ 5
colYellow	equ 6
colWhite	equ 7


; PLAYER CONSTANTS

PLAYER_WIDTH		equ 2	; in tiles
PLAYER_HEIGHT		equ 16	; in scanlines
PLAYER_START_LIVES	equ 5
PLAYER_COLOUR		equ 7

playerXFrac		equ 0
playerX			equ 1
playerYFrac		equ 2
playerY			equ 3
playerVelocityX		equ 4 ; and 5
playerVelocityY		equ 6 ; and 7
playerLives		equ 8
playerFrame		equ 9
playerMovement		equ 10	; 0:dir,1:jmp
playerJump		equ 11
playerTileX		equ 12
playerTileY		equ 13
playerNumTilesX		equ 14
playerNumTilesY		equ 15
playerTileOffset	equ 16; and 16
playerTileRowDelta	equ 18; and 18
playerFlags		equ 20
playerDeathCounter	equ 21
playerCollectables	equ 24
playerElevator		equ 25
playerLiftCounter	equ 26
playerStunCounter	equ 27
playerLastPosition	equ 28; to 32

; playerMovement
;  0 - face direction
;  1 - on lift
;  2 - 0: upwards 1:downwards
;  3 - stunned
;  7 - on ground


pf_DYING		equ 0
pf_NO_RENDER		equ 1
pf_INFO_HIT		equ 2
pf_BUILD_ROOM		equ 3

;--------------------------------------------------------
org __GAME_VARS_DEFAULT

db (__ROOM_START_X*8)+__ROOM_START_Y 		; ROOM ID
db 0						; ROOM INFO
dw $ffff					; INFO_POS
db 0						; INFO BRIGHT

db 0,PLAYER_START_X				; playerX
db 0,PLAYER_START_Y				; playerY
dw 0						; velocityX
dw 0						; velocityY
db PLAYER_START_LIVES				; playerLives
ds 32,0						; other player data

;--------------------------------------------------------
org __GAME_VARS

ROOM_ID			db 0
ROOM_INFO		db 0
INFO_POS		dw 0
INFO_BRIGHT		db 0
PLAYER_DATA
			ds 26

;--------------------------------------------------------
org __GAMECODE_1
MAIN	
	jp _start
;--------------------------------------------------------
__FRAMECOUNT	db 0
__LASTFRAME	db 0
__FRAMESKIP	db 0,0,0,0,0,0,0,0
DISPLAY_BANK	db 0
;--------------------------------------------------------
__SIZEOF_SPRITE_INST	equ 16

SWITCHTYPE_TOGGLE	equ 1
SWITCHTYPE_SINGLE	equ 2
SWITCHTYPE_PRESSURE	equ 3

; sprite instance offsets

siSpriteId		equ 0
siPixelX		equ 1
siPixelY		equ 2
siExtentMin		equ 3
siExtentMax		equ 4
siColour		equ 5
siSpeed			equ 6
siMovement		equ 7
siFrame			equ 8
siNumFrames		equ 9
siDeltaX		equ 10
siDeltaY		equ 11
siStatus		equ 12
siPixelWidth		equ 13
siFrameCount		equ 14
siState			equ 15

BLOCK_EMPTY		equ 0
BLOCK_PLATFORM		equ 1
BLOCK_WALL		equ 2
;BLOCK_DEADLY		equ 3
BLOCK_COLLECTABLE	equ 4
BLOCK_INFO		equ 5

SIZEOF_SWITCH_INSTANCE	equ 6
;--------------------------------------------------------
LIFT_ROOMS_UP
	db $3a,$3b,$3c,$3d,$3e
	db $62,$63,$64,$65	
LIFT_ROOMS_UP_END
LIFT_ROOMS_DOWN
	db $39,$3a,$3b,$3c,$3d
	db $61,$62,$63,$64
LIFT_ROOMS_DOWN_END

;--------------------------------------------------------
; Custom sprite movement
; IX - sprite instance
; A - movement ID
;--------------------------------------------------------
move_sprite_custom:
	add a,a
	ld (mscX),a
mscX equ $+1
	jr $
	jr mov_blade			; 1
	jr mov_fireball			; 2
	ret				; 3
	ret				; 4
	ret				; 5
	ret				; 6
	ret				; 7
	ret				; 8
	ret				; 9
	ret				; 10
	ret				; 11
	ret				; 12
	ret				; 13
	ret				; 14
	ret				; 15

mov_fireball:
	
	ld a,(ix+siState)		; get timer
	or a				; if not started, do normal movement
	jr z,movf_1
	inc a				; inc timer
	cp 60				; timer = 60?
	jr z,movf_4
	ld b,a
	ld a,(ix+siFrame)
	inc a
	cp 4
	jr z,movf_2
	ld (ix+siFrame),a
	jr movf_2

movf_4:	ld a,(ix+siExtentMax)		; position sprite at extentMax
	ld (ix+siPixelY),a
	xor a
	ld (ix+siFrame),a
	ld b,0

movf_2:	ld a,b
	ld (ix+siState),a
	ret
movf_1:	ld a,(ix+siPixelY)
	cp (ix+siExtentMin)
	jr nz,movf_3
	ld b,1	
	ld (ix+siState),b		; start timer
	ret
movf_3:	dec a
	dec a
	ld (ix+siPixelY),a
	ret


mov_blade:
	ld a,(ix+siPixelY)
	bit 7,(ix+siMovement)
	jr nz,movb_1
	cp (ix+siExtentMin)		; y == minY ?
	jr nz,movb_2
	set 7,(ix+siMovement)		; yes, set movement downwards
	ret
movb_2:	dec a				; no, y--
	jr movb_3
movb_1:	cp (ix+siExtentMax)		; y == maxY ?
	jr nz,movb_5
	ld b,a
	ld a,(ix+siState)
	cp 50				; timer == 50 ?
	jr z,movb_4
	inc (ix+siState)		; no, timer++
	ret 
movb_4: xor a
	ld (ix+siState),a		; timer = 0
	res 7,(ix+siMovement)		; set movement upwards
	ret
movb_5:	add a,4				; y += 4
movb_3:	ld (ix+siPixelY),a
	ret
;--------------------------------------------------------
; Run all evaluators
;--------------------------------------------------------
_run_evaluators:

	ld hl,__EVALUATORS
re1:	ld a,(hl)
	cp $ff
	ret z
	
	add a,a
	add a,a
	add a,a
	add a,a
	ld ixl,a
	ld ixh,__SPRITES/256
	inc hl
	call _eval
	ld (ix+siStatus),a

	jr re1
;--------------------------------------------------------
; Evaluate an expression
;  HL : first byte of evaluator stream
; returns value in A
;--------------------------------------------------------
_eval:
	ld a,(hl)
	inc hl
	bit 7,a
	jr nz,eval1
	
	ld b,(hl)
	inc hl
	bit 6,a
	jr z,eval2
	ld a,b			; integer literal
	ret	

eval2:	ld a,b			; object ID
	push hl
	call _get_switch_val
	pop hl
	jr nz,eval3
	xor a
	ret z
eval3:	ld a,1
	ret
	
eval1:	res 7,a			; function
	add a,a
	ld (evalX),a
evalX equ $+1
	jr $
	jr eval_if
	jr eval_and
	jr eval_or
	jr eval_xor
	jr eval_not
	jr eval_add	
	jr eval_sub
	jr eval_eq
	jr eval_neg

eval_if:
	call _eval
	ld d,a
	call _eval
	ld b,a
	call _eval
	ld c,a
	ld a,d
	or a
	jr nz,eval_if1
	ld a,b
	ret
eval_if1:
	ld a,c
	ret

eval_and:
	call eval_args2
	and b
	ret

eval_or:
	call eval_args2
	or b
	ret

eval_xor:
	call eval_args2
	xor b
	ret

eval_not:
	call _eval
	cpl
	ret

eval_sub:
	call eval_args2
	sub b
	ret

eval_eq:
	call eval_args2
	cp b
	jr nz,eval_eq1
	xor a
	ret
eval_eq1:
	ld a,1	
	ret

eval_add:
	call eval_args2
	add a,b
	ret

eval_neg:
	call _eval
	neg
	ret

eval_args2:			; returns A=arg1,B=arg2
	call _eval
	push af
	call _eval
	ld b,a
	pop af
	ret
	

;--------------------------------------------------------
_is_blocked_v:
	; D : pixelX
	; E : pixelY
	; returns A0 set if platforms found, A1 set if walls found, A2 set if info found, A3 for a switch.
	; Z set if zero.

	; transform XY to tile ptr -> HL
	; and calc tile offset

	ld a,1
	ld (ibX),a		; set tile increment
	
	ld b,2			; Default 2 tiles width
	ld a,d
	and 7
	cp 7	
	jr nz,ib1
	inc b			; extra tile required.
	jr ib1

_is_blocked_h:

	ld a,32
	ld (ibx),a
	ld b,2			; height of 2 tiles
	ld a,e
	and 7
	jr z,ib1
	inc b			; inc to 3 if not on 8px boundary

ib1:
	ld a,e			; Set a mask to apply to the final result.
	and 7			; If pixelY isn't on an 8 bit boundary
	ld a,$ff		; then disregard platforms which are only
	jr z,ib5		; 1 pixel high.
	res 0,a
ib5:	push af

	ld a,e			; calc tile prop ptr
	and $f8
	rlca
	rlca
	ld l,a
	and 3
	or __TILE_PROPS/256
	ld h,a
	ld a,d
	rrca
	rrca
	rrca
	and $1f
	ld d,a
	ld a,l
	and $e0
	or d
	ld l,a	
		

	ld c,0			; check tile props
ibX:	equ $+1
	ld de,0			; DE = ptr increment
ib2:	ld a,(hl)
	and 7
	or a
	jr z,ib4		; nothing here, skip.
	cp 1			; platform?
	jr nz,ib3
	set 0,c
	jr ib4
ib3:	cp 2			; wall?
	jr nz,ib6
	set 1,c
	jr ib4
ib6:	cp 7			; switch?
	jr nz,ib7
	set 1,c
	set 3,c
	ld a,(hl)		; set bits 5-7 to switch ID
	and $e0
	or c
	ld c,a
	set 3,c
	jr ib4
ib7:	cp 5
	jr nz,ib4
	set 1,c			; also set wall for info boxes.
	set 2,c
ib4:	add hl,de
	djnz ib2
	ld a,c
	pop bc			; retrieve mask
	and b
	ret
;--------------------------------------------------------
; D : pixelX
; E : pixelY
; Returns C reset if collision, IX = sprite instance
;--------------------------------------------------------
_test_elevator_collisions:

	exx
	ld hl,__SPRITES_TOP
	exx
tec1:	exx
	ld a,(hl)
	dec l
	exx
	cp $ff
	ret z
	ld ixl,a
	ld ixh,__SPRITES/256
	ld a,(ix+siPixelY)
	cp e
	jr nz,tec1
	ld a,(ix+siPixelX)
	ld c,a
	ld b,(ix+siPixelWidth)
	add a,b
	cp d
	jr c,tec1
	ld a,d	
	add a,8			; Player collision width = 8
	cp c
	jr c,tec1
	scf
	ret
;--------------------------------------------------------

INPUTFLAGS db 0

_update_player
	
	ld iy,PLAYER_DATA
	bit 1,(iy+playerMovement)		
	jp z,up80			; all control given to lift
	call _update_lift
	jp up70

up80:	bit 3,(iy+playerMovement)		; is player stunned?
	jr z,up81
	dec (iy+playerStunCounter)		; dec stun counter
	ld a,(iy+playerStunCounter)	
	or a					; is it zero?
	jr nz,up82
	res 3,(iy+playerMovement)		; remove stun flag
	jr up81
up82	xor a
	ld (INPUTFLAGS),a			; disable input if stunned

up81:	bit pf_DYING,(iy+playerFlags)		; Is player dying?
	jr z,up5
	;-----------------------------------
	ld a,(iy+playerDeathCounter)		; Animate player death and skip rest of update.
	dec a
	ld (iy+playerDeathCounter),a
	jp z,_respawn_player
	and 3
	jr nz,up72
	ld a,(iy+playerFrame)
	cp 15
	jr nz,up73
	set pf_NO_RENDER,(iy+playerFlags)
up73:	inc a
	ld (iy+playerFrame),a
up72:	jp up70
	;------------------------------------

up5:	ld a,(INPUTFLAGS)
	push af
	bit keyD,a				; is down button pressed?
	jr z,up75
	ld a,(ROOM_ID)				; are in a lift room?
	call _if_lift_down
	or a
	jr z,up75
up76:	ld a,(iy+playerElevator)		; was player on an elevator last frame?
	cp $ff
	jr z,up75
	set 1,(iy+playerMovement)		; set 'on lift' flag
	set 2,(iy+playerMovement)		; set 'downwards'
up75:	pop af
	

up79:	push af
	bit keyU,a
	jr z,up77
	ld a,(ROOM_ID)
	call _if_lift_up
	or a
	jr z,up77
up78:	ld a,(iy+playerElevator)
	cp $ff
	jr z,up77
	set 1,(iy+playerMovement)		; set 'on lift' flag
	res 2,(iy+playerMovement)		; set 'upwards'	
up77:	pop af


up74:	ld hl,PLAYER_DATA+playerMovement
	bit 7,(hl)				; If player was standing on ground last frame,
	res 7,(hl)				; allow the player to jump.
	jr z,up4
	bit keyJump,a				; is jump button pressed?
	jr z,up4
	ld bc,JUMP_VELOCITY			; yes, set initial jump velocity.
	ld (PLAYER_DATA+playerVelocityY),bc

up4:	ld bc,0					; BC = delta vX
	bit keyL,a				; Is left pressed?
	jr z,up8
	ld bc,$10000-PLAYER_SPEED_H
	set 0,(hl)
up8:	bit keyR,a				; Is right pressed?
	jr z,up30
	ld bc,PLAYER_SPEED_H
	res 0,(hl)
						;*****************************
						; HORIZONTAL MOVEMENT
						;*****************************	
up30:	ld hl,(PLAYER_DATA+playerXFrac)
	ld a,h
	add hl,bc
	cp h
	jr z,up32				; No horizontal movement, skip rest.
	bit 7,b					; test sign of velocity
	jr z,up44				; separate paths for left and right movement

						; **** Test columns to the left ****

up34:	dec a					; Test all columns in path
	jr nc,up33				; At left border?
up35:	ld de,$ff00
	call _offset_room			; Change room.
	ld h,240				; Position player at right of screen.
	ld l,0
	jr up32
up33:	push af
	push hl
	add a,3
	ld d,a
	ld e,(iy+playerY)
	call _is_blocked_h
	bit 1,a					; only walls affect H movement
	jr z,up36
	pop hl
	pop af
	inc a					; We've hit a wall.
	ld h,a					; Set position to the right of wall.
	ld l,0
	jr up32	
up36:	pop hl
	pop af
	cp h					; Is this the final column?
	jr z,up32
	jr up34	
						; **** Test columns to the right ****

up44:	inc a					; test all columns in path
up45:	cp 240					; at right border?
	jr c,up43
	ld de,$0100
	call _offset_room			; Change room
	ld hl,0					; Position player at left of screen.
	jr up32
up43:	push af
	push hl
	add a,12
	ld d,a
	ld e,(iy+playerY)
	call _is_blocked_h
	bit 1,a					; only walls affect H movement
	jr z,up46
	pop hl
	pop af
	dec a					; We've hit a wall, go back a column.
	ld h,a
	ld l,0
	jr up32	
up46:	pop hl
	pop af
	cp h					; Is this the final column?
	jr nz,up44	


up32:	ld (PLAYER_DATA+playerXFrac),hl		; Set X position.

						;*****************************
						; VERTICAL MOVEMENT
						;*****************************	
	
						; First check whether an elevator has
						; moved upwards into lowest row of player.
						; This is a push and should push player with it.

	ld a,(iy+playerElevator)
	ld b,$ff
	ld (iy+playerElevator),b
	cp b
	jr z,up58_2

	ld ixl,a
	ld ixh,__SPRITES/256
	
	ld a,(iy+playerX)
	ld b,(ix+siDeltaX)
	add a,b
	ld (iy+playerX),a
	
	ld a,(iy+playerY)
	ld b,(ix+siDeltaY)
	add a,b
	ld (iy+playerY),a
	;jp up3
	

up58_2:	ld hl,(PLAYER_DATA+playerVelocityY)
	ld de,GRAVITY				
	add hl,de				; add gravity to velocityY
	ld b,h
	ld (PLAYER_DATA+playerVelocityY),hl
	ex de,hl
	ld hl,(PLAYER_DATA+playerYFrac)
	ld a,h					; A = player Y
	add hl,de				; H = target Y
	ld (PLAYER_DATA+playerYFrac),hl		; Set new position, overwritten if collision detected.
	cp h
	jp z,up3				; If no change in pY, skip downwards detection.
	bit 7,b					; Moving up or down?
	jr z,up59_1
						;**** Moving upwards ****

up58_1:	dec a					; Move to row above us.
	jr nc,up58				; Check if we're at y=0

	ld de,$00ff
	call _offset_room			; Change room
	ld hl,144*256				; Position player at bottom of screen
	ld (PLAYER_DATA+playerYFrac),hl
	jp up70

up58:	ld l,a					; Test for collisions with tiles.
	push hl	
	ld a,(iy+playerX)
	add a,3
	ld d,a
	ld e,l
	push de
	call _is_blocked_v
	pop de	
	ld b,a
	pop hl
	ld a,l

	bit 3,b					; have we hit a switch?
	jr z,up57_1
	push af
	push bc
	push de
	push hl
	push iy
	ld a,b
	rlca
	rlca
	rlca
	and 7
	call _hit_switch
	pop iy
	pop hl
	pop de
	pop bc
	pop af
up57_1:	
	bit 1,b
	jr z,up56				; Only walls and info block
	bit 2,b					; If info, set player flag.
	jr z,up57
	set pf_INFO_HIT,(iy+playerFlags)

up57:	ld h,a
	ld l,$ff
	ld (PLAYER_DATA+playerYFrac),hl		; Set positionY
	ld hl,0
	ld (PLAYER_DATA+playerVelocityY),hl	; Cancel velocityY
	jr up3

up56:	cp h					; Reached maximum?
	jr z,up3
	jr up58_1
						;**** Moving downwards ****

up59_1:	or a
	jr z,up59
	dec a	
up59:	cp 144					; Bottom of screen?
	jr c,up53
	ld de,$0001
	call _offset_room
	ld hl,0
	ld (PLAYER_DATA+playerYFrac),hl
	jr up70

up53:	ld l,a
	push hl	
	add a,PLAYER_HEIGHT
	ld e,a
	ld a,(iy+playerX)
	add a,3
	ld d,a
	push de
	call _is_blocked_v
	pop de
	pop hl
	ld a,l
	jr nz,up52_2				; Platforms and walls can be landed on.
	ld l,a
	push hl
	call _test_elevator_collisions
	pop hl
	ld a,l
	jr nc,up52
	ld b,ixl
	ld (iy+playerElevator),b

up52_2:	
	ld h,a
	ld l,255
	ld (PLAYER_DATA+playerYFrac),hl
	ld hl,(PLAYER_DATA+playerVelocityY)
	push hl
	ld hl,255				; Hack to 'glue' player to elevators.
	ld (PLAYER_DATA+playerVelocityY),hl	; Zero velocity.
	set 7,(iy+playerMovement)		; Set 'on ground' flag.
	pop hl
	ld bc,PLAYER_STUN_VELOCITY
	sbc hl,bc
	jp m,up52_3
	set 3,(iy+playerMovement)		; set 'stunned' flag
	ld a,PLAYER_STUN_DURATION
	ld (PLAYER_DATA+playerStunCounter),a	; init stun counter
up52_3:	jr up3

up52:	cp h
	jr z,up3
	inc a	
	jr up59

	; ****************************************************************

up3:	bit pf_DYING,(iy+playerFlags)
	jr nz,up70
					;********************************
					;            FRAME
					;********************************
	bit 3,(iy+playerMovement)
	jr z,up70_1
	ld a,(iy+playerStunCounter)
	rrca
	rrca
	and 3
	add a,4
	jr up70_2
	
up70_1:	ld a,(iy+playerX)		; TODO - much ado about frames
	and 3
up70_2:	ld (iy+playerFrame),a
					; **** set tile based vars ****
up70:	ld d,(iy+playerX)
	ld e,(iy+playerY)

	ld a,d
	rrca
	rrca
	rrca
	and $1f
	ld (iy+playerTileX),a
	ld h,a
	
	ld a,e
	rrca
	rrca
	rrca
	and $1f
	ld (iy+playerTileY),a
	ld l,a
					
	ld a,d				; set width, clamping to screen
	ld b,2
	and 7
	jr z,up60
	inc b
up60:	ld a,h	
	add a,b
	sub 32
	jp m,up61
	ld c,a
	ld a,b
	sub c
	ld b,a
up61:	ld (iy+playerNumTilesX),b

	ld a,e				; set height, clamping to screen
	ld b,2
	and 7
	jr z,up62
	inc b
up62:	ld a,l
	add a,b
	sub 20
	jp m,up63
	ld c,a
	ld a,b
	sub c
	ld b,a
up63:	ld (iy+playerNumTilesY),b

	ld b,l				; set tile offset
	ld c,h
	call _get_tile_offset
	ld (PLAYER_DATA+playerTileOffset),bc

	ret
;----------------------------------------------------------------
rp_TILE_PTR dw 0

_render_player:

	ld hl,PLAYER_DATA+playerFlags
	bit 1,(hl)		; 'no draw player' flag
	ret nz
	
	ld iy,PLAYER_DATA
	ld l,(iy+playerTileOffset)
	ld a,(iy+playerTileOffset+1)
	add a,__TILE_PROPS/256
	ld h,a
	ld (rp_TILE_PTR),hl

	ld a,(iy+playerX)
	push af
	rrca
	rrca
	rrca
	and $1f
	exx
	ld h,__Y_LOOKUP/256
	ld l,(iy+playerY)
	ld b,a
	exx
	pop af

	and 7
	ld b,a
	add a,a
	add a,b		
	ld (rp_sX),a		; Init scanline jump

	ld hl,__PLAYER_FRAMES
	ld a,(iy+playerFrame)
	or a
	jr z,rp2
	ld b,a
	ld de,32*2
rp1:	add hl,de
	djnz rp1
rp2:	ld b,16
rp3:	push bc

	bit 0,(iy+playerMovement)
	jr nz,rp10
				; Copy current scanline from source.
	ld de,__SCRATCHMEM
	xor a
	ldi			; bmp 1
	ldi			; bmp 2
	ld (de),a
	inc e
	ldi			; mask 1
	ldi			; mask 2
	ld a,$ff
	ld (de),a
	inc e
	jr rp9

rp10:
	ld bc,__SCRATCHMEM+2		; copy scanline + mirror
	ex de,hl
	ld h,__FLIP_BITS_TABLE/256

	xor a
	ld (bc),a
	dec c

	ld a,(de)
	inc e
	ld l,a
	ld a,(hl)
	ld (bc),a
	dec c

	ld a,(de)
	inc e
	ld l,a
	ld a,(hl)
	ld (bc),a
	dec c

	ld c,5
	ld a,$ff
	ld (bc),a
	dec c	

	ld a,(de)
	inc e
	ld l,a
	ld a,(hl)
	ld (bc),a
	dec c

	ld a,(de)
	inc e
	ld l,a
	ld a,(hl)
	ld (bc),a

	ex de,hl


rp9:				; Shift scanline
	push hl
	ld hl,__SCRATCHMEM
	ld d,l

rp_sX equ $+1
	jr $
	jp rp_s0
	jp rp_s1
	jp rp_s2
	jp rp_s3
	jp rp_s4
	jp rp_s5
	jp rp_s6
	jp rp_s7
	
rp_shifters:

rp_s2:
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	ld l,d
rp_s1:	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
rp_s0:	jp rp4


rp_s3:	
	xor a
	rrd
	inc l
	rrd
	inc l
	rrd
	inc l
	dec a
	rrd
	inc l
	rrd
	inc l
	rrd
	
	scf
	rl (hl)
	dec l
	rl (hl)
	dec l
	rl (hl)
	dec l
	xor a
	rl (hl)
	dec l
	rl (hl)
	dec l
	rl (hl)
	jp rp4

rp_s4:	xor a
	rrd
	inc l
	rrd
	inc l
	rrd
	inc l
	dec a
	rrd
	inc l
	rrd
	inc l
	rrd
	jp rp4

rp_s5:	xor a
	rrd
	inc l
	rrd
	inc l
	rrd
	inc l
	dec a
	rrd
	inc l
	rrd
	inc l
	rrd
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	jp rp4

rp_s6:	xor a
	rrd
	inc l
	rrd
	inc l
	rrd
	inc l
	dec a
	rrd
	inc l
	rrd
	inc l
	rrd
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	jr rp4

rp_s7:	xor a
	rrd
	inc l
	rrd
	inc l
	rrd
	inc l
	dec a
	rrd
	inc l
	rrd
	inc l
	rrd
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	
	ld l,d
	srl (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)
	inc l
	scf
	rr (hl)
	inc l
	rr (hl)
	inc l
	rr (hl)

rp4:	
	ld de,__SCRATCHMEM
	ld hl,__SCRATCHMEM+3

	exx
	ld a,(hl)		; HL = entry in y lookup table
	add a,b
	ld e,a
	inc h
	ld d,(hl)
	dec h
	ld a,(de)		; get first screen byte
	exx

	ld b,3
rp5:	ld c,(hl)
	and c			; apply mask
	ld c,a
	ld a,(de)
	or c			; add bitmap

	push af
	ld a,(de)
	ld c,a
	exx
	ld a,(de)
	exx
	and c
	jr z,rp6

	push hl
	ld hl,(rp_TILE_PTR)
	set 4,(hl)
	pop hl

rp6:	pop af

	exx
	ld (de),a		; write back to screen
	inc e			; inc screen address

	ld a,(de)		; get next screen byte
	exx
	inc e			; inc bmp ptr
	inc l			; inc mask ptr

	push hl
	ld hl,rp_TILE_PTR
	inc (hl)
	pop hl

	djnz rp5

	exx
	inc l			; inc y lookup ptr
	ld c,l

	push bc
	push hl
	
	ld hl,(rp_TILE_PTR)
	ld a,l
	and $e0
	ld l,a
	ld a,(iy+playerTileOffset)
	and $1f
	or l
	ld l,a

	ld a,c			; if y % 8 == 0
	and 7
	or a
	jr nz,rp12
	ld bc,32
	add hl,bc
rp12:	ld (rp_TILE_PTR),hl
	pop hl
	pop bc
	exx
	
	pop hl
	pop bc
	dec b
	jp nz,rp3
				; set player colour attributes

	ld hl,(PLAYER_DATA+playerTileOffset)	
	ld de,32
	ld a,h
	add a,__TILE_PROPS/256
	ld h,a
	ld c,(iy+playerNumTilesY)
rp7:	ld b,(iy+playerNumTilesX)
	push hl
rp8:	ld a,(hl)
	and 3
	or a
	jr z,rp13
	cp 3
	jr nz,rp11
	
rp13:	ld a,$7c			; Hack to switch to display attribute address
	xor h
	ld h,a
	ld a,PLAYER_COLOUR
	ld (hl),a
	ld a,$7c
	xor h
	ld h,a

rp11:	inc l
	djnz rp8
	pop hl
	add hl,de
	dec c
	jr nz,rp7
					; add dirty rect
	ld hl,__RECTS_COUNT
	ld a,(hl)
	inc a
	ld (hl),a
	dec a
	add a,a
	add a,a
	ld d,__RECTS/256
	ld e,a
	push de	
	ld hl,PLAYER_DATA+playerTileX
	ld bc,4
	ldir
	pop hl
	set 7,(hl)
	ret
;--------------------------------------------------------
; Pulse object colours
;--------------------------------------------------------
po_COLOURS db 4,5,6,7,7,6,5,4

_pulse_objects:

	ld a,(__FRAMECOUNT)
	rrca
	and 7
	ld hl,po_COLOURS
	ld c,a
	ld b,0
	add hl,bc
	ld a,(hl)
	ld (po_COLOUR),a

	ld ix,__ROOM_OBJECTS
	ld de,32

	ld a,(ROOM_ID)		; get room object mask
	ld l,a
	ld h,__OBJECT_MASKS/256
	ld a,(hl)

	exx
	ld bc,4
	exx

	ld b,8
po4:	rrca	
	jr c,po5			; skip objects that are masked out
	push af
	push bc
	ld l,(ix+0)
	ld a,(ix+1)
	and $07
	or __DISPLAY_ATTRIBUTES/256
	ld h,a				; HL = attribute addr
po_COLOUR equ $+1	
	ld a,0
	ld c,(ix+3)			; C = tile height
po3:	ld b,(ix+2)			; B = tile width
	push hl
po2:	ld (hl),a
	inc l
	djnz po2
	pop hl
	add hl,de
	dec c
	jr nz,po3	
	pop bc
	pop af
po5:	exx
	add ix,bc
	exx
	djnz po4
po6:	ret
;--------------------------------------------------------
; Copy and expand frameset
; ===========================
; HL - pointer to frameset
; DE - pointer to destination buffer
; A - number of src frames (1,2,4 or 8)
; B - frame width
; C - frame height (multiple of 8)
; - on return
; DE points to end of destination buffer + 1
;--------------------------------------------------------
_gfx_expand_frameset:

	ld (gefX8),a

	ld a,c
	ld (gefX2),a		; Set frame height

	push bc
	xor a
gef5:	add a,c
	djnz gef5
	ld (gefX4),a		; Set frame size
	pop bc

	ld a,8
	sub b
	add a,a
	ld (gefX1),a		; Set loop jump

	ld c,0
gef2:	push bc
	push hl
	
	xor a
	ld b,a

	push de			; Calculate offset into source data.
	ld a,c
gefX8 equ $+1
	ld b,0
gef8:	bit 3,b
	jr nz,gef9
	rlc b
	rrca
	jr gef8
gef9:	and 7
	jr z,gef6
	ld b,a
gefX4 equ $+1
	ld de,0			; DE = sizeof(frame)
gef3:	add hl,de
	djnz gef3
gef6:	pop de

gefX2 equ $+1
	ld b,0			; B = frame height
gef1:	push bc
	xor a
gefX1 equ $+1
	jr $	
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ld (de),a
	inc de
	pop bc
	djnz gef1

	pop hl
	pop bc
	inc c
	ld a,8
	cp c
	jr nz,gef2

	ret
;--------------------------------------------------------
; Shift frame-set of 8 frames
; - Frame n is shifted n times (0-7)
; ===========================
; HL - pointer to frameset
; B - width
; C - height (multiple of 8)
;--------------------------------------------------------
_gfx_shift_frameset:

	ld (gseX1),bc
	ld a,1
	ld (gseX3),a	
	xor a			; Have to skip first frame.
gse3:	add a,c			; Advance ptr by w*h bytes.
	djnz gse3
	ld c,a
	xor a
	ld b,a
	add hl,bc
	ld b,7
gse2:	push bc
gseX3 equ $+1	
	ld b,1
gse1:	push bc
	push hl
gseX1 equ $+1
	ld bc,0
	call _gfx_shift_frame
	ld (gseX2),hl
	pop hl
	pop bc
	djnz gse1
	ld hl,gseX3
	inc (hl)
gseX2 equ $+1
	ld hl,0
	pop bc
	djnz gse2
	ret
;--------------------------------------------------------
; Shift a frame by one pixel
; ===========================
; HL - pointer to frame
; B - width
; C - height (multiple of 8)
; On completion, HL points one past end of frame
;--------------------------------------------------------
_gfx_shift_frame:

	ld a,c			; Calculate number of times through shift loop.
	rrca			; i.e.
	rrca			; n = (h/8)*w
	rrca
	and $1f
	ld c,a
	xor a	
gsf1:	add a,b
	dec c
	jr nz,gsf1
	ld b,a
gsf2:	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	djnz gsf2
	ret
;--------------------------------------------------------
; Create a mirrored frameset
; ==========================
; HL - dest
; DE - source
; B - frame width
; C - frame height
;--------------------------------------------------------
_gfx_mirror_frameset:
	
	ld a,b
	ld (gmf_X1),a
	ld (gmf_X2),a
	ld a,c
	ld (gmf_X3),a
	exx
	ld h,__FLIP_BITS_TABLE/256
	exx
	ld b,8
gmf3:	push bc
gmf_X3 equ $+1
	ld b,0
gmf2:	push bc
gmf_X2 equ $+1
	ld bc,0
	add hl,bc
	push hl
gmf_X1 equ $+1
	ld b,0
gmf1:	dec hl
	ld a,(de)
	exx
	ld l,a
	ld a,(hl)
	exx
	ld (hl),a
	inc de
	djnz gmf1
	pop hl
	pop bc
	djnz gmf2
	pop bc
	djnz gmf3
	ret
;--------------------------------------------------------
; Multiply two 8 digit numbers (overflow not handled)
; ----------------------------
; B = num digits in value 2 (not zero!)
; D = value 1
; E = value 2
; returns
; A = v1 * v2

_mul8:	xor a
mul_0:	rrc e
	jr nc,mul_1
	add a,d
mul_1:	sla d
	djnz mul_0
	ret
;--------------------------------------------------------
_clear_hud:

	ld hl,__DISPLAY_ATTRIBUTES+640
	ld bc,32
	ld a,30
	call _fill_mem

	ld h,__Y_LOOKUP/256
	ld l,160
	ld e,(hl)
	inc h
	ld d,(hl)

	xor a
	ld c,8
ch2:	ld b,32
	push de
ch1:	ld (de),a
	inc e
	djnz ch1
	pop de
	inc d
	dec c
	jr nz,ch2

	ret
	
;---------------------------------------------------------
; Clear dirty rects in current display bank
;---------------------------------------------------------
_update_sprites:

	ld ix,__SPRITES
	ld iy,PLAYER_DATA

us1:	ld a,(ix+siSpriteId)		; A = sprite ID
	cp $ff
	ret z

	ld a,(ix+siStatus)
	or a
	jp z,us16

	xor a
	ld (ix+siDeltaX),a
	ld (ix+siDeltaY),a
	
	ld a,(ix+siSpeed)		; check speed against frame skip counters
	and 7
	ld hl,__FRAMESKIP
	add a,l
	ld l,a
	ld a,(hl)
	or a
	jp nz,us16

	ld a,(ix+siExtentMax)
	ld b,(ix+siExtentMin)
	or b
	jr nz,us14
	ld a,(ix+siFrame)
	ld b,(ix+siNumFrames)
	inc a
	dec b
	and b
	ld (ix+siFrame),a
	jp us4

us14:	ld a,(ix+siMovement)		; A = movement type
	ld b,a
	rrca
	and 15				; A = movement ID
	jp nz,us3			; moveID == 0 :- auto
	ld a,b
	and 1				; H=0, V=1
	jr nz,us2
					; horizontal movement
	ld d,(ix+siExtentMax)
	bit 7,b				; 0 = positive, 1 = negative
	jr z,us11
	ld d,(ix+siExtentMin)
us11:	ld a,(ix+siPixelX)
	ld e,a
	cp d				; is x equal to extent?
	jr nz,us12
	ld a,b
	xor $80
	ld b,a
	ld (ix+siMovement),b		; toggle direction
us12:	bit 7,b
	ld a,1
	jr z,us13
	neg
us13:	bit 7,(ix+siStatus)
	jr z,us19
	neg
us19:	ld (ix+siDeltaX),a
	add a,e
	ld (ix+siPixelX),a

	and 7				; update frame
	bit 7,(ix+siMovement)
	jr z,us13_1
	bit 6,(ix+siMovement)
	jr z,us13_1
	ld b,a
	ld a,7
	sub b
	add a,8	
us13_1:	ld b,a
	ld a,(ix+siNumFrames)
	dec a
	and b
	ld (ix+siFrame),a

	jp us4

us2:					; vertical movement
	ld d,(ix+siExtentMax)
	bit 7,b				; 0 = positive, 1 = negative
	jr z,us6
	ld d,(ix+siExtentMin)
us6:	ld a,(ix+siPixelY)		; pixel Y
	ld e,a
	cp d				; is y equal to extent?
	jr nz,us8
	ld a,b
	xor $80
	ld b,a
	ld (ix+siMovement),b		; toggle direction
us8:	bit 7,b
	ld a,1
	jr z,us9
	neg
us9:	ld (ix+siDeltaY),a
	add a,e
	ld (ix+siPixelY),a
	
	ld b,a				; check frame speed
	ld a,(ix+siSpeed)
	and $78
	jr z,us9_1
	ld a,(ix+siSpeed)
	rrca
	rrca
	rrca
	and $1f
	ld c,(ix+siFrameCount)
	cp c
	jr nz,us9_3
	ld c,0
	ld (ix+siFrameCount),c
	ld a,(ix+siFrame)
	inc a
	ld c,(ix+siNumFrames)
	dec c
	and c
	ld (ix+siFrame),a
us9_3:	inc (ix+siFrameCount)
	jp us16

us9_1:	ld a,b
	and 7				; update frame
	ld b,a
	ld a,(ix+siNumFrames)
	dec a
	and b
	ld (ix+siFrame),a
	jp us16

					; custom movement
us3:	dec a
	call move_sprite_custom

us10:
us4:	
us16:	ld bc,__SIZEOF_SPRITE_INST
	add ix,bc
	jp us1
;---------------------------------------------------------
; Clear dirty rects in current display bank
;---------------------------------------------------------
_clear_rects:
	
	ld ix,__RECTS
	ld a,(__RECTS_COUNT)
	or a
	ret z

	ld b,a
cr1:	push bc				; store rect counter
	bit 7,(ix+0)			; are we redrawing the background?
	jr z,cr5
	call _draw_background
	jr cr4
					; C: tileX
					; E: pixelY
					; A: tile width
					; B: num scanlnes
cr5:	ld c,(ix+0)
	ld e,(ix+1)
	ld a,(ix+2)
	ld b,(ix+3)
					; set LDI jump
	ld l,a				; = (8-tileWidth)*2
	ld a,8
	sub l
	rlca
	ld (cr3+1),a

	ld l,e
	ld h,__Y_LOOKUP/256		; HL = base y lookup
	
cr2:	ld a,(hl)
	or c
	ld e,a
	inc h
	ld d,(hl)			; DE = screen address
	dec h
	inc l	

	xor a				
cr3:	jr $				; modified above according to tile width
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	ld (de),a
	inc e
	djnz cr2			; loop for num scanlines
	
cr4:	ld bc,4				; to next rect
	add ix,bc

	pop bc				; retrieve rect counter
	djnz cr1			; loop for next rect
	
	ret

_draw_background:
					; when drawing background:
					; ix+0 : tile X
					; ix+1 : tile Y
					; ix+2 : tile W
					; ix+3 : tile H

	res 7,(ix+0)			; remove flag bit
	ld c,(ix+0)
	ld e,(ix+1)

	ld a,c				; clamp right hand overflow
	ld b,(ix+2)
	add a,b
	ld d,a
	sub 32
	jr c,db4
	neg
	add a,b
	ld (ix+2),a
db4:					; calc room tile ptr origin
	ld hl,__ROOM_DATA
	ld a,(ix+1)
	rrca
	rrca
	rrca
	ld c,a
	and 3
	ld b,a
	ld a,c
	and $e0
	add a,(ix+0)
	ld c,a
	add hl,bc			; HL = top left room tile ptr
	push hl
	exx
	pop de
	ld a,d
	and 3
	or __DISPLAY_ATTRIBUTES/256
	ld d,a				; DE' = top left attr ptr
	ld a,32
	sub (ix+2)
	ld c,a
	ld b,0				; BC' = offset to next row
	exx
	
db2:	push hl				; store room data ptr
	push hl				; calc display ptr
	ld h,__Y_LOOKUP/256
	ld a,(ix+1)			; calc pixelY
	add a,a
	add a,a
	add a,a
	ld l,a
	ld e,(hl)
	inc h
	ld d,(hl)
	ld a,(ix+0)			; A = tile width
	add a,e				; add X offset
	ld e,a
	pop hl

	ld b,(ix+2)			; get tile width
db1:	push hl				; store room data ptr
	push de				; store display ptr
	
	ld a,(hl)			; get tile data ptr
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	push de
	ld de,__TILE_DATA
	add hl,de
	exx
	ld h,__TILE_ATTR/256
	ld l,a
	exx
	pop de	

	ld a,(hl)			; copy tile to screen
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	inc l
	inc d
	ld a,(hl)
	ld (de),a
	
	pop de
	inc e

	pop hl				; room tile ptr to next column
	inc l

	exx
	ld a,(hl)
	ld (de),a
	inc de
	exx

	djnz db1			; next tile horizontally

	pop hl				; room tile ptr down to next row
	ld bc,32
	add hl,bc

	exx				; attr ptr to next row
	ex de,hl
	add hl,bc
	ex de,hl
	exx

	inc (ix+1)			; move Y down 1 row
	dec (ix+3)
	jr nz,db2			; go down a tile row

	ret
;---------------------------------------------------------
; Render sprites
;---------------------------------------------------------
_render_sprites:

	ld ix,__SPRITES
	ld iy,__RECTS
	xor a
	ld (__RECTS_COUNT),a

rs1:	ld a,(ix+siSpriteId)		; A = sprite ID
	cp $ff
	ret z

	ld hl,__RECTS_COUNT
	inc (hl)
	
	rlca
	rlca
	ld l,a
	ld h,__SPRITE_TABLE/256		; HL = sprite def
	
	ld c,(hl)			; C = tile width
	ld (iy+2),c			; write to rect
	inc l
	ld b,(hl)			; B = num scanlines
	ld (iy+3),b			; write to rect
	inc l
	ld e,(hl)
	inc l
	ld d,(hl)			; HL = bitmap data	
	ex de,hl	

	push bc				; Find data ptr for frame index.
	ld d,b				; If this is slow, construct jump tables
	ld e,c				; within sprite data.
	ld b,3
	call _mul8			; *** 795 T-states per sprite ***
					; A = tileW * tileH
	
	ld e,a
	ld a,(ix+siFrame)
	or a
	jr z,rs8
	ld b,a
	ld d,0
rs7:	add hl,de
	djnz rs7

rs8:	pop bc
	ld a,8				; Use tile width to modify LDI jump
	sub c
	rlca				; a = (8-a)*2
	ld (rs3+1),a
	
	exx
	ld a,(ix+siPixelX)			
	rrca
	rrca
	rrca
	and 31
	ld c,a				; C' = tile X
	ld (iy+0),c
	ld d,(ix+siPixelY)		; D' = pixel Y
	ld (iy+1),d
	exx	

rs2:	push hl
	exx
	ld a,d
	inc d
	exx
	ld l,a
	ld h,__Y_LOOKUP/256
	ld a,(hl)			; get screen address
	inc h
	ld d,(hl)
	dec h
	inc l
	exx
	or c
	exx
	ld e,a				; DE = screen address
	pop hl
	push bc
rs3:	jr $	
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	pop bc
	djnz rs2			; next scanline

					; write colours + deadly flags, use current RECT
					; iy+0: tile X
					; iy+1: pixel Y
					; iy+2: tile W
					; iy+3: num scanlines
	
	ld a,(iy+3)
	dec a
	rrca
	rrca
	rrca
	and 7
	inc a				
	ld c,a				; C = num tiles Y
	ld a,(iy+1)
	ld b,a
	and 7
	jr z,rs4
	inc c				; if not a tileY boundary, increase num tiles Y
rs4:	ld a,b				; A = pixel Y
	rlca
	rlca
	ld h,a
	and $e0
	add a,(iy+0)
	ld l,a
	ld a,h
	and 3
	add a,__DISPLAY_ATTRIBUTES/256
	ld h,a				; HL = screen attribute PTR
	push hl
	exx
	pop hl
	ld a,$7c
	xor h
	ld h,a				; HL' = tile props ptr
	exx	
	ld a,32
	sub (iy+2)
	ld e,a
	ld d,0
	push bc
	push de
	exx
	pop de
	pop bc
	exx
	
	ld a,(ix+siColour)		; write colour attributes
rs6:	ld b,(iy+2)
rs5:	ld (hl),a
	inc l
	djnz rs5
	add hl,de
	dec c
	jr nz,rs6
					; write deadly flags into tile props
	bit 5,(ix+siMovement)
	jr z,rs11
	exx
rs10:	ld b,(iy+2)
rs9:	ld a,(hl)
	or 3
	ld (hl),a
	inc l
	djnz rs9
	add hl,de
	dec c
	jr nz,rs10
	exx

rs11:	ld de,__SIZEOF_SPRITE_INST	; increment sprite instance ptr
	add ix,de
	ld e,4
	add iy,de			; increment RECT ptr
	
	jp rs1
;---------------------------------------------------------
; Render room
; - Renders room tiles to current $c000
;---------------------------------------------------------
_render_room:

	xor a
	ld (__RECTS_COUNT),a		; set rects to 0
	
	ld hl,__ROOM_DATA
	ld de,__DISPLAY_PIXELS
	ld c,20
rr1:	push de
	ld b,32
rr2:	push bc
	push de
	ld b,8
	ld a,(hl)
	inc hl
	push hl
	or a				; If offset is 0, draw blank tile.
	jr nz,rr8
rr5:	xor a				; Draw a blank tile
rr4:	ld (de),a
	inc d
	djnz rr4
	jr rr7

rr8:	rlca				; Calculate tile offset
	rlca
	rlca
	ld h,a
	and $f8
	ld l,a
	ld a,h
	and 3
	add a,__TILE_DATA/256
	ld h,a				; HL = TILE DATA + tile offset * 8

rr6:	ld a,(hl)			
	ld (de),a
	inc d
	inc l
	djnz rr6
rr7:	pop hl
	pop de
	inc e
	pop bc
	djnz rr2
	pop de
	ld a,e
	add a,32
	ld e,a
	jr nz,rr3
	ld a,d
	add a,$08
	ld d,a
rr3:	dec c
	jr nz,rr1

	ld hl,__ATTRIBUTES		; copy attributes
	ld de,__DISPLAY_ATTRIBUTES
	ld bc,640
	ldir
	
	call _clear_hud
	
	ld hl,__ROOM_TITLE		; Write room title
	ld c,2				; 2px padding between chars
	push hl
	call _get_string_width
	
	xor a				; Taking advantage of both A and B holding result.
	sub b
	rrca
	ld b,a
	rrca
	rrca
	rrca
	and 15
	pop hl
	ld de,$d080
	add a,e
	ld e,a
	ld a,b
	and 7
	ld b,a
	call _render_string_compact

	ld a,$ff
	call _set_dirty_flags
	
	ret


;---------------------------------------------------------
; Build room into buffer
;---------------------------------------------------------
br_vars:

br_TILE_DATA_OFFSET	equ __LOCAL_VARS+($-br_vars)
	db 1
br_CURR_SPRITE_ID	equ __LOCAL_VARS+($-br_vars)
	db 0
br_SPRITE_DATA		equ __LOCAL_VARS+($-br_vars)
	dw __SPRITE_DATA
br_SPRITE_ENTRY		equ __LOCAL_VARS+($-br_vars)
	dw __SPRITE_TABLE
br_SPRITE_INSTANCE	equ __LOCAL_VARS+($-br_vars)
	dw __SPRITES
br_OBJECT_MASK		equ __LOCAL_VARS+($-br_vars)
	db $ff
br_ROOM_OBJECT_PTR	equ __LOCAL_VARS+($-br_vars)
	dw __ROOM_OBJECTS
br_FRAME_SIZE		equ __LOCAL_VARS+($-br_vars)
	dw 0
br_TILE_V_OFFSET	equ __LOCAL_VARS+($-br_vars)
	dw 0
br_OBJECT_ID		equ __LOCAL_VARS+($-br_vars)
	db 0
br_ROOM_OBJ_MASK	equ __LOCAL_VARS+($-br_vars)
	db 0
br_ELEVATOR_PTR		equ __LOCAL_VARS+($-br_vars)
	db $ff
br_SWITCH_PTR		equ __LOCAL_VARS+($-br_vars)
	dw __SWITCHES
br_SWITCH_ID		equ __LOCAL_VARS+($-br_vars)
	db 0
br_ROOMDATA_PTR		equ __LOCAL_VARS+($-br_vars)
	dw 0

_build_room:
		
	ld hl,br_vars			; init local vars
	ld de,__LOCAL_VARS
	ld bc,_build_room-br_vars
	ldir

	ld a,$ff
	ld (__SPRITES),a
	ld h,a
	ld l,a
	ld (INFO_POS),hl

	ld hl,__ROOM_DATA		; Clear the room buffer
	ld bc,640
	xor a
	call _fill_mem

	ld hl,__ATTRIBUTES		; Clear room attributes
	ld bc,640
	ld a,7
	call _fill_mem

	ld hl,__TILE_PROPS		; Clear propertiesp
	ld bc,640
	xor a
	call _fill_mem

	ld a,7
	ld (__TILE_ATTR),a

	ld b,__BANK_ROOMS
	call _switchbank

	ld a,(ROOM_ID)		
	push af
	
	push af
	ld de,__ROOM_TITLE
	call _copy_title
	pop af

	ld h,__OBJECT_MASKS/256		; Get the room object mask.
	ld l,a
	ld a,(hl)
	ld (br_ROOM_OBJ_MASK),a	
	pop af
	ld l,a				; Calculate room pointer
	xor a				; $c000 + idx*2
	ld h,a
	add hl,hl
	ld a,$c0
	add a,h
	ld h,a

	ld a,(hl)			; Get room pointer
	ld e,a
	inc hl				;	
	ld d,(hl)			;
	or d
	jp z,br11			; If ptr is NULL exit
	ex de,hl			; HL = room ptr

	ld de,__SCRATCHMEM		; Copy room data
	push de
	ld bc,256			; Copying 256 byte chunk regardless of room size
	ldir				; Don't think this matters!

	pop hl				; HL = SCRATCHMEM
	ld a,(hl)
	ld (ROOM_INFO),a		; copy room info ID
	inc l
	ld b,(hl)			; B = num tile groups
	inc l
	ld c,(hl)			; C = num sprite groups
	inc l
	ld (br_ROOMDATA_PTR),hl
	push bc				; Store sprite instance count

	push bc
	
	ld b,__BANK_TILES
	call _switchbank
	pop bc
br1:					; Iterate tile groups
	push bc
	ld c,b
	ld hl,(br_ROOMDATA_PTR)
	ld a,(hl)			; A = tile ID
	inc l
	ld b,(hl)			; B = Num instances in group
	inc l
	ld (br_ROOMDATA_PTR),hl

	push bc				; store num instances in group

					; Copy tile bitmaps into tile table

	ld l,a				; Calculate tile pointer
	xor a				; = $c000 + id*2
	ld h,a
	add hl,hl
	ld a,__ROOM_TABLE/256
	add a,h
	ld h,a
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl			; HL = tile data
	push hl
	pop iy				; IY = HL = tile data
	
	ld b,(hl)			; B = tile width
	inc hl
	ld c,(hl)			; C = tile height
	inc hl				; skip block type
	inc hl				; skip switch type
	inc hl				; HL = tile bitmaps

	ld a,(br_TILE_DATA_OFFSET)	; A = current tile table offset
	push af
	rlca				
	rlca
	rlca
	ld d,a
	and $f8
	ld e,a
	ld a,d
	and 3
	add a,__TILE_DATA/256
	ld d,a				; DE = TILE DATA + tile offset * 8
	pop af

	exx
	ld d,__TILE_ATTR/256
	ld e,a
	exx

br6:	push bc				; Copy bitmaps into tile table
br3:	push bc	
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	
	ld a,(hl)			; Write attribute into TILE ATTR table
	inc hl
	exx
	ld (de),a
	inc e
	exx
	
	pop bc
	djnz br3			; Loop for tile width
	
	pop bc
	dec c
	jr nz,br6			; Loop for tile height
	pop bc

br2:					
	push bc				; Iterate tile group
	ld hl,(br_ROOMDATA_PTR)		; retrieve room data pointer
	ld a,(hl)			; D = tile X
	inc l
	ld b,a
	exx
	rlca
	rlca
	rlca
	and 7
	inc a
	ld b,a				; B' = repeat X
	exx
	ld a,b
	and $1f
	ld d,a
	ld a,(hl)			; E = tile Y
	inc l
	ld b,a
	exx
	rlca
	rlca
	rlca
	and 7
	inc a
	ld c,a				; C' = repeat Y
	exx
	ld a,b
	and $1f
	ld e,a
	ld (br_ROOMDATA_PTR),hl
	
	ld a,(iy+2)			; is this an info box?
	and 15
	cp BLOCK_INFO
	jr nz,br27
	push de				; need to swap X and Y
	ld a,d
	ld d,e
	ld e,a
	ld (INFO_POS),de	
	pop de

br27:	ld a,e				; Calculate base room buffer pointer
	rrca				; = ROOM_DATA + y * 32 + x
	rrca
	rrca
	ld h,a
	and $e0
	or d
	ld l,a
	ld a,h
	and 3
	add a,__ROOM_DATA/256
	ld h,a				; HL = base room buffer pointer

	ld a,(iy+2)			
	and 15
	cp 4				; is this a collectable?
	jr z,br19_1			
	cp 8				; ,,, or a goodie?
	jr nz,br19

					; if yes then we need to write it into the object table
					; *** max of 8 ***
br19_1:	push hl				; write into object table
	res 2,h				; switch to attribute address
	ld b,h
	ld c,l
	ld hl,(br_ROOM_OBJECT_PTR)
	ld (hl),c
	inc l
	ld (hl),b
	inc l
	ld a,(iy+0)			; write tile width
	ld (hl),a
	inc l
	ld a,(iy+1)			; write tile height
	ld (hl),a
	inc l
	ld (br_ROOM_OBJECT_PTR),hl	

	ld hl,br_OBJECT_MASK		; shift a zero into the object mask
	ld a,(hl)
	sla a
	ld (hl),a

	ld hl,br_OBJECT_ID		; Write current object index into properties byte.
	ld a,(hl)			; Written to bits 5-7.
	or a
	rrca
	rrca
	rrca
	ld b,a
	ld a,(iy+2)
	and $1f
	or b
	ld (iy+2),a		
	inc (hl)			; increase object ID

	ld hl,br_ROOM_OBJ_MASK		; skip objects that have been masked out.
	ld a,(hl)
	rrca
	ld (hl),a
	pop hl
	jp c,br9
	jr br17

br19:	ld a,(iy+2)			; is this a switch?
	and $0f
	cp 7				; if yes then we need to write it into the switch table
	jr nz,br17			; *** max of 8 ***

	push hl

	ld hl,(br_ROOMDATA_PTR)
	ld c,(hl)			; grab switch ID
	inc hl
	ld (br_ROOMDATA_PTR),hl
	
	ld hl,br_SWITCH_ID		; write into properties byte.
	ld a,(hl)			; TODO - this is the same as above for collectables.
	inc (hl)			; Could cut a few bytes here!
	rrca
	rrca
	rrca
	ld b,a
	ld a,(iy+2)
	and $1f
	or b
	ld (iy+2),a

	pop hl
	push hl				; calc display attr ptr
	ld a,h
	and 3
	or __DISPLAY_ATTRIBUTES/256
	ld h,a
	ex de,hl
	ld hl,(br_SWITCH_PTR)
	ld (hl),e
	inc l
	ld (hl),d
	inc l
	
	ld a,(iy+0)
	ld (hl),a
	inc l
	ld a,(iy+1)
	ld (hl),a
	inc l
	ld a,(iy+3)			; switch type
	ld (hl),a
	inc l
	ld (hl),c			; switch variable
	inc l
	ld (br_SWITCH_PTR),hl

	pop hl

br17:	push hl				; Calculate tile v offset
	ld hl,0
	ld b,(iy+1)
	ld de,32
br24:	add hl,de
	djnz br24
	ld (br_TILE_V_OFFSET),hl
	pop hl

	exx
	ld a,c
	exx
	ld b,a				; B = num repeats (V)
br25:	push bc				
	push hl
	exx
	ld a,b
	exx
	ld b,a				; B = num repeats (H)
br26:	push bc
	push hl

	ld a,(br_TILE_DATA_OFFSET)	; A = base tile table offset
	ld c,(iy+1)			; C = tile height
br5:	ld b,(iy+0)			; B = tile width
	ld e,l
br4:	ld (hl),a			; write tile ID into room data
	ld d,a
	exx
	ld e,a
	ld a,(de)
	exx
	res 2,h
	ld (hl),a			; write attribute
	set 2,h

	res 3,h			
	ld a,(iy+2)
	ld (hl),a			; write block type
	set 3,h

	ld a,d
	inc a				; increment tile offset
	inc l
	djnz br4
	ld l,e
	push bc
	ld bc,32
	add hl,bc			; Drop down to next tile row (+32)
	pop bc
	dec c
	jr nz,br5

	pop hl
	ld c,(iy+0)
	ld b,0
	add hl,bc			; Add tile width
	pop bc
	djnz br26

	pop hl
	ld bc,(br_TILE_V_OFFSET)	; Add tile V offset
	add hl,bc
	pop bc
	djnz br25


br9:	pop bc
	dec b
	jp nz,br2			; Next instance in group

	ld c,(iy+0)
	ld b,(iy+1)
	xor a
br28:	add a,c
	djnz br28
	ld b,a
	ld a,(br_TILE_DATA_OFFSET)
	add a,b
	ld (br_TILE_DATA_OFFSET),a	; store the new tile table offset

	pop bc
	dec b
	jp nz,br1			; Next tile instance

	; ************* SPRITES ***************
	
	ld b,__BANK_SPRITES
	call _switchbank
	pop bc				; Retrieve sprite group count (in c)
	ld a,c
	or a
	jp z,br11			; No sprites, skip.
	ld b,c

br7:	push bc
	ld hl,(br_ROOMDATA_PTR)
	ld a,(hl)			; A = sprite ID
	inc hl
	ld (br_ROOMDATA_PTR),hl
	
	ld l,a				; calc pointer to sprite def from id
	xor a				; ptr = $c000 + id * 2
	ld h,a
	add hl,hl
	ld a,$c0
	add a,h
	ld h,a
	ld a,(hl)
	ld ixl,a
	inc hl
	ld a,(hl)
	ld ixh,a	
					; IX = sprite def		
	ld hl,(br_SPRITE_ENTRY)		; HL = sprite table
	push hl				; save into HL' as may be needed to adjust fields later.
	exx
	pop hl
	exx
	
	ld b,(ix+0)			; B = tile width			
	ld (hl),b			; copy to sprite entry
	inc l
	ld c,(ix+1)			; C = num scanlines			
	ld (hl),c			; copy to sprite entry
	inc l
					; calc num of bitmap bytes
	push hl
	xor a
	ld d,a
	ld e,c
	ld h,a
	ld l,a
br8:	add hl,de
	djnz br8			; HL = tile width * num scanlines
	ld (br_FRAME_SIZE),hl
	pop hl

	ld de,(br_SPRITE_DATA)		; copy sprite data ptr
					; DE = sprite data ptr, HL = sprite entry ptr
	ld (hl),e
	inc l
	ld (hl),d
	inc l
	ld (br_SPRITE_ENTRY),hl		; done with sprite entry, store local var

					; *** Copy bitmap data to sprite buffer ***
	push ix
	pop hl
	inc hl
	inc hl
	inc hl
	inc hl				; HL = start of frame bitmaps

	ld a,(ix+3)			; Check movement type.
	and 3				; If H, expand and shift.
	jr z,br23
					; ------------------------------					
					;     *** straight copy ***
					; ------------------------------					
br22:	push de				
	push hl
	ld hl,0
	ld de,(br_FRAME_SIZE)
	ld b,(ix+2)			; B = num frames
br21:	add hl,de
	djnz br21
	ld b,h
	ld c,l
	pop hl
	pop de
	ldir
	ld (br_SPRITE_DATA),de
	jr br18
					; ------------------------------					
					; *** copy, expand and shift ***
					; ------------------------------					

br23:	ld a,(ix+2)			; A = num frames
	ld b,(ix+0)			; B = frame width
	ld c,(ix+1)			; C = frame height
	push de
	push bc
	call _gfx_expand_frameset
	ld (br_SPRITE_DATA),de
	pop bc
	inc b				; increment frame width.
	pop de
	ex de,hl
	push hl
	push bc
	call _gfx_shift_frameset
	pop bc
	pop hl
	exx
	inc (hl)			; increase entry's frame width.
	exx

	bit 6,(ix+3)			; do we need to generate a mirrored frameset?
	jr z,br18
	ex de,hl
	ld hl,(br_SPRITE_DATA)
	call _gfx_mirror_frameset
	ld (br_SPRITE_DATA),hl
	set 6,(ix+3)
	

br18:	ld hl,(br_ROOMDATA_PTR)		; retrieve sprite group ptr
	ld b,(hl)			; B = instance count
	inc hl

	ld de,(br_SPRITE_INSTANCE)
					; SPRITE INSTANCE:
					;   0: sprite id ($ff to terminate)
					;   1: pixelX
					;   2: pixelY
					;   3: extent min
					;   4: extent max
					;   5: colour attribute
					;   6: speed
					;   7: movement flag
					;   8: frame
					;   9: num frames
					;  10: deltaX
					;  11: deltaY
					;  12: status
					;  13: pixel width

br10:	push bc	
	ld a,(br_CURR_SPRITE_ID)
	ld (de),a			; copy sprite id
	inc e
	ldi				; copy start X
	ldi				; copy start Y
	ldi				; copy extent min
	ldi				; copy extent max

					; if sprite is not deadly then
					; add the instance ptr's low byte
	bit 5,(ix+3)			; to the elevator stack at the top of
	jr nz,br10_1			; sprite instance space.
	push hl
	ld hl,br_ELEVATOR_PTR
	ld a,(hl)
	dec (hl)
	ld h,__SPRITES/256
	ld l,a
	ld a,e
	sub 5
	ld (hl),a
	pop hl

br10_1:	ldi				; copy colour
	ldi				; copy speed
	ld (br_ROOMDATA_PTR),hl
	
	push af

	ld a,(ix+3)
	ld b,a
	ld (de),a			; copy movement + flags
	inc e

	xor a
	ld (de),a			; set frame to 0
	inc e
	
	ld a,b				; If H, num frames = 8
	and 3				; or 16 if mirrored.
	or a
	jr nz,br10_4
	ld a,8
br10_2:	bit 6,(ix+3)
	jr z,br10_3
	add a,a				; double num frames for mirrored.
	jr br10_3
br10_4:	ld a,(ix+2)
br10_3:	ld (de),a			; set num frames
	inc e

	xor a				; add zero delta x,y
	ld (de),a
	inc e
	ld (de),a
	inc e

	inc a				; set status to 1
	ld (de),a
	inc e

	ld a,(ix+0)
	add a,a
	add a,a
	add a,a
	ld (de),a
	inc e				
	

	inc e				; sprite instance padding
	inc e
	
	pop af
	pop bc
	djnz br10			; loop for next instance
	
	ld (br_SPRITE_INSTANCE),de	; store sprite instance ptr

	ld hl,br_CURR_SPRITE_ID		
	inc (hl)

	pop bc
	dec b
	jp nz,br7			; Loop for next sprite group
	
br11:	ld a,(br_OBJECT_MASK)		; update room object mask
	ld b,a
	ld a,(ROOM_ID)		
	ld l,a
	ld h,__OBJECT_MASKS/256
	ld a,(hl)
	or b
	ld (hl),a
					; ------------------------
					;  EVALUATORS
					; ------------------------
br100:	ld hl,(br_ROOMDATA_PTR)
	ld de,__EVALUATORS
	ld a,(hl)
	or a
	jr z,br101
	ld c,a
	ld b,0
	inc hl
	ldir
br101:	ld a,$ff
	ld (de),a

	ld b,a
	ld hl,(br_SPRITE_INSTANCE)	; add sprite terminator
	ld (hl),b
	ld a,(br_ELEVATOR_PTR)		
	ld l,a
	ld h,__SPRITES/256
	ld (hl),b			; add elevator terminator
	ld hl,(br_SWITCH_PTR)
	ld (hl),b

brEXIT:	ret
;---------------------------------------------------------
; Switch memory bank
; B - bank to switch in to $c000 (0-7)
;---------------------------------------------------------
_switchbank:
	di
	ld a,(__BANKM)
  and $f8
  or b
  ld bc,$7ffd
  ld (__BANKM),A
  out (c),a
	ei
  ret
;---------------------------------------------------------
_swap_displays:

	di
	ld hl,__BANKM
	ld a,(hl)
	and $f8
	or 5				; switch in bank 5 (main screen)
	xor 8				; Flip between main and shadow screen
	bit 3,a
	jr nz,sd1
	or 2				; change to bank 7 (shadow screen)
sd1:	ld (hl),a
	ld bc,$7ffd
   	out (c),a			; do the switch
	ld (DISPLAY_BANK),a
	ei

	ret
;---------------------------------------------------------
_vsync:
	ld hl,__LASTFRAME		; VSYNC
	ld a,(hl)
	dec l
vsync1:	ld b,(hl)			; B = FRAMECOUNT
	cp b
	jr z,vsync1
	inc l
	ld (hl),b
	ld hl,__FRAMESKIP+7		; update frame skip counters
	ld b,8
vsync2:	ld a,(hl)
	inc a
	cp b
	jr nz,vsync3
	xor a
vsync3:	ld (hl),a
	dec l
	djnz vsync2
	ret

;----------------------------------------------------
; IM2 Handler
;----------------------------------------------------
_IM2Handler:	
	push af
	push bc
	push de
	push hl
	push ix
	push iy
	ld hl,__FRAMECOUNT
	inc (hl)
	call _sfx_update
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	ei
	reti

org $bdbd
_IM2Hook:
	jp _IM2Handler		; TODO - if handler small enough, inline this

org $be00
_IM2Table:
	defs 257, $bd
;----------------------------------------------------


org __GAMECODE_2
;---------------------------------------------------------
; Start the game
;---------------------------------------------------------
_start:	
	di
	ld sp,$c000		; move stack out of bank RAM

	ld hl,__BANKM		; select 48K ROM
	ld a,(hl)
	or %10000
	ld (hl),a
	ld bc,$7ffd
	out (c),a

	xor a
	;inc a
	out ($fe),a		; Set border to black.

	call _sfx_init

	ld a,$be		; set IM2 handler
	ld i,a
	im 2

	ei

	jp _main_menu

_new_game:

	ld hl,__GAME_VARS_DEFAULT
	ld de,__GAME_VARS
	ld bc,$100
	ldir

	ld b,__BANK_ROOMS	; init object masks
	call _switchbank
	ld hl,__OBJECT_MASKS		
	ld bc,256
	xor a
	call _fill_mem

	ld hl,__SWITCHES	; clear switches
	ld bc,48
	call _fill_mem


	; draw UI
	
	ld b,__BANK_DISPLAY
	call _switchbank
	call rh1	

	ld b,__BANK_SHADOW
	call _switchbank
	call rh1

	ld hl,PLAYER_DATA+playerFlags
	set pf_BUILD_ROOM,(hl)

	ld hl,KEYCODES_GAME
	ld (scankeys_codes),hl
	
	jr _mainloop

rh1:	ld hl,__DISPLAY_ATTRIBUTES+(21*32)
	ld de,__DISPLAY_ATTRIBUTES+(21*32)+1
	ld bc,(32*3)-1
	ld a,7
	ld (hl),a
	ldir

	ld a,21*8
	exx
	ld h,__Y_LOOKUP/256
	ld l,a
	ld b,24
	xor a
rh2:	ld e,(hl)
	inc h
	ld d,(hl)
	dec h
	inc l
	push de
	exx
	pop hl
	ld e,l
	ld d,h
	inc e
	ld (hl),a
	ld bc,31
	ldir
	exx
	djnz rh2
	exx

	xor a
	ld de,0*256 + 21
	call _render_static

	ld a,1
	ld de,8*256 + 22
	call _render_static

	ld a,2
	ld de,28*256 + 21
	call _render_static

	ret

_mainloop:
	
	ld a,(PLAYER_DATA+playerFlags)		; display dialog?
	bit pf_INFO_HIT,a
	jr z,m2
	ld a,(ROOM_INFO)
	call _render_dialog
	ld hl,PLAYER_DATA+playerFlags
	res pf_INFO_HIT,(hl)
	jr _mainloop

m2:	call _vsync
m4:	call _swap_displays

	ld hl,PLAYER_DATA+playerFlags
	bit pf_BUILD_ROOM,(hl)
	jr z,m3
	push hl	
	call _change_room
	pop hl
	res pf_BUILD_ROOM,(hl)
	jr _mainloop

m3:	
	call _scan_keys	
	call _clear_rects
	call _update_sprites
	call _update_player

	ld hl,PLAYER_DATA+playerFlags
	bit pf_BUILD_ROOM,(hl)
	jr nz,m4
	
	ld a,(DISPLAY_BANK)
	ld b,a
	call _switchbank

	call _run_evaluators
	call _render_sprites
	call _render_player
	call _render_switches
	call _check_collisions
	call _pulse_objects

	ld a,(__FRAMECOUNT)		; flash info box
	and 15
	jr nz,m10
	ld a,DISPLAY_FLAG_INFO_BOX
	call _set_dirty_flags
	ld hl,INFO_BRIGHT
	ld a,(hl)
	xor $40
	ld (hl),a

m10:	ld a,(__DISPLAY_DIRTY_FLAGS)
	and DISPLAY_FLAG_INFO_BOX
	jr z,m9
	ld bc,(INFO_POS)
	ld a,b
	cp $ff
	jr z,m9
	ld a,(INFO_BRIGHT)
	ld d,a
	call _get_tile_offset
	ld hl,__DISPLAY_ATTRIBUTES
	add hl,bc
	ld c,2
m7:	ld b,2
m8:	ld a,(hl)
	and $bf
	or d
	ld (hl),a
	inc hl
	djnz m8
	ld a,l
	add a,30
	ld l,a
	dec c
	jr nz,m7


m9:	ld a,(__DISPLAY_DIRTY_FLAGS)
	bit DISPLAY_FLAG_LIVES,a
	jr z,m11
	push af
	ld a,(PLAYER_DATA+playerLives)
	ld hl,LIVES_STRING+3
	call _byte_to_string
	ld hl,LIVES_STRING
	call _write_string
	pop af

m11:	bit DISPLAY_FLAG_COLLECTABLES,a	
	jr z,m6
	ld a,(PLAYER_DATA+playerCollectables)
	ld hl,OBJECTS_STRING+3
	call _byte_to_string
	ld hl,OBJECTS_STRING
	call _write_string

m6:	xor a
	ld (__DISPLAY_DIRTY_FLAGS),a

	jp _mainloop

LIVES_STRING db 4,22,7,0,0,0,0
OBJECTS_STRING db 27,22,7,0,0,0,0
;--------------------------------------------------------
; Get switch value
;  A : switch ID
;  returns set/reset in Zero flag
;--------------------------------------------------------
_get_switch_val:

	push af
	rrca
	rrca
	rrca
	and $1f
	ld c,a
	ld b,0
	ld hl,__SWITCH_VARS
	add hl,bc
	pop af
	rlca
	rlca
	rlca
	and $38
	or $46			; code for BIT b,(hl)
	ld (gsvX),a		
gsvX equ $+1:
	bit 0,(hl)		; will be self-modified
	ret

;--------------------------------------------------------
; Set switch value
;  B : switch ID
;  C : zero = off, non-zero = on
;--------------------------------------------------------
_set_switch_val:

	ld a,b
	rrca
	rrca
	rrca
	and $1f
	ld e,a
	ld d,0
	ld hl,__SWITCH_VARS
	add hl,de

	ld a,b
	rlca
	rlca
	rlca
	and $38
	ld b,a
	ld a,c
	or a
	ld a,b
	jr nz,ssv1
	or $86			; code for RES b,(hl)
	jr ssv2
ssv1:	or $c6			; code for SET b,(hl)
ssv2:	ld (ssvX),a
ssvX equ $+1:
	set 0,(hl)		; will be self-modified
	ret


;---------------------------------------------------------
; Render switches
;---------------------------------------------------------
_render_switches:

rsw_PLAYERTILE equ __LOCAL_VARS+0

	ld ix,__SWITCHES

	ld a,(PLAYER_DATA+playerY)		; calc tile under player's feet
	add a,16
	rlca
	rlca
	and $e3
	ld l,a
	and 3
	or __DISPLAY_ATTRIBUTES/256
	ld h,a
	ld a,l
	and $e0
	ld l,a
	ld a,(PLAYER_DATA+playerX)
	add a,7
	rrca
	rrca
	rrca
	and $1f
	or l
	ld l,a
	ld (rsw_PLAYERTILE),hl

rsw2:	ld a,(ix+0)
	cp $ff
	ret z

	ld a,(ix+4)
	cp SWITCHTYPE_PRESSURE
	jr nz,rsw4
	ld hl,(rsw_PLAYERTILE)
	ld c,0
	ld a,(ix+1)
	cp h
	jr nz,rsw5
	ld a,(ix+0)
	ld b,(ix+2)
rsw6:	cp l
	jr z,rsw7
	dec l
	djnz rsw6
	jr rsw5
rsw7:	ld c,1
rsw5:	ld b,(ix+5)
	call _set_switch_val
	
rsw4:
				; get switch value to decide colour
	ld a,(ix+5)		; A = switch ID
	call _get_switch_val
	ld a,2
	jr z,rsw3
	ld a,4+64		; on = green (bright)
rsw3:	ld (rswX),a
	

	ld a,(ix+0)
	ld l,a
	ld h,(ix+1)		; HL = tile attr
	
	ld c,(ix+3)
	ld b,(ix+2)
	ld a,32
	sub b
	ld e,a
	xor a
	ld d,a
	
rswX equ $+1
	ld a,0			; A = colour attribute
	
rsw8:	ld b,(ix+2)
rsw1:	ld (hl),a
	inc hl
	djnz rsw1
	add hl,de
	dec c
	jr nz,rsw8

	ld a,SIZEOF_SWITCH_INSTANCE
	add a,ixl
	ld ixl,a

	jr rsw2
;--------------------------------------------------------
; Check for player collisions
;--------------------------------------------------------
_check_collisions:

cc_OBJECT_IDS	equ	__LOCAL_VARS+0
cc_OBJECT_COUNT	equ	__LOCAL_VARS+8

	ld iy,PLAYER_DATA

	bit pf_DYING,(iy+playerFlags)		; Player dying?
	ret nz

	ld hl,cc_OBJECT_IDS			; Clear objects ids and count.
	ld b,9
	xor a
cc5:	ld (hl),a
	inc l
	djnz cc5

	ld hl,(PLAYER_DATA+playerTileOffset)
	ld a,h
	or __TILE_PROPS/256
	ld h,a
	ld c,(iy+playerNumTilesY)
	xor a
	or c
	ret z
	ld b,(iy+playerNumTilesX)
	xor a
	or b
	ret z

	xor a
	exx
	ld b,a				; B' = collision types
	ld c,a				; C' = object IDs
	exx

	ld a,32
	sub b
	ld d,0
	ld e,a
cc1:	push bc
cc2:	ld a,(hl)
	bit 4,a				; Test collision flag.
	jr z,cc8
	and $0f
	cp 3
	jr nz,cc9			; If tile is deadly, oops.	

cc20:	exx				; Tile is deadly, so set
	set 0,b				; deadly collision result flag.
	exx
	jr cc8
cc9:	cp 4				; Is this a collectable?
	jr z,cc16
	cp 8				; Is this a goodie?
	jr nz,cc8

cc16:	and 8				; bit 3 reset = collectable, set = goodie
	ld c,a
	ld a,(hl)			; We've hit a collectable/goodie.
	rlca				; Get the object id.
	rlca
	rlca
	and 7
	push hl
	ex af,af'
	ld hl,cc_OBJECT_COUNT
	ld a,(hl)
	inc (hl)
	ld hl,cc_OBJECT_IDS
	add a,l
	ld l,a
	ex af,af'
	or c				; set collectable/goodie flag
	ld (hl),a
	pop hl

cc8:	res 4,(hl)			; Clear collision flag.
cc3:	inc hl
	djnz cc2
	add hl,de
cc7:	pop bc
	dec c
	jr nz,cc1	

	exx
	ld a,b	
	exx
	bit 0,a					; Deadly collision found?
	jr z,cc11
						; Kill player!
	set pf_DYING,(iy+playerFlags)		; Set 'dying' flag
	ld a,12
	ld (iy+playerFrame),a
	ld a,NUM_DEATH_FRAMES
	ld (iy+playerDeathCounter),a	
	ld h,a
	ld l,a
	ld (PLAYER_DATA+playerVelocityX),hl
	ld (PLAYER_DATA+playerVelocityY),hl
	ret

cc11:	ld a,(cc_OBJECT_COUNT)			; collectable or goodie found?
	or a
	ret z

	ld b,a
	ld a,(ROOM_ID)
	ld h,__OBJECT_MASKS/256
	ld l,a					; HL = room mask ptr
	ld de,cc_OBJECT_IDS
	
cc32:	push bc
cc30:	ld a,(de)				; C = object ID
	res 3,a
	ld b,a
	ld c,$80
	inc b
	or a
cc31:	rlc c
	djnz cc31

	ld a,(hl)
	ld b,a
	and c
	jr nz,cc33
	ld a,b
	or c
	ld (hl),a

	push de
	push hl

	ld a,(de)
	bit 3,a
	jr nz,cc34
	inc (iy+playerCollectables)
	ld a,2^DISPLAY_FLAG_COLLECTABLES
	jr cc35
cc34:	inc (iy+playerLives)
	ld a,2^DISPLAY_FLAG_COLLECTABLES
cc35:	call _set_dirty_flags

	ld a,(de)
	and 7
	call _erase_object
	
	pop hl
	pop de

cc33:	inc de	
	pop bc
	djnz cc32
	ret
;--------------------------------------------------------
_get_string_width:

	; HL : zero-terminated string ptr
	; C : padding
	; returns
	; A : width in px

	ld d,__CHAR_MASKS/256
	ld b,0	

gsw1:	ld a,(hl)
	or a
	jr z,gsw2
	ld e,a
	inc hl
	ld a,(de)
	rrca
	rrca
	rrca		
	and $f
	add a,b			; add char width
	add a,c			; add padding
	ld b,a
	jr gsw1

gsw2:	ld a,b
	ret
;--------------------------------------------------------
; Copy string/title
; =================
; A : string/room ID
; DE : dest buffer
;--------------------------------------------------------
_copy_string:
	ld h,$c2
	jr cs1
_copy_title:
	ld h,$c0
cs1:	ld c,a
	xor a
	ld l,a	
	ld b,a
	add hl,bc
	add hl,bc
	push de

	ld a,(__BANKM)
	and 7
	ld (csX1),a
	ld b,6
	call _switchbank

	ld e,(hl)
	inc l
	ld d,(hl)
	ex de,hl
	pop de
	xor a
	ld c,(hl)
	ld b,a
	inc hl
	ldir
	ld (de),a		; zero terminator
csX1 equ $+1
	ld b,0
	call _switchbank
	ret
;--------------------------------------------------------
; Write a string
; (HL) : x,y,col,string,0
;  -Strings can contain newline characters.
;--------------------------------------------------------
wsX 	equ 0
wsY	equ 1
wsCol 	equ 2
db 0,0,0

_write_string:

	ld ix,_write_string-3		
	ld e,ixl
	ld d,ixh
	ldi
	ldi
	ldi

wts3:					; Calculate first pixel address
	push hl
	ld a,(ix+wsY)
	rlca
	rlca
	rlca
	and $f8
	ld l,a
	ld h,__Y_LOOKUP/256
	ld a,(hl)
	add a,(ix+wsX)
	ld e,a
	inc h
	ld d,(hl)
	
	exx				; Calculate first attribute address
	ld a,(ix+wsY)
	rrca
	rrca
	rrca
	and $e3
	ld l,a
	and $03
	or __DISPLAY_ATTRIBUTES/256
	ld h,a
	ld a,l
	and $e0
	add a,(ix+wsX)
	ld l,a
	exx
	
	pop hl

wts1:	ld a,(hl)
	inc hl
	or a
	ret z
	cp 10
	jr nz,wts4
	inc (ix+wsY)
	jr wts3	

wts4:	push de
	push hl
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,__CHARS_TITLE/256
	add a,h
	ld h,a
	ld b,8
wts2:	ld a,(hl)
	ld (de),a
	inc l
	inc d
	djnz wts2
	pop hl
	pop de
	inc e

	exx
	ld a,(ix+2)
	ld (hl),a
	inc l
	exx

	jr wts1
;--------------------------------------------------------
rsc_VARS 	equ $
rscPadding	equ 0
rscBit		equ 1
rscFrames	equ 2
		ds 17

_render_string_compact:

	; DE : display ptr (must be first in row)
	; HL : string ptr
	; B : bit offset
		
	push ix
	ld ix,rsc_VARS

	ld (ix+rscPadding),c
	ld (ix+rscBit),b
	ld a,3

rsc1:	ld a,(hl)		; A = next char
	or a
	jp z,rsc4		; Exit if terminator found.

	push hl	
	push af
	ld l,a			
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,h
	add a,__CHARS/256
	ld h,a			; HL = char data

	push de
	ld b,8
	ld de,rsc_VARS+rscFrames
rsc5:	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	xor a
	ld (de),a
	inc de
	djnz rsc5
	pop de
	
	pop af			; retrieve current char
	ld h,__CHAR_MASKS/256
	ld l,a
	ld a,(hl)		; A = char mask
	push af			; store it
	and 7			; A = number of margin bits
	ld b,a
	ld a,(ix+rscBit)
	sub b			; A = number of shifts required
	bit 7,a			; test sign bit	
	jr z,rsc7
	ld c,34	
	neg
	jr rsc8
rsc7:	ld c,0
rsc8:	ld b,a	
	ld a,8
	sub b			; A = 8 - a
	add a,a
	add a,a			; A *= 4
	add a,c			; A += offset
	ld (rscX1+1),a		; set shift jump

	ld b,8
	ld hl,rsc_VARS+rscFrames
	push de
rsc2:	push bc
	
	ld a,(hl)		; A = frame 1
	inc hl
	ld b,(hl)		; B = frame 2
	inc hl

rscX1:	jr $			; shift jump

	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	srl a
	rr b	
	
	jr rsc6

	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	sla b
	rl a
	
rsc6:	ld c,a			; Write the shifted bytes to the display.
	ld a,(de)
	or c
	ld (de),a
	inc e
	ld a,(de)
	or b
	ld (de),a
	dec e

	inc d
	pop bc
	djnz rsc2		; next scanline
	
	pop de			; restore display ptr
	pop af			; retrieve char mask
	rrca
	rrca
	rrca
	and 15			; A = num bits written
	ld b,(ix+rscPadding)
	add a,b
	ld b,(ix+rscBit)
	add a,b
	ld c,a
	rrca
	rrca
	rrca
	and $1f
	add a,e
	ld e,a
	ld a,c
	and 7
	ld (ix+rscBit),a	; Advance current bit

	pop hl
	inc hl
	jp rsc1			; next char
	
rsc4:	pop ix
	ret
;--------------------------------------------------------
_respawn_player

	dec (iy+playerLives)
	ld a,2^DISPLAY_FLAG_LIVES
	call _set_dirty_flags

	res pf_DYING,(iy+playerFlags)
	res pf_NO_RENDER,(iy+playerFlags)
	set pf_BUILD_ROOM,(iy+playerFlags)

	ld hl,PLAYER_DATA+playerLastPosition
	ld de,PLAYER_DATA+playerXFrac
	ld bc,8
	ldir
	
	ret

;---------------------------------------------------------
; Add dirty rect
;  ix+0 - tX
;  ix+1 - tY
;  ix+2 - tW
;  ix+3 - tH
;---------------------------------------------------------
_add_dirty_rect:

	ld hl,__RECTS_COUNT
	ld a,(hl)
	inc (hl)
	add a,a
	add a,a
	ld l,a
	ld h,__RECTS/256
	ld d,ixh
	ld e,ixl
	ex de,hl
	ldi
	ldi
	ldi
	ldi
	ret
;--------------------------------------------------------
; Fill memory
; A : value to fill
; BC : number of bytes
; HL : start of memory
;--------------------------------------------------------
_fill_mem:
	ld e,l
	ld d,h
	inc de
	dec bc
	ld (hl),a
	ldir
	ret

;--------------------------------------------------------
; KEY CODES
;
; Prefix	Port		0	1	2	3	4
;
; 000		$FEFE 		Shift	Z	X	C	V
; 001		$FDFE		A	S	D	F	G
; 010		$FBFE		Q	W	E	R	T
; 011		$F7FE		1	2	3	4	5
; 100		$EFFE		0	9	8	7	6
; 101		$DFFE		P	O	I	U	Y
; 110		$BFFE		Enter	L	K	J	H
; 111		$7FFE		Space	Sym	M	N	B

KEYBOARD_STATE	db 0,0,0,0,0,0,0,0
KEY_PORT_PREFIX	db $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f
;--------------------------------------------------------
KEYCODES_GAME:
	db $41	; 010 00001 - Q
	db $21	; 001 00001 - A
	db $a2	; 101 00010 - O
	db $a1	; 101 00001 - P
	db $e1 	; 111 00001 - SPACE
	db $c1	; 110 00001 - Enter
	db 0,0
;--------------------------------------------------------
KEYCODES_MENU:
	db $81	; 100 00001 - 0
	db $61	; 011 00001 - 1
	db $62	; 011 00010 - 2
	db $64	; 011 00100 - 3
	db $68 	; 011 01000 - 4
	db $70	; 011 10000 - 5
	db 0,0
;--------------------------------------------------------
keyU 		equ 0
keyD		equ 1
keyL		equ 2
keyR		equ 3
keyJump		equ 4
keyPause	equ 5

keyMenu0	equ 0
keyMenu1	equ 1
keyMenu2	equ 2
keyMenu3	equ 3
keyMenu4	equ 4
keyMenu5	equ 5
;--------------------------------------------------------
; Scan keyboard state + update inputflags
;  HL - keycode table for inputflags
;--------------------------------------------------------
_scan_keys:
scankeys_codes equ $+1	
	ld hl,0
	push hl

	ld de,KEYBOARD_STATE
	ld hl,KEY_PORT_PREFIX
	ld c,$fe
	ld b,8
sk10:	push bc	
	ld b,(hl)
	inc hl
	in a,(c)
	cpl
	and $1f
	ld (de),a
	inc de
	pop bc
	djnz sk10

	pop hl
	xor a
	ld c,a
	ld b,8
sk11:	ld a,(hl)
	inc hl
	or a
	jr z,sk13
	ld d,a
	rlca
	rlca
	rlca
	and 7
	ld e,a
	ld a,d
	and $1f
	ld d,0
	push hl
	ld hl,KEYBOARD_STATE
	add hl,de
	ld d,(hl)
	pop hl
	and d
	jr z,sk13
	scf
sk13:	rr c
	djnz sk11
	ld a,c
	ld (INPUTFLAGS),a
sk12:	ret
;--------------------------------------------------------
_byte_to_string:

	; A : byte value
	; HL : output buffer

	ld e,$30
	res 0,d
	ld b,100
	call bts0
	ld b,10
	call bts0
	set 0,d
	call bts3
	xor a
	ld (hl),a
	inc hl
	ret

bts0:	ld c,0
bts1:	cp b
	jr c,bts2
	inc c
	sub b
	jr bts1
bts2:	ld b,a
	ld a,c
	bit 0,d
	jr nz,bts3
	or a
	jr z,bts4	
bts3:	set 0,d
	add a,e
	ld (hl),a
	inc hl
	inc d
bts4:	ld a,b
	ret
;--------------------------------------------------------
_set_dirty_flags:

	; A = flags

	ld hl,__DISPLAY_DIRTY_FLAGS
	ld b,(hl)
	or b
	ld (hl),a
	push af
	push hl
	call _swap_displays
	pop hl
	pop af
	ld b,(hl)
	or b
	ld (hl),a
	call _swap_displays
	ret
;--------------------------------------------------------
; Calculate tile offset for width=32 arrays
;  D = tile X
;  E = tile Y
; Returns offset in HL (y*32+x)
;--------------------------------------------------------
_tile_to_offset:

	ld a,e
	rrca
	rrca
	rrca
	ld h,a
	and $e0
	or d
	ld l,a
	ld a,h
	and 3
	ld h,a
	ret
;--------------------------------------------------------
_blank_display:
	
	ld hl,__DISPLAY_ATTRIBUTES
	ld de,__DISPLAY_ATTRIBUTES+1
	ld bc,639
	xor a
	ld (hl),a
	ldir
	ret
;--------------------------------------------------------
_change_room:

	ld hl,__BANKM
	ld a,(hl)
	push af
	and $08
	rrca
	rrca
	ld b,7
	xor b
	ld b,a
	pop af
	and $f8
	or b
	ld bc,$7ffd
	di	
	out (c),a	
	ei

	exx				; Render blank holding screen.
	ex af,af'
	call _blank_display
	ex af,af'
	exx
	di	
	xor %1010			; Toggle display screen and backbuffer.
	ld (hl),a
	out (c),a
	ei

	push af
	push bc
	push hl
	call _build_room
	pop hl
	pop bc
	pop af

	di
	ld (hl),a
	out(c),a
	ei

	push af
	push bc
	push hl
	call _render_room
	pop hl
	pop bc
	pop af

	di	
	xor %1010
	ld (hl),a
	out (c),a
	ei

	push af
	push bc
	push hl
	call _render_room
	pop hl
	pop bc
	pop af

	di
	xor %1010
	ld (hl),a
	out (c),a
	ei

	ld hl,PLAYER_DATA+playerXFrac
	ld de,PLAYER_DATA+playerLastPosition
	ld bc,8
	ldir

	ld a,$ff
	ld (PLAYER_DATA+playerElevator),a

	ret

;--------------------------------------------------------
; Render static to currently paged screen
;  A = static ID
;  DE = dest x,y
;--------------------------------------------------------
_render_static:

	push de

	push af

	push de
	exx
	pop bc
	ld a,c
	add a,a
	add a,a
	add a,a
	ld h,__Y_LOOKUP/256
	ld l,a
	exx

	ld a,(__BANKM)
	and 7
	ld (rstaX),a
	ld b,__BANK_STATICS
	call _switchbank

	pop af

	ld hl,$c002			; beginning of static table
	add a,a				; static ID x 2
	add a,l
	ld l,a
	ld e,(hl)
	inc l
	ld d,(hl)
	ex de,hl			; HL = static header

	ld de,__SCRATCHMEM
	ld ix,__SCRATCHMEM
	ldi				; copy tile W
	ldi				; copy tile H
					; calc size
	ld b,(ix+0)
	ld c,(ix+1)
	xor a
rsta1:	add a,c
	djnz rsta1			; calc num tiles
	ld b,a
rsta5:	ld a,b
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ld b,a
	djnz rsta5

	ld hl,__SCRATCHMEM+2

rstaX equ $+1
	ld b,0
	call _switchbank		; switch back to display buffer

	ld a,(ix+1)
	add a,a
	add a,a
	add a,a
	ld b,a
rsta2:	push bc
	exx
	ld e,(hl)
	inc h
	ld d,(hl)
	dec h
	inc l
	ld a,e
	add a,b
	ld e,a
	push de
	exx
	pop de
	ld b,0
	ld c,(ix+0)
	ldir
	pop bc
	djnz rsta2

	pop de
	push hl
	call _tile_to_offset
	ld a,h
	or __DISPLAY_ATTRIBUTES/256
	ld h,a
	ex de,hl
	pop hl

	ld b,(ix+1)
rsta4:	push bc
	push de
	ld b,0
	ld c,(ix+0)
	ldir
	pop de
	ex de,hl
	ld bc,32
	add hl,bc
	ex de,hl
	pop bc
	djnz rsta4

	ret
;--------------------------------------------------------
_cls:
	ld hl,__DISPLAY_PIXELS
	ld bc,32*24*8
	xor a
	call _fill_mem

	ld hl,__DISPLAY_ATTRIBUTES
	ld bc,32*24
	ld a,7
	call _fill_mem

	ret
;--------------------------------------------------------
; Called when player hits a switch
;  A : switch ID
;--------------------------------------------------------
_hit_switch:

	ld ix,__SWITCHES
	ld de,SIZEOF_SWITCH_INSTANCE
	or a
	jr z,hs2
	ld b,a
hs1:	add ix,de
	djnz hs1
hs2:	ld a,(ix+5)
	call _get_switch_val
	jr z,hs3	
	ld c,0
	jr hs4
hs3:	ld c,1
hs4:	ld b,(ix+5)
	call _set_switch_val
	ret

;--------------------------------------------------------
_main_menu:

	di
	ld hl,__BANKM
	ld bc,$7ffd
	ld a,(hl)
	and $f8
	or 7
	bit 3,a				; normal or shadow screen?
	jr z,mm1
	xor 2
mm1:	ld (hl),a
	out (c),a			; switch in other screen.
	ei

	ld hl,__DISPLAY_ATTRIBUTES	; black out screen
	ld bc,768
	xor a
	call _fill_mem

	ld hl,__BANKM
	ld a,(hl)
	xor %00001010			; both toggle switched in screen and current
	ld (hl),a
	ld bc,$7ffd
	out (c),a			

	call _cls


	ld a,1
	ld de,8*256+0
	call _render_static		; render logo

	ld hl,__DISPLAY_ATTRIBUTES	; make logo bright
	ld b,32
mm8:	set 6,(hl)
	inc l
	djnz mm8

	ld hl,mm_IN			; write strings
	ld b,6
mm3:	push bc
	call _write_string	
	pop bc
	djnz mm3

	ld hl,$d880
	ld b,32
	ld e,3
	ld a,e
mm5:	ld (hl),a
	inc l
	inc a
	cp 8
	jr nz,mm6
	ld a,e
mm6:	djnz mm5

	
	ld hl,__BANKM
	ld a,(hl)
	xor %00001000
	ld (hl),a
	ld bc,$7ffd
	out (c),a			

	ei

	ld hl,KEYCODES_MENU
	ld (scankeys_codes),hl

	ld a,1
	ld ix,SFX_MODULE_A
	call _sfx_play_module		; play the main theme

mm7:	call _vsync

	call _scan_keys
	ld a,(INPUTFLAGS)
	bit keyMenu0,a
	jp nz,_new_game
	
	ld a,(__FRAMESKIP+3)		; rotate title colours
	or a
	jr nz,mm2
	ld hl,$d89e
	ld d,h
	ld e,$9f
	ld a,(de)
	push af
	ld bc,31
	lddr
	pop af
	ld (de),a

mm2:	jr mm7

mm_IN:		
	db 15,2,7,"IN",0
 	db 2,4,7,"DESCENT INTO RETRO MADNESS!!",0
	db 10,8,7,"0. START GAME",0
	db 10,10,7,"1. KEYBOARD",0
	db 10,12,7,"2. KEMPSTON",0
	db 10,14,7,"3. DEFINE KEYS",0

;--------------------------------------------------------
; Render a dialog
;  A : string ID
;--------------------------------------------------------
rdWidth 	equ 0
rdHeight 	equ 1
	db 0,0
_render_dialog:

	ld de,__SCRATCHMEM
	push de
	call _copy_string
	pop hl

	push hl
	ld ix,_render_dialog-2
	xor a
	ld (ix+rdWidth),a
	ld (ix+rdHeight),a
					; calculate dialog dimensions	
rd22:	inc (ix+rdHeight)
	ld b,0
rd21:	ld a,(hl)
	inc l
	or a				; check for string terminator
	jr z,rd20			
	cp $0a				; check for newline
	jr z,rd23
	inc b
	jr rd21	
rd23:	ld a,(ix+rdWidth)
	cp b
	jr nc,rd22
	ld (ix+rdWidth),b
	jr rd22
					; *** Create the string buffer ****
					; This is the dialog frame with the string lines within it.
rd20:	pop hl				
	
	ld de,__STATIC_GFX_BUFFER
	xor a
	ld (de),a			; Tile X
	inc de
	ld (de),a			; Tile Y
	inc de
	ld a,colBlue*8+colWhite
	ld (de),a			; Colour
	inc de
	
	ld a,16				; Top of frame
	ld (de),a
	inc de
	ld b,(ix+rdWidth)
	ld a,17
rd24:	ld (de),a
	inc de
	djnz rd24
	ld a,18
	ld (de),a
	inc de
	ld a,10
	ld (de),a
	inc de

	ld c,(ix+rdHeight)
rd26:	ld a,19				; Content row
	ld (de),a
	inc de
	ld b,(ix+rdWidth)
rd25:	ld a,(hl)
	cp 10
	jr z,rd30
	or a
	jr z,rd30
	inc hl
	jr rd27
rd30:	ld a,32
rd27:	ld (de),a
	inc de
	djnz rd25
	ld a,20
	ld (de),a
	inc de
	ld a,10
	ld (de),a
	inc de
	inc hl
	dec c
	jr nz,rd26

	ld a,21				; Bottom of frame
	ld (de),a
	inc de
	ld b,(ix+rdWidth)
	ld a,22
rd28:	ld (de),a
	inc de
	djnz rd28
	ld a,23
	ld (de),a
	inc de
	xor a
	ld (de),a
	
	ld b,(ix+rdWidth)
	ld c,(ix+rdHeight)
	ld ix,rd_rect
	ld (ix+0),$80
	ld (ix+1),$00
	ld a,2
	add a,b
	ld (ix+2),a
	ld a,2
	add a,c
	ld (ix+3),a
	call _add_dirty_rect

	ld hl,__STATIC_GFX_BUFFER
	call _write_string
	call _vsync
	call _swap_displays

rd31:	call _scan_keys
	ld a,(INPUTFLAGS)
	or a
	jr nz,rd31
rd32:	call _scan_keys
	ld a,(INPUTFLAGS)
	or a
	jr z,rd32
	ret
rd_rect: 
	ds 4
;--------------------------------------------------------	
_invert_info_box:

	ld bc,(INFO_POS)
	call _get_tile_offset
	ld hl,__DISPLAY_ATTRIBUTES
	add hl,bc
	ld a,(hl)
	ld b,a
	and 7
	rlca
	rlca
	rlca
	ld c,a
	ld a,b
	and $38
	rrca
	rrca
	rrca
	or c
	or $40				; add bright
	ld c,2
rd5:	ld b,2
rd6:	ld (hl),a
	inc hl
	djnz rd6
	push af
	ld a,l
	add a,30
	ld l,a
	pop af
	dec c
	jr nz,rd5	
	ret
;------------------------------------------------------------------
_erase_object:

	; A = object ID

	ld hl,__ROOM_OBJECTS
	add a,a
	add a,a
	add a,l
	ld l,a			; HL = object ptr
	
	ld e,(hl)
	inc l
	ld a,(hl)		; DE = tile offset
	xor $0c			; Convert to tile props ptr
	ld d,a
	inc l
	ld b,(hl)		; B = tile width
	inc l
	ld c,(hl)		; C = tile height
	ex de,hl

	call eo_clear		; Clear properties.
	set 3,h
	call eo_clear		; Clear tile data.

	ld a,l			; Convert offset to X,Y
	and $e0
	ld d,a
	ld a,h
	and 3
	or d
	ld h,a
	rlca
	rlca
	rlca
	and 31
	ld h,a
	ld a,l
	and $1f
	ld l,h			; and swap Y,X -> X,Y
	ld h,a
	ex de,hl

	ld ix,eo_rect
	set 7,d
	ld (ix+0),d
	ld (ix+1),e
	ld (ix+2),b
	ld (ix+3),c

	call _add_dirty_rect	; Add background dirty rects.
	di			; i.e. to both buffers.
	ld a,(__BANKM)
	push af
	and 7
	xor 2
	exx
	ld bc,$7ffd
	out (c),a
	ei
	exx
	call _add_dirty_rect
	pop af
	exx
	out (c),a
	exx

	ret

eo_clear:
	push bc
	push hl
	xor a
eo1:	push bc
	push hl
eo2:	ld (hl),a
	inc l
	djnz eo2
	pop hl
	ld bc,32
	add hl,bc
	pop bc
	dec c
	jr nz,eo1	
	pop hl
	pop bc
	ret

eo_rect
	ds 4
;--------------------------------------------------------
_get_tile_offset:

	; B = tile Y
	; C = tile X
	; returns offset in BC
	; 000y yyyy 000x xxxx => 0000 00yy yyyx xxxx

	xor a
	srl b
	rra
	srl b
	rra
	srl b
	rra
	or c
	ld c,a
	ret
;--------------------------------------------------------
_get_tile_offset_px:

	; B = pixel Y
	; C = pixel X
	; returns offset in BC
	; yyyy yYYY xxxx xXXX => 0000 00yy yyyx xxxx

	ld a,c
	rrca
	rrca
	rrca
	and $f8
	ld c,a
	ld a,b
	rlca
	rlca
	ld b,a
	and $e0
	or c
	ld c,a
	ld a,b
	and 3
	ld b,a
	ret

;--------------------------------------------------------
_offset_room:
	
	; D = offset X
	; E = offset Y

	ld a,(ROOM_ID)
	ld l,a
	and $f8
	rrca
	rrca
	rrca
	add a,d
	and $1f
	rlca
	rlca
	rlca
	ld h,a
	ld a,l
	and 7
	add a,e
	and 7
	or h
	ld (ROOM_ID),a
	ld hl,PLAYER_DATA+playerFlags
	set pf_BUILD_ROOM,(hl)
	ret
;------------------------------------------------------------------
_game_over:

	

	ret
;------------------------------------------------------------------
_update_lift:
	
	ld iy,PLAYER_DATA
	ld a,(iy+playerY)
	bit 2,(iy+playerMovement)
	jr z,ul3

	inc a
	cp 128
	jr nz,ul1

	xor a
	ld (iy+playerY),a
	ld de,$0001
	call _offset_room
	ret

ul3:
	dec a
	or a
	jr nz,ul1	

	ld a,128
	ld (iy+playerY),a
	ld de,$00ff
	call _offset_room
	ret

ul1:	cp 96
	jr nz,ul2
	res 1,(iy+playerMovement)

ul2:	ld (iy+playerY),a
	ld ix,__SPRITES
	ld a,(iy+playerY)
	add a,16
	ld (ix+siPixelY),a

	ret
;------------------------------------------------------------------
; Check if room lift can go up
;  A : room ID
; Returns A unchanged if OK, A = zero if not.
;------------------------------------------------------------------
_if_lift_up:
	ld hl,LIFT_ROOMS_UP
	ld b,LIFT_ROOMS_UP_END-LIFT_ROOMS_UP
	jr ilu1
_if_lift_down:
	ld hl,LIFT_ROOMS_DOWN
	ld b,LIFT_ROOMS_DOWN_END-LIFT_ROOMS_DOWN
ilu1:	ld c,(hl)
	inc hl
	cp c
	ret z
	djnz ilu1
	xor a
	ret
;------------------------------------------------------------------
; Set a tile area - will be drawn next frame
;  ix+0 - tW
;  ix+1 - tH
;  ix+2 - tX
;  ix+3 - tY
;  HL - tile IDs ptr
;------------------------------------------------------------------
_set_tile:

	push hl
	ld d,(ix+0)
	ld e,(ix+1)
	call _tile_to_offset
	ld a,__ROOM_DATA/256
	add a,h
	ld h,a
	ex de,hl			; DE = dst ptr
	ld a,32
	sub (ix+0)
	ld l,a
	ld h,0
	ld (stX),hl
	pop hl

	ld b,(ix+1)
st1:	push bc
	ld b,(ix+0)
	ld c,0
	ldir
	
stX equ $+1
	ld bc,0
	ex de,hl
	add hl,bc
	ex de,hl
	pop bc
	djnz st1

	call _add_dirty_rect
	ld a,(__BANKM)
	xor 2
	ld bc,$7ffd
	out (c),a
	push af
	call _add_dirty_rect
	pop af
	xor 2
	out (c),a

	ret
;------------------------------------------------------------------
; Allocate a dynamic sprite
;  - Adds a new uninitialised sprite entry
;  - Returns sprite address in HL  
;------------------------------------------------------------------
_alloc_sprite:
	ld hl,__SPRITES
	ld bc,__SIZEOF_SPRITE_INST
as2:	ld a,(hl)
	cp $ff
	jr z,as1
	add hl,bc
	jr as2
as1:	push hl
	add hl,bc
	ld a,$ff
	ld (hl),a
	pop hl
	ret
;------------------------------------------------------------------
; Delete a dynamic sprite
;  HL - sprite instance ptr
;------------------------------------------------------------------
_delete_sprite:

	ld d,h
	ld e,l
	ld bc,__SIZEOF_SPRITE_INST
dsp1:	ld a,(hl)
	cp $ff
	jr z,dsp2
	add hl,bc
	jr dsp1
dsp2:	

	ret

;------------------------------------------------------------------
; ********************* AUDIO *******************
;------------------------------------------------------------------

; sfx module data header
sfxSpeed			equ 0
sfxFlags			equ 1
sfxPlayOrderCount		equ 2
sfxPlayOrderLoop		equ 3
sfxPlayOrderPtr			equ 4
sfxSamplesPtr			equ 6
sfxOrnamentsPtr			equ 8
sfxChannelsPtr			equ 10
SIZEOF_SFX_MODULE_HEADER	equ 12

; sfx module runtime buffer
sfxmModulePtr			equ 0
sfxmSpeedCounter		equ 2
sfxmLineCounter			equ 3
sfxmOrderIdx			equ 4
sfxmChannelPtr			equ 7
sfxmFlags			equ 9
SIZEOF_SFXM_BUFFER		equ 10

SFXMFLAG_ENABLED		equ 0

__NOTE_TABLE			equ $ff00

;------------------------------------------------------------------
; Audio per-frame update
;------------------------------------------------------------------
su_CHANNEL_MASK db 0

_sfx_update:
	
	ld a,(__BANKM)
	push af
	ld b,__BANK_STATICS
	call _switchbank

	xor a
	ld (su_CHANNEL_MASK),a
	ld (SFX_CHANNEL_A+sfxcFlags),a
	ld (SFX_CHANNEL_B+sfxcFlags),a
	ld (SFX_CHANNEL_C+sfxcFlags),a

	dec a
	ld (SFX_AUDIO_BUFFER+sfxMixer),a	; set mixer to $ff

	ld ix,SFX_MODULE_A
	bit SFXMFLAG_ENABLED,(ix+sfxmFlags)
	jr z,su1
	call _sfx_update_module

su1:	
	ld ix,SFX_MODULE_B
	bit SFXMFLAG_ENABLED,(ix+sfxmFlags)
	jr z,su2
	call _sfx_update_module

su2:	ld a,(su_CHANNEL_MASK)
	bit 0,a
	jr z,su3
	ld ix,SFX_CHANNEL_A
	call _sfx_update_channel
	
su3:	ld a,(su_CHANNEL_MASK)
	bit 1,a
	jr z,su4
	ld ix,SFX_CHANNEL_B
	call _sfx_update_channel
	
su4:	ld a,(su_CHANNEL_MASK)
	bit 2,a
	jr z,su5
	ld ix,SFX_CHANNEL_C
	call _sfx_update_channel

su5:	call _sfx_send_buffer

	pop af
	and 7
	ld b,a
	call _switchbank
	ret

;------------------------------------------------------------------
; Play an audio module
;  A - module ID, $ff for none
;  IX - sfxm buffer
;------------------------------------------------------------------
_sfx_play_module:

	push af
	ld a,(__BANKM)	
	ld (spmX),a
	ld b,__BANK_STATICS
	call _switchbank
	pop af
	
	cp $ff					; $ff - disable module buffer
	jr nz,spm1
	res SFXMFLAG_ENABLED,(ix+sfxmFlags)
	jr spm2

spm1:	set SFXMFLAG_ENABLED,(ix+sfxmFlags)
	ld hl,($c000)				; HL = start of audio table
	add a,a					; adjust ID for ptr size
	ld c,a
	ld b,0
	add hl,bc				
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld iyh,d
	ld iyl,e
	call _sfx_init_module	
spm2:
spmX equ $+1
	ld a,0
	and 7
	ld b,a
	jp _switchbank
;------------------------------------------------------------------
; Init the audio system
;------------------------------------------------------------------
_sfx_init:

	ld a,(__BANKM)
	push af
	ld b,__BANK_STATICS
	call _switchbank

	call _sfx_init_send_buffer
	
	ld hl,SFX_CHANNEL_A
	ld bc,SIZEOF_SFXC_CHANNEL*3 + SIZEOF_SFXM_BUFFER*2
	xor a
	call _fill_mem

	xor a					; set channel offsets
	ld (SFX_CHANNEL_A+sfxcOffset),a
	inc a
	ld (SFX_CHANNEL_B+sfxcOffset),a
	inc a
	ld (SFX_CHANNEL_C+sfxcOffset),a

	ld a,$ff				; set both modules to be playing nothing.
	push af
	ld ix,SFX_MODULE_A
	call _sfx_play_module
	pop af
	ld ix,SFX_MODULE_B
	call _sfx_play_module

	pop af
	and 7
	ld b,a
	call _switchbank
	ret
;------------------------------------------------------------------
; Init an audio module
;  IX - sfxm buffer
;  IY - module data ptr
;------------------------------------------------------------------
_sfx_init_module:

	di
	ld a,iyl
	ld (ix+sfxmModulePtr),a
	ld a,iyh
	ld (ix+sfxmModulePtr+1),a

	ld a,1						; set counters to 1 so that they
	ld (ix+sfxmSpeedCounter),a			; will be decremented to 0 in first update
	ld (ix+sfxmLineCounter),a
	ld a,$ff
	ld (ix+sfxmOrderIdx),a				; ...and order idx will be incremented to 0
	set SFXMFLAG_ENABLED,(ix+sfxmFlags)
	push ix

	bit 0,(iy+1)					; channel A enabled?
	jr z,sim1
	ld ix,SFX_CHANNEL_A
	call _sfx_init_channel
	xor a
	ld (ix+sfxcOffset),a

sim1:
	bit 1,(iy+1)					; channel B enabled?
	jr z,sim2
	ld ix,SFX_CHANNEL_B
	ld a,1
	call _sfx_init_channel
	ld a,1
	ld (ix+sfxcOffset),a
sim2:
	bit 2,(iy+1)					; channel C enabled?
	jr z,sim3
	ld ix,SFX_CHANNEL_C
	call _sfx_init_channel
	ld a,2
	ld (ix+sfxcOffset),a
sim3:	
	pop ix
	ei
	ret
;------------------------------------------------------------------
; Init audio channel
;  IX - channel buffer
;  IY - module data
;------------------------------------------------------------------
_sfx_init_channel:

	ld hl,sfx_CHANNEL_DEFAULT
	ld d,ixh
	ld e,ixl
	ld bc,SIZEOF_SFXC_CHANNEL
	ldir
	ret

sfx_ORNAMENT_DEFAULT db 1,0,0
sfx_SAMPLE_DEFAULT db $ff,0,0,0
sfx_CHANNEL_DEFAULT:
	dw 0				; sfxcNotePtr
	dw sfx_SAMPLE_DEFAULT		; sfxcSamplePtr
	db 0				; sfxcSampleLineIdx
	dw sfx_ORNAMENT_DEFAULT		; sfxcOrnamentPtr
	db 0				; sfxcOrnamentLineIdx
	dw 0				; sfxcPitchDeviation
	db 15				; sfxcVolume
	db 0				; sfxcVolumeDeviation
	db 0				; sfxcFlags
	db 0				; sfxcOffset
	db 0				; sfxcMixer
	db 0				; sfxcNoiseDeviation
;------------------------------------------------------------------
; Update audio module
;  IX - sfxm_buffer
;------------------------------------------------------------------
_sfx_update_module:

	bit SFXMFLAG_ENABLED,(ix+sfxmFlags)
	ret z

	ld a,(ix+sfxmModulePtr)
	ld iyl,a
	ld a,(ix+sfxmModulePtr+1)
	ld iyh,a				; IY = module ptr

	ld hl,su_CHANNEL_MASK			; Enable channels
	ld a,(hl)
	and 7
	or (iy+sfxFlags)
	ld (hl),a

	dec (ix+sfxmSpeedCounter)		; dec speed counter
	ret nz
	ld a,(iy+sfxSpeed)
	ld (ix+sfxmSpeedCounter),a		; reset speed counter

	dec (ix+sfxmLineCounter)		; dec line counter
	jr nz,_sfx_load_pattern_line		; if not 0, init the next line.
	
	ld a,(ix+sfxmOrderIdx)
	inc a
	cp (iy+sfxPlayOrderCount)
	jr nz,sum2
	ld a,(iy+sfxPlayOrderLoop)
sum2:	ld (ix+sfxmOrderIdx),a
						; fall through...
;------------------------------------------------------------------
; Init an sfx pattern
;  IX - sfx_module buffer
;  IY = sfx module data
;------------------------------------------------------------------
_sfx_load_pattern:
	
	ld c,(ix+sfxmOrderIdx)
	ld l,(iy+sfxPlayOrderPtr)
	ld h,(iy+sfxPlayOrderPtr+1)
	ld b,0
	add hl,bc
	ld a,(hl)				; A = current pattern idx
	push iy
	pop hl
	ld bc,SIZEOF_SFX_MODULE_HEADER
	add hl,bc				; HL = pattern ptr table
	add a,a
	ld c,a
	add hl,bc				; HL = current pattern ptr
	ld e,(hl)
	inc hl
	ld d,(hl)				; DE = current pattern
	ld a,(de)				; A = num pattern lines
	ld (ix+sfxmLineCounter),a		; init line counter
	inc de
	ld (ix+sfxmChannelPtr),e
	ld (ix+sfxmChannelPtr+1),d
						; fall through...
;------------------------------------------------------------------
; Init an sfx pattern
;  IX - sfx_module buffer
;  IY = sfx module data
;------------------------------------------------------------------
_sfx_load_pattern_line:

	ld l,(ix+sfxmChannelPtr)
	ld h,(ix+sfxmChannelPtr+1)
	ld a,(iy+sfxFlags)
	push ix

	ld ix,SFX_CHANNEL_A
	bit 0,a					; channel A enabled?
	call nz,_sfx_load_channel

	ld ix,SFX_CHANNEL_B
	bit 1,a					; channel B enabled?
	call nz,_sfx_load_channel

	ld ix,SFX_CHANNEL_C
	bit 2,a					; channel C enabled?
	call nz,_sfx_load_channel
	
	pop ix
	ld (ix+sfxmChannelPtr),l		; update channel ptr
	ld (ix+sfxmChannelPtr+1),h

	ret
;------------------------------------------------------------------
; Init an sfx pattern
;  HL - address of channel data idx
;  IX - channel buffer
;  IY - module data
;------------------------------------------------------------------
_sfx_load_channel:

	push af
	push hl
	ld a,(hl)
	or a
	jp z,sic6					; channel = 0, no change

	dec a
	ld l,(iy+sfxChannelsPtr)
	ld h,(iy+sfxChannelsPtr+1)
	add a,a
	add a,a					; channels are 4 bytes long
	ld c,a
	xor a
	ld b,a
	add hl,bc				
	ex de,hl				; DE = channel data

	ld a,(de)				; A = note idx
	cp $ff					; check for rest indicator
	jr nz,sic1
	xor a
	ld (ix+sfxcNotePtr),a
	ld (ix+sfxcNotePtr+1),a
	ld (ix+sfxcVolume),a	
	jp sic6

sic1:	or a
	jr z,sic2				; if 0, no change to note.
	dec a
	ld hl,__NOTE_TABLE
	add a,a
	ld c,a
	xor a
	ld b,a
	add hl,bc
	ld (ix+sfxcNotePtr),l
	ld (ix+sfxcNotePtr+1),h
	xor a
	ld (ix+sfxcVolumeDeviation),a		; reset volume deviation
	ld (ix+sfxcSampleLineIdx),a		; reset sample
	ld (ix+sfxcOrnamentLineIdx),a		; reset ornament
	ld (ix+sfxcNoiseDeviation),a		; reset noise
	
sic2:	inc de
	ld a,(de)				; A = sample idx
	or a
	jr z,sic3				; if 0, no change to sample.
	dec a
	ld l,(iy+sfxSamplesPtr)
	ld h,(iy+sfxSamplesPtr+1)
	add a,a
	ld c,a
	xor a
	ld b,a
	add hl,bc
	push de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	pop de
	ld (ix+sfxcSamplePtr),l
	ld (ix+sfxcSamplePtr+1),h
	xor a
	ld (ix+sfxcSampleLineIdx),a
	ld (ix+sfxcPitchDeviation),a
	ld (ix+sfxcPitchDeviation+1),a

sic3:	inc de
	ld a,(de)				; A = ornament idx
	or a
	jr z,sic4				; if 0, no change to ornament.
	dec a
	ld l,(iy+sfxOrnamentsPtr)
	ld h,(iy+sfxOrnamentsPtr+1)
	add a,a
	ld c,a
	xor a
	ld b,a
	add hl,bc
	push de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	pop de
	ld (ix+sfxcOrnamentPtr),l
	ld (ix+sfxcOrnamentPtr+1),h
	xor a
	ld (ix+sfxcOrnamentLineIdx),a

sic4:	inc de
	ld a,(de)				; A = volume
	or a
	jr z,sic5				; if 0, no change to volume.
	ld (ix+sfxcVolume),a

sic5:	
	
sic6:	pop hl
	pop af
	inc hl
	ret
;------------------------------------------------------------------
; Update audio channel
;  IX - sfxc_channel buffer
;------------------------------------------------------------------
_sfx_update_channel:

sfxcNotePtr			equ 0
sfxcSamplePtr			equ 2
sfxcSampleLineIdx		equ 4
sfxcOrnamentPtr			equ 5
sfxcOrnamentLineIdx		equ 7
sfxcPitchDeviation		equ 8
sfxcVolume			equ 10
sfxcVolumeDeviation		equ 11
sfxcFlags			equ 12
sfxcOffset			equ 13
sfxcMixer			equ 14
sfxcNoiseDeviation		equ 15

SIZEOF_SFXC_CHANNEL		equ 16
SFX_CHANNEL_A			equ $e020
SFX_CHANNEL_B			equ SFX_CHANNEL_A + SIZEOF_SFXC_CHANNEL
SFX_CHANNEL_C			equ SFX_CHANNEL_B + SIZEOF_SFXC_CHANNEL
SFX_MODULE_A			equ SFX_CHANNEL_C + SIZEOF_SFXC_CHANNEL
SFX_MODULE_B			equ SFX_MODULE_A + SIZEOF_SFXM_BUFFER

	ld l,(ix+sfxcOrnamentPtr)
	ld h,(ix+sfxcOrnamentPtr+1)	
	ld a,(ix+sfxcOrnamentLineIdx)
	add a,2					; add 2 for ornament header
	add a,l
	ld l,a
	ld a,(hl)				; a = ornament
	add a,a
	ld c,a
	ld b,0
	ld l,(ix+sfxcNotePtr)
	ld h,(ix+sfxcNotePtr+1)	
	ld a,l
	or h
	jr nz,suc4
	xor a
	jp suc33

suc4:	add hl,bc
	ld e,(hl)
	inc hl	
	ld d,(hl)				; DE = pitch frequency

	ld l,(ix+sfxcSamplePtr)
	ld h,(ix+sfxcSamplePtr+1)
	ld a,(ix+sfxcSampleLineIdx)	
	ld b,a
	add a,a
	add a,a					
	add a,b
	add a,2					; A = A*5+2 (sample line size = 5)
	ld c,a
	ld b,0
	add hl,bc				; HL = sample line
	
	ld a,(hl)				; A = sample flags
	inc hl	
	ld b,a
	and 9					; Set mixer status
	ld (ix+sfxcMixer),a
	ld a,b

	ld c,(hl)
	inc hl
	ld b,(hl)				; DE = tone value
	inc hl
	bit 1,a					; Is tone value relative
	jr z,suc5
						; relative tone value
	push hl
	ld l,(ix+sfxcPitchDeviation)
	ld h,(ix+sfxcPitchDeviation+1)	
	add hl,bc
	ld (ix+sfxcPitchDeviation),l
	ld (ix+sfxcPitchDeviation+1),h	
	pop hl
	jr suc6
						; absolute tone value
suc5:	ex de,hl
	add hl,bc
	ex de,hl

	
suc6:	ld c,a
	push af
	ld a,(hl)				; A = noise value
	inc hl
	add a,(ix+sfxcNoiseDeviation)
	bit 4,c					; is tone value relative?
	jr z,suc9
	ld (ix+sfxcNoiseDeviation),a		; relative, update deviation
suc9:	push de
	ld de,SFX_AUDIO_BUFFER+sfxNoisePitch
	ld (de),a
	pop de
	pop af
	
	ld b,(hl)				; B = volume + delta flags
	ld a,b
	and 15					
	ld c,a					; C = absolute volume
	ld a,(ix+sfxcVolumeDeviation)
	bit 4,b					; increase deviation?
	jr z,suc30
	inc a
	and 15
	jr suc31
suc30:	bit 5,b					; decrease deviation?
	jr z,suc34
	cp $f0
	jr z,suc31
	dec a
suc31:	ld (ix+sfxcVolumeDeviation),a
suc34:	add a,c
	add a,(ix+sfxcVolume)
	sub 15
	bit 7,a
	jr z,suc32
	xor a
	jr suc33
suc32:	cp 16
	jr c,suc33
	ld a,15
suc33:	ld c,a
	ld a,sfxVolumeA
	add a,(ix+sfxcOffset)
	ld l,a
	ld h,SFX_AUDIO_BUFFER/256
	ld (hl),c				; write volume to buffer

	ld l,(ix+sfxcPitchDeviation)
	ld h,(ix+sfxcPitchDeviation+1)
	add hl,de
	ex de,hl
						;*******************************
						; Output values to audio buffer
						;*******************************
suc20:	ld h,SFX_AUDIO_BUFFER/256	
	ld a,(ix+sfxcOffset)
	add a,a
	ld l,a
	ld (hl),e				; write pitch coarse
	inc l
	ld (hl),d				; write pitch fine

	ld c,(ix+sfxcMixer)
	ld a,(ix+sfxcOffset)
	or a
	jr z,suc7
	ld b,a
suc8:	sla c
	djnz suc8
suc7:	ld l,sfxMixer
	ld a,c
	cpl
	and (hl)
	ld (hl),a				; write mixer flags

	xor a
	set 0,a
	ld (ix+sfxcFlags),a
	
	; TODO

	ld l,(ix+sfxcOrnamentPtr)		; inc ornament line +
	ld h,(ix+sfxcOrnamentPtr+1)		; loop if necessary
	ld a,(ix+sfxcOrnamentLineIdx)
	inc a
	cp (hl)
	jr nz,suc10
	inc hl
	ld a,(hl)
suc10:	ld (ix+sfxcOrnamentLineIdx),a

	ld l,(ix+sfxcSamplePtr)			; inc sample line +
	ld h,(ix+sfxcSamplePtr+1)		; loop if necessary
	ld a,(ix+sfxcSampleLineIdx)
	inc a
	cp (hl)
	jr nz,suc11
	inc hl
	ld a,(hl)
suc11:	ld (ix+sfxcSampleLineIdx),a

	ret
;------------------------------------------------------------------
; Output audio buffer to AY
;------------------------------------------------------------------
_sfx_send_buffer:

SFX_AUDIO_BUFFER 		equ $e000	; move, must be XX00
SIZEOF_SFX_AUDIO_BUFFER		equ 14

sfxChannelA_h			equ 0
sfxChannelA_l			equ 1
sfxChannelB_h			equ 2
sfxChannelB_l			equ 3
sfxChannelC_h			equ 4
sfxChannelC_l			equ 5
sfxNoisePitch			equ 6
sfxMixer			equ 7
sfxVolumeA			equ 8
sfxVolumeB			equ 9
sfxVolumeC			equ 10
sfxEnvelopeDuration_h		equ 11
sfxEnvelopeDuration_l		equ 12
sfxEnvelopeShape		equ 13

ssb2:	ld hl,SFX_AUDIO_BUFFER
	ld bc,$fffd			; $fffd = reg no, $7ffd = reg val.
	ld d,11
ssb1:	set 6,b
	out (c),l			; send register number
	ld a,(hl)
	inc l
	res 6,b
	out (c),a			; send register value
	dec d
	jr nz,ssb1
	ret

_sfx_init_send_buffer:

	ld hl,SFX_AUDIO_BUFFER_DEFAULT
	ld de,SFX_AUDIO_BUFFER	
	ld bc,11
	ldir
	ret

SFX_AUDIO_BUFFER_DEFAULT:
	db 0,0,0,0,0,0,0,$ff,0,0,0

	ret
;------------------------------------------------------------------
org __FLIP_BITS_TABLE

	db $00,$80,$40,$c0,$20,$a0,$60,$e0,$10,$90,$50,$d0,$30,$b0,$70,$f0
	db $08,$88,$48,$c8,$28,$a8,$68,$e8,$18,$98,$58,$d8,$38,$b8,$78,$f8
	db $04,$84,$44,$c4,$24,$a4,$64,$e4,$14,$94,$54,$d4,$34,$b4,$74,$f4
	db $0c,$8c,$4c,$cc,$2c,$ac,$6c,$ec,$1c,$9c,$5c,$dc,$3c,$bc,$7c,$fc
	db $02,$82,$42,$c2,$22,$a2,$62,$e2,$12,$92,$52,$d2,$32,$b2,$72,$f2
	db $0a,$8a,$4a,$ca,$2a,$aa,$6a,$ea,$1a,$9a,$5a,$da,$3a,$ba,$7a,$fa
	db $06,$86,$46,$c6,$26,$a6,$66,$e6,$16,$96,$56,$d6,$36,$b6,$76,$f6
	db $0e,$8e,$4e,$ce,$2e,$ae,$6e,$ee,$1e,$9e,$5e,$de,$3e,$be,$7e,$fe
	db $01,$81,$41,$c1,$21,$a1,$61,$e1,$11,$91,$51,$d1,$31,$b1,$71,$f1
	db $09,$89,$49,$c9,$29,$a9,$69,$e9,$19,$99,$59,$d9,$39,$b9,$79,$f9
	db $05,$85,$45,$c5,$25,$a5,$65,$e5,$15,$95,$55,$d5,$35,$b5,$75,$f5
	db $0d,$8d,$4d,$cd,$2d,$ad,$6d,$ed,$1d,$9d,$5d,$dd,$3d,$bd,$7d,$fd
	db $03,$83,$43,$c3,$23,$a3,$63,$e3,$13,$93,$53,$d3,$33,$b3,$73,$f3
	db $0b,$8b,$4b,$cb,$2b,$ab,$6b,$eb,$1b,$9b,$5b,$db,$3b,$bb,$7b,$fb
	db $07,$87,$47,$c7,$27,$a7,$67,$e7,$17,$97,$57,$d7,$37,$b7,$77,$f7
	db $0f,$8f,$4f,$cf,$2f,$af,$6f,$ef,$1f,$9f,$5f,$df,$3f,$bf,$7f,$ff

org __Y_LOOKUP

	db $00,$00,$00,$00,$00,$00,$00,$00,$20,$20,$20,$20,$20,$20,$20,$20
	db $40,$40,$40,$40,$40,$40,$40,$40,$60,$60,$60,$60,$60,$60,$60,$60
	db $80,$80,$80,$80,$80,$80,$80,$80,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
	db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
	db $00,$00,$00,$00,$00,$00,$00,$00,$20,$20,$20,$20,$20,$20,$20,$20
	db $40,$40,$40,$40,$40,$40,$40,$40,$60,$60,$60,$60,$60,$60,$60,$60
	db $80,$80,$80,$80,$80,$80,$80,$80,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
	db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
	db $00,$00,$00,$00,$00,$00,$00,$00,$20,$20,$20,$20,$20,$20,$20,$20
	db $40,$40,$40,$40,$40,$40,$40,$40,$60,$60,$60,$60,$60,$60,$60,$60
	db $80,$80,$80,$80,$80,$80,$80,$80,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
	db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0

org __Y_LOOKUP+$100

	db $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	db $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	db $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	db $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	db $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
	db $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
	db $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
	db $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
	db $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
	db $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
	db $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7
	db $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7

org __CHAR_MASKS

	ds 32,0
	db $20,$0b,$42,$31,$2a,$31,$2a,$13,$14,$12,$2a,$2a,$13,$2a,$13,$2a
	db $31,$2a,$31,$31,$31,$31,$31,$31,$31,$31,$0b,$12,$1b,$2a,$1b,$31
	db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	db $31,$31,$31,$31,$31,$31,$31,$31,$31,$38,$31,$1c,$21,$19,$21,$31
	db $31,$29,$2a,$22,$21,$29,$1b,$21,$29,$1a,$42,$22,$1b,$21,$21,$29
	db $21,$31,$22,$21,$22,$21,$21,$21,$21,$21,$21,$2a,$0c,$21,$42,$80

; $08	0001 100	0c
; $0c	0010 100	14
; $0e	0011 100	1c
; $10	0001 011	0b
; $18	0010 011	13
; $1c	0011 011	1b
; $30	0010 010	12
; $38	0011 010	1a
; $3e	0101 010	2a
; $3c	0100 010	42
; $80	0001 000	04
; $70	0011 001	19
; $7c	0101 001	21
; $7e	0110 001	31
; $fe	0111 000	38
; $ff	1000 000	80





