;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                          H e l p - B r o w s e r                           @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Todo
;- different fonts
;- "index" feature
;- improvements regarding handling and switching between "contents"/"search"

;--- PROGRAM-ROUTINES ---------------------------------------------------------
;### ...

;HLP format description
;8B "SYMHLP10"
;1W length of list data (=number of entries * 4) [a]
;1W length of list text + title text [b]
;?B list data [a]
;   1W chapter length [bit0-12, 8K max], bit13=flag, if list entry is coloured
;   1W list text offset

;?B title text, list texts (all 0-terminated) [b]

;?B chapter data
;   1B font type (0=multi, 1=system, 2=code, +128=compressed chapter text)
;   1B number of links
;   ?B link data
;      1B chapter ID
;      1B link text length
;      1W link text offset
;   ?B chapter text


txtbufmax   equ 8192-1
txtlinmax   equ 1000
lstlinmax   equ 128

;==============================================================================
;### CODE AREA ################################################################
;==============================================================================

;### PRGPRZ -> Application process
prgwin      db 0    ;main     window ID

prgprz  call prgmem
        jr c,prgend
        call modini
        call parall
        push de

        ld bc,256*DSK_SRV_SCRCNV+MSC_DSK_DSKSRV
        ld de,gfxcnvtab
        ld hl,(App_BnkNum)
        call msgsnd
        rst #30

        ld de,prgwindat
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open startmenu edit window
        jp c,prgend             ;memory full -> quit process
        ld (prgwin),a           ;window has been opened -> store ID

        pop af
        or a
        jr z,prgprz0
        ld hl,(SyShell_CmdParas+0)
        ld de,hlppth
        ld bc,256
        ldir
        call hlpopn

prgprz0 ld ix,(App_PrcID)           ;check for messages
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #08
        db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
;        cp MSR_SYS_SELOPN
;        jp z,selopna
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_CLOSE
        jr z,prgend
        cp DSK_ACT_MENU
        jr z,prgprz1
        cp DSK_ACT_TOOLBAR
        jr z,prgprz1
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld hl,(App_MsgBuf+8)
        ld a,h
        or l
        jr z,prgprz0
        jp (hl)

;### PRGEND -> End program
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGMEM -> reserves memory for textcontrol/font and prepares memory areas
;### Output     CF=0 -> ok, (prgmemtab+0)=text record, (prgmemtab+5)=text buffer
;###            CF=1 -> memory full
prgmem  ld hl,jmp_memsum:rst #28    ;D=number of 64K banks
        ld e,1                      ;E=current bank
prgmem1 ld bc,txtlinmax*2+txtmulobj0-txtmulobj+fndstr0-fndstrt
        push de                     ;** reserve memory
        push bc
        ld a,e
        ld e,2
        rst #20:dw jmp_memget
        pop bc
        pop de
        jr nc,prgmem3
prgmem2 inc e
        dec d
        jr nz,prgmem1
        scf
        ret
prgmem3 ld (prgmemtab+0+1),hl
        ld (prgmemtab+0+3),bc
        ld a,e
        ld (prgmemtab+0+0),a
        ld (txtmulobjb),hl
        ld (prgwinobj1+3),a
        ld (prgwinobj1+4),hl
        ld bc,8192+4096+2
        push de
        push bc
        ld e,1
        rst #20:dw jmp_memget
        pop bc
        pop de
        jr nc,prgmem4
        ld a,(prgmemtab+0+0)
        ld hl,(prgmemtab+0+1)
        ld bc,(prgmemtab+0+3)
        push de
        rst #20:dw jmp_memfre
        pop de
        jr prgmem2
prgmem4 ld (prgmemtab+5+1),hl
        ld (prgmemtab+5+3),bc
        ld a,e
        ld (prgmemtab+5+0),a
        ld (txtmulobj+0),hl         ;** prepare and copy textcontrol-datarecord and font
        ld b,0
        rst #20:dw jmp_bnkwbt       ;set 0-terminator for text
        add a:add a:add a:add a
        ld e,a
        ld a,(App_BnkNum)
        add e
        ld de,8192-1
        add hl,de
        ld (txtmulobja),hl
        ex de,hl
        ld hl,hlpfnt
        ld bc,4096+2
        push af
        rst #20:dw jmp_bnkcop       ;copy font
        pop af
        ld hl,txtmulobj
        ld de,(prgmemtab+0+1)
        ld bc,txtmulobj0-txtmulobj
        push de
        push af
        rst #20:dw jmp_bnkcop       ;copy datarecord
        pop af
        pop hl
        ld bc,txtlinmax*2+txtmulobj0-txtmulobj
        add hl,bc
        ld (fndstr+1),hl
        ex de,hl
        ld hl,fndstry-fndstrt+3+2
        add hl,de
        ld (fndstry+1),hl
        ld hl,fndstrz-fndstrt+3+2
        add hl,de
        ld (fndstrz+1),hl
        ld hl,fndstrc-fndstrt
        add hl,de
        ld (fndstr+3+2),hl
        ld hl,fndstrt
        ld bc,fndstr0-fndstrt
        rst #20:dw jmp_bnkcop       ;copy search routine
        or a
        ret

SyShell_CmdParas   ds 8*3   ;command line parameters (1W address, 1B length)
SyShell_CmdSwtch   ds 8*3   ;command line switches

;******************************************************************************
;*** Name           PARALL
;*** Input          -
;*** Output         (SyShell_CmdParas) = list of parameters; 3 bytes/entry
;***                                     Byte0,1 = pointer
;***                                     Byte2   = length
;***                (SyShell_CmdSwtch) = list of switches (see above; a switch
;***                                     is recognized with a % at the
;***                                     beginning, which is skipped in this
;***                                     list)
;***                D                  = number of parameters
;***                E                  = number of switches
;***                ZF                 = if 1, no parameters and switches
;*** Destroyed      AF,BC,HL,IX,IY
;*** Description    This function fetches all parameters and switches from the
;***                command line and generates two pointer tables. A switch is
;***                recognized with a leading "%" char. A pointer to a switch
;***                links to the first char behind the %. All parameters and
;***                switches are zero terminated.
;*** Example        A\>EXAMPLE.COM filename.ext %x %all hello123
;***                This will generate two entries in the parameter table:
;***                - pointer to "filename.ext", length=12
;***                - pointer to "hello123", length=8
;***                And two entries in the switch table:
;***                - pointer to "x", length=1
;***                - pointer to "all", length=3
;***                Please note, that SymShell itself will add an own switch to
;***                the command line (see "SymShell_Parameters_Shell").
;******************************************************************************
parall  ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de                   ;HL = CodeEnd = Command line
        ld ix,SyShell_CmdParas      ;IX = Parameter List
        ld iy,SyShell_CmdSwtch      ;IY = Switch    List
        ld bc,8*256+8       ;B=8-number of parameters, C=8-number of switches
SyHPAl1 push bc
        call SyHPAl2
        pop bc
        jr z,SyHPAl7
        ld a,(hl)
        cp "%"
        jr z,SyHPAl6
        ld (ix+0),l         ;Parameter
        ld (ix+1),h
        push hl
        call SyHPAl4
        pop hl
        ld (ix+2),e
        ld de,3
        add ix,de
        dec b
        jr nz,SyHPAl1
        jr SyHPAl7
SyHPAl6 inc hl              ;Switch
        ld (iy+0),l
        ld (iy+1),h
        push hl
        call SyHPAl4
        pop hl
        ld (iy+2),e
        ld de,3
        add iy,de
        dec c
        jr nz,SyHPAl1
SyHPAl7 ld a,8
        sub c
        ld e,a
        ld a,8
        sub b
        ld d,a
        or e
        ret
;HL=position inside the string -> jump to next string -> HL=next string, ZF=1 end reached
SyHPAl2 ld a,(hl)
        inc hl
        or a
        ret z
        cp " "
        jr nz,SyHPAl2
        dec hl
        ld (hl),0
SyHPAl3 inc hl
        ld a,(hl)
        cp " "
        jr z,SyHPAl3
        ld a,1
        or a
        ret
;HL=position inside the string -> E=length until the end
SyHPAl4 ld e,0
SyHPAl5 ld a,(hl)
        or a
        ret z
        cp " "
        ret z
        inc e
        inc hl
        jr SyHPAl5


;==============================================================================
;### HLP LOADING-ROUTINES #####################################################
;==============================================================================

hlptxtidn   db "SYMHLP10"
hlpdatidn   ds 8        ;format identifier
hlpdatlen   dw 0,0      ;length of list data, list text

;### HLPOPN -> opens new HLP file and displays TOC and first chapter
;### Input      (hlppth)=path
hlpopn  ld hl,hlppth
        ld a,(hl)
        or a
        ret z
        ld a,(App_BnkNum)
        db #dd:ld h,a
        call SyFile_FILOPN          ;open file
        jp c,hlpopn3
        ld hl,hlpdatidn
        ld bc,2*2+8
        ld de,(App_BnkNum)
        push af
        call SyFile_FILINP          ;load header with identifier and length information
        pop de
        jp c,hlpopn2
        ld ix,hlptxtidn
        ld b,8
hlpopn0 ld a,(ix+0)
        cp (ix+8)
        jp nz,hlpopn2
        inc ix
        djnz hlpopn0
        ld a,d
        ld de,(App_BnkNum)
        ld bc,(hlpdatlen+0)
        ld hl,hlptoclst
        push af
        call SyFile_FILINP          ;load list data
        pop de
        jr c,hlpopn2
        ld a,d
        ld de,(App_BnkNum)
        ld bc,(hlpdatlen+2)
        ld hl,hlptoctxt
        push af
        call SyFile_FILINP          ;load list text
        pop de
        jr c,hlpopn2
        ld a,d
        call SyFile_FILCLO
        xor a
        ld l,a
        ld h,a
        ld (navpos),hl
        ld (hlptocobj+2),a
        ld (hlptocobj+12),a
        ld hl,(hlpdatlen+0)         ;init list
        srl h:rr l
        ld a,l
        srl h:rra
        ld (hlptocobj+0),a

        push af
        ld hl,hlptoclst             ;backup chapter sizes
        ld de,hlptocsiz
hlpopn4 ldi:ldi
        inc hl:inc hl
        dec a
        jr nz,hlpopn4
        pop af

        ld hl,hlptoclst+1
        set 7,(hl)
        ld ix,hlptoclst             ;relocate list text pointers
        ld bc,4
        ld de,hlptoctxt
hlpopn1 res 4,(ix+1)
        ld l,(ix+2)
        ld h,(ix+3)
        add hl,de
        ld (ix+2),l
        ld (ix+3),h
        add ix,bc
        dec a
        jr nz,hlpopn1

        ld a,(prgwin)
        push af
        call SyDesktop_WINTIT
        pop af
        ld e,9
        call SyDesktop_WINDIN
        xor a
        jr hlptxt
hlpopn2 ld a,d                  ;** error
        call SyFile_FILCLO
hlpopn3 ld a,(App_BnkNum)
        ld hl,errlodobj
        ld b,8*1+1+64
        ld de,prgwindat
        call SySystem_SYSWRN
        xor a
        ld (hlptocobj),a
        ld (hlptocobj+2),a
        ld c,a:ld b,a
        ld (navpos),bc
        call hlptxt4
        ld de,256*8+256-4
        ld a,(prgwin)
        jp SyDesktop_WINDIN

;### HLPTXT -> displays HLP chapter
;### Input      A=chapter ID (0-127)
hlptxt  ld (tocclkn),a
        call hlplod
        jp c,hlpopn3
        ret z
        dec bc                  ;init textcontrol
        call hlptxt4
        ld a,(prgwin)
        ld e,11
        jp SyDesktop_WINDIN
hlptxt4 push bc
        ld a,(prgmemtab+0+0)
        ld hl,(prgmemtab+0+1)
        ld bc,4
        add hl,bc
        ld c,0
        rst #20:dw jmp_bnkwwd   ;+04 -> curpos
        ld bc,0
        rst #20:dw jmp_bnkwwd   ;+06 -> mrklen
        pop bc
        rst #20:dw jmp_bnkwwd   ;+08 -> txtlen
        ld bc,34-10
        add hl,bc
        ld c,0
        rst #20:dw jmp_bnkwwd   ;+34 -> offstx
        ld bc,0
        rst #20:dw jmp_bnkwwd   ;+36 -> offsty
        ld bc,24-38
        add hl,bc
        ld c,-8
        rst #20:dw jmp_bnkwwd   ;+24 -> force reformatting
        ret

;### HLPLOD -> loads one chapter
;### Input      A=chapter ID
;### Output     CF=0 ok, BC=text length (including 0-terminator), ZF=1 -> chapter is empty
;###            CF=1 disc error
hlplodl dw 0

hlplod  ld hl,(hlpdatlen+0)
        ld de,8+4
        add hl,de
        ld de,(hlpdatlen+2)
        add hl,de
        ld iy,0
        ld ix,hlptocsiz
        or a
        jr z,hlplod3
        ld b,a
hlplod1 ld e,(ix+0)
        ld a,(ix+1)
        and #1f
        ld d,a
        xor a
        add hl,de
        jr nc,hlplod2
        db #fd:inc l            ;iy,hl=chapter offset
hlplod2 inc ix:inc ix
        djnz hlplod1
hlplod3 ld c,(ix+0)
        ld a,(ix+1)
        and #1f
        ld b,a
        dec bc:dec bc:dec bc
        ld a,b
        or c
        ret z                   ;chapter is empty -> don't load
        inc bc:inc bc:inc bc    ;bc=length with terminator
        ld (hlplodl),bc
        push iy
        push hl
        ld hl,hlppth
        ld a,(App_BnkNum)
        db #dd:ld h,a
        call SyFile_FILOPN      ;open file
        pop ix
        pop iy
        ret c
        ld c,0
        push af
        call SyFile_FILPOI      ;seek to chapter
        pop de
        jr c,hlplod4
        ld a,d
        ld hl,hlpfnttyp
        ld bc,2
        ld de,(App_BnkNum)
        push af
        call SyFile_FILINP      ;load font type and number of links
        pop de
        jr c,hlplod4
        ld a,(hlplnknum)
        add a:add a
        ld c,a
        ld b,0
        ld a,d
        ld hl,hlplnkdat
        ld de,(App_BnkNum)
        push bc
        push af
        call nz,SyFile_FILINP   ;load link data
        pop de
        pop bc
        jr c,hlplod4
        ld hl,(hlplodl)
        sbc hl,bc
        dec hl
        dec hl
        ld c,l:ld b,h
        ld a,(hlpfnttyp)
        rla
        jr nc,hlplod5
        ld bc,8192
hlplod5 ld a,d
        ld hl,(prgmemtab+5+1)
        ld de,(prgmemtab+5+0)
        push af
        call SyFile_FILCPR      ;load chapter
        pop de
        jr c,hlplod4
        ld a,d
        push bc
        call SyFile_FILCLO
        pop bc
        ld a,c
        or b
        ret
hlplod4 ld a,d                  ;** error
        call SyFile_FILCLO
        scf
        ret


;==============================================================================
;### INTERACTION ROUTINES #####################################################
;==============================================================================

;### TOCCLK -> user clicked in list
tocclkn db 0
tocclk  ld a,(hlptocobj+12)
        ld hl,tocclkn
        cp (hl)
        jp z,prgprz0
        push af
        call navnew
        pop af
tocclk1 call hlptxt
        jp prgprz0

;### TXTCLK -> user clicked in text (check for link)
txtclkf db 0    ;1=probably triggered because of found event

txtclk  ld hl,txtclkf
        bit 0,(hl)
        res 0,(hl)
        jp nz,prgprz0
        ld a,(hlplnknum)
        or a
        jp z,prgprz0
        db #fd:ld l,a
        ld hl,(prgmemtab+0+1)
        ld bc,4
        add hl,bc
        push bc
        ld a,(prgmemtab+0+0)
        rst #20:dw jmp_bnkrwd
        ld e,c                  ;DE=cursor pos
        ld d,b
        pop bc
        ld ix,hlplnkdat
txtclk1 ld l,(ix+2)             ;HL=first link char
        ld h,(ix+3)
        scf
        sbc hl,de
        jr nc,txtclk2
        ld a,l
        add (ix+1)
        jr nc,txtclk2
        ld a,(ix+0)
txtclk0 push af
        call navnew
        pop af
txtclk4 push af
        call txtclk3
        res 7,(hl)
        pop af
        ld (hlptocobj+12),a
        push af
        call txtclk3
        set 7,(hl)
        ld a,(prgwin)
        ld de,256*8+256-2
        call SyDesktop_WINDIN
        pop af
        jr tocclk1
txtclk2 add ix,bc
        db #fd:dec l
        jr nz,txtclk1
        jp prgprz0
txtclk3 ld a,(hlptocobj+12)
        add a:add a
        ld l,a
        ld h,0
        ld bc,hlptoclst+1
        add hl,bc
        ret

navhis  ds 16
navofs  db 0    ;start offset in history (0-15)
navpos  db 0    ;relative position in history starting from start offset (0-15)
navlen  db 0    ;number of entries in history (0-16)

;### NAVNEW -> adds a new entry in history
;### Input      A=entry
;### Destroyed  AF,BC,E,HL
navnew  ld e,a
        ld hl,navofs
        ld a,(navpos)
        inc a
        cp 16
        jr c,navnew1
        inc (hl)
        res 4,(hl)
        dec a
navnew1 ld (navpos),a
        inc a
        ld (navlen),a
        dec a
        add (hl)
        and 15
        ld l,a
        ld h,0
        ld bc,navhis
        add hl,bc
        ld (hl),e
        ret

;### NAVBCK -> history back
navbck  ld hl,navpos
        ld a,(hl)
        or a
        jp z,prgprz0
        dec a
        ld (hl),a
navbck1 ld hl,navofs
        add (hl)
        and 15
        ld l,a
        ld h,0
        ld bc,navhis
        add hl,bc
        ld a,(hl)
        jp txtclk4

;### NAVFRW -> history forward
navfrw  ld hl,navpos
        ld a,(navlen)
        sub 1
        jp c,prgprz0
        cp (hl)
        jp z,prgprz0
        inc (hl)
        ld a,(hl)
        jr navbck1

;### NAVHOM -> go to first chapter
navhom  xor a
        ld hl,hlptocobj
        cp (hl)
        jp z,prgprz0
        jp txtclk0

;### NAVOPN -> open new help file
navopn  ld hl,hlpmsk
        ld a,(App_BnkNum)
        ld c,8
        ld ix,200
        ld iy,8000
        ld de,prgwindat
        call SySystem_SELOPN
        or a
        jp nz,prgprz0
        ld hl,hlppthnew
        ld de,hlppth
        ld bc,256
        ldir
        call hlpopn
        jp prgprz0

;### NAVTAB -> switches between content and search mode
navtabt db 0    ;last tab
navtabl db 0    ;last entry of topic tab
navtabc db 0    ;flag, if textbox filled during search tab

navtab  ld a,(hlptabobj0)
        ld hl,navtabt
        cp (hl)
        jp z,prgprz0
        ld (hl),a
        push af
        call modini
        pop af
        or a
        jr nz,navtab1
        ld a,(navtabc)          ;** contents-mode
        or a
        jr z,navtab2
        ld hl,hlptoclst+1       ;active text -> refresh list
        ld a,(navtabl)
        ld (hlptocobj+12),a
        ld bc,(hlptocobj-1)
        ld c,0
        ld de,4
navtab3 res 7,(hl)
        cp c
        jr nz,navtab4
        set 7,(hl)
navtab4 add hl,de
        inc c
        djnz navtab3
        ld de,256*1+256-9
        ld a,(prgwin)
        call SyDesktop_WINDIN
        jp prgprz0
navtab2 ld de,256*1+256-2       ;cleared -> reload whole help
        ld a,(prgwin)
        call SyDesktop_WINDIN
        call hlpopn
        jp prgprz0
navtab1 ld a,(hlptocobj+12)     ;** search-mode -> clear textbox
        ld (navtabl),a
        xor a
        ld (navtabc),a
        ld l,a
        ld h,a
        ld (fnddsps),hl
        dec hl
        ld (hlpfndobj+12),hl
        ld hl,hlpfndlst+1
        ld a,(hlptocobj-1)
        or a
        jr z,navtab6
        ld de,4
navtab5 res 7,(hl)
        add hl,de
        dec a
        jr nz,navtab5
navtab6 call navtab0
        ld de,256*1+256-11
        ld a,(prgwin)
        call SyDesktop_WINDIN
        jp prgprz0
navtab0 ld a,(prgmemtab+5+0)
        ld hl,(prgmemtab+5+1)
        ld b,0
        rst #20:dw jmp_bnkwbt
        ld bc,0
        jp hlptxt4

;### MODINI -> inits window controls for current mode
modini  ld a,(hlptabobj0)
        rrca
        xor 128
        ld c,a
        ld hl,prgwinobj0+2
        ld de,16
        ld b,6
modini2 ld a,(hl)
        and 127
        or c
        ld (hl),a
        add hl,de
        djnz modini2
        ld a,(hl)
        and 127
        or c
        xor 128
        ld (hl),a
        ret


;==============================================================================
;### SEARCH ROUTINES ##########################################################
;==============================================================================

;### FNDLST -> find topics
fndlst  call fndpre         ;prepare search with actual string
        jp c,prgprz0
        ld a,(hlptocobj)
        ld b,a
        xor a
        ld c,a
        ld (hlpfndobj+0),a  ;clear result list
        ld (hlpfndobj+2),a
        ld ix,hlptoclst
        ld iy,hlpfndlst
fndlst1 push bc
        push ix
        push iy
        ld a,c
        call hlplod         ;load next chapter
        jr c,fndlst4
        ld de,0
        call nz,fndbnk      ;search in chapter
        pop iy
        pop ix
        pop bc
        jr z,fndlst3
        ld (iy+0),c         ;found -> add entry in result list
        ld (iy+1),0
        ld l,(ix+2)
        ld h,(ix+3)
        ld a,32             ;skip chaptername-insertion
        dec hl
fndlst2 inc hl
        cp (hl)
        jr z,fndlst2
        ld (iy+2),l
        ld (iy+3),h
        ld de,4
        add iy,de
        ld hl,hlpfndobj
        inc (hl)
fndlst3 inc c
        ld de,4
        add ix,de
        djnz fndlst1
        call navtab0        ;clear textbox
        ld e,8
        ld a,(prgwin)
        push af
        call SyDesktop_WINDIN
        pop af
        ld e,11
        call SyDesktop_WINDIN
        ld hl,0
        ld (fnddsps),hl
        ld a,(hlpfndobj)
        or a
        jp nz,prgprz0
        ld a,(App_BnkNum)   ;nothing found -> show message
        ld hl,errfndobj
        ld b,1+64+128
        ld de,prgwindat
        call SySystem_SYSWRN
        jp prgprz0
fndlst4 pop hl
        pop hl
        pop hl
        call hlpopn3
        jp prgprz0

;### FNDCLK -> user clicked in list
fndclk  ld hl,0
        ld (fnddsps),hl
        ld a,(App_MsgBuf+3)
        cp DSK_SUB_MDCLICK
        jp nz,prgprz0
        jr fnddsp

;### FNDDSP -> display selected topic/find next
fnddsps dw 0    ;start search at this position
fnddspl dw 0    ;length

fnddsp  ld hl,(fnddsps)
        ld a,l
        or h
        jr nz,fnddsp1
        ld a,(hlpfndobj+12)
        ld hl,hlpfndobj+0
        cp (hl)
        jp nc,prgprz0
        add a
        ld l,a
        ld h,0
        add hl,hl
        ld bc,hlpfndlst+0
        add hl,bc
        ld a,1
        ld (navtabc),a
        ld a,(hl)
        ld (navtabl),a
        push af
        call navnew
        pop af
        call hlptxt
        rst #30
fnddsp1 ld de,(fnddsps)
        push de
        call fndbnk
        pop de
        jr nz,fnddsp2
        ld a,e
        or d
        jp z,prgprz0
        ld hl,0
        ld (fnddsps),hl
        jr fnddsp1
fnddsp2 ld c,l
        ld b,h
        ld hl,(fnddspl)
        add hl,bc
        ld (fnddsps),hl
        ld a,(prgmemtab+0+0)
        ld hl,(prgmemtab+0+1)
        ld de,40
        add hl,de
        rst #20:dw jmp_bnkwwd
        ld bc,(fnddspl)
        rst #20:dw jmp_bnkwwd
        ld a,11+1
        ld (prgwingrp+14),a
        ld a,31
        rst #20:dw jmp_keyput
        ld a,1
        ld (txtclkf),a
        jp prgprz0

;### FNDPRE -> prepares string search with actual keyword
;### Output     CF=1 -> no keyword
fndpre  ld a,(hlpinpobj+8)
        sub 1
        ret c
        ld hl,(fndstr+1)
        push hl
        ld bc,fndstr3+1-fndstrt
        add hl,bc
        ld b,a
        inc a
        ld (fnddspl),a
        ld a,(prgmemtab+0+0)
        rst #20:dw jmp_bnkwbt   ;set search string length
        ld de,hlpinptxt
        pop hl
        db #dd:ld l,31
fndpre1 ld a,(de)
        inc de
        call clclcs
        ld b,a
        ld a,(prgmemtab+0+0)
        rst #20:dw jmp_bnkwbt   ;copy search string lowercased
        db #dd:dec l
        jr nz,fndpre1
        or a
        ret

;### FNDBNK -> finds string (calls search routine in txtbuf-bank)
;### Input      DE=startposition
;### Output     ZF=0 -> found, HL=position inside text
fndbnk  ld ix,(fndstr+1)
        ld bc,fndstr-fndstrt
        add ix,bc               ;iy=routine
        ld iy,(fndstr+1)
        ld bc,fndstr0-fndstrt
        add iy,bc               ;iy=stack
        ld bc,(prgmemtab+5+0-1) ;b=bank
        ld hl,(prgmemtab+5+1)
        add hl,de
        ex de,hl                ;de=textadr
        call jmp_bnkcll
        inc c:dec c
        ret z
        ld hl,(prgmemtab+5+1)
        ex de,hl
        xor a
        sbc hl,de
        inc a
        ret

;### FNDSTR -> finds string
;### Input      DE=text, (fndstr+1)=fndstrt, (fndstrtxt)=string (lcase), (fndstr3+1)=length
;### Output     C=0 -> not found, C=1 -> found at DE
fndstrt ds 31
fndstr  ld hl,0
        ld ix,fndstrc
fndstr1 ld a,(de)
        or a
        ld c,0
        jp z,jmp_bnkret
fndstry ld (0),a
        ld a,(ix+0)
        cp (hl)
        jr z,fndstr3
fndstr7 inc de
        jr fndstr1
fndstr3 ld b,0
        push de
fndstr4 inc de
        inc hl
        ld a,(de)
        or a
        jr z,fndstr6
fndstrz ld (0),a
        ld a,(ix+0)
        cp (hl)
        jr nz,fndstr6
        djnz fndstr4
        pop de
        ld c,1
        jp jmp_bnkret
fndstr6 pop de
        inc de
        jr fndstr

db 034,035,036,037,038,039,040,041,042,043,044,045,046,047,048,049,050,051,052,053,054,055,056,057,058,059,060,061,062,063,064,097  ;128-255
db 098,099,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,091,092,093,094,032,032,097
db 098,099,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,048,049,050,051
db 052,053,054,055,056,057,097,098,099,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122
fndstrc
db 000,097,098,099,100,101,102,103,104,105,010,107,108,013,110,111,112,113,114,115,116,117,118,119,120,121,122,106,109,032,255,255  ;000-127
db 032,033,034,035,036,037,038,039,040,041,042,043,044,045,046,047,048,049,050,051,052,053,054,055,056,057,058,059,060,061,062,063
db 064,097,098,099,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,091,092,093,094,095
db 096,097,098,099,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,033

fndstrs ds 32
fndstr0


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

;### CLCLCS -> A=lcase(A)
clclcs  cp "A"
        ret c
        cp "Z"+1
        ret nc
        add "a"-"A"
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd  ld a,PRC_ID_DESKTOP
msgsnd1 db #dd:ld h,a
        ld a,(App_PrcID)
        db #dd:ld l,a
        ld iy,App_MsgBuf
        ld (App_MsgBuf+0),bc
        ld (App_MsgBuf+2),de
        ld (App_MsgBuf+4),hl
        rst #10
        ret

hlptocsiz   ds 2*lstlinmax

txtmulobj   dw 0            ;texdatadr       equ 0           ;Zeiger auf Text
            dw 0            ;texdatbeg       equ 2           ;erstes angezeigtes Zeichen (nur singleline)
            dw 0            ;texdatpos       equ 4           ;Cursorposition im Gesamttext
            dw 0            ;texdatmrk       equ 6           ;0/Anzahl markierter Zeichen [neg->Cursor=Ende Markierung]
            dw 0            ;texdatlen       equ 8           ;Textlänge
            dw txtbufmax    ;texdatmax       equ 10          ;maximal zulässige Textlänge (8K-1B; exklusive 0-Terminator)
            db 2+4+8        ;texdatflg       equ 12          ;Flags (Bit0=Paßwort [nur singleline], Bit1=ReadOnly, Bit2=AltColor, Bit3=AltFont [nur multiline], Bit7=Text has been modified)
                            ;;** extended 16c/altfont
            db 8+16         ;texdatcol       equ 13          ;4bit txtpap, 4bit txtpen
            db 0            ;texdatrhm       equ 14          ;4bit rahmen1, 4bit rahmen2 (nur singleline)
txtmulobja  dw hlpfnt       ;texdatfnt       equ 15          ;Adresse des alternativen Fonts
            ds 1            ;texdatrs1       equ 17          ;*reserved 2byte*
                            ;;** ab hier nur Multiline
            dw 0            ;texdatlnt       equ 18          ;aktuelle Anzahl Zeilen
            dw -1           ;texdatxmx       equ 20          ;maximale Zeilenbreite in Pixeln bei Wordwrap-Pos-Vorgabe (-1=unbegrenzt)
            dw txtlinmax    ;texdatymx       equ 22          ;maximale Anzahl Zeilen (*2 bytes ab texdatlln!)
            dw -8           ;texdatxwn       equ 24          ;win y len ohne slider für bisherige formatierung (winx<>aktx -> Neuformatierung notwendig, durch -8 erzwingen)
            dw 0            ;texdatywn       equ 26          ;win y len ohne slider
                            ;
txtmulobjb  dw txtmulobj    ;texdatzgr       equ 28          ;pointer auf diesen datensatz
            dw 120          ;texdatxfl       equ 30          ;full x len (=Länge der längsten Textzeile in Pixel)
            dw 80           ;texdatyfl       equ 32          ;full y len (=Anzahl aller Textzeilen * 8)
            dw 0            ;texdatxof       equ 34          ;offset x
            dw 0            ;texdatyof       equ 36          ;offset y
            db 0+2          ;texdatfg2       equ 38          ;Flags (Bit0=kein Auto-WordWrap [dann Xslider], Bit1=1)
            db 0            ;texdattab       equ 39          ;Tabstop Größe (1-255; 0=kein Tabstop)
                            ;
            ds 4            ;texdatmsg       equ 40          ;Message-Buffer (in POS, MRK, out CXP, CYP)
            ds 4            ;texdatrs2       equ 44          ;*reserved 4byte*
            dw 0            ;texdatlln       equ 48          ;Zeilen-Längentabelle (bit15=cr+lf am ende vorhanden, wird mitgezählt)
txtmulobj0


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #66,#66,#66,#66,#66,#61,#11,#11,#11,#66,#66,#66,#66,#66,#66,#66,#61,#1C,#CC,#CC,#CC,#11,#66,#66,#66,#66,#66,#66,#1C,#CC,#CC,#CC,#CC,#CC,#16,#66,#66,#66,#66,#61,#CC,#CC,#CC,#CC,#CC,#CC,#C1,#66
db #66,#66,#66,#61,#CC,#CC,#C1,#11,#CC,#CC,#C1,#66,#66,#66,#66,#1C,#CC,#CC,#16,#66,#1C,#CC,#CC,#16,#66,#66,#66,#1C,#CC,#C1,#66,#66,#61,#CC,#CC,#16,#66,#66,#66,#1C,#CC,#C1,#66,#66,#1C,#CC,#CC,#16
db #6D,#DD,#DD,#1C,#CC,#C1,#D1,#61,#CC,#CC,#C1,#66,#6D,#88,#88,#81,#11,#18,#81,#1C,#CC,#CC,#C1,#66,#6D,#88,#88,#88,#88,#88,#1C,#CC,#CC,#CC,#16,#66,#6D,#88,#DD,#1D,#DD,#11,#CC,#CC,#CC,#C1,#66,#66
db #6D,#88,#88,#88,#88,#81,#CC,#CC,#CC,#16,#66,#66,#6D,#88,#88,#88,#88,#81,#CC,#CC,#C1,#66,#66,#66,#6D,#88,#D1,#DD,#1D,#D1,#CC,#CC,#C1,#66,#66,#66,#6D,#88,#88,#88,#88,#88,#11,#11,#16,#66,#66,#66
db #6D,#88,#88,#88,#88,#88,#81,#66,#66,#66,#66,#66,#6D,#88,#DD,#D1,#D1,#DD,#11,#11,#16,#66,#66,#66,#6D,#88,#88,#88,#88,#81,#CC,#CC,#C1,#66,#66,#66,#6D,#88,#88,#88,#88,#81,#CC,#CC,#C1,#66,#66,#66
db #6D,#88,#D1,#DD,#1D,#D1,#CC,#CC,#C1,#66,#66,#66,#6D,#88,#88,#88,#88,#81,#CC,#CC,#C1,#66,#66,#66,#6D,#88,#88,#88,#88,#88,#11,#11,#16,#66,#66,#66,#6D,#11,#11,#11,#11,#11,#11,#66,#66,#66,#66,#66

;1W source, 1W destination+9, 1B byte width, 1B pixel width, 1B height, 1B number of sourcebytes <=128)
gfxcnvtab
dw tolprvspr0,tolprvspr+9:db 8,16,12,12*4
dw tolnxtspr0,tolnxtspr+9:db 8,16,12,12*4
dw tolhomspr0,tolhomspr+9:db 8,16,12,12*4
dw tolselspr0,tolselspr+9:db 8,16,12,12*4
dw 0

tolprvspr db 4,16,12:dw tolprvspr+10,tolprvspr+9,4*12:db 0
ds 4*12
tolprvspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#2D,#0F,#4B
db #8F,#69,#0F,#4B
db #8F,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#F0,#E1,#4B
db #8F,#69,#0F,#4B
db #8F,#2D,#0F,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

tolnxtspr db 4,16,12:dw tolnxtspr+10,tolnxtspr+9,4*12:db 0
ds 4*12
tolnxtspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#1E,#0F,#4B
db #8F,#1E,#87,#4B
db #9E,#F0,#C3,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#C3,#4B
db #8F,#1E,#87,#4B
db #8F,#1E,#0F,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

tolhomspr db 4,16,12:dw tolhomspr+10,tolhomspr+9,4*12:db 0
ds 4*12
tolhomspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#3C,#0F,#4B
db #8F,#78,#87,#4B
db #8F,#F0,#C3,#4B
db #9E,#F0,#E1,#4B
db #BC,#F0,#F0,#4B
db #9E,#C3,#E1,#4B
db #9E,#C3,#E1,#4B
db #9E,#C3,#E1,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

tolselspr db 4,16,12:dw tolselspr+10,tolselspr+9,4*12:db 0
ds 4*12
tolselspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#3C,#0F,#4B
db #8F,#78,#87,#4B
db #8F,#F0,#C3,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#0F,#0F,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

hlpmsk      db "HLP",0
hlppthnew   ds 256
hlppth      ds 256

hlpinptxt   ds 32   ;search string

;### STRINGS ##################################################################

hlptabtxt1  db "Contents",0
hlptabtxt2  db "Search",0
hlpdsptxt1  db "Keyword to find:",0
hlpdsptxt2  db "Select Topic:",0
hlpbuttxt1  db "List Topics",0
hlpbuttxt2  db "Show/Next",0

errlodtxt1  db "Error while loading help file",0
errlodtxt2  db "A disc error or a file format error",0
errlodtxt3  db "occured.",0

errfndtxt1  db "No topics found.",0
errfndtxt2  db "This document doesn't contain",0
errfndtxt3  db "the keyword you entered.",0

hlptoctxt   db "Help document browser for SymbOS",0
            ;TOC text buffer using HLPFNT space
hlpfnt
db 8,1
db 5,#00,#60,#90,#F0,#90,#90,#90,#ff,#00,#00,#00,#00,#00,#00,#00 ;A 001 26+2 UNDERLINE (A-Z)
db 5,#00,#E0,#90,#E0,#90,#90,#E0,#ff,#00,#00,#00,#00,#00,#00,#00 ;B 002
db 5,#00,#60,#90,#80,#80,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;C 003
db 5,#00,#E0,#90,#90,#90,#90,#E0,#ff,#00,#00,#00,#00,#00,#00,#00 ;D 004
db 5,#00,#F0,#80,#E0,#80,#80,#F0,#ff,#00,#00,#00,#00,#00,#00,#00 ;E 005
db 5,#00,#F0,#80,#E0,#80,#80,#80,#ff,#00,#00,#00,#00,#00,#00,#00 ;F 006
db 5,#00,#70,#80,#B0,#90,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;G 007
db 5,#00,#90,#90,#F0,#90,#90,#90,#ff,#00,#00,#00,#00,#00,#00,#00 ;H 008
db 2,#00,#80,#80,#80,#80,#80,#80,#ff,#00,#00,#00,#00,#00,#00,#00 ;I 009
db 0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;  010 ##
db 5,#00,#90,#A0,#C0,#A0,#90,#90,#ff,#00,#00,#00,#00,#00,#00,#00 ;K 011
db 5,#00,#80,#80,#80,#80,#80,#F0,#ff,#00,#00,#00,#00,#00,#00,#00 ;L 012
db 0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;  013 ##
db 6,#00,#88,#C8,#A8,#98,#88,#88,#ff,#00,#00,#00,#00,#00,#00,#00 ;N 014
db 5,#00,#60,#90,#90,#90,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;O 015
db 5,#00,#E0,#90,#90,#E0,#80,#80,#ff,#00,#00,#00,#00,#00,#00,#00 ;P 016
db 5,#00,#60,#90,#90,#90,#B0,#68,#ff,#00,#00,#00,#00,#00,#00,#00 ;Q 017
db 5,#00,#E0,#90,#E0,#90,#90,#90,#ff,#00,#00,#00,#00,#00,#00,#00 ;R 018
db 5,#00,#70,#80,#60,#10,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;S 019
db 6,#00,#F8,#20,#20,#20,#20,#20,#ff,#00,#00,#00,#00,#00,#00,#00 ;T 020
db 5,#00,#90,#90,#90,#90,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;U 021
db 6,#00,#88,#88,#88,#50,#50,#20,#ff,#00,#00,#00,#00,#00,#00,#00 ;V 022
db 6,#00,#88,#88,#88,#A8,#D8,#88,#ff,#00,#00,#00,#00,#00,#00,#00 ;W 023
db 6,#00,#88,#50,#20,#50,#88,#88,#ff,#00,#00,#00,#00,#00,#00,#00 ;X 024
db 6,#00,#88,#50,#20,#20,#20,#20,#ff,#00,#00,#00,#00,#00,#00,#00 ;Y 025
db 5,#00,#F0,#10,#20,#40,#80,#F0,#ff,#00,#00,#00,#00,#00,#00,#00 ;Z 026
db 5,#00,#10,#10,#10,#10,#90,#60,#ff,#00,#00,#00,#00,#00,#00,#00 ;J 027
db 6,#00,#88,#D8,#A8,#88,#88,#88,#ff,#00,#00,#00,#00,#00,#00,#00 ;M 028
db 1,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;s1 029 03 SPECIALS
db 7,#00,#30,#78,#fc,#30,#30,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;up 030
db 7,#00,#30,#30,#30,#fc,#78,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;dw 031
db 3,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;  032 95 DEFAULT (ALL)
db 2,#00,#80,#80,#80,#80,#00,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;! 033
db 4,#00,#A0,#A0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;" 034
db 6,#00,#50,#F8,#50,#50,#F8,#50,#00,#00,#00,#00,#00,#00,#00,#00 ;# 035
db 4,#00,#40,#E0,#C0,#60,#E0,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;$ 036
db 5,#00,#C0,#D0,#20,#40,#B0,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;% 037
db 5,#00,#40,#A0,#C0,#50,#A0,#50,#00,#00,#00,#00,#00,#00,#00,#00 ;& 038
db 2,#00,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;' 039
db 3,#40,#80,#80,#80,#80,#80,#80,#40,#00,#00,#00,#00,#00,#00,#00 ;( 040
db 3,#80,#40,#40,#40,#40,#40,#40,#80,#00,#00,#00,#00,#00,#00,#00 ;) 041
db 6,#00,#00,#20,#F8,#70,#88,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;* 042
db 6,#00,#00,#20,#20,#F8,#20,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;+ 043
db 3,#00,#00,#00,#00,#00,#C0,#40,#80,#00,#00,#00,#00,#00,#00,#00 ;, 044
db 5,#00,#00,#00,#00,#F0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;- 045
db 2,#00,#00,#00,#00,#00,#00,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;. 046
db 5,#10,#10,#20,#20,#40,#40,#80,#80,#00,#00,#00,#00,#00,#00,#00 ;/ 047
db 5,#00,#60,#90,#90,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;0 048
db 5,#00,#20,#60,#20,#20,#20,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;1 049
db 5,#00,#60,#90,#20,#40,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;2 050
db 5,#00,#60,#90,#20,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;3 051
db 5,#00,#80,#90,#90,#F0,#10,#10,#00,#00,#00,#00,#00,#00,#00,#00 ;4 052
db 5,#00,#F0,#80,#E0,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;5 053
db 5,#00,#70,#80,#E0,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;6 054
db 5,#00,#F0,#10,#20,#20,#40,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;7 055
db 5,#00,#60,#90,#60,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;8 056
db 5,#00,#60,#90,#90,#70,#10,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;9 057
db 2,#00,#00,#00,#80,#00,#00,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;dp058
db 3,#00,#00,#00,#40,#00,#00,#40,#80,#00,#00,#00,#00,#00,#00,#00 ;; 059
db 4,#00,#00,#20,#40,#80,#40,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;< 060
db 5,#00,#00,#00,#F0,#00,#F0,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;= 061
db 4,#00,#00,#80,#40,#20,#40,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;> 062
db 5,#00,#60,#90,#20,#40,#00,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;? 063
db 5,#00,#60,#90,#B0,#B0,#80,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;@ 064
db 5,#00,#60,#90,#F0,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;A 065
db 5,#00,#E0,#90,#E0,#90,#90,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;B 066
db 5,#00,#60,#90,#80,#80,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;C 067
db 5,#00,#E0,#90,#90,#90,#90,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;D 068
db 5,#00,#F0,#80,#E0,#80,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;E 069
db 5,#00,#F0,#80,#E0,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;F 070
db 5,#00,#70,#80,#B0,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;G 071
db 5,#00,#90,#90,#F0,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;H 072
db 2,#00,#80,#80,#80,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;I 073
db 5,#00,#10,#10,#10,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;J 074
db 5,#00,#90,#A0,#C0,#A0,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;K 075
db 5,#00,#80,#80,#80,#80,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;L 076
db 6,#00,#88,#D8,#A8,#88,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;M 077
db 6,#00,#88,#C8,#A8,#98,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;N 078
db 5,#00,#60,#90,#90,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;O 079
db 5,#00,#E0,#90,#90,#E0,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;P 080
db 5,#00,#60,#90,#90,#90,#B0,#68,#00,#00,#00,#00,#00,#00,#00,#00 ;Q 081
db 5,#00,#E0,#90,#E0,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;R 082
db 5,#00,#70,#80,#60,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;S 083
db 6,#00,#F8,#20,#20,#20,#20,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;T 084
db 5,#00,#90,#90,#90,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;U 085
db 6,#00,#88,#88,#88,#50,#50,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;V 086
db 6,#00,#88,#88,#88,#A8,#D8,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;W 087
db 6,#00,#88,#50,#20,#50,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;X 088
db 6,#00,#88,#50,#20,#20,#20,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;Y 089
db 5,#00,#F0,#10,#20,#40,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;Z 090
db 3,#00,#C0,#80,#80,#80,#80,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;[ 091
db 5,#80,#80,#40,#40,#20,#20,#10,#10,#00,#00,#00,#00,#00,#00,#00 ;\ 092
db 3,#00,#C0,#40,#40,#40,#40,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;] 093
db 4,#00,#40,#A0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;^ 094
db 6,#00,#00,#00,#00,#00,#00,#F8,#00,#00,#00,#00,#00,#00,#00,#00 ;_ 095
db 3,#00,#80,#40,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;` 096
db 5,#00,#00,#70,#90,#90,#90,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;a 097
db 5,#00,#80,#E0,#90,#90,#90,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;b 098
db 5,#00,#00,#60,#90,#80,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;c 099
db 5,#00,#10,#70,#90,#90,#90,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;d 100
db 5,#00,#00,#60,#90,#F0,#80,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;e 101
db 3,#00,#40,#80,#C0,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;f 102
db 5,#00,#00,#60,#90,#90,#70,#10,#E0,#00,#00,#00,#00,#00,#00,#00 ;g 103
db 5,#00,#80,#E0,#90,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;h 104
db 2,#00,#80,#00,#80,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;i 105
db 3,#00,#40,#00,#40,#40,#40,#40,#80,#00,#00,#00,#00,#00,#00,#00 ;j 106
db 5,#00,#80,#90,#A0,#C0,#A0,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;k 107
db 3,#00,#80,#80,#80,#80,#80,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;l 108
db 6,#00,#00,#50,#A8,#A8,#A8,#A8,#00,#00,#00,#00,#00,#00,#00,#00 ;m 109
db 5,#00,#00,#A0,#D0,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;n 110
db 5,#00,#00,#60,#90,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;o 111
db 5,#00,#00,#E0,#90,#90,#E0,#80,#80,#00,#00,#00,#00,#00,#00,#00 ;p 112
db 5,#00,#00,#70,#90,#90,#70,#10,#10,#00,#00,#00,#00,#00,#00,#00 ;q 113
db 4,#00,#00,#A0,#C0,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;r 114
db 4,#00,#00,#60,#80,#40,#20,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;s 115
db 3,#00,#80,#C0,#80,#80,#80,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;t 116
db 5,#00,#00,#90,#90,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;u 117
db 6,#00,#00,#88,#88,#50,#50,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;v 118
db 6,#00,#00,#88,#88,#88,#A8,#50,#00,#00,#00,#00,#00,#00,#00,#00 ;w 119
db 5,#00,#00,#90,#90,#60,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;x 120
db 5,#00,#00,#90,#90,#90,#70,#10,#60,#00,#00,#00,#00,#00,#00,#00 ;y 121
db 5,#00,#00,#F0,#10,#20,#40,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;z 122
db 4,#20,#40,#40,#80,#40,#40,#40,#20,#00,#00,#00,#00,#00,#00,#00 ;{ 123
db 2,#00,#80,#80,#80,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;| 124
db 4,#80,#40,#40,#20,#40,#40,#40,#80,#00,#00,#00,#00,#00,#00,#00 ;} 125
db 6,#00,#00,#48,#B0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;~ 126
db 3,#00,#C0,#C0,#C0,#C0,#00,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;! 127 93 BOLD (ALL)
db 6,#00,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;" 128
db 8,#00,#6C,#FE,#6C,#6C,#FE,#6C,#00,#00,#00,#00,#00,#00,#00,#00 ;# 129
db 5,#00,#60,#F0,#E0,#70,#F0,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;$ 130
db 6,#00,#C0,#D8,#30,#60,#D8,#18,#00,#00,#00,#00,#00,#00,#00,#00 ;% 131
db 6,#00,#60,#F0,#C0,#78,#F0,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;& 132
db 3,#00,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;' 133
db 4,#60,#C0,#C0,#C0,#C0,#C0,#C0,#60,#00,#00,#00,#00,#00,#00,#00 ;( 134
db 4,#C0,#60,#60,#60,#60,#60,#60,#C0,#00,#00,#00,#00,#00,#00,#00 ;) 135
db 7,#00,#00,#30,#FC,#78,#CC,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;* 136
db 7,#00,#00,#30,#30,#FC,#30,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;+ 137
db 4,#00,#00,#00,#00,#00,#E0,#60,#C0,#00,#00,#00,#00,#00,#00,#00 ;, 138
db 7,#00,#20,#60,#fc,#fc,#60,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;lf 139 ## -
db 3,#00,#00,#00,#00,#00,#00,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;. 140
db 6,#18,#18,#30,#30,#60,#60,#C0,#C0,#00,#00,#00,#00,#00,#00,#00 ;/ 141
db 6,#00,#70,#D8,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;0 142
db 5,#00,#60,#E0,#60,#60,#60,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;1 143
db 6,#00,#70,#D8,#30,#60,#C0,#F8,#00,#00,#00,#00,#00,#00,#00,#00 ;2 144
db 6,#00,#70,#D8,#30,#18,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;3 145
db 6,#00,#C0,#D8,#D8,#F8,#18,#18,#00,#00,#00,#00,#00,#00,#00,#00 ;4 146
db 6,#00,#F8,#C0,#F0,#18,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;5 147
db 6,#00,#78,#C0,#F0,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;6 148
db 6,#00,#F8,#18,#30,#30,#60,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;7 149
db 6,#00,#70,#D8,#70,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;8 150
db 6,#00,#70,#D8,#D8,#78,#18,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;9 151
db 3,#00,#00,#00,#C0,#00,#00,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;dp152
db 4,#00,#00,#00,#60,#00,#00,#60,#C0,#00,#00,#00,#00,#00,#00,#00 ;; 153
db 5,#00,#00,#30,#60,#C0,#60,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;< 154
db 7,#00,#10,#18,#fc,#fc,#18,#10,#00,#00,#00,#00,#00,#00,#00,#00 ;rg 155 ## =
db 5,#00,#00,#C0,#60,#30,#60,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;> 156
db 6,#00,#70,#D8,#30,#60,#00,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;? 157
db 7,#00,#78,#CC,#DC,#DC,#C0,#78,#00,#00,#00,#00,#00,#00,#00,#00 ;@ 158
db 6,#00,#70,#D8,#F8,#D8,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;A 159
db 6,#00,#F0,#D8,#F0,#D8,#D8,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;B 160
db 6,#00,#70,#D8,#C0,#C0,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;C 161
db 6,#00,#F0,#D8,#D8,#D8,#D8,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;D 162
db 6,#00,#F8,#C0,#F0,#C0,#C0,#F8,#00,#00,#00,#00,#00,#00,#00,#00 ;E 163
db 6,#00,#F8,#C0,#F0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;F 164
db 6,#00,#78,#C0,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;G 165
db 6,#00,#D8,#D8,#F8,#D8,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;H 166
db 3,#00,#C0,#C0,#C0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;I 167
db 6,#00,#18,#18,#18,#18,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;J 168
db 6,#00,#D8,#F0,#E0,#F0,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;K 169
db 6,#00,#C0,#C0,#C0,#C0,#C0,#F8,#00,#00,#00,#00,#00,#00,#00,#00 ;L 170
db 7,#00,#CC,#FC,#FC,#CC,#CC,#CC,#00,#00,#00,#00,#00,#00,#00,#00 ;M 171
db 7,#00,#CC,#EC,#FC,#DC,#CC,#CC,#00,#00,#00,#00,#00,#00,#00,#00 ;N 172
db 6,#00,#70,#D8,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;O 173
db 6,#00,#F0,#D8,#D8,#F0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;P 174
db 7,#00,#70,#D8,#D8,#D8,#F8,#6C,#00,#00,#00,#00,#00,#00,#00,#00 ;Q 175
db 6,#00,#F0,#D8,#F0,#D8,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;R 176
db 6,#00,#78,#C0,#70,#18,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;S 177
db 7,#00,#FC,#30,#30,#30,#30,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;T 178
db 6,#00,#D8,#D8,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;U 179
db 7,#00,#CC,#CC,#CC,#78,#78,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;V 180
db 7,#00,#CC,#CC,#CC,#FC,#FC,#CC,#00,#00,#00,#00,#00,#00,#00,#00 ;W 181
db 7,#00,#CC,#78,#30,#78,#CC,#CC,#00,#00,#00,#00,#00,#00,#00,#00 ;X 182
db 7,#00,#CC,#78,#30,#30,#30,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;Y 183
db 6,#00,#F8,#18,#30,#60,#C0,#F8,#00,#00,#00,#00,#00,#00,#00,#00 ;Z 184
db 4,#00,#E0,#C0,#C0,#C0,#C0,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;[ 185
db 6,#C0,#C0,#60,#60,#30,#30,#18,#18,#00,#00,#00,#00,#00,#00,#00 ;\ 186
db 4,#00,#E0,#60,#60,#60,#60,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;] 187
db 5,#00,#70,#D8,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;^ 188
db 8,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;s8 189 ## _
db 3,#00,#00,#00,#00,#00,#00,#00,#ff,#00,#00,#00,#00,#00,#00,#00 ;su 190 ## `
db 6,#00,#00,#78,#D8,#D8,#D8,#78,#00,#00,#00,#00,#00,#00,#00,#00 ;a 191
db 6,#00,#C0,#F0,#D8,#D8,#D8,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;b 192
db 6,#00,#00,#78,#C0,#C0,#C0,#78,#00,#00,#00,#00,#00,#00,#00,#00 ;c 193
db 6,#00,#18,#78,#D8,#D8,#D8,#78,#00,#00,#00,#00,#00,#00,#00,#00 ;d 194
db 6,#00,#00,#70,#D8,#F8,#C0,#78,#00,#00,#00,#00,#00,#00,#00,#00 ;e 195
db 4,#00,#60,#C0,#E0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;f 196
db 6,#00,#00,#70,#D8,#D8,#78,#18,#F0,#00,#00,#00,#00,#00,#00,#00 ;g 197
db 6,#00,#C0,#F0,#D8,#D8,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;h 198
db 3,#00,#C0,#00,#C0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;i 199
db 4,#00,#60,#00,#60,#60,#60,#60,#C0,#00,#00,#00,#00,#00,#00,#00 ;j 200
db 6,#00,#C0,#D8,#F0,#E0,#F0,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;k 201
db 4,#00,#C0,#C0,#C0,#C0,#C0,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;l 202
db 8,#00,#00,#7C,#D6,#D6,#D6,#D6,#00,#00,#00,#00,#00,#00,#00,#00 ;m 203
db 6,#00,#00,#F0,#D8,#D8,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;n 204
db 6,#00,#00,#70,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;o 205
db 6,#00,#00,#F0,#D8,#D8,#D8,#F0,#C0,#00,#00,#00,#00,#00,#00,#00 ;p 206
db 6,#00,#00,#78,#D8,#D8,#D8,#78,#18,#00,#00,#00,#00,#00,#00,#00 ;q 207
db 5,#00,#00,#F0,#C0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;r 208
db 6,#00,#00,#78,#E0,#70,#38,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;s 209
db 4,#00,#C0,#E0,#C0,#C0,#C0,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;t 210
db 6,#00,#00,#D8,#D8,#D8,#D8,#70,#00,#00,#00,#00,#00,#00,#00,#00 ;u 211
db 7,#00,#00,#CC,#CC,#CC,#78,#30,#00,#00,#00,#00,#00,#00,#00,#00 ;v 212
db 8,#00,#00,#C6,#D6,#D6,#FE,#6C,#00,#00,#00,#00,#00,#00,#00,#00 ;w 213
db 6,#00,#00,#D8,#D8,#70,#D8,#D8,#00,#00,#00,#00,#00,#00,#00,#00 ;x 214
db 6,#00,#00,#D8,#D8,#D8,#78,#18,#70,#00,#00,#00,#00,#00,#00,#00 ;y 215
db 7,#00,#00,#FC,#18,#30,#60,#FC,#00,#00,#00,#00,#00,#00,#00,#00 ;z 216
db 5,#30,#60,#60,#C0,#60,#60,#60,#30,#00,#00,#00,#00,#00,#00,#00 ;{ 217
db 3,#00,#C0,#C0,#C0,#C0,#C0,#C0,#00,#00,#00,#00,#00,#00,#00,#00 ;| 218
db 5,#C0,#60,#60,#30,#60,#60,#60,#C0,#00,#00,#00,#00,#00,#00,#00 ;} 219
db 5,#00,#30,#48,#48,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;0 220 36 ITALIC 0-9,A-Z
db 3,#00,#20,#60,#20,#40,#40,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;1 221
db 5,#00,#30,#48,#10,#60,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;2 222
db 5,#00,#30,#48,#10,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;3 223
db 5,#00,#40,#48,#88,#F0,#10,#10,#00,#00,#00,#00,#00,#00,#00,#00 ;4 224
db 5,#00,#78,#40,#70,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;5 225
db 5,#00,#38,#40,#70,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;6 226
db 5,#00,#78,#08,#10,#20,#40,#40,#00,#00,#00,#00,#00,#00,#00,#00 ;7 227
db 5,#00,#30,#48,#70,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;8 228
db 5,#00,#30,#48,#48,#70,#10,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;9 229
db 5,#00,#30,#48,#78,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;A 230
db 5,#00,#70,#48,#70,#90,#90,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;B 231
db 5,#00,#30,#48,#40,#80,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;C 232
db 5,#00,#70,#48,#48,#90,#90,#E0,#00,#00,#00,#00,#00,#00,#00,#00 ;D 233
db 5,#00,#78,#40,#70,#80,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;E 234
db 5,#00,#78,#40,#70,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;F 235
db 5,#00,#38,#40,#58,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;G 236
db 5,#00,#48,#48,#78,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;H 237
db 2,#00,#40,#40,#40,#80,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;I 238
db 5,#00,#08,#08,#08,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;J 239
db 5,#00,#48,#50,#60,#A0,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;K 240
db 5,#00,#40,#40,#40,#80,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;L 241
db 6,#00,#44,#6C,#54,#88,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;M 242
db 6,#00,#44,#64,#54,#98,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;N 243
db 5,#00,#30,#48,#48,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;O 244
db 5,#00,#70,#48,#58,#E0,#80,#80,#00,#00,#00,#00,#00,#00,#00,#00 ;P 245
db 6,#00,#30,#48,#48,#90,#B0,#68,#00,#00,#00,#00,#00,#00,#00,#00 ;Q 246
db 5,#00,#70,#48,#70,#90,#90,#90,#00,#00,#00,#00,#00,#00,#00,#00 ;R 247
db 5,#00,#38,#40,#30,#10,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;S 248
db 6,#00,#7C,#10,#10,#20,#20,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;T 249
db 5,#00,#48,#48,#48,#90,#90,#60,#00,#00,#00,#00,#00,#00,#00,#00 ;U 250
db 6,#00,#44,#44,#48,#50,#50,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;V 251
db 6,#00,#44,#44,#44,#A8,#D8,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;W 252
db 6,#00,#44,#28,#30,#50,#88,#88,#00,#00,#00,#00,#00,#00,#00,#00 ;X 253
db 6,#00,#44,#28,#10,#20,#20,#20,#00,#00,#00,#00,#00,#00,#00,#00 ;Y 254
db 5,#00,#78,#08,#30,#40,#80,#F0,#00,#00,#00,#00,#00,#00,#00,#00 ;Z 255

hlpfnttyp   db 0                ;0=multi, 1=system, 2=code, +128=crunched
hlplnknum   db 0
hlplnkdat   db 0        ;!!! last byte in data area !!!


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns

;### PRGPRZS -> Stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14


;### ALERT WINDOW #############################################################

errlodobj   dw errlodtxt1,4*1+2, errlodtxt2,4*1+2, errlodtxt3,4*1+2             ;disc error
errfndobj   dw errfndtxt1,4*1+2, errfndtxt2,4*1+2, errfndtxt3,4*1+2,prgicnbig   ;not found

;### MAIN WINDOW ##############################################################

prgwindat dw #1f02,3,30,20,220,128,0,0,200,106,200,64,10000,10000,prgicnsml,hlptoctxt,0,0,prgwingrp,prgtolgrp,15:ds 136+14

prgtolgrp db 5,0:dw prgtolobj,0,0,256*0+0,0,0,2
prgtolobj
dw     00,255*256+0, 2,       0,0,10000,10000,0     ;00=Background
dw navbck,255*256+10,tolprvspr ,  1,  1, 16,12,0    ;01=Button "Previous"
dw navfrw,255*256+10,tolnxtspr , 17,  1, 16,12,0    ;02=Button "Next"
dw navhom,255*256+10,tolhomspr , 33,  1, 16,12,0    ;03=Button "Home"
dw navopn,255*256+10,tolselspr , 49,  1, 16,12,0    ;04=Button "Open"

prgwingrp db 12,0:dw prgwinobj,prgwinclc,0,256*0+0,0,0,2
prgwinclc
dw       0, 0, 0, 0, 10000,       0, 10000,       0     ;Background
dw       1, 0, 0, 0,   102,       0,     0, 256*1+1     ;Tab Background
dw       1, 0, 1, 0,   102,       0,    11,       0     ;Tab
dw       3, 0,14, 0,    98,       0,     8,       0     ;text   "keyword to find"
dw       3, 0,23, 0,    98,       0,    12,       0     ;input control
dw      49, 0,36, 0,    52,       0,    12,       0     ;button "list topics"
dw       3, 0,52, 0,    98,       0,     8,       0     ;text   "select topic"
dw 49,0,-13,256*1+1,    52,       0,    12,       0     ;button "display"
dw       3, 0,61, 0,    98,       0,   -75, 256*1+1     ;List content (search)
dw       1, 0,13, 0,   102,       0,   -13, 256*1+1     ;List content (contents)
dw   103+1, 0, 0, 0,     2,       0,    -1, 256*1+1     ;Text separator
dw   105+1, 0, 0, 0,-105-2, 256*1+1,    -1, 256*1+1     ;Text

prgwinobj
dw     00,255*256+00,1         ,0,0,0,0,0   ;00=Background
dw     00,255*256+00,2         ,0,0,0,0,0   ;01=Tabs Background
dw navtab,255*256+20,hlptabobj ,0,0,0,0,0   ;02=Tabs
prgwinobj0
dw     00,255*256+01,hlpdspobj1,0,0,0,0,0   ;03=text   "keyword to find"
dw     00,255*256+32,hlpinpobj ,0,0,0,0,0   ;04=input control
dw fndlst,255*256+16,hlpbuttxt1,0,0,0,0,0   ;05=button "list topics"
dw     00,255*256+01,hlpdspobj2,0,0,0,0,0   ;06=text   "select topic"
dw fnddsp,255*256+16,hlpbuttxt2,0,0,0,0,0   ;07=button "display"
dw fndclk,255*256+41,hlpfndobj ,0,0,0,0,0   ;08=List Content (search)
dw tocclk,255*256+41,hlptocobj ,0,0,0,0,0   ;09=List Content (contents)
dw     00,255*256+00,128+8     ,0,0,0,0,0   ;10=Text separator
prgwinobj1
dw txtclk,255*256+33,0         ,0,0,0,0,0   ;11=Text

hlpdspobj1  dw hlpdsptxt1,2+4
hlpdspobj2  dw hlpdsptxt2,2+4
hlpinpobj   dw hlpinptxt,0,0,0,0,31,0

;### status control data
hlptabobj   db 2,2+4+48+64
hlptabobj0  db 0:dw hlptabtxt1:db -1:dw hlptabtxt2:db -1

;### find list
hlpfndobj   dw 0,0,hlpfndlst,0,256*0+1,hlpfndrow,0,1
hlpfndrow   dw 0,10000,00,0
hlpfndlst   ds lstlinmax*4

;### TOC list
hlptocobj   dw 0,0,hlptoclst,0,256*0+1,hlptocrow,0,1
hlptocrow   dw 0,10000,00,0
hlptoclst   db 0        ;!!! last byte in transfer area !!!

prgtrnend
