; z88dk compatibility macros for um80
; Include this before including z88dk source files
; Also requires um80 to be invoked with $ hex syntax support

; EXTERN -> EXTRN
EXTERN  MACRO sym
        EXTRN sym
        ENDM

; SECTION is ignored (use CSEG/DSEG at module level)
SECTION MACRO name
        ENDM

; defw -> DW with variable arguments
defw    MACRO val
        IRP v,<val>
        DW v
        ENDM
        ENDM

; defb -> DB with variable arguments
defb    MACRO val
        IRP v,<val>
        DB v
        ENDM
        ENDM

; defs -> DS
defs    MACRO size
        DS size
        ENDM
