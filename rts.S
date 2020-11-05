; https://stackoverflow.com/questions/29617008/how-to-make-global-arrays-and-dwords-in-x86-assembly

extern malloc, free

section .data
    kempe_data times 0x800000 db 0

kempe_init:
    mov rbx, kempe_data
