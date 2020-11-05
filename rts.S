; https://stackoverflow.com/questions/29617008/how-to-make-global-arrays-and-dwords-in-x86-assembly

extern malloc, free

section .bss
    kempe_data: resb 0x800000

kempe_init:
    mov rbx, kempe_data
