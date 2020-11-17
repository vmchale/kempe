; https://stackoverflow.com/questions/29617008/how-to-make-global-arrays-and-dwords-in-x86-assembly

section .bss

global kempe_data
kempe_data: resb 0x800000

section .text

global kempe_init
kempe_init:
    mov rbx, kempe_data
