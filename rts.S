; https://stackoverflow.com/questions/29617008/how-to-make-global-arrays-and-dwords-in-x86-assembly

extern malloc, free

kempe_init:
    push QWORD 0x800000
    CALL malloc
    mov rbx, rdx

kempe_cleanup:
    push rbx
    CALL free
