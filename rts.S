extern malloc

init:
    push QWORD 0x800000
    CALL malloc
    mov rbx, rdx
