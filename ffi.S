global kempe_init
kempe_init:
    mov rbx, kempe_data
    ret

global get_kempe_pointer
get_kempe_pointer:
    mov rax, kempe_data
    ret

global set_kempe_pointer
set_kempe_pointer:
    mov rbx, rdi
    ret
