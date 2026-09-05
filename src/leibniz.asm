; NASM, Linux x86_64 System V ABI. Scalar double precision Leibniz series.
default rel
extern fopen, fscanf, fclose, printf
global main
section .rodata
path: db 'rounds.txt', 0
mode: db 'r', 0
scan_format: db '%lu', 0
print_format: db '%.16f', 10, 0
one: dq 1.0
section .text
main:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    lea rdi, [path]
    lea rsi, [mode]
    call fopen wrt ..plt
    test rax, rax
    jz .error
    mov [rbp-8], rax
    mov rdi, rax
    lea rsi, [scan_format]
    lea rdx, [rbp-16]
    xor eax, eax
    call fscanf wrt ..plt
    mov [rbp-24], eax
    mov rdi, [rbp-8]
    call fclose wrt ..plt
    cmp dword [rbp-24], 1
    jne .error
    cmp qword [rbp-16], 1000000000
    ja .error
    movsd xmm2, [one]
    xor ecx, ecx
.loop:
    cmp rcx, [rbp-16]
    jae .print
    mov eax, ecx
    and eax, 1
    lea rax, [rax*2-1]
    cvtsi2sd xmm0, rax
    lea rax, [rcx*2+3]
    cvtsi2sd xmm1, rax
    divsd xmm0, xmm1
    addsd xmm2, xmm0
    inc rcx
    jmp .loop
.print:
    movsd xmm0, xmm2
    addsd xmm0, xmm0
    addsd xmm0, xmm0
    lea rdi, [print_format]
    mov eax, 1
    call printf wrt ..plt
    xor eax, eax
    leave
    ret
.error:
    mov eax, 1
    leave
    ret
section .note.GNU-stack noalloc noexec nowrite progbits
