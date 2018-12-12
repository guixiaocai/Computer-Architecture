#include <asm.h>
#include <regdef.h>
#include <ucas_cde.h>

##s0, mode
##s1, number adress 
##s2, exception use
##s3, time
##s4, exception pc
	.set	noreorder
    .set    mips32
	.globl	_start
	.globl	start
	.globl	__main
_start:
start:

	b	locate
	nop

##avoid "j locate" not taken
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
    nop

##avoid cpu run error
.org 0x0ec
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
.org 0x100
test_finish:
    addiu t0, t0, 1
    li t1, 0xff
    LI (t2, UART_ADDR)
    sw t1, 0x0(t2)
    b test_finish
    nop
##avoid cpu run error
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
/*
 *  exception handle
 */
.org 0x380
    mfc0 k0, CAUSE
    nop
    andi k0, k0, 0x8000
    beq  k0, zero, 1f
    nop
    jal add_1s
    nop
    #mtc0 zero, COUNT
    #nop
    #LI (t6, SEC_N)
	mfc0 t6, COMPARE
	nop
	LI (k0, SEC)	
	addu t6, t6, k0
    mtc0 t6, COMPARE
    nop
    eret
    nop
#硬件中断
1:
    lw   k0, 0(s2)
    andi k0, k0, 0x1
    bne  k0, zero, 1b
    nop
    beq  s0, zero, 3f
    nop
    lui  s0, 0
2:
    #计时
    la   k0, wait_1s
    mtc0 k0, EPC
    nop
    mfc0 k0, CAUSE
    nop
    li   k1, 0xffffbfff
    and  k0, k0, k1
    mtc0 k0, CAUSE
    nop
    mtc0 zero, COUNT
    nop
    LI (t6, SEC)
    mtc0 t6, COMPARE
    nop
    eret
    nop
3:
    addiu s0, s0, 0x1 #设置模式
4:
    mtc0  zero, COUNT
    nop
    lw    k0, 0(s2) 
    andi  k1, k0, 0x1
    bne   k1, zero, 1b #是否又按了一次暂停/开始键
    nop
    andi  k1, k0, 0x2
    beq   k1, zero, 6f #是否是按了加1s键
    nop
    jal   add_1s
    nop
5:
    mfc0  t7, COUNT
    nop
    li    t8, 0xffb3b4c0 #-500,000    
    addu  t7, t7, t8 #200ms
    blez  t7, 10f
    nop
    mtc0  zero, COUNT
    nop
    jal   add_1s
    nop
10:
    lw    k0, 0(s2)
    andi  k1, k0, 0x2
    bne   k1, zero, 5b #等待松开按键
    nop
    b     4b
    nop
6:
    andi  k1, k0, 0x4
    beq   k1, zero, 8f #是否按了加1min键
    nop
    jal   add_1m
    nop
7:
    mfc0  t7, COUNT
    nop
    li    t8, 0xffb3b4c0 #-500,000    
    addu  t7, t7, t8 #200ms
    blez  t7, 10f
    nop
    mtc0  zero, COUNT
    nop
    jal   add_1m
    nop
10:
    lw    k0, 0(s2)
    andi  k1, k0, 0x4
    bne   k1, zero, 7b #等待松开按键
    nop
    b     4b
    nop
8:
    andi  k1, k0, 0x8
    beq   k1, zero, 4b #是否按了加1h键
    nop
    jal   add_1h
    nop
9:
    mfc0  t7, COUNT
    nop
    li    t8, 0xffb3b4c0 #-500,000    
    addu  t7, t7, t8 #200ms
    blez  t7, 10f
    nop
    mtc0  zero, COUNT
    nop
    jal   add_1h
    nop
10:
    lw    k0, 0(s2)
    andi  k1, k0, 0x8
    bne   k1, zero, 9b #等待松开按键
    nop
    b     4b
    nop


locate:
	.set noreorder

    LI (a0, LED_RG1_ADDR)
    LI (a1, LED_RG0_ADDR)
    LI (a2, LED_ADDR)
    LI (s1, NUM_ADDR)
    LI (s2, BTN_KEY_ADDR)

    LI (t1, 0x0002)
    LI (t2, 0x0001)
    LI (t3, 0x0000ffff)

    #initial
    LI (t0, 0x0005)
    LI (t1, 0x0005)
    LI (t2, 0x0009)
    LI (t3, 0x0005)
    LI (t4, 0x0003)
    LI (t5, 0x0002)
    lui s0, 0
    jal show_time # time: 23:59:55
    ###
    LI (t6, SEC)
    mtc0 t6, COMPARE
    nop
    mtc0 zero, COUNT
    nop
    mfc0 t8, STATUS
    nop
    li   t7, 0xfffffff9
    and  t8, t8, t7
    ori  t8, t8, 0xc001
    mtc0 t8, STATUS
    nop
wait_1s:
    nop
    b wait_1s
    nop
    
add_1s:
    #秒个位是否进位
    addiu t0, t0, 0x1
    addiu t7, t0, 0xfff6
    bltz  t7, show_time
    nop
    lui   t0, 0
    #秒十位是否进位
    addiu t1, t1, 0x1
    addiu t7, t1, 0xfffa
    bltz  t7, show_time
    nop
    lui   t1, 0
add_1m:
    #分个位是否进位
    addiu t2, t2, 0x1
    addiu t7, t2, 0xfff6
    bltz  t7, show_time
    nop
    lui   t2, 0
    #分十位是否进位
    addiu t3, t3, 0x1
    addiu t7, t3, 0xfffa
    bltz  t7, show_time
    nop
    lui   t3, 0
add_1h:
    #时个位是否进位
    addiu t4, t4, 0x1
    addiu t7, t5, 0xfffe
    bltz  t7, 1f #十位是0或1
    nop
    addiu t7, t4, 0xfffc
    bltz  t7, show_time
    nop
    lui   t4, 0
    lui   t5, 0 #00:00:00
    b     show_time
    nop
1:
    addiu t7, t4, 0xfff6
    bltz  t7, show_time
    nop
    lui   t4, 0
    #时十位
    addiu t5, t5, 0x1

show_time:
    lui s3, 0
    #hour
    or  s3, s3, t5
    sll s3, s3, 0x4
    or  s3, s3, t4
    #minute
    sll s3, s3, 0x4
    or  s3, s3, t3
    sll s3, s3, 0x4
    or  s3, s3, t2
    #second
    sll s3, s3, 0x4    
    or  s3, s3, t1
    sll s3, s3, 0x4
    or  s3, s3, t0
    #display
    sw  s3, 0(s1)
    jr ra
    nop
