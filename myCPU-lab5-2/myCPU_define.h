`define myCPU_INIT_INST_ADDR 32'hbfc00000

/****** MIPS32 Op & Fmt Code ******/

/******* R-Type *********/
`define MIPS32_OP_R_TYPE 6'b000_000
`define MIPS32_OP_JR	 6'b001_000
`define MIPS32_OP_SLL	 6'b000_000
`define MIPS32_OP_SRL	 6'b000_010
`define MIPS32_OP_SRA	 6'b000_011
`define MIPS32_OP_SLLV	 6'b000_100
`define MIPS32_OP_SRLV	 6'b000_110
`define MIPS32_OP_SRAV	 6'b000_111
`define MIPS32_OP_JALR	 6'b001_001
`define MIPS32_OP_ADD	 6'b100_000
`define MIPS32_OP_ADDU	 6'b100_001
`define MIPS32_OP_SUB	 6'b100_010
`define MIPS32_OP_SUBU	 6'b100_011
`define MIPS32_OP_AND	 6'b100_100
`define MIPS32_OP_OR	 6'b100_101
`define MIPS32_OP_XOR	 6'b100_110
`define MIPS32_OP_NOR	 6'b100_111
`define MIPS32_OP_SLT	 6'b101_010
`define MIPS32_OP_SLTU	 6'b101_011
`define MIPS32_OP_MOVZ	 6'b001_010
`define MIPS32_OP_MOVN	 6'b001_011

`define MIPS32_OP_MULT	 6'b011_000
`define MIPS32_OP_MULTU	 6'b011_001
`define MIPS32_OP_MFHI	 6'b010_000
`define MIPS32_OP_MFLO	 6'b010_010
`define MIPS32_OP_MTHI	 6'b010_001
`define MIPS32_OP_MTLO	 6'b010_011
`define MIPS32_OP_DIV	 6'b011_010
`define MIPS32_OP_DIVU	 6'b011_011

`define MIPS32_OP_SYS	 6'b001_100

/******* other-Type *********/
`define MIPS32_OP_BLGZ   6'b000_001 //bltz, bgez
`define MIPS32_OP_BEQ    6'b000_100
`define MIPS32_OP_BNE    6'b000_101
`define MIPS32_OP_BLEZ   6'b000_110
`define MIPS32_OP_BGTZ   6'b000_111
`define MIPS32_OP_BGEZAL 6'b000_001 //bgezal, bltzal
`define MIPS32_OP_JMP    6'b000_010
`define MIPS32_OP_JAL    6'b000_011
`define MIPS32_OP_LUI    6'b001_111
`define MIPS32_OP_LB     6'b100_000 //lb, lbu
`define MIPS32_OP_LBU    6'b100_100
`define MIPS32_OP_LH	 6'b100_001 //lh, lhu
`define MIPS32_OP_LHU	 6'b100_101
`define MIPS32_OP_LWL	 6'b100_010 //lwl
`define MIPS32_OP_LWR	 6'b100_110 //lwr
`define MIPS32_OP_LW     6'b100_011
`define MIPS32_OP_SB	 6'b101_000
`define MIPS32_OP_SH	 6'b101_001
`define MIPS32_OP_SWL	 6'b101_010
`define MIPS32_OP_SW	 6'b101_011
`define MIPS32_OP_SWR	 6'b101_110
`define MIPS32_OP_SLTI   6'b001_010
`define MIPS32_OP_SLTIU	 6'b001_011
`define MIPS32_OP_ADDIU	 6'b001_001
`define MIPS32_OP_ADDI	 6'b001_000
`define MIPS32_OP_ANDI	 6'b001_100
`define MIPS32_OP_ORI	 6'b001_101
`define MIPS32_OP_XORI	 6'b001_110
`define MIPS32_OP_BP	 6'b001_101

/************ CP0 ***************/
`define MIPS32_OP_MTC0	 11'b010_000_00100
`define MIPS32_OP_MFC0	 11'b010_000_00000
`define MIPS32_OP_COP0	 6'b010_000
`define MIPS32_OP_ERET	 6'b011_000