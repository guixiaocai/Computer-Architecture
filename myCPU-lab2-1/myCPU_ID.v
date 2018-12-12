`include "myCPU_define.h"

module inst_decode(
    input  [31:0] inst_sram_rdata,

    //rf
    output [ 1:0] RegDst,
    output        RegWrite,
    output        MemtoReg,
    
    //alu
    output [ 1:0] alu_src1_sel,
    output [ 1:0] alu_src2_sel,
    output [11:0] alu_control,

    //Mem
    output        MemRead,
    output [ 3:0] MemWrite, 

    output        Jump,
    output        Branch,
    output [ 4:0] op_branch,
    output [ 1:0] PCsrc
);

wire op_Rtype =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_R_TYPE);
wire op_blgz  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BLGZ);
wire op_beq   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BEQ);
wire op_bne   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BNE);
wire op_blez  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BLEZ);
wire op_bgtz  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BGTZ);
wire op_jmp   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_JMP);
wire op_jal   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_JAL);
wire op_lui   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LUI);
wire op_lb    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LB);
wire op_lbu   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LBU);
wire op_lh    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LH);
wire op_lhu   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LHU);
wire op_lwl   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LWL);
wire op_lwr   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LWR);
wire op_lw    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LW);
wire op_sb    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SB);
wire op_sh    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SH);
wire op_swl   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SWL);
wire op_sw    =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SW);
wire op_swr   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SWR);
wire op_slti  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SLTI);
wire op_sltiu =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SLTIU);
wire op_addiu =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ADDIU);
wire op_andi  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ANDI);
wire op_ori   =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ORI);
wire op_xori  =  ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_XORI);
wire op_jr    = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_JR))   & op_Rtype;
wire op_sll   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLL))  & op_Rtype;
wire op_srl   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRL))  & op_Rtype;
wire op_sra   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRA))  & op_Rtype;
wire op_sllv  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLLV)) & op_Rtype;
wire op_srlv  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRLV)) & op_Rtype;
wire op_srav  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRAV)) & op_Rtype;
wire op_jalr  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_JALR)) & op_Rtype;
wire op_addu  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_ADDU)) & op_Rtype;
wire op_subu  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SUBU)) & op_Rtype;
wire op_and   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_AND))  & op_Rtype;
wire op_or    = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_OR))   & op_Rtype;
wire op_xor   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_XOR))  & op_Rtype;
wire op_nor   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_NOR))  & op_Rtype;
wire op_slt   = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLT))  & op_Rtype;
wire op_sltu  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLTU)) & op_Rtype;
wire op_movz  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_MOVZ)) & op_Rtype;
wire op_movn  = (~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_MOVN)) & op_Rtype;

wire branch   = op_blgz | op_beq | op_bne | op_blez | op_bgtz;
wire load     = op_lb   | op_lbu | op_lh   | op_lhu | op_lwl | op_lwr | op_lw;
wire store    = op_sb   | op_sh  | op_swl | op_swr  | op_sw;

//control signal
assign RegDst[0] = op_Rtype;
assign RegDst[1] = op_jal;
assign RegWrite  = (op_Rtype & ~op_jr) | op_jal  | op_addiu | op_slti | op_sltiu
                    | op_andi | op_ori | op_xori | op_lui   | load;
assign MemtoReg  = load;

assign alu_src1_sel[0] = op_jal | op_jalr;
assign alu_src1_sel[1] = op_sll | op_srl | op_sra;
assign alu_src2_sel[0] = load   | store  | op_movz | op_movn | op_addiu | op_sltiu
                      | op_slti | op_ori | op_andi | op_xori | op_lui;
assign alu_src2_sel[1] = op_jal | op_movn | op_movz | op_jalr;

assign Jump   = op_jr | op_jalr | op_jal | op_jmp;
assign Branch = branch;
assign MemWrite    = {4{store}};
assign MemRead     = load;

assign op_branch[0] = op_blgz & ~inst_sram_rdata[16];
assign op_branch[1] = op_blgz &  inst_sram_rdata[16];
assign op_branch[2] = op_beq;
assign op_branch[3] = op_bne;
assign op_branch[4] = op_blez;

assign PCsrc[0] = op_jr  | op_jalr | branch;
assign PCsrc[1] = op_jal | op_jmp  | branch;

assign alu_control[0]  =  op_lui;
assign alu_control[1]  =  op_sra  | op_srav;
assign alu_control[2]  =  op_srl  | op_srlv;
assign alu_control[3]  =  op_sll  | op_sllv;
assign alu_control[4]  =  op_xor  | op_xori;
assign alu_control[5]  =  op_ori  | op_or;
assign alu_control[6]  =  op_nor;
assign alu_control[7]  =  op_and  | op_andi;
assign alu_control[8]  =  op_sltu | op_sltiu;
assign alu_control[9]  =  op_slt | op_slti;
assign alu_control[10] =  op_subu | op_beq | op_bne | op_blez;
assign alu_control[11] = ~| alu_control[10:0];

endmodule