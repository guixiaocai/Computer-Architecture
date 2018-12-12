`include "myCPU_define.h"

module inst_decode(
    input  [31:0] inst_sram_rdata,
    input  [ 3:0] cur_state,
    input  [31:0] alu_result,
    input         Zero,

    //rf
    output        rf_wen,
    output [ 1:0] rf_waddr_sel,
    output        rf_wdata_sel,
    
    //alu
    output        alu_src1_sel,
    output        alu_src2_sel,
    output        alu_src_op_jal,
    output [11:0] alu_control,

    output        data_sram_en,
    output [ 3:0] data_sram_wen, 

    output        branch_cond, 
    output        inst_sram_addr_dirtw, 
    output        inst_sram_addr_condw, 
    output [ 1:0] inst_sram_addr_src,
    output        data_sram_addr_src
);

wire op_Rtype = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_R_TYPE);
wire op_blgz  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BLGZ);
wire op_beq   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BEQ);
wire op_bne   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BNE);
wire op_blez  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BLEZ);
wire op_bgtz  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_BGTZ);
wire op_jmp   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_JMP);
wire op_jal   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_JAL);
wire op_lui   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LUI);
wire op_lb    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LB);
wire op_lbu   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LBU);
wire op_lh    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LH);
wire op_lhu   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LHU);
wire op_lwl   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LWL);
wire op_lwr   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LWR);
wire op_lw    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_LW);
wire op_sb    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SB);
wire op_sh    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SH);
wire op_swl   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SWL);
wire op_sw    = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SW);
wire op_swr   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SWR);
wire op_slti  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SLTI);
wire op_sltiu = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_SLTIU);
wire op_addiu = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ADDIU);
wire op_andi  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ANDI);
wire op_ori   = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_ORI);
wire op_xori  = ~| (inst_sram_rdata[31:26] ^ `MIPS32_OP_XORI);
wire op_jr    = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_JR);
wire op_sll   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLL);
wire op_srl   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRL);
wire op_sra   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRA);
wire op_sllv  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLLV);
wire op_srlv  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRLV);
wire op_srav  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SRAV);
wire op_jalr  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_JALR);
wire op_addu  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_ADDU);
wire op_subu  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SUBU);
wire op_and   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_AND);
wire op_or    = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_OR);
wire op_xor   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_XOR);
wire op_nor   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_NOR);
wire op_slt   = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLT);
wire op_sltu  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_SLTU);
wire op_movz  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_MOVZ);
wire op_movn  = ~| (inst_sram_rdata[ 5: 0] ^ `MIPS32_OP_MOVN);

wire branch   = op_blgz | op_beq | op_bne | op_blez | op_bgtz;
wire jump     = op_jmp  | op_jal;
wire load     = op_lui  | op_lb  | op_lbu | op_lh   | op_lhu | op_lwl | op_lwr | op_lw;
wire store    = op_sb   | op_sh  | op_swl | op_swr  | op_sw;
wire move_c   = op_Rtype & (op_movz | op_movn);

//control signal
assign rf_wen               = cur_state[2] & ~(branch | (op_Rtype & op_jr) | op_sw);
assign rf_waddr_sel[1]      = op_jal;
assign rf_waddr_sel[0]      = load | op_addiu;
assign alu_src1_sel         = ~(op_Rtype & (op_sll | op_sra | op_srl));
assign alu_src2_sel         = ~(op_lui | op_addiu | store | load);
assign alu_src_op_jal       = op_jal;
assign rf_wdata_sel         = op_lw;

assign inst_sram_addr_dirtw  = cur_state[2] & (jump | (op_Rtype & op_jr));
assign inst_sram_addr_condw  = cur_state[2] & branch;
assign branch_cond           = (op_bgtz & ~(alu_result[31] | Zero))
                             | (op_blgz & (inst_sram_rdata[16] ^ alu_result[0]))
                             | ((op_beq | op_bne) & (inst_sram_rdata[26] ^ Zero))
                             | (op_blez & (alu_result[31] | Zero));
assign inst_sram_addr_src[0] = ~((op_Rtype & (op_jr | op_jalr)) | op_jal | op_jmp);
assign inst_sram_addr_src[1] = ~((op_Rtype & (op_jr | op_jalr)) | branch);
assign data_sram_addr_src    = load & ~op_lui;

assign data_sram_en          = (load & ~op_lui) | store;
assign data_sram_wen         = {4{store & cur_state[3]}};

assign alu_control[0]  = cur_state[2] & op_lui;
assign alu_control[1]  = cur_state[2] & op_Rtype & (op_sra  | op_srav);
assign alu_control[2]  = cur_state[2] & op_Rtype & (op_srl  | op_srlv);
assign alu_control[3]  = cur_state[2] & op_Rtype & (op_sll  | op_sllv);
assign alu_control[4]  = cur_state[2] & ((op_Rtype & op_xor)  | op_xori);
assign alu_control[5]  = cur_state[2] & (op_ori  | (op_Rtype & op_or));
assign alu_control[6]  = cur_state[2] & op_Rtype & op_nor;
assign alu_control[7]  = cur_state[2] & ((op_Rtype & op_and)  | op_andi);
assign alu_control[8]  = cur_state[2] & ((op_Rtype & op_sltu) | op_sltiu);
assign alu_control[9]  = (cur_state[2] & ((op_Rtype & op_slt) | op_slti)) | (cur_state[2] & op_blgz);
assign alu_control[10] = (cur_state[2] & op_Rtype & op_subu) | (cur_state[2] & (op_beq | op_bne | op_blez));
assign alu_control[11] = ~| alu_control[10:0];

endmodule