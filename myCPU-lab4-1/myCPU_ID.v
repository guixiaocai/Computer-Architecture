`include "myCPU_define.h"

module inst_decode(
    input  [31:0] inst,

    //rf
    output [ 1:0] RegDst,
    output        RegWrite,
    output        MemtoReg,
    
    //alu
    output [ 1:0] alu_src1_sel,
    output [ 1:0] alu_src2_sel,
    output        sign_ext,
    output [11:0] alu_control,

    //Mem
    output        MemRead,
    output [ 3:0] MemWrite, 
    output [ 5:0] op_load,
    output [ 3:0] op_store,

    output        Jump,
    output        Branch,
    output [ 4:0] op_branch,
    output [ 1:0] PCsrc,

    //div && mul
    output        div_signed,
    output        div,
    output        mul_signed,
    output        mul,
    output        HIWrite,
    output        HIRead,
    output        LOWrite,
    output        LORead,

    //cp0
    output        out_op_mtc0,
    output        out_op_mfc0,
    output        syscall,
    output        eret_cmt
);

wire op_Rtype  =  ~| (inst[31:26] ^ `MIPS32_OP_R_TYPE);
wire op_blgz   =  ~| (inst[31:26] ^ `MIPS32_OP_BLGZ);
wire op_beq    =  ~| (inst[31:26] ^ `MIPS32_OP_BEQ);
wire op_bne    =  ~| (inst[31:26] ^ `MIPS32_OP_BNE);
wire op_blez   =  ~| (inst[31:26] ^ `MIPS32_OP_BLEZ);
wire op_bgtz   =  ~| (inst[31:26] ^ `MIPS32_OP_BGTZ);
wire op_bgezal =  ~| (inst[31:26] ^ `MIPS32_OP_BGEZAL);
wire op_jmp    =  ~| (inst[31:26] ^ `MIPS32_OP_JMP);
wire op_jal    =  ~| (inst[31:26] ^ `MIPS32_OP_JAL);
wire op_lui    =  ~| (inst[31:26] ^ `MIPS32_OP_LUI);
wire op_lb     =  ~| (inst[31:26] ^ `MIPS32_OP_LB);
wire op_lbu    =  ~| (inst[31:26] ^ `MIPS32_OP_LBU);
wire op_lh     =  ~| (inst[31:26] ^ `MIPS32_OP_LH);
wire op_lhu    =  ~| (inst[31:26] ^ `MIPS32_OP_LHU);
wire op_lwl    =  ~| (inst[31:26] ^ `MIPS32_OP_LWL);
wire op_lwr    =  ~| (inst[31:26] ^ `MIPS32_OP_LWR);
wire op_lw     =  ~| (inst[31:26] ^ `MIPS32_OP_LW);
wire op_sb     =  ~| (inst[31:26] ^ `MIPS32_OP_SB);
wire op_sh     =  ~| (inst[31:26] ^ `MIPS32_OP_SH);
wire op_swl    =  ~| (inst[31:26] ^ `MIPS32_OP_SWL);
wire op_sw     =  ~| (inst[31:26] ^ `MIPS32_OP_SW);
wire op_swr    =  ~| (inst[31:26] ^ `MIPS32_OP_SWR);
wire op_slti   =  ~| (inst[31:26] ^ `MIPS32_OP_SLTI);
wire op_sltiu  =  ~| (inst[31:26] ^ `MIPS32_OP_SLTIU);
wire op_addi   =  ~| (inst[31:26] ^ `MIPS32_OP_ADDI);
wire op_addiu  =  ~| (inst[31:26] ^ `MIPS32_OP_ADDIU);
wire op_andi   =  ~| (inst[31:26] ^ `MIPS32_OP_ANDI);
wire op_ori    =  ~| (inst[31:26] ^ `MIPS32_OP_ORI);
wire op_xori   =  ~| (inst[31:26] ^ `MIPS32_OP_XORI);
wire op_jr     = (~| (inst[ 5: 0] ^ `MIPS32_OP_JR))    & op_Rtype;
wire op_sll    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SLL))   & op_Rtype;
wire op_srl    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SRL))   & op_Rtype;
wire op_sra    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SRA))   & op_Rtype;
wire op_sllv   = (~| (inst[ 5: 0] ^ `MIPS32_OP_SLLV))  & op_Rtype;
wire op_srlv   = (~| (inst[ 5: 0] ^ `MIPS32_OP_SRLV))  & op_Rtype;
wire op_srav   = (~| (inst[ 5: 0] ^ `MIPS32_OP_SRAV))  & op_Rtype;
wire op_jalr   = (~| (inst[ 5: 0] ^ `MIPS32_OP_JALR))  & op_Rtype;
wire op_addu   = (~| (inst[ 5: 0] ^ `MIPS32_OP_ADDU))  & op_Rtype;
wire op_sub    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SUB))   & op_Rtype;
wire op_subu   = (~| (inst[ 5: 0] ^ `MIPS32_OP_SUBU))  & op_Rtype;
wire op_and    = (~| (inst[ 5: 0] ^ `MIPS32_OP_AND))   & op_Rtype;
wire op_or     = (~| (inst[ 5: 0] ^ `MIPS32_OP_OR))    & op_Rtype;
wire op_xor    = (~| (inst[ 5: 0] ^ `MIPS32_OP_XOR))   & op_Rtype;
wire op_nor    = (~| (inst[ 5: 0] ^ `MIPS32_OP_NOR))   & op_Rtype;
wire op_slt    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SLT))   & op_Rtype;
wire op_sltu   = (~| (inst[ 5: 0] ^ `MIPS32_OP_SLTU))  & op_Rtype;
wire op_movz   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MOVZ))  & op_Rtype;
wire op_movn   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MOVN))  & op_Rtype;
wire op_mult   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MULT))  & op_Rtype;
wire op_multu  = (~| (inst[ 5: 0] ^ `MIPS32_OP_MULTU)) & op_Rtype;
wire op_mfhi   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MFHI))  & op_Rtype;
wire op_mflo   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MFLO))  & op_Rtype;
wire op_mthi   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MTHI))  & op_Rtype;
wire op_mtlo   = (~| (inst[ 5: 0] ^ `MIPS32_OP_MTLO))  & op_Rtype;
wire op_div    = (~| (inst[ 5: 0] ^ `MIPS32_OP_DIV))   & op_Rtype;
wire op_divu   = (~| (inst[ 5: 0] ^ `MIPS32_OP_DIVU))  & op_Rtype;
wire op_sys    = (~| (inst[ 5: 0] ^ `MIPS32_OP_SYS ))  & op_Rtype;
wire op_eret   = (~| (inst[31:26] ^ `MIPS32_OP_COP0))  & (~| (inst[ 5: 0] ^ `MIPS32_OP_ERET));
wire op_mtc0   = (~| (inst[31:21] ^ `MIPS32_OP_MTC0));
wire op_mfc0   = (~| (inst[31:21] ^ `MIPS32_OP_MFC0));

wire branch    = op_blgz | op_beq | op_bne | op_blez | op_bgtz | op_bgezal;
wire load      = op_lb   | op_lbu | op_lh   | op_lhu | op_lwl | op_lwr | op_lw;
wire store     = op_sb   | op_sh  | op_swl | op_swr  | op_sw;

//control signal
assign RegDst[0] = op_Rtype | op_mtc0;
assign RegDst[1] = op_jal | op_bgezal;
assign RegWrite  = (op_Rtype & ~(op_jr | op_mult | op_multu | op_mthi | op_mtlo | op_div | op_divu)) 
                    | op_jal  | op_addiu | op_slti | op_sltiu
                    | op_andi | op_ori | op_xori | op_lui   | load    | op_mfhi
                    | op_mflo | op_sub | op_addi | (op_bgezal & inst[20]) | op_mfc0;

assign MemtoReg  = load;

assign alu_src1_sel[0] = op_jal | op_jalr | op_bgezal | op_mfc0;
assign alu_src1_sel[1] = op_sll | op_srl | op_sra | op_mfc0;
assign alu_src2_sel[0] = load   | store  | op_movz | op_movn | op_addiu | op_sltiu
                      | op_slti | op_ori | op_andi | op_xori | op_lui   | op_addi | op_mfc0;
assign alu_src2_sel[1] = op_jal | op_movn | op_movz | op_jalr | op_bgezal | op_mfc0;
//for zero extend, sign_ext = 0
assign sign_ext        = ~(op_ori | op_xori | op_andi);

assign Jump   = op_jr | op_jalr | op_jal | op_jmp;
assign Branch = branch;
assign MemWrite    = {4{store}};
assign MemRead     = load;
assign op_load[0]  = op_lb;
assign op_load[1]  = op_lbu;
assign op_load[2]  = op_lh;
assign op_load[3]  = op_lhu;
assign op_load[4]  = op_lwl;
assign op_load[5]  = op_lwr;
assign op_store[0] = op_sb;
assign op_store[1] = op_sh;
assign op_store[2] = op_swl;
assign op_store[3] = op_swr;

assign op_branch[0] = (op_blgz | op_bgezal) & ~inst[16]; //rdata1 <  0
assign op_branch[1] = (op_blgz | op_bgezal) &  inst[16]; //rdata1 >= 0
assign op_branch[2] = op_beq; //rdata1 == rdata2
assign op_branch[3] = op_bne; //rdata1 != rdata2
assign op_branch[4] = op_blez; //rdata1 <= 0

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
assign alu_control[9]  =  op_slt  | op_slti;
assign alu_control[10] =  op_sub  | op_subu | op_beq | op_bne | op_blez;
assign alu_control[11] = ~| alu_control[10:0];

assign div_signed = op_div;
assign div        = op_div | op_divu;
assign mul_signed = op_mult;
assign mul        = op_mult | op_multu;
assign HIWrite    = op_div | op_divu | op_mult | op_multu | op_mthi;
assign HIRead     = op_mfhi;
assign LOWrite    = op_div | op_divu | op_mult | op_multu | op_mtlo;
assign LORead     = op_mflo;

assign out_op_mtc0 = op_mtc0;
assign out_op_mfc0 = op_mfc0;
assign syscall     = op_sys;
assign eret_cmt    = op_eret;
endmodule