`include "myCPU_define.h"

module mycpu_top(
    input                clk,
    input                resetn,            //low active

    output               inst_sram_en,
    output      [ 3:0]   inst_sram_wen,
    output      [31:0]   inst_sram_addr,
    output      [31:0]   inst_sram_wdata,
    input       [31:0]   inst_sram_rdata,

    output               data_sram_en,
    output      [ 3:0]   data_sram_wen,
    output      [31:0]   data_sram_addr,
    output      [31:0]   data_sram_wdata,
    input       [31:0]   data_sram_rdata,

    //debug interface
    output      [31:0]   debug_wb_pc,
    output      [3 :0]   debug_wb_rf_wen,
    output      [4 :0]   debug_wb_rf_wnum,
    output      [31:0]   debug_wb_rf_wdata
);

//myCPU_alu
wire [31:0] alu_src1;
wire [31:0] alu_src2;
wire        Overflow;
wire        CarryOut;
wire        Zero;
wire [11:0] alu_op;
wire [31:0] alu_result;

//myCPU_rf
wire [ 4:0] raddr1;
wire [ 4:0] raddr2;
wire        wen;
wire [ 4:0] waddr;
wire [31:0] wdata;
wire [31:0] rdata1;
wire [31:0] rdata2;
wire [31:0] wdata_mid;

wire        RegWrite;
wire [ 1:0] RegDst;
wire        MemtoReg;
wire [ 1:0] alu_src1_sel;
wire [ 1:0] alu_src2_sel;
wire        sign_ext;
wire [11:0] alu_control;
wire        MemRead;
wire [ 3:0] MemWrite;
wire [ 5:0] op_load;
wire [ 3:0] op_store;
wire        Jump;
wire        Branch;
wire        Branch_cond;
wire [ 4:0] op_branch;
wire [ 1:0] PCsrc;
wire        div_signed;
wire        div;
wire        complete;
wire        mul_signed;
wire        mul;
wire        HIWrite;
wire        HIRead;
wire        LOWrite;
wire        LORead;
wire        syscall;

reg  [31:0] PC;
reg  [31:0] inst_regD;
reg  [31:0] HI;
reg  [31:0] LO;
wire [31:0] inst;
wire [63:0] mul_result; 
wire [63:0] div_result;

/* CP0_reg */
wire [31:0] cp0_status;
wire        cp0_status_bev;
reg  [ 7:0] cp0_status_im;
reg         cp0_status_exl;
reg         cp0_status_ie;

wire [31:0] cp0_cause;
reg         cp0_cause_bd;
reg         cp0_cause_ti;
reg  [ 7:0] cp0_cause_ip;
reg  [ 4:0] cp0_cause_exccode;

reg  [31:0] cp0_epc;
reg  [31:0] cp0_count;
reg  [31:0] cp0_compare;

wire [31:0] cp0_rdata;
wire        op_mtc0;
wire        op_mfc0;
wire        cp0_status_wen;
wire        cp0_cause_wen;
wire        cp0_epc_wen;
wire        cp0_count_wen;
wire        cp0_compare_wen;
wire        exception_cmt;
wire        eret_cmt;
reg         exception_cmt_i;

/* pipeline */
reg         IF_valid;
reg         ID_valid;
reg         EX_valid;
reg         MA_valid;
reg         WB_valid;
wire        validin;
wire        IF_allowin;
wire        IF_ready_go;
wire        IF_to_ID_valid;
wire        ID_allowin;
wire        ID_ready_go;
wire        ID_to_EX_valid;
wire        EX_allowin;
wire        EX_ready_go;
wire        EX_to_MA_valid;
wire        MA_allowin;
wire        MA_ready_go;
wire        MA_to_WB_valid;
wire        WB_allowin;
wire        WB_ready_go;
wire        out_allow;

//IF
wire [31:0] PC_add_src1;
wire [31:0] PC_add_src2;
wire [31:0] PC_add_result;
reg  [31:0] PC_ID;
reg  [31:0] NPC;
reg         inst_in_dt;
reg         inst_in_dtD;

//ID
reg         MemReadE;
reg  [ 3:0] MemWriteE;
reg  [ 5:0] op_loadE;
reg  [ 3:0] op_storeE;
reg         MemtoRegE;
reg         RegWriteE;
reg  [ 4:0] raddr1E;
reg  [ 4:0] raddr2E;
reg  [ 4:0] WaddrE;
reg  [31:0] alu_src1E;
reg  [31:0] alu_src2E;
reg  [11:0] alu_controlE;
reg  [31:0] PCE;
reg  [31:0] rdata1E;
reg  [31:0] rdata2E;
reg  [ 1:0] alu_bypass1;
reg  [ 1:0] alu_bypass2;
reg         div_signedE;
reg         divE;
reg         mul_signedE;
reg         mulE;
reg         HIWriteE;
reg         HIReadE;
reg         LOWriteE;
reg         LOReadE;
reg         cp0_readE;
reg  [31:0] cp0_rdataE;
reg         cp0_status_wenE;
reg         cp0_cause_wenE;
reg         cp0_epc_wenE;
reg         cp0_count_wenE;
reg         cp0_compare_wenE;
reg         inst_in_dtE;
reg         exceptionE;

wire [31:0] HI_rdataE;
wire [31:0] LO_rdataE;
reg         IF_allowinD;
wire        ID_EX_rlt1;
wire        ID_MA_rlt1;
wire        ID_WB_rlt1;
wire        ID_EX_rlt2;
wire        ID_MA_rlt2;
wire        ID_WB_rlt2;
wire        bran_EX_rlt1;
wire        bran_MA_rlt1;
wire        bran_WB_rlt1;
wire        bran_EX_rlt2;
wire        bran_MA_rlt2;
wire        bran_WB_rlt2;
wire        data_rtl_stall;
wire        div_stall;
wire        ID_stall;
wire        bran_rtl_stall;

//branch
wire [32:0] zero_cond;
wire [31:0] bran_add1;
wire [31:0] bran_add2;

//EX
reg         MemReadM;
reg  [ 3:0] MemWriteM;
reg  [ 5:0] op_loadM;
reg  [ 3:0] op_storeM;
reg         MemtoRegM;
reg         RegWriteM;
reg  [ 4:0] WaddrM;
reg  [31:0] ResM;
reg  [31:0] rdata2M;
reg  [31:0] PCM;
reg         divM;
reg         mulM;
reg         HIWriteM;
reg         LOWriteM;
reg  [31:0] HI_wdataM;
reg  [31:0] HI_rdataM;
reg  [31:0] LO_wdataM;
reg  [31:0] LO_rdataM;
reg         cp0_status_wenM;
reg         cp0_cause_wenM;
reg         cp0_epc_wenM;
reg         cp0_count_wenM;
reg         cp0_compare_wenM;
reg         inst_in_dtM;
reg         exceptionM;
wire [31:0] sb_data;
wire [31:0] sh_data;
wire [31:0] swl_data;
wire [31:0] swr_data;

//MA
reg  [ 5:0] op_loadW;
reg         MemtoRegW;
reg  [31:0] ResW;
reg  [31:0] rdata2W;
reg  [ 4:0] WaddrW;
reg         RegWriteW;
reg         HIWriteW;
reg         LOWriteW;
reg  [31:0] HI_wdataW;
reg  [31:0] LO_wdataW;
reg  [31:0] PCW;
reg         cp0_status_wenW;
reg         cp0_cause_wenW;
reg         cp0_epc_wenW;
reg         cp0_count_wenW;
reg         cp0_compare_wenW;
wire [31:0] lb_data;
wire [31:0] lbu_data;
wire [31:0] lh_data;
wire [31:0] lhu_data;
wire [31:0] lwl_data;
wire [31:0] lwr_data;

/**************** cp0 *****************/
assign cp0_status = { 9'd0,
                      cp0_status_bev, //22
                      6'd0,
                      cp0_status_im,  //15:8
                      6'd0,
                      cp0_status_exl, //1
                      cp0_status_ie   //0
                    };
assign cp0_status_bev = 1'b1;
always@(posedge clk)
begin
    if(cp0_status_wenW)
        cp0_status_im <= rdata2W[15:8];
    
    if(~resetn)
        cp0_status_ie <= 1'b0;
    else if(cp0_status_wenW)
        cp0_status_ie <= rdata2W[0];
end
always@(posedge clk)
begin
    if(~resetn)
        cp0_status_exl <= 1'b0;
    else if(cp0_status_wenW)
        cp0_status_exl <= rdata2W[1];
    else if(exception_cmt)
        cp0_status_exl <= 1'b1;
    else if(eret_cmt)
        cp0_status_exl <= 1'b0;
end

assign cp0_cause = { cp0_cause_bd, //32
                     cp0_cause_ti, //31
                     14'd0,
                     cp0_cause_ip, //15:8
                     1'b0,
                     cp0_cause_exccode, //6:2
                     2'd0
                    };
always@(posedge clk)
begin
    if(~resetn)
        cp0_cause_bd <= 1'b0;
    else if(exception_cmt & inst_in_dtM)
        cp0_cause_bd <= 1'b1;
end
always@(posedge clk)
begin
    if(~resetn)
        cp0_cause_ti <= 1'b0;
    else if(cp0_count == cp0_compare)
        cp0_cause_ti <= 1'b1;
    else if(cp0_cause_wenW)
        cp0_cause_ti <= 1'b0;
end
always@(posedge clk)
begin
    if(~resetn)
        cp0_cause_ip <= 8'd0;
    else if(cp0_cause_wenW)
        cp0_cause_ip[1:0] <= rdata2W[9:8];
end 
always@(posedge clk)
begin
    if(exceptionM)
        cp0_cause_exccode <= 5'd8;
end
assign exception_cmt = exceptionM;

wire [31:0] now_inst_brpc;
assign now_inst_brpc = inst_in_dtM ? PCM - 3'd4
                                   : PCM;
always@(posedge clk)
begin
    if(~resetn)
        cp0_epc <= 32'd0;
    else if(exception_cmt && cp0_status_exl != 1'b1)
        cp0_epc <= now_inst_brpc;
    else if(cp0_epc_wenW)
        cp0_epc <= rdata2W;
end
always@(posedge clk)
begin
    exception_cmt_i <= exception_cmt;
end
/***************  IF  ********************/
assign validin        = resetn;
assign IF_ready_go    = ~ID_stall;//1'b1;
assign IF_allowin     = (~IF_valid | (IF_ready_go & ID_allowin));
assign IF_to_ID_valid = IF_valid & IF_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        IF_valid <= 1'b0;
    else if(IF_allowin)
        IF_valid <= validin;
end

assign PC_add_src1     = PC;
assign PC_add_src2     = (PCsrc[1] & PCsrc[0])? {{14{inst[15]}}, inst[15:0], 2'b00} : 32'd4;
assign PC_add_result   = PC_add_src1 + PC_add_src2;
assign inst_sram_addr  = PC;
assign inst_sram_en    = validin;
assign inst_sram_wdata = 32'd0;
assign inst_sram_wen   = 4'd0;
assign inst            = exception_cmt_i ? 32'd0 :
                         IF_allowinD   ? inst_sram_rdata :
                                         inst_regD;
always@(posedge clk)
begin
    if(validin & IF_allowin)
    begin
        PC_ID       <= PC;
        inst_in_dtD <= inst_in_dt;
    end
end

wire [31:0] cp0_epc_t;
assign cp0_epc_t = cp0_epc_wenE ? rdata2E :
                   cp0_epc_wenM ? rdata2M :
                   cp0_epc_wenW ? rdata2W :
                                  cp0_epc;
always@(posedge clk or negedge resetn)
begin
    if(~resetn)
    begin
        PC <= `myCPU_INIT_INST_ADDR;
        inst_in_dt <= 1'b0;
    end
    else if(validin & IF_allowin)
    begin
        inst_in_dt <= Jump | (Branch & Branch_cond);
        if(exception_cmt)
            PC <= 32'hbfc00380;
        else if(eret_cmt)
            PC <= cp0_epc_t;
        else if(Jump | (Branch & Branch_cond))
        begin
            casex(PCsrc)
            2'b01:
                PC <= {bran_add1[31:1], 1'b0};
            2'b10:
                PC <= {PC[31:28], inst[25:0], 2'b00};
            2'b11:
                PC <= PC + {{14{inst[15]}}, inst[15:0], 2'b00};
            default:
                PC <= PC + 32'd4;
            endcase
        end else begin
            PC <= PC + 32'd4;
        end
    end
end

/***************  ID  ********************/
assign ID_ready_go    = 1'b1;
assign ID_allowin     = (~ID_valid | (ID_ready_go & EX_allowin));
assign ID_to_EX_valid = ID_valid & ID_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        ID_valid <= 1'b0;
    else if(ID_allowin)
        ID_valid <= IF_to_ID_valid & ~eret_cmt & ~exception_cmt;
end
always@(posedge clk)
begin
    if(~resetn)
        IF_allowinD <= 1'b1;
    else
        IF_allowinD <= IF_allowin;
end

assign ID_EX_rlt1 = RegWriteE && alu_src1_sel == 2'b00 && raddr1 == WaddrE && raddr1 && PC_ID != PCE;
assign ID_MA_rlt1 = RegWriteM && alu_src1_sel == 2'b00 && raddr1 == WaddrM && raddr1 && PC_ID != PCM;
assign ID_WB_rlt1 = RegWriteW && alu_src1_sel == 2'b00 && raddr1 == WaddrW && raddr1 && PC_ID != PCW;
assign ID_EX_rlt2 = RegWriteE && alu_src2_sel == 2'b00 && raddr2 == WaddrE && raddr2 && PC_ID != PCE;
assign ID_MA_rlt2 = RegWriteM && alu_src2_sel == 2'b00 && raddr2 == WaddrM && raddr2 && PC_ID != PCM;
assign ID_WB_rlt2 = RegWriteW && alu_src2_sel == 2'b00 && raddr2 == WaddrW && raddr2 && PC_ID != PCW;

assign bran_EX_rlt1 = RegWriteE && (~|alu_src1_sel | Branch | inst[5:1]==5'b00100) && raddr1 == WaddrE && raddr1 && PC_ID != PCE;
assign bran_MA_rlt1 = RegWriteM && (~|alu_src1_sel | Branch | inst[5:1]==5'b00100) && raddr1 == WaddrM && raddr1 && PC_ID != PCM;
assign bran_WB_rlt1 = RegWriteW && (~|alu_src1_sel | Branch | inst[5:1]==5'b00100) && raddr1 == WaddrW && raddr1 && PC_ID != PCW;
assign bran_EX_rlt2 = RegWriteE && (~|alu_src2_sel | (Branch & |op_branch[3:2])) && raddr2 == WaddrE && raddr2 && PC_ID != PCE;
assign bran_MA_rlt2 = RegWriteM && (~|alu_src2_sel | (Branch & |op_branch[3:2])) && raddr2 == WaddrM && raddr2 && PC_ID != PCM;
assign bran_WB_rlt2 = RegWriteW && (~|alu_src2_sel | (Branch & |op_branch[3:2])) && raddr2 == WaddrW && raddr2 && PC_ID != PCW;

assign data_rtl_stall = (ID_EX_rlt1 | ID_EX_rlt2) & MemReadE;
assign bran_rtl_stall = (bran_MA_rlt1 | bran_MA_rlt2) & MemReadM & (Branch | inst[5:1]==5'b00100);
assign div_stall      = divE & ~complete;
assign ID_stall       = data_rtl_stall | bran_rtl_stall;

assign cp0_count_wen      = op_mtc0 & (inst[15:11] == 4'd9);
assign cp0_compare_wen    = op_mtc0 & (inst[15:11] == 4'd11);
assign cp0_status_wen     = op_mtc0 & (inst[15:11] == 4'd12);
assign cp0_cause_wen      = op_mtc0 & (inst[15:11] == 4'd13);
assign cp0_epc_wen        = op_mtc0 & (inst[15:11] == 4'd14);
assign cp0_rdata = inst[15:11] == 4'd9  ? cp0_count   :
                   inst[15:11] == 4'd11 ? cp0_compare :
                   inst[15:11] == 4'd12 ? cp0_status  :
                   inst[15:11] == 4'd13 ? cp0_cause   :
                   inst[15:11] == 4'd14 ? cp0_epc     :
                                     32'd0;

always@(posedge clk)
begin
    if(~resetn)
        inst_regD <= 32'd0;
    else if(IF_allowinD)
        inst_regD <= inst_sram_rdata;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(ID_EX_rlt1)
            alu_bypass1 <= 2'b01;
        else if(ID_MA_rlt1)
            alu_bypass1 <= 2'b10;
        else
            alu_bypass1 <= 2'b00;
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(ID_EX_rlt2)
            alu_bypass2 <= 2'b01;
        else if(ID_MA_rlt2)
            alu_bypass2 <= 2'b10;
        else
            alu_bypass2 <= 2'b00;
    end
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid)
        MemReadE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        MemReadE <= MemRead;
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid || exception_cmt)
        MemWriteE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        MemWriteE <= MemWrite;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin) begin
        op_loadE  <= op_load;
        op_storeE <= op_store;
    end
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid)
        MemtoRegE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        MemtoRegE <= MemtoReg;
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid || exception_cmt)
        RegWriteE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        RegWriteE <= RegWrite;
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid)
        raddr2E <= 5'd0;
    else if(IF_to_ID_valid & ID_allowin)
        raddr2E <= raddr2;
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid)
        raddr1E <= 5'd0;
    else if(IF_to_ID_valid & ID_allowin)
        raddr1E <= raddr1;
end
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid || exception_cmt)
        WaddrE <= 5'd0;
    else if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegDst[1])
            WaddrE <= 5'd31;
        else if(RegDst[0])
            WaddrE <= inst[15:11];
        else
            WaddrE <= inst[20:16];
    end
end

assign HI_rdataE = HIWriteW ? HI_wdataW : HI;
assign LO_rdataE = LOWriteW ? LO_wdataW : LO;
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(alu_src1_sel[1] & alu_src1_sel[0])
            alu_src1E <= cp0_rdata;
        else if(alu_src1_sel[1] & ~alu_src1_sel[0])
            alu_src1E <= {27'd0, inst[10:6]};
        else if(~alu_src1_sel[1] & alu_src1_sel[0])
            alu_src1E <= PC_ID;
        else
        begin
            if(HIRead)
                alu_src1E <= HI_rdataE;
            else if(LORead)
                alu_src1E <= LO_rdataE;
            else if(ID_WB_rlt1)
                alu_src1E <= wdata_mid;
            else
                alu_src1E <= rdata1;
        end
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(alu_src2_sel[1] & alu_src2_sel[0])
            alu_src2E <= 32'd0;
        else if(alu_src2_sel[1] & ~alu_src2_sel[0])
            alu_src2E <= 32'd8;
        else if(~alu_src2_sel[1] & alu_src2_sel[0])
            alu_src2E <= {{16{sign_ext & inst[15]}}, inst[15:0]};
        else
        begin
            if(ID_WB_rlt2)
                alu_src2E <= wdata_mid;
            else
                alu_src2E <= rdata2;
        end
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
        alu_controlE <= alu_control;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        PCE         <= PC_ID;
        inst_in_dtE <= inst_in_dtD;
    end
end
always@(posedge clk)
begin
    if(~resetn)
        exceptionE <= 1'b0;
    if(IF_to_ID_valid & ID_allowin)
        exceptionE <= syscall;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegWriteW && raddr1 == WaddrW && raddr1 && PC_ID != PCW)
            rdata1E <= wdata_mid;
        else
            rdata1E <= rdata1;
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegWriteW && raddr2 == WaddrW && raddr2 && PC_ID != PCW)
            rdata2E <= wdata_mid;
        else
            rdata2E <= rdata2;
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        cp0_readE        <= op_mfc0;
        cp0_rdataE       <= cp0_rdata;
        cp0_status_wenE  <= cp0_status_wen;
        cp0_cause_wenE   <= cp0_cause_wen;
        cp0_epc_wenE     <= cp0_epc_wen;
        cp0_count_wenE   <= cp0_count_wen;
        cp0_compare_wenE <= cp0_compare_wen;
    end
end
//mul && div
always@(posedge clk)
begin
    if(~resetn || ~IF_to_ID_valid || exception_cmt)
    begin
        div_signedE <= 1'b0;
        divE        <= 1'b0;
        mul_signedE <= 1'b0;
        mulE        <= 1'b0;
        HIWriteE    <= 1'b0;
        HIReadE     <= 1'b0;
        LOWriteE    <= 1'b0;
        LOReadE     <= 1'b0;
    end
    if(IF_to_ID_valid & ID_allowin)
    begin
        div_signedE <= div_signed;
        divE        <= div;
        mul_signedE <= mul_signed;
        mulE        <= mul;
        HIWriteE    <= HIWrite;
        HIReadE     <= HIRead;
        LOWriteE    <= LOWrite;
        LOReadE     <= LORead;
    end
end

inst_decode myCPU_ID(.inst            (inst           ),
                     .RegDst          (RegDst         ),
                     .RegWrite        (RegWrite       ),
                     .MemtoReg        (MemtoReg       ),
                     .alu_src1_sel    (alu_src1_sel   ),
                     .alu_src2_sel    (alu_src2_sel   ),
                     .sign_ext        (sign_ext       ),
                     .alu_control     (alu_control    ),
                     .MemRead         (MemRead        ),
                     .MemWrite        (MemWrite       ),
                     .op_load         (op_load        ),
                     .op_store        (op_store       ),
                     .Jump            (Jump           ),
                     .Branch          (Branch         ),
                     .op_branch       (op_branch      ),
                     .PCsrc           (PCsrc          ),
                     .div_signed      (div_signed     ),
                     .div             (div            ),
                     .mul_signed      (mul_signed     ),
                     .mul             (mul            ),
                     .HIWrite         (HIWrite        ),
                     .HIRead          (HIRead         ),
                     .LOWrite         (LOWrite        ),
                     .LORead          (LORead         ),
                     .out_op_mtc0     (op_mtc0        ),
                     .out_op_mfc0     (op_mfc0        ),
                     .syscall         (syscall        ),
                     .eret_cmt        (eret_cmt       )
);

assign bran_add1   = (bran_EX_rlt1)? alu_result :
                     (bran_MA_rlt1)? ResM       :
                     (bran_WB_rlt1)? wdata_mid  :
                                     rdata1     ;
assign bran_add2   = (bran_EX_rlt2)? alu_result :
                     (bran_MA_rlt2)? ResM       :
                     (bran_WB_rlt2)? wdata_mid  :
                                     rdata2     ;
assign zero_cond   = bran_add1 + ~bran_add2 + 1'b1;
assign Branch_cond = (op_branch[0] & bran_add1[31])
                   | (op_branch[1] & ~bran_add1[31])
                   | (op_branch[2] & ~|zero_cond[31:0])
                   | (op_branch[3] &  |zero_cond[31:0])
                   | (op_branch[4] & (bran_add1[31] | ~|bran_add1))
                   | (~|op_branch  & ~bran_add1[31] & |bran_add1);

/***************  EX  ********************/
assign EX_ready_go    = ~div_stall;//1'b1;
assign EX_allowin     = ~EX_valid | (EX_ready_go & MA_allowin);
assign EX_to_MA_valid = EX_valid & EX_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        EX_valid <= 1'b0;
    else if(ID_allowin)
        EX_valid <= ID_to_EX_valid & ~eret_cmt & ~exception_cmt;
end

always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid)
        MemReadM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        MemReadM <= MemReadE;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin) begin
        op_loadM  <= op_loadE;
        op_storeM <= op_storeE;
    end
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid || exception_cmt)
        MemWriteM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        MemWriteM <= MemWriteE;
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid)
        MemtoRegM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        MemtoRegM <= MemtoRegE;
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid || exception_cmt)
        RegWriteM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        RegWriteM <= RegWriteE;
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid || exception_cmt)
        WaddrM <= 5'd0;
    else if(ID_to_EX_valid & EX_allowin)
        WaddrM <= WaddrE;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
        ResM <= alu_result;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
    begin
        if(RegWriteM && raddr2E == WaddrM && raddr2E)
            rdata2M <= ResM;
        else if(RegWriteW && raddr2E == WaddrW && raddr2E)
            rdata2M <= wdata_mid;
        else
            rdata2M <= rdata2E;
    end
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
    begin
        PCM         <= PCE;
        inst_in_dtM <= inst_in_dtE;
    end
end
always@(posedge clk)
begin
    if(~resetn)
        exceptionM <= 1'b0;
    if(ID_to_EX_valid & EX_allowin)
        exceptionM <= exceptionE;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
    begin
        if(HIWriteE && divE && complete)
            HI_wdataM <= div_result[31:0];
        else if(HIWriteE)
        begin
            if(RegWriteM && raddr1E == WaddrM && raddr1E)
                HI_wdataM <= ResM;
            else if(RegWriteW && raddr1E == WaddrW && raddr1E)
                HI_wdataM <= wdata_mid;
            else
                HI_wdataM <= rdata1E;
        end
    end
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
    begin
        if(LOWriteE && divE && complete)
            LO_wdataM <= div_result[63:32];
        else if(LOWriteE)
        begin
            if(RegWriteM && raddr1E == WaddrM && raddr1E)
                LO_wdataM <= ResM;
            else if(RegWriteW && raddr1E == WaddrW && raddr1E)
                LO_wdataM <= wdata_mid;
            else
                LO_wdataM <= rdata1E;
        end
    end
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid || exception_cmt)
    begin
        cp0_status_wenM  <= 1'b0;
        cp0_cause_wenM   <= 1'b0;
        cp0_epc_wenM     <= 1'b0;
        cp0_count_wenM   <= 1'b0;
        cp0_compare_wenM <= 1'b0;
    end
    if(ID_to_EX_valid & EX_allowin)
    begin
        cp0_status_wenM  <= cp0_status_wenE;
        cp0_cause_wenM   <= cp0_cause_wenE;
        cp0_epc_wenM     <= cp0_epc_wenE;
        cp0_count_wenM   <= cp0_count_wenE;
        cp0_compare_wenM <= cp0_compare_wenE;
    end
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid || exception_cmt)
    begin
        divM        <= 1'b0;
        mulM        <= 1'b0;
        HIWriteM    <= 1'b0;
        LOWriteM    <= 1'b0;
    end
    if(ID_to_EX_valid & EX_allowin)
    begin
        divM        <= divE;
        mulM        <= mulE;
        HIWriteM    <= HIWriteE;
        LOWriteM    <= LOWriteE;
    end
end

assign alu_src1 = (alu_bypass1 == 2'b01)?        ResM            :
                  (alu_bypass1 == 2'b10)?        wdata_mid       :
                  (HIReadE & HIWriteM & mulM)? mul_result[63:32]  :
                  (HIReadE & HIWriteM)?        HI_wdataM         :
                  (HIReadE & HIWriteW)?        HI_wdataW         :
                  (LOReadE & LOWriteM & mulM)? mul_result[31:0] :
                  (LOReadE & LOWriteM)?        LO_wdataM         :
                  (LOReadE & LOWriteW)?        LO_wdataW         :
                                               alu_src1E         ;
assign alu_src2 = (alu_bypass2 == 2'b01)? ResM      :
                  (alu_bypass2 == 2'b10)? wdata_mid :
                                          alu_src2E ;
assign alu_op   = alu_controlE;
alu myCPU_alu(
    .alu_control (alu_op    ), 
    .alu_src1    (alu_src1  ),
    .alu_src2    (alu_src2  ),
    .alu_result  (alu_result),
    .Overflow    (Overflow  ),
    .CarryOut    (CarryOut  ),
    .Zero        (Zero      )
);

wire [31:0] mul_x;
wire [31:0] mul_y;
wire        mul_sign;
assign mul_x    = alu_src1;
assign mul_y    = alu_src2;
assign mul_sign = mul_signedE;
mul u_mul(
    .mul_clk    (clk        ),
    .resetn     (resetn     ),
    .mul_signed (mul_sign   ),
    .x          (mul_x      ),
    .y          (mul_y      ),
    .result     (mul_result )
);

wire [31:0] div_x;
wire [31:0] div_y;
wire        div_en;
wire        div_sign;
assign div_x    = alu_src1;
assign div_y    = alu_src2;
assign div_en   = divE;
assign div_sign = div_signedE;

div u_div(
    .div_clk    (clk              ),
    .resetn     (resetn           ),
    .div        (div_en           ),
    .div_signed (div_sign         ),
    .x          (div_x            ),
    .y          (div_y            ),
    .s          (div_result[63:32]),
    .r          (div_result[31:0] ),
    .complete   (complete         )
);

/***************  MA  ********************/
assign MA_ready_go    = 1'b1;
assign MA_allowin     = ~MA_valid | (MA_ready_go & WB_allowin);
assign MA_to_WB_valid = MA_valid & MA_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        MA_valid <= 1'b0;
    else if(EX_allowin)
        MA_valid <= EX_to_MA_valid  & ~eret_cmt & ~exception_cmt;
end

always@(posedge clk)
begin
    if(~resetn || ~EX_to_MA_valid)
        WaddrW <= 5'd0;
    else if(EX_to_MA_valid & MA_allowin)
        WaddrW <= WaddrM;
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
        op_loadW <= op_loadM;
end
always@(posedge clk)
begin
    if(~resetn || ~EX_to_MA_valid)
        RegWriteW <= 1'b0;
    else if(EX_to_MA_valid & MA_allowin)
        RegWriteW <= RegWriteM;
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        MemtoRegW <= MemtoRegM;
    end
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        ResW <= ResM;
    end
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
        PCW <= PCM;
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        if(HIWriteM && mulM)
            HI_wdataW <= mul_result[63:32];
        else
            HI_wdataW <= HI_wdataM;
    end
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        if(LOWriteM && mulM)
            LO_wdataW <= mul_result[31:0];
        else
            LO_wdataW <= LO_wdataM;
    end
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        cp0_status_wenW  <= cp0_status_wenM;
        cp0_cause_wenW   <= cp0_cause_wenM;
        cp0_epc_wenW     <= cp0_epc_wenM;
        cp0_count_wenW   <= cp0_count_wenM;
        cp0_compare_wenW <= cp0_compare_wenM;
    end
end
always@(posedge clk)
begin
    if(EX_to_MA_valid & MA_allowin)
    begin
        HIWriteW    <= HIWriteM;
        LOWriteW    <= LOWriteM;
    end
end

assign sb_data  = ResM[1:0] == 2'b00 ? {24'd0, rdata2M[7:0]}       :
                  ResM[1:0] == 2'b01 ? {16'd0, rdata2M[7:0], 8'd0} :
                  ResM[1:0] == 2'b10 ? {8'd0, rdata2M[7:0], 16'd0} :
                                       {rdata2M[7:0], 24'd0}       ;
assign sh_data  = ResM[1]            ? {rdata2M[15:0], 16'd0}      :
                                       {16'd0, rdata2M[15:0]}      ;
assign swl_data = ResM[1:0] == 2'b00 ? {24'd0, rdata2M[31:24]}     :
                  ResM[1:0] == 2'b01 ? {16'd0, rdata2M[31:16]}     :
                  ResM[1:0] == 2'b10 ? { 8'd0, rdata2M[31:8]}      :
                                       rdata2M[31:0]               ;
assign swr_data = ResM[1:0] == 2'b00 ? rdata2M[31:0]               :
                  ResM[1:0] == 2'b01 ? {rdata2M[23:0],  8'd0}      :
                  ResM[1:0] == 2'b10 ? {rdata2M[15:0], 16'd0}      :
                                       {rdata2M[7:0],  24'd0}      ;


assign data_sram_addr  = ResM;
assign data_sram_en    = MemReadM | (|MemWriteM);
assign data_sram_wen   = op_storeM[0] ? {ResM[1]&ResM[0], ResM[1]&~ResM[0], ~ResM[1]&ResM[0], ~ResM[1]&~ResM[0]} :
                         op_storeM[1] ? {ResM[1], ResM[1], ~ResM[1], ~ResM[1]}                                   :
                         op_storeM[2] ? {ResM[1]&ResM[0], ResM[1], ResM[1]|ResM[0], 1'b1}                        :
                         op_storeM[3] ? {1'b1, ~(ResM[1]&ResM[0]), ~ResM[1], ~(ResM[1]|ResM[0])}                 :
                                        MemWriteM                                                                ;
assign data_sram_wdata = op_storeM[0] ? sb_data  :
                         op_storeM[1] ? sh_data  :
                         op_storeM[2] ? swl_data :
                         op_storeM[3] ? swr_data :
                                        rdata2M  ;

/***************  WB  ********************/
assign WB_ready_go  = 1'b1;
assign WB_allowin   = ~WB_valid | (WB_ready_go & out_allow);
assign out_allow    = 1'b1;
always@(posedge clk)
begin
    if(~resetn)
        WB_valid <= 1'b0;
    else if(MA_allowin)
        WB_valid <= MA_to_WB_valid;
end

always@(posedge clk)
begin
    if(WB_allowin & HIWriteW)
        HI <= HI_wdataW;
end
always@(posedge clk)
begin
    if(MA_to_WB_valid & WB_allowin)
        rdata2W <= rdata2M;
end
always@(posedge clk)
begin
    if(WB_allowin & LOWriteW)
        LO <= LO_wdataW;
end

assign lb_data  = ResW[1:0] == 2'b00 ? {{24{data_sram_rdata[ 7]}}, data_sram_rdata[ 7: 0]} :
                  ResW[1:0] == 2'b01 ? {{24{data_sram_rdata[15]}}, data_sram_rdata[15: 8]} :
                  ResW[1:0] == 2'b10 ? {{24{data_sram_rdata[23]}}, data_sram_rdata[23:16]} :
                                       {{24{data_sram_rdata[31]}}, data_sram_rdata[31:24]} ;
assign lbu_data = ResW[1:0] == 2'b00 ? {24'd0, data_sram_rdata[ 7: 0]} :
                  ResW[1:0] == 2'b01 ? {24'd0, data_sram_rdata[15: 8]} :
                  ResW[1:0] == 2'b10 ? {24'd0, data_sram_rdata[23:16]} :
                                       {24'd0, data_sram_rdata[31:24]} ;
assign lh_data  = ResW[1]            ? {{16{data_sram_rdata[31]}}, data_sram_rdata[31:16]} :
                                       {{16{data_sram_rdata[15]}}, data_sram_rdata[15: 0]} ;
assign lhu_data = ResW[1]            ? {16'd0, data_sram_rdata[31:16]} :
                                       {16'd0, data_sram_rdata[15: 0]} ;
assign lwl_data = ResW[1:0] == 2'b00 ? {data_sram_rdata[ 7: 0], rdata2W[23: 0]} :
                  ResW[1:0] == 2'b01 ? {data_sram_rdata[15: 0], rdata2W[15: 0]} :
                  ResW[1:0] == 2'b10 ? {data_sram_rdata[23: 0], rdata2W[ 7: 0]} :
                                       data_sram_rdata[31: 0]                   ;
assign lwr_data = ResW[1:0] == 2'b00 ? data_sram_rdata[31: 0]                   :
                  ResW[1:0] == 2'b01 ? {rdata2W[31:24], data_sram_rdata[31: 8]} :
                  ResW[1:0] == 2'b10 ? {rdata2W[31:16], data_sram_rdata[31:16]} :
                                       {rdata2W[31: 8], data_sram_rdata[31:24]} ;

assign wdata_mid = (MemtoRegW == 1'b1)? ({32{op_loadW[0]}} &  lb_data) 
                                      | ({32{op_loadW[1]}} & lbu_data) 
                                      | ({32{op_loadW[2]}} &  lh_data) 
                                      | ({32{op_loadW[3]}} & lhu_data) 
                                      | ({32{op_loadW[4]}} & lwl_data) 
                                      | ({32{op_loadW[5]}} & lwr_data) 
                                      | ({32{~| op_loadW}} & data_sram_rdata) 
                                      : ResW;
assign raddr1    = inst[25:21];
assign raddr2    = inst[20:16];
assign wen       = RegWriteW;
assign waddr     = WaddrW;
assign wdata     = wdata_mid;

regfile myCPU_rf(.clk(clk),
                 .raddr1(raddr1),
                 .rdata1(rdata1),
                 .raddr2(raddr2),
                 .rdata2(rdata2),
                 .wen(wen),
                 .waddr(waddr),
                 .wdata(wdata));
                 
assign debug_wb_pc       = PCW            ;
assign debug_wb_rf_wen   = {4{RegWriteW}} ;
assign debug_wb_rf_wnum  = WaddrW         ;
assign debug_wb_rf_wdata = wdata          ;

endmodule