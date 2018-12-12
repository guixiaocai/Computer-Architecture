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

wire        RegWrite;
wire [ 1:0] RegDst;
wire        MemtoReg;
wire [ 1:0] alu_src1_sel;
wire [ 1:0] alu_src2_sel;
wire [11:0] alu_control;
wire        MemRead;
wire [ 3:0] MemWrite;
wire        Jump;
wire        Branch;
wire        Branch_cond;
wire [ 4:0] op_branch;
wire [ 1:0] PCsrc;

reg  [31:0] PC;

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

//ID
reg         MemReadE;
reg  [ 3:0] MemWriteE;
reg         MemtoRegE;
reg         RegWriteE;
reg  [ 4:0] WaddrE;
reg  [31:0] alu_src1E;
reg  [31:0] alu_src2E;
reg  [11:0] alu_controlE;
reg  [31:0] PCE;
reg  [31:0] rdata2E;
reg  [ 1:0] alu_bypass1;
reg  [ 1:0] alu_bypass2;

//EX
reg         MemReadM;
reg  [ 3:0] MemWriteM;
reg         MemtoRegM;
reg         RegWriteM;
reg  [ 4:0] WaddrM;
reg  [31:0] ResM;
reg  [31:0] rdata2M;
reg  [31:0] PCM;

//MA
reg         MemtoRegW;
reg  [31:0] ResW;
reg  [ 4:0] WaddrW;
reg         RegWriteW;
reg  [31:0] PCW;

/***************  IF  ********************/
assign validin        = resetn;
assign IF_ready_go    = 1'b1;
assign IF_allowin     = ~IF_valid | (IF_ready_go & ID_allowin);
assign IF_to_ID_valid = IF_valid & IF_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        IF_valid <= 1'b0;
    else if(IF_allowin)
        IF_valid <= validin;
end

assign PC_add_src1     = PC;
assign PC_add_src2     = (PCsrc[1] & PCsrc[0])? {{14{inst_sram_rdata[15]}}, inst_sram_rdata[15:0], 2'b00} : 32'd4;
assign PC_add_result   = PC_add_src1 + PC_add_src2;
assign inst_sram_addr  = PC;
assign inst_sram_en    = validin;
assign inst_sram_wdata = 32'd0;
assign inst_sram_wen   = 4'd0;
always@(posedge clk)
begin
    if(validin & IF_allowin)
        PC_ID <= PC;
end

always@(posedge clk or negedge resetn)
begin
    if(~resetn)
        PC <= `myCPU_INIT_INST_ADDR;
    else if(validin & IF_allowin)
    begin
        if(Jump | (Branch & Branch_cond))
            casex(PCsrc)
            2'b01:
                PC <= {rdata1[31:1], 1'b0};
            2'b10:
                PC <= {PC[31:28], inst_sram_rdata[25:0], 2'b00};
            2'b11:
                PC <= PC + {{14{inst_sram_rdata[15]}}, inst_sram_rdata[15:0], 2'b00};
            default:
                PC <= PC + 32'd4;
            endcase
        else
            PC <= PC + 32'd4;
    end
end

/***************  ID  ********************/
assign ID_ready_go    = 1'b1;
assign ID_allowin     = ~ID_valid | (ID_ready_go & EX_allowin);
assign ID_to_EX_valid = ID_valid & ID_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        ID_valid <= 1'b0;
    else if(ID_allowin)
        ID_valid <= IF_to_ID_valid;
end

always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegWriteE && alu_src1_sel == 2'b00 && raddr1 == WaddrE)
            alu_bypass1 <= 2'b01;
        else if(RegWriteM && alu_src1_sel == 2'b00 && raddr1 == WaddrM)
            alu_bypass1 <= 2'b10;
        else
            alu_bypass1 <= 2'b00;
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegWriteE && alu_src2_sel == 2'b00 && raddr2 == WaddrE)
            alu_bypass2 <= 2'b01;
        else if(RegWriteM && alu_src2_sel == 2'b00 && raddr2 == WaddrM)
            alu_bypass2 <= 2'b10;
        else
            alu_bypass2 <= 2'b00;
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
        MemReadE <= MemRead;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
        MemWriteE <= MemWrite;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
        MemtoRegE <= MemtoReg;
end
always@(posedge clk)
begin
    if(~resetn)
        RegWriteE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        RegWriteE <= RegWrite;
end
always@(posedge clk)
begin
    if(~resetn)
        WaddrE <= 5'd0;
    else if(IF_to_ID_valid & ID_allowin)
    begin
        if(RegDst[1])
            WaddrE <= 5'd31;
        else if(RegDst[0])
            WaddrE <= inst_sram_rdata[15:11];
        else
            WaddrE <= inst_sram_rdata[20:16];
    end
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
    begin
        if(alu_src1_sel[1])
            alu_src1E <= {27'd0, inst_sram_rdata[10:6]};
        else if(alu_src1_sel[0])
            alu_src1E <= PC_ID;
        else
        begin
            if(RegWriteW && raddr1 == WaddrW)
                alu_src1E <= ResW;
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
            alu_src2E <= {{16{inst_sram_rdata[15]}}, inst_sram_rdata[15:0]};
        else
        begin
            if(RegWriteW && raddr2 == WaddrW)
                alu_src2E <= ResW;
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
        PCE <= PC_ID;
end
always@(posedge clk)
begin
    if(IF_to_ID_valid & ID_allowin)
        rdata2E <= rdata2;
end
inst_decode myCPU_ID(.inst_sram_rdata(inst_sram_rdata),
                     .RegDst(RegDst),
                     .RegWrite(RegWrite),
                     .MemtoReg(MemtoReg),
                     .alu_src1_sel(alu_src1_sel),
                     .alu_src2_sel(alu_src2_sel),
                     .alu_control(alu_control),
                     .MemRead(MemRead),
                     .MemWrite(MemWrite),
                     .Jump(Jump),
                     .Branch(Branch),
                     .op_branch(op_branch),
                     .PCsrc(PCsrc)
);

wire [32:0] zero_cond;
wire [31:0] bran_add1;
wire [31:0] bran_add2;
assign bran_add1   = (RegWriteE && raddr1 == WaddrE)? alu_result :
                     (RegWriteM && raddr1 == WaddrM)? ResM       :
                     (RegWriteW && raddr1 == WaddrW)? ResW       :
                                                      rdata1     ;
assign bran_add2   = (RegWriteE && raddr2 == WaddrE)? alu_result :
                     (RegWriteM && raddr2 == WaddrM)? ResM       :
                     (RegWriteW && raddr2 == WaddrW)? ResW       :
                                                      rdata2     ;
assign zero_cond   = bran_add1 + ~bran_add2 + 1'b1;
assign Branch_cond = (op_branch[0] & bran_add1[31])
                   | (op_branch[1] & ~bran_add1[31])
                   | (op_branch[2] & ~|zero_cond[31:0])
                   | (op_branch[3] &  |zero_cond[31:0])
                   | (op_branch[4] & (bran_add1[31] | ~|bran_add1));

/***************  EX  ********************/
assign EX_ready_go    = 1'b1;
assign EX_allowin     = ~EX_valid | (EX_ready_go & MA_allowin);
assign EX_to_MA_valid = EX_valid & EX_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        EX_valid <= 1'b0;
    else if(ID_allowin)
        EX_valid <= ID_to_EX_valid;
end

always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
        MemReadM <= MemReadE;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
        MemWriteM <= MemWriteE;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
        MemtoRegM <= MemtoRegE;
end
always@(posedge clk)
begin
    if(~resetn)
        RegWriteM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        RegWriteM <= RegWriteE;
end
always@(posedge clk)
begin
    if(~resetn)
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
        rdata2M <= rdata2E;
end
always@(posedge clk)
begin
    if(ID_to_EX_valid & EX_allowin)
        PCM <= PCE;
end
assign alu_src1 = (alu_bypass1 == 2'b01)? ResM     :
                  (alu_bypass1 == 2'b10)? ResW     :
                                          alu_src1E;
assign alu_src2 = (alu_bypass2 == 2'b01)? ResM     :
                  (alu_bypass2 == 2'b10)? ResW     :
                                          alu_src2E;
assign alu_op   = alu_controlE;
alu myCPU_alu(.alu_control(alu_op), 
              .alu_src1(alu_src1),
              .alu_src2(alu_src2),
              .alu_result(alu_result),
              .Overflow(Overflow),
              .CarryOut(CarryOut),
              .Zero(Zero));

/***************  MA  ********************/
assign MA_ready_go    = 1'b1;
assign MA_allowin     = ~MA_valid | (MA_ready_go & WB_allowin);
assign MA_to_WB_valid = MA_valid & MA_ready_go;
always@(posedge clk)
begin
    if(~resetn)
        MA_valid <= 1'b0;
    else if(EX_allowin)
        MA_valid <= EX_to_MA_valid;
end

always@(posedge clk)
begin
    if(~resetn)
        WaddrW <= 5'd0;
    else if(EX_to_MA_valid & MA_allowin)
        WaddrW <= WaddrM;
end
always@(posedge clk)
begin
    if(~resetn)
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
assign data_sram_addr  = ResM;
assign data_sram_en    = MemReadM | (|MemWriteM);
assign data_sram_wen   = MemWriteM;
assign data_sram_wdata = rdata2M;

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

assign raddr1   = inst_sram_rdata[25:21];
assign raddr2   = inst_sram_rdata[20:16];
assign wen      = RegWriteW;
assign waddr    = WaddrW;
assign wdata    = MemtoRegW? data_sram_rdata : ResW;

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