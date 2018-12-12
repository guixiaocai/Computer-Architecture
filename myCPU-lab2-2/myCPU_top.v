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

reg  [31:0] PC;
reg  [31:0] inst_regD;
reg  [31:0] HI;
reg  [31:0] LO;
wire [31:0] inst;
wire [63:0] mul_result; 
wire [63:0] div_result;

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
wire [31:0] HI_rdataE;
wire [31:0] LO_rdataE;
reg         IF_allowinD;
wire        ID_EX_rlt1;
wire        ID_MA_rlt1;
wire        ID_WB_rlt1;
wire        ID_EX_rlt2;
wire        ID_MA_rlt2;
wire        ID_WB_rlt2;
wire        data_rtl_stall;
wire        div_stall;
wire        ID_stall;
wire        bran_rtl_stall;

//EX
reg         MemReadM;
reg  [ 3:0] MemWriteM;
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

//MA
reg         MemtoRegW;
reg  [31:0] ResW;
reg  [ 4:0] WaddrW;
reg         RegWriteW;
reg         HIWriteW;
reg         LOWriteW;
reg  [31:0] HI_wdataW;
reg  [31:0] LO_wdataW;
reg  [31:0] PCW;

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
assign inst            = IF_allowinD ? inst_sram_rdata : inst_regD;
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
                PC <= {bran_add1[31:1], 1'b0};
            2'b10:
                PC <= {PC[31:28], inst[25:0], 2'b00};
            2'b11:
                PC <= PC + {{14{inst[15]}}, inst[15:0], 2'b00};
            default:
                PC <= PC + 32'd4;
            endcase
        else
            PC <= PC + 32'd4;
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
        ID_valid <= IF_to_ID_valid;
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

assign data_rtl_stall = (ID_EX_rlt1 | ID_EX_rlt2) & MemReadE;
assign bran_rtl_stall = (ID_MA_rlt1 | ID_MA_rlt2) & MemReadM & Branch;
assign div_stall      = divE & ~complete;
assign ID_stall       = data_rtl_stall | bran_rtl_stall;

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
    if(~resetn || ~IF_to_ID_valid)
        MemWriteE <= 1'b0;
    else if(IF_to_ID_valid & ID_allowin)
        MemWriteE <= MemWrite;
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
    if(~resetn || ~IF_to_ID_valid)
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
    if(~resetn || ~IF_to_ID_valid)
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
        if(alu_src1_sel[1])
            alu_src1E <= {27'd0, inst[10:6]};
        else if(alu_src1_sel[0])
            alu_src1E <= PC_ID;
        else
        begin
            if(HIRead)
                alu_src1E <= HI_rdataE;
            else if(LORead)
                alu_src1E <= LO_rdataE;
            if(ID_WB_rlt1)
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
    if(IF_to_ID_valid & IF_allowinD)
        PCE <= PC_ID;
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
//mul && div
always@(posedge clk)
begin
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

inst_decode myCPU_ID(.inst            (inst),
                     .RegDst          (RegDst         ),
                     .RegWrite        (RegWrite       ),
                     .MemtoReg        (MemtoReg       ),
                     .alu_src1_sel    (alu_src1_sel   ),
                     .alu_src2_sel    (alu_src2_sel   ),
                     .sign_ext        (sign_ext       ),
                     .alu_control     (alu_control    ),
                     .MemRead         (MemRead        ),
                     .MemWrite        (MemWrite       ),
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
                     .LORead          (LORead         )
);

//branch
wire [32:0] zero_cond;
wire [31:0] bran_add1;
wire [31:0] bran_add2;
assign bran_add1   = (ID_EX_rlt1)? alu_result :
                     (ID_MA_rlt1)? ResM       :
                     (ID_WB_rlt1)? wdata_mid  :
                                   rdata1     ;
assign bran_add2   = (ID_EX_rlt2)? alu_result :
                     (ID_MA_rlt2)? ResM       :
                     (ID_WB_rlt2)? wdata_mid  :
                                   rdata2     ;
assign zero_cond   = bran_add1 + ~bran_add2 + 1'b1;
assign Branch_cond = (op_branch[0] & bran_add1[31])
                   | (op_branch[1] & ~bran_add1[31])
                   | (op_branch[2] & ~|zero_cond[31:0])
                   | (op_branch[3] &  |zero_cond[31:0])
                   | (op_branch[4] & (bran_add1[31] | ~|bran_add1));

/***************  EX  ********************/
assign EX_ready_go    = ~div_stall;//1'b1;
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
    if(~resetn || ~ID_to_EX_valid)
        MemReadM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        MemReadM <= MemReadE;
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid)
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
    if(~resetn || ~ID_to_EX_valid)
        RegWriteM <= 1'b0;
    else if(ID_to_EX_valid & EX_allowin)
        RegWriteM <= RegWriteE;
end
always@(posedge clk)
begin
    if(~resetn || ~ID_to_EX_valid)
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
        PCM <= PCE;
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
        MA_valid <= EX_to_MA_valid;
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
        HIWriteW    <= HIWriteM;
        LOWriteW    <= LOWriteM;
    end
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

always@(posedge clk)
begin
    if(~resetn)
        HI <= 32'd0;
    else if(MA_allowin & HIWriteW)
        HI <= HI_wdataW;
end
always@(posedge clk)
begin
    if(~resetn)
        LO <= 32'd0;
    else if(MA_allowin & LOWriteW)
        LO <= LO_wdataW;
end

assign wdata_mid = (MemtoRegW == 1'b1)? data_sram_rdata : ResW;
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