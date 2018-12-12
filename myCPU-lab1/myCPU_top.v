`include "myCPU_define.h"

module mycpu_top(
    input                clk,
    input                resetn,            //low active

    output               inst_sram_en,
    output      [ 3:0]   inst_sram_wen,
    output  reg [31:0]   inst_sram_addr,
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

parameter S0 = 4'b0_001;
parameter S1 = 4'b0_010;
parameter S2 = 4'b0_100;
parameter S3 = 4'b1_000;

reg  [ 3:0] cur_state;
reg  [ 3:0] next_state;
reg  [31:0] alu_out; //ALUOut
reg  [31:0] inst_sram_addr_f;
reg  [31:0] inst_sram_addr_wb;
reg         inst_sram_addr_bj;

//myCPU_alu
wire [31:0] alu_src1;
wire [31:0] alu_src2;
wire        Overflow;
wire        CarryOut;
wire        Zero;
wire [31:0] alu_result;

//myCPU_rf
wire [ 4:0] raddr1;
wire [ 4:0] raddr2;
wire [ 4:0] waddr;
wire [31:0] wdata;
wire [31:0] rdata1;
wire [31:0] rdata2;


wire        rf_wen; //RegWrite
wire [ 1:0] rf_waddr_sel;
wire        rf_wdata_sel;

wire        alu_src1_sel; //ALUSrcA//
wire        alu_src2_sel; //ALUSrcB//
wire        alu_src_op_jal;
wire [11:0] alu_control;
wire        branch_cond;
wire        inst_sram_addr_dirtw;
wire        inst_sram_addr_condw;
wire [ 1:0] inst_sram_addr_src; //PCSource
wire        data_sram_addr_src;

always@(posedge clk or negedge resetn)
begin
    if(~resetn)
        cur_state <= S0;
    else
        cur_state <= next_state; 
end

always@(*)
begin
    case(cur_state)
        S0:
            next_state = S1;
        S1: 
            next_state = S2;
        S2: 
            next_state = S3;
        default:
            next_state = S0;
    endcase
end

always@(posedge clk)
begin
    if(cur_state[2]) begin
        if(inst_sram_addr_dirtw | (inst_sram_addr_condw & branch_cond))
            case(inst_sram_addr_src)
                2'b00: //jr, jalr
                    inst_sram_addr_f <= {alu_result[31:1], 1'b0};
                2'b01: //branch
                    inst_sram_addr_f <= inst_sram_addr + {{14{inst_sram_rdata[15]}}, inst_sram_rdata[15:0], 2'b00};
                2'b10: //j,jal
                    inst_sram_addr_f <= {inst_sram_addr[31:28], inst_sram_rdata[25:0], 2'b00};
                default:
                    inst_sram_addr_f <= inst_sram_addr_f;
            endcase
    end
end

always@(posedge clk)
begin
    if(cur_state[2])
        inst_sram_addr_bj <= inst_sram_addr_dirtw | (inst_sram_addr_condw & branch_cond);
end

always@(posedge clk)
begin
    if(cur_state[0])
        inst_sram_addr_wb <= inst_sram_addr;
end

always@(posedge clk or negedge resetn)
begin
    if(~resetn)
        inst_sram_addr <= `myCPU_INIT_INST_ADDR;
    else if(cur_state[0])
        inst_sram_addr <= inst_sram_addr + 4;
    else if(cur_state[2] & inst_sram_addr_bj)
        inst_sram_addr <= inst_sram_addr_f;
end

inst_decode myCPU_ID(.inst_sram_rdata(inst_sram_rdata),
                     .cur_state(cur_state),
                     .alu_result(alu_result),
                     .Zero(Zero),
                     .rf_wen(rf_wen),
                     .rf_waddr_sel(rf_waddr_sel),
                     .rf_wdata_sel(rf_wdata_sel),
                     .alu_src1_sel(alu_src1_sel),
                     .alu_src2_sel(alu_src2_sel),
                     .alu_src_op_jal(alu_src_op_jal),
                     .alu_control(alu_control),
                     .data_sram_en(data_sram_en),
                     .data_sram_wen(data_sram_wen),
                     .branch_cond(branch_cond),
                     .inst_sram_addr_dirtw(inst_sram_addr_dirtw),
                     .inst_sram_addr_condw(inst_sram_addr_condw),
                     .inst_sram_addr_src(inst_sram_addr_src),
                     .data_sram_addr_src(data_sram_addr_src)
);

assign alu_src1 = alu_src_op_jal? inst_sram_addr_wb : alu_src1_sel ? rdata1 : {27'd0 ,inst_sram_rdata[10:6]};
assign alu_src2 = alu_src_op_jal? 8 : alu_src2_sel ? rdata2          :
                  {{16{inst_sram_rdata[15]}}, inst_sram_rdata[15:0]} ;

alu myCPU_alu(.alu_control(alu_control), 
              .alu_src1(alu_src1),
              .alu_src2(alu_src2),
              .alu_result(alu_result),
              .Overflow(Overflow),
              .CarryOut(CarryOut),
              .Zero(Zero));

assign raddr1   = inst_sram_rdata[25:21];
assign raddr2   = inst_sram_rdata[20:16];
assign waddr    = rf_waddr_sel[1] ? 5'd31                  :
                  rf_waddr_sel[0] ? inst_sram_rdata[20:16] :
                                    inst_sram_rdata[15:11] ;
assign wdata    = rf_wdata_sel    ? data_sram_rdata : alu_result; 

regfile myCPU_rf(.clk(clk),
                 .raddr1(raddr1),
                 .rdata1(rdata1),
                 .raddr2(raddr2),
                 .rdata2(rdata2),
                 .wen(rf_wen),
                 .waddr(waddr),
                 .wdata(wdata));

always@(posedge clk)
begin
    if(cur_state[2])
        alu_out <= alu_result;
end

assign inst_sram_en    = cur_state[0];
assign inst_sram_wen   = 4'd0;
assign inst_sram_wdata = 32'd0;

assign data_sram_addr   = data_sram_addr_src ? alu_result : alu_out;
assign data_sram_wdata  = rdata2;

assign debug_wb_pc       = inst_sram_addr_wb;
assign debug_wb_rf_wen   = {4{rf_wen}}      ;
assign debug_wb_rf_wnum  = waddr            ;
assign debug_wb_rf_wdata = wdata            ;

endmodule