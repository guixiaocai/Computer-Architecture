`timescale 10 ns / 1 ns

module regfile(
    input         clk,
    
    input  [ 4:0] raddr1,
    output [31:0] rdata1,
    
    input  [ 4:0] raddr2,
    output [31:0] rdata2,
    
    input         wen,
    input  [ 4:0] waddr,
    input  [31:0] wdata
);
reg [31:0] rf [31:0];

//write
always@(posedge clk)
begin
    if(wen) rf[waddr] <= wdata;
end

//read
assign rdata1 = (raddr1 | 5'd0) ? rf[raddr1] : 32'd0;
assign rdata2 = (raddr2 | 5'd0) ? rf[raddr2] : 32'd0;

endmodule