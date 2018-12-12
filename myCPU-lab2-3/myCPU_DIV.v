`timescale 1ns / 1ps
module div(
    input             div_clk,
    input             resetn,
    input             div,
    input             div_signed,
    input      [31:0] x,
    input      [31:0] y,
    output     [31:0] s,
    output     [31:0] r,
    output reg        complete
);
reg  [63:0] A;
reg  [32:0] B;
reg  [ 5:0] cnt;

wire x_signed = x[31] & div_signed;
wire y_signed = y[31] & div_signed;
wire [31:0] x_abs;
wire [31:0] y_abs;
assign x_abs = ({32{x_signed}}^x) + x_signed;
assign y_abs = ({32{y_signed}}^y) + y_signed;

wire [32:0] sub_result;
assign sub_result = A[63:31] + ~B[32:0] + 1'b1;
always@(posedge div_clk)
begin
    if(~resetn || ~div)
        cnt <= 6'd0;
    else if(div && !cnt)
        cnt <= 6'd32;
    else if(div)
        cnt <= cnt - 1'b1;
end
always@(posedge div_clk)
begin
    if(~resetn || ~div)
        B <= 32'd0;
    else if(div && !cnt)
        B <= {1'b0, y_abs};
end
always@(posedge div_clk)
begin
    if(~resetn || ~div)
        A <= 65'd0;
    else if(div && !cnt)
        A <= {32'd0, x_abs};
    else if(div && sub_result[32])
        A <= {A[62:0], 1'b0};
    else if(div && ~sub_result[32])
        A <= {sub_result[31:0], A[30:0], 1'b1};
end
always@(posedge div_clk)
begin
    if(~resetn || ~div)
        complete <= 1'b0;
    else if(div && cnt == 1'd1)
        complete <= 1'b1;
    else
        complete <= 1'b0;
end
wire s_signed = x_signed ^ y_signed;
wire r_signed = x_signed;
assign s = ({32{s_signed}} ^ A[31:0]) + s_signed;
assign r = ({32{r_signed}} ^ A[63:32]) + r_signed;
endmodule