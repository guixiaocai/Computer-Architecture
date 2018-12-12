`timescale 1ns / 1ps
module mul(
    input         mul_signed,
    input  [31:0] x,
    input  [31:0] y,

    output [63:0] add_s_out,
    output [63:0] add_c_out,
    output        cin_out,

    input  [63:0] add_s_in,
    input  [63:0] add_c_in,
    input         cin_in,  

    output [63:0] result
);
wire [34:0] mul_y;
wire [63:0] mul_x;
wire [63:0] p  [16:0];
wire [16:0] c;
wire [63:0] ps  [16:0];

wire [16:0] pt   [63:0];
wire [13:0] cin  [63:0];
wire [13:0] cout [62:0];

wire [63:0] add_s;
wire [63:0] add_c;

reg  [63:0] add_s_reg;
reg  [63:0] add_c_reg;
reg         cin_reg;
wire        re_cout;

assign mul_x = {{32{mul_signed & x[31]}}, x};
assign mul_y = {{2{mul_signed & y[31]}}, y, 1'b0};
generate
    genvar i;
    for(i = 0; i < 17; i = i + 1)
    begin : encoder
        BoothDe u_BoothDe(
            .y2 (mul_y[2*(i+1)] ),
            .y1 (mul_y[2*i+1]   ),
            .y0 (mul_y[2*i]     ),
            .x  (mul_x << 2*i   ),
            .c  (c[i]           ),
            .p  (p[i]           )
        );
    end
endgenerate

//switch
assign cin[0] = c[13:0];
generate
    for(i = 0; i < 64; i = i + 1)
    begin : transpose1
        genvar j;
        for(j = 0; j < 17; j = j + 1)
        begin : transpose2
            assign pt[i][j] = p[j][i];
        end
    end
endgenerate

generate
    for(i = 0; i < 63; i = i + 1)
    begin : WT
        WallaceTree u_WallaceTree(
            .a    (pt[i]      ),
            .cin  (cin[i]     ),
            .s    (add_s[i]   ),
            .c    (add_c[i+1] ),
            .cout (cout[i]    )
        );
        assign cin[i+1] = cout[i];
    end
endgenerate

WallaceTree u_WallaceTree(
            .a    (pt[63]      ),
            .cin  (cin[63]     ),
            .s    (add_s[63]   ),
            .c    (            ),
            .cout (            )
        );
assign add_c[0] = c[14];

assign add_s_out = add_s;
assign add_c_out = add_c;
assign cin_out   = c[15];

assign {re_cout, result} = add_c_in + add_s_in + cin_in;
endmodule

module WallaceTree(
    input  [16:0] a,
    input  [13:0] cin,
    output        s,
    output        c,
    output [13:0] cout
);
wire [4:0] s1;
wire [3:0] s2;
wire [1:0] s3;
wire [1:0] s4;
wire       s5;

//first
full_adder u_full_adder_one1(
    .a    (a[4]   ),
    .b    (a[3]   ),
    .cin  (a[2]   ),
    .sum  (s1[0]  ),
    .cout (cout[0])
);
full_adder u_full_adder_one2(
    .a    (a[7]   ),
    .b    (a[6]   ),
    .cin  (a[5]   ),
    .sum  (s1[1]  ),
    .cout (cout[1])
);
full_adder u_full_adder_one3(
    .a    (a[10]  ),
    .b    (a[9]   ),
    .cin  (a[8]   ),
    .sum  (s1[2]  ),
    .cout (cout[2])
);
full_adder u_full_adder_one4(
    .a    (a[13]  ),
    .b    (a[12]  ),
    .cin  (a[11]  ),
    .sum  (s1[3]  ),
    .cout (cout[3])
);
full_adder u_full_adder_one5(
    .a    (a[16]  ),
    .b    (a[15]  ),
    .cin  (a[14]  ),
    .sum  (s1[4]  ),
    .cout (cout[4])
);

//second
full_adder u_full_adder_second1(
    .a    (cin[2] ),
    .b    (cin[1] ),
    .cin  (cin[0] ),
    .sum  (s2[0]  ),
    .cout (cout[5])
);
full_adder u_full_adder_second2(
    .a    (a[0]   ),
    .b    (cin[3] ),
    .cin  (cin[4] ),
    .sum  (s2[1]  ),
    .cout (cout[6])
);
full_adder u_full_adder_second3(
    .a    (s1[1]  ),
    .b    (s1[0]  ),
    .cin  (a[1]   ),
    .sum  (s2[2]  ),
    .cout (cout[7])
);
full_adder u_full_adder_second4(
    .a    (s1[4]  ),
    .b    (s1[3]  ),
    .cin  (s1[2]  ),
    .sum  (s2[3]  ),
    .cout (cout[8])
);

//third
full_adder u_full_adder_third1(
    .a    (s2[0]  ),
    .b    (cin[6] ),
    .cin  (cin[5] ),
    .sum  (s3[0]  ),
    .cout (cout[9])
);
full_adder u_full_adder_third2(
    .a    (s2[3]   ),
    .b    (s2[2]   ),
    .cin  (s2[1]   ),
    .sum  (s3[1]   ),
    .cout (cout[10])
);

//fourth
full_adder u_full_adder_four1(
    .a    (cin[9]  ),
    .b    (cin[8]  ),
    .cin  (cin[7]  ),
    .sum  (s4[0]   ),
    .cout (cout[11])
);
full_adder u_full_adder_four2(
    .a    (s3[1]   ),
    .b    (s3[0]   ),
    .cin  (cin[10] ),
    .sum  (s4[1]   ),
    .cout (cout[12])
);

//fifth
full_adder u_full_adder_five1(
    .a    (s4[1]   ),
    .b    (s4[0]   ),
    .cin  (cin[11] ),
    .sum  (s5      ),
    .cout (cout[13])
);

//sixth
full_adder u_full_adder_six1(
    .a    (s5     ),
    .b    (cin[13]),
    .cin  (cin[12]),
    .sum  (s      ),
    .cout (c      )
);
endmodule

module full_adder(
    input  a,
    input  b,
    input  cin,
    output sum,
    output cout
);
assign sum  = ~a & ~b &  cin
            | ~a &  b & ~cin
            |  a & ~b & ~cin
            |  a &  b &  cin;
assign cout = a & b | a & cin | b & cin;
endmodule 

module BoothDe(
    input         y2,
    input         y1,
    input         y0,
    input  [63:0] x,
    output        c,
    output [63:0] p
);
wire        s_p1;
wire        s_p2;
wire        s_n1;
wire        s_n2;

assign s_p1 = ~(~(~y2 &  y1 & ~y0) & ~(~y2 & ~y1 & y0));
assign s_n1 = ~(~( y2 &  y1 & ~y0) & ~( y2 & ~y1 & y0));
assign s_p2 = ~(~(~y2 &  y1 &  y0));
assign s_n2 = ~(~( y2 & ~y1 & ~y0));

assign c = s_n1 | s_n2;
assign p = s_p1 ?  x               :
           s_n1 ? ~x               :
           s_p2 ? { x[62:0], 1'b0} :
           s_n2 ? {~x[62:0], 1'b1} :
                  64'd0            ;
endmodule