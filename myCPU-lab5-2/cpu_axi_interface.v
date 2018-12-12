module cpu_axi_interface(
    input     clk,
    input     resetn,

    //inst sram-like 
    input             inst_req     ,
    input             inst_wr      ,
    input      [ 1:0] inst_size    ,
    input      [31:0] inst_addr    ,
    input      [31:0] inst_wdata   ,
    output reg [31:0] inst_rdata   ,
    output            inst_addr_ok ,
    output reg        inst_data_ok ,
    
    //data sram-like 
    input             data_req     ,
    input             data_wr      ,
    input      [ 1:0] data_size    ,
    input      [31:0] data_addr    ,
    input      [31:0] data_wdata   ,
    output reg [31:0] data_rdata   ,
    output            data_addr_ok ,
    output            data_data_ok ,

    //axi
    //ar
    output reg [ 3:0] arid    ,
    output reg [31:0] araddr  ,
    output     [ 7:0] arlen   , //0
    output reg [ 2:0] arsize  ,
    output     [ 1:0] arburst , //2'b01
    output     [ 1:0] arlock  , //0
    output     [ 3:0] arcache , //0
    output     [ 2:0] arprot  , //0
    output reg        arvalid ,
    input             arready ,
    //r              
    input      [ 3:0] rid     ,
    input      [31:0] rdata   ,
    input      [ 1:0] rresp   , //ignore
    input             rlast   , //ignore
    input             rvalid  ,
    output reg        rready  ,
    //aw               
    output     [ 3:0] awid    , //1
    output reg [31:0] awaddr  ,
    output     [ 7:0] awlen   , //0
    output reg [ 2:0] awsize  ,
    output     [ 1:0] awburst , //2'b01
    output     [ 1:0] awlock  , //0
    output     [ 3:0] awcache , //0
    output     [ 2:0] awprot  , //0
    output reg        awvalid ,
    input             awready ,
    //w               
    output     [ 3:0] wid     , //1
    output reg [31:0] wdata   ,
    output reg [ 3:0] wstrb   ,
    output            wlast   , //1
    output reg        wvalid  ,
    input             wready  ,
    //b              
    input      [ 3:0] bid     , //ignore
    input      [ 1:0] bresp   , //ignore
    input             bvalid  ,
    output reg        bready  ,

    input      [ 3:0] data_wen
);

reg [2:0] r_curstate;
reg [2:0] r_nxtstate;
reg [2:0] w_curstate;
reg [2:0] w_nxtstate;
parameter ReadSt    = 3'd0;
parameter Readinst  = 3'd1;
parameter Readdata  = 3'd2;
parameter ReadValid = 3'd5;
parameter ReadFin   = 3'd4;
parameter WriteSt   = 3'd4;
parameter Writeinst = 3'd5;
parameter Writedata = 3'd6;
parameter WriteFin  = 3'd7;

always@(posedge clk)
begin
    if(~resetn) begin
        r_curstate <= ReadSt;
        w_curstate <= WriteSt;
    end else begin
        r_curstate <= r_nxtstate;
        w_curstate <= w_nxtstate;
    end
end

reg [31:0] awaddr_t;
always@(posedge clk)
begin
    if(~resetn) begin
        awaddr_t   <= 32'd0;
    end else if(data_req && data_wr && w_curstate == WriteSt) begin
        awaddr_t   <= data_addr;
    end else if(bvalid) begin
        awaddr_t   <= 32'd0;
    end
end

reg read_turn;
reg write_turn;
always@(posedge clk)
begin
    if(~resetn) 
        read_turn <= 1'b0;
    else if(r_curstate == ReadSt && r_nxtstate == Readdata && bready && ~bvalid)
        read_turn <= 1'b1;
    else if(bvalid)
        read_turn <= 1'b0;
end
always@(posedge clk)
begin
    if(~resetn)
        write_turn <= 1'b0;
    else if(w_curstate == WriteSt && w_nxtstate == Writedata && rready && ~rvalid)
        write_turn <= 1'b1;
    else if(rvalid)
        write_turn <= 1'b0;
end


always@(*)
begin
    case(r_curstate)
        ReadSt:
        begin
            if(inst_req && ~inst_wr)
                r_nxtstate = Readinst;
            else if(data_req && ~data_wr)
                r_nxtstate = Readdata;
            else
                r_nxtstate = r_curstate;
        end 
        Readinst, ReadValid:
        begin
            if(rvalid)
                r_nxtstate = ReadFin;
            else
                r_nxtstate = r_curstate;
        end
        Readdata:
        begin
            if(bready && awaddr_t[31:2] == araddr[31:2])
                r_nxtstate = r_curstate;
            else
                r_nxtstate = ReadValid;
        end
        ReadFin:
        begin
            if(read_turn)
                r_nxtstate = r_curstate;
            else
                r_nxtstate = ReadSt;
        end
        default:
            r_nxtstate = ReadSt;
    endcase
end

always@(*)
begin
    case (w_curstate)
        WriteSt:
        begin
            if(inst_req && inst_wr)
                w_nxtstate = Writeinst;
            else if(data_req && data_wr)
                w_nxtstate = Writedata;
            else
                w_nxtstate = w_curstate;
        end 
        Writeinst, Writedata:
        begin
            if(bvalid)
                w_nxtstate = WriteFin;
            else
                w_nxtstate = w_curstate;
        end
        WriteFin:
        begin
            if(write_turn)
                w_nxtstate = w_curstate;
            else
                w_nxtstate = WriteSt;
        end
        default:
            w_nxtstate = WriteSt;
    endcase
end

assign inst_addr_ok = r_curstate == ReadSt && r_nxtstate == Readinst || w_curstate == WriteSt && w_nxtstate == Writeinst;
assign data_addr_ok = r_curstate == ReadSt && r_nxtstate == Readdata || w_curstate == WriteSt && w_nxtstate == Writedata;

reg rvalid_t;
always@(posedge clk)
begin
    if(~resetn)
        rvalid_t <= 1'b0;
    else if(r_curstate == ReadFin && r_nxtstate == ReadSt && rid == 4'd1 
                 && w_curstate == WriteFin && w_nxtstate == WriteSt)
        rvalid_t <= 1'b1;
    else
        rvalid_t <= 1'b0;
end

always@(posedge clk)
begin
    if(rvalid && rid == 4'd0)
        inst_rdata <= rdata;
    //else
    //    inst_rdata <= 32'd0;
    if(rvalid && rid == 4'd1)
        data_rdata <= rdata;
    //else
    //    data_rdata <= 32'd0;
    
    inst_data_ok <= rvalid && rid == 4'd0;
    //data_data_ok <= r_curstate == ReadFin && r_nxtstate == ReadSt && rid == 4'd1 
    //             || w_curstate == WriteFin && w_nxtstate == WriteSt || rvalid_t;
end
//data_data_ok chage from reg to wire
assign data_data_ok = r_curstate == ReadFin && r_nxtstate == ReadSt && rid == 4'd1 
                 || w_curstate == WriteFin && w_nxtstate == WriteSt || rvalid_t;
always@(posedge clk)
begin
    if(r_curstate == ReadSt && r_nxtstate == Readinst) begin
        arid   <= 4'd0;
        araddr <= inst_addr;
        arsize <= !inst_size ? 3'd1 : {inst_size, 1'b0};
    end else if(r_curstate == ReadSt && r_nxtstate == Readdata) begin
        arid   <= 4'd1;
        araddr <= {data_addr[31:2], 2'd0};
        arsize <= !data_size ? 3'd1 : {data_size, 1'b0};
    end else if(r_curstate == ReadFin) begin
        araddr <= 32'd0;
    end
end

always@(posedge clk)
begin
    if(~resetn)
        arvalid <= 1'b0;
    else if(r_curstate == ReadSt && r_nxtstate == Readinst || r_curstate == Readdata && r_nxtstate == ReadValid)
        arvalid <= 1'b1;
    else if(arready)
        arvalid <= 1'b0;
end

always@(posedge clk)
begin
    if(~resetn)
        rready <= 1'b1;
    else if(r_nxtstate == Readinst || r_nxtstate == Readdata)
        rready <= 1'b1;
    else if(rvalid)
        rready <= 1'b0;
end

always@(posedge clk)
begin
    if(w_curstate == WriteSt && w_nxtstate == Writeinst) begin
        awaddr <= inst_addr;
        awsize <= !inst_size ? 3'd1 : {inst_size, 1'b0};
    end else if(w_curstate == WriteSt && w_nxtstate == Writedata) begin
        awaddr <= {data_addr[31:2], 2'd0};
        awsize <= !data_size ? 3'd1 : {data_size, 1'b0};
    end
end

always@(posedge clk)
begin
    if(~resetn)
        awvalid <= 1'b0;
    else if(w_curstate == WriteSt && (w_nxtstate == Writeinst || w_nxtstate == Writedata))
        awvalid <= 1'b1;
    else if(awready)
        awvalid <= 1'b0;
end

always@(posedge clk)
begin
    if(w_curstate == WriteSt && w_nxtstate == Writeinst) begin
        wdata <= inst_wdata;
        wstrb <= data_wen;
    end else if(w_curstate == WriteSt && w_nxtstate == Writedata) begin
        wdata <= data_wdata;
        wstrb <= data_wen;
    end
end

always@(posedge clk)
begin
    if(~resetn)
        wvalid <= 1'b0;
    else if(w_curstate == WriteSt && (w_nxtstate == Writeinst || w_nxtstate == Writedata))
        wvalid <= 1'b1;
    else if(wready)
        wvalid <= 1'b0;
end

always@(posedge clk)
begin
    if(~resetn)
        bready <= 1'b0;
    else if(w_nxtstate == Writeinst || w_nxtstate == Writedata)
        bready <= 1'b1;
    else if(bvalid)
        bready <= 1'b0;
end

// ar
assign arlen   = 8'd0;
assign arburst = 2'b01;
assign arlock  = 2'd0;
assign arcache = 4'd0;
assign arprot  = 3'd0;

//aw
assign awid    = 4'd1;
assign awlen   = 8'd0;
assign awburst = 2'b01;
assign awlock  = 2'd0;
assign awcache = 4'd0;
assign awprot  = 3'd0;

//w
assign wid   = 4'd1;
assign wlast = 1'b1;

endmodule