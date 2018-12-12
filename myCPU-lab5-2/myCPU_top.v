module mycpu_top(
    input  [5:0] int,
    
    input        aclk,
    input        aresetn,

    //axi
    //ar
    output [ 3:0] arid    ,
    output [31:0] araddr  ,
    output [ 7:0] arlen   , //0
    output [ 2:0] arsize  ,
    output [ 1:0] arburst , //2'b01
    output [ 1:0] arlock  , //0
    output [ 3:0] arcache , //0
    output [ 2:0] arprot  , //0
    output        arvalid ,
    input         arready ,
    //r              
    input  [ 3:0] rid     ,
    input  [31:0] rdata   ,
    input  [ 1:0] rresp   , //ignore
    input         rlast   , //ignore
    input         rvalid  ,
    output        rready  ,
    //aw               
    output [ 3:0] awid    , //1
    output [31:0] awaddr  ,
    output [ 7:0] awlen   , //0
    output [ 2:0] awsize  ,
    output [ 1:0] awburst , //2'b01
    output [ 1:0] awlock  , //0
    output [ 3:0] awcache , //0
    output [ 2:0] awprot  , //0
    output        awvalid ,
    input         awready ,
    //w               
    output [ 3:0] wid     , //1
    output [31:0] wdata   ,
    output [ 3:0] wstrb   ,
    output        wlast   , //1
    output        wvalid  ,
    input         wready  ,
    //b              
    input  [ 3:0] bid     , //ignore
    input  [ 1:0] bresp   , //ignore
    input         bvalid  ,
    output        bready  ,

    //debug
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_wen,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);
//inst sram-like
wire         inst_req     ;
wire         inst_wr      ;
wire  [ 1:0] inst_size    ;
wire  [31:0] inst_addr    ;
wire  [31:0] inst_wdata   ;
wire  [31:0] inst_rdata   ;
wire         inst_addr_ok ;
wire         inst_data_ok ;
    
//data sram-like 
wire         data_req     ;
wire         data_wr      ;
wire  [ 1:0] data_size    ;
wire  [31:0] data_addr    ;
wire  [31:0] data_wdata   ;
wire  [31:0] data_rdata   ;
wire         data_addr_ok ;
wire         data_data_ok ;

wire  [ 3:0] data_wen     ;

//cpu core
cpu_core u_core(
    .clk           (aclk        ),
    .resetn        (aresetn     ),

    .inst_req      (inst_req     ),
    .inst_wr       (inst_wr      ),
    .inst_size     (inst_size    ),
    .inst_addr     (inst_addr    ),
    .inst_wdata    (inst_wdata   ),
    .inst_rdata    (inst_rdata   ),
    .inst_addr_ok  (inst_addr_ok ),
    .inst_data_ok  (inst_data_ok ),

    .data_req      (data_req     ),
    .data_wr       (data_wr      ),
    .data_size     (data_size    ),
    .data_addr     (data_addr    ),
    .data_wdata    (data_wdata   ),
    .data_rdata    (data_rdata   ),
    .data_addr_ok  (data_addr_ok ),
    .data_data_ok  (data_data_ok ),

    .data_wen      (data_wen     ),

    //debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_wen  (debug_wb_rf_wen  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata)
);

//sram-like AXI bridge
cpu_axi_interface u_bridge(
    .clk          (aclk         ),
    .resetn       (aresetn      ),
    
    .inst_req     (inst_req     ),
    .inst_wr      (inst_wr      ),
    .inst_size    (inst_size    ),
    .inst_addr    (inst_addr    ), 
    .inst_wdata   (inst_wdata   ),
    .inst_rdata   (inst_rdata   ),
    .inst_addr_ok (inst_addr_ok ),
    .inst_data_ok (inst_data_ok ),

    .data_req     (data_req     ),
    .data_wr      (data_wr      ),
    .data_size    (data_size    ),
    .data_addr    (data_addr    ), 
    .data_wdata   (data_wdata   ),
    .data_rdata   (data_rdata   ),
    .data_addr_ok (data_addr_ok ),
    .data_data_ok (data_data_ok ),

    .arid         (arid         ),
    .araddr       (araddr       ),
    .arlen        (arlen        ),
    .arsize       (arsize       ),
    .arburst      (arburst      ),
    .arlock       (arlock       ),
    .arcache      (arcache      ),
    .arprot       (arprot       ),
    .arvalid      (arvalid      ),
    .arready      (arready      ),

    .rid          (rid          ),
    .rdata        (rdata        ),
    .rresp        (rresp        ),
    .rlast        (rlast        ),
    .rvalid       (rvalid       ),
    .rready       (rready       ),

    .awid         (awid         ),
    .awaddr       (awaddr       ),
    .awlen        (awlen        ),
    .awsize       (awsize       ),
    .awburst      (awburst      ),
    .awlock       (awlock       ),
    .awcache      (awcache      ),
    .awprot       (awprot       ),
    .awvalid      (awvalid      ),
    .awready      (awready      ),

    .wid          (wid          ),
    .wdata        (wdata        ),
    .wstrb        (wstrb        ),
    .wlast        (wlast        ),
    .wvalid       (wvalid       ),
    .wready       (wready       ),

    .bid          (bid          ),
    .bresp        (bresp        ),
    .bvalid       (bvalid       ),
    .bready       (bready       ),

    .data_wen     (data_wen     )
);
endmodule