`include "mycpu.h"

module if_stage(
    input                          clk            ,
    input                          reset          ,
    input                          flush          ,
    //allwoin
    input                          ds_allowin     ,
    //brbus
    input  [`BR_BUS_WD       -1:0] br_bus         ,
    //expc
    input  [`WS_TO_FS_BUS_WD -1:0] ws_to_fs_bus   ,
    //to ds
    output                         fs_to_ds_valid ,
    output [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus   ,
    // inst sram interface
    output        inst_sram_en   ,
    output [ 3:0] inst_sram_wen  ,
    output [31:0] inst_sram_addr ,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,
    output stall                 ,
    input  ws_back                   
);

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;
wire        ws_eret;
wire [31:0] cp0pcnext;
wire [31:0] seq_pc;
wire [31:0] nextpc;
wire [ 3:0] instget ;
wire        instj0  ;
wire        instj1  ;
wire         br;
wire         br_taken;
wire [ 31:0] br_target;
wire         inst_adel_ex;
assign instget = nextpc[31:28];
assign instj0  = (instget==4'h8)||(instget==4'ha);
assign instj1  = (instget==4'h9)||(instget==4'hb);
assign {br,br_taken,br_target} = br_bus; //branch control
wire         exc;
wire         excr;
assign {ws_eret,
        cp0pcnext,
        excr}= ws_to_fs_bus;
assign exc = ws_back;
wire [31:0] fs_inst;
reg  [31:0] fs_pc;
assign fs_to_ds_bus = flush ? 66'h0:
                      {inst_adel_ex,
                       br,
                       fs_inst ,
                       fs_pc   }&{`FS_TO_DS_BUS_WD{fs_valid}};
// pre-IF stage
assign to_fs_valid  = ~reset;
assign seq_pc       = fs_pc + 3'h4; //pc=pc+4
assign nextpc       = br_taken ? br_target : 
                      exc      ? 32'hbfc00380:
                      ws_eret  ? cp0pcnext :
                      seq_pc; 
// IF stage
assign stall          = !fs_allowin;
assign fs_ready_go    = 1'b1;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go;

always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end

    if (reset) begin
        fs_pc <= 32'hbfbffffc;  //trick: to make nextpc be 0xbfc00000 during reset 
    end
    else if (to_fs_valid && fs_allowin || exc ) begin
        fs_pc <= nextpc;
    end
end

assign inst_sram_en    = to_fs_valid && fs_allowin;
assign inst_sram_wen   = 4'h0;
assign inst_sram_addr  = instj0?{4'h0,nextpc[27:0]}:
                         instj1?{4'h1,nextpc[27:0]}:
                         nextpc;
assign inst_sram_wdata = 32'b0;

assign fs_inst         = inst_sram_rdata;
assign inst_adel_ex    = fs_valid & (fs_pc[1:0]!=2'b00);
endmodule


