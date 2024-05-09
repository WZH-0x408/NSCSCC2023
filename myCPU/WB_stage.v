`include "mycpu.h"

module wb_stage(
    input                           clk           ,
    input                           reset         ,
    //allowin
    output                          ws_allowin    ,
    //from ms
    input                           ms_to_ws_valid,
    input  [`MS_TO_WS_BUS_WD -1:0]  ms_to_ws_bus  ,
    //to rf: for write back, can be reused as forwarding datapath
    output [`WS_TO_RF_BUS_WD -1:0]  ws_to_rf_bus  ,
    output [`WS_TO_FS_BUS_WD -1:0]  ws_to_fs_bus  ,
    output                          ws_to_es_bus  ,
    //flush
    output                          ws_to_fs_flush  ,
    output                          ws_to_ds_flush  ,
    output                          ws_to_es_flush  ,
    output                          ws_to_ms_flush  ,
    //trace debug interface
    output [31:0] debug_wb_pc     ,
    output [ 3:0] debug_wb_rf_wen ,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata,
    input stall                    ,
    output ws_back                 ,
    output has_int                 ,
    input  [ 5:0] ws_ext_in
);

reg         ws_valid;
wire        ws_ready_go;
reg         ws_exback;

wire        ws_has_int     ;
wire        ws_alu_overflow;
wire [31:0] ws_bad_addr    ;
wire        ws_bad_inst    ;
wire        ws_break       ;
wire        ws_adel_inst   ;
wire        ws_adel_mem    ;
wire        ws_ades_mem    ;
reg [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus_r;
wire [ 3:0] ws_gr_we;
wire [ 4:0] ws_dest;
wire [ 4:0] ws_dstcp0;
wire [31:0] ws_final_result;
wire [31:0] ws_pc;
wire        ws_mfc0_op;
wire        ws_mtc0_op;
wire        ws_eret_op;
wire        ws_sysc_ex;
wire [ 2:0] ws_sel;
wire [31:0] ws_cp0datw;
wire [31:0] cp0regread;
wire        cp0regvalid;
wire        ws_bd;
wire        ws_ex;
wire [ 4:0] ws_excode;    
wire instj0;
wire instj1; 
always@(posedge clk)
begin
    if(!stall)
        ws_exback <= 1'b0;
end
always@(negedge clk)
begin
    if(ws_ex)
        ws_exback <= 1'b1;
end

assign ws_to_fs_bus = { ws_eret_op,//33:33
                        cp0regread,//32:1
                        ws_exback  //0:0
                        }&{`WS_TO_FS_BUS_WD{ws_valid}};
assign ws_to_es_bus = ws_exback | ws_eret_op ;
assign ws_back      = ws_exback;
assign {ws_to_fs_flush,
        ws_to_ds_flush,
        ws_to_es_flush,
        ws_to_ms_flush} = {4{ws_exback|ws_eret_op&ws_valid}};
assign {instj0         ,  //158:158
        instj1         ,  //157:157
        ws_has_int     ,  //156:156
        ws_alu_overflow,  //155:155
        ws_bad_addr    ,  //154:123
        ws_bad_inst    ,  //122:122
        ws_break       ,  //121:121
        ws_adel_inst   ,  //120:120
        ws_adel_mem    ,  //119:119
        ws_ades_mem    ,  //118:118
        ws_bd          ,  //117:117
        ws_cp0datw     ,  //116:85
        ws_dstcp0      ,  //84:80
        ws_gr_we       ,  //79:76
        ws_eret_op     ,  //75:75
        ws_mfc0_op     ,  //74:74
        ws_mtc0_op     ,  //73:73
        ws_sysc_ex     ,  //72:72
        ws_dest        ,  //71:67
        ws_sel         ,  //66:64
        ws_final_result,  //63:32
        ws_pc             //31:0
       } = ms_to_ws_bus_r;
assign ws_ex = (ws_sysc_ex|ws_alu_overflow|ws_bad_inst|ws_break
               |ws_adel_inst|ws_adel_mem|ws_ades_mem|ws_has_int )&& ws_valid;
assign ws_excode = ws_sysc_ex     ? 5'h08:
                   ws_alu_overflow? 5'h0c:
                   ws_bad_inst    ? 5'h0a:
                   ws_break       ? 5'h09:
                   (ws_adel_inst||ws_adel_mem)? 5'h04:
                   ws_ades_mem    ? 5'h05: 
                   ws_has_int     ? 5'h00: 5'h1f;
wire [ 3:0] rf_we;
wire [4 :0] rf_waddr;
wire [31:0] rf_wdata;
assign ws_to_rf_bus = {ws_exback,  //42:42
                       ws_mfc0_op, //41:41
                       rf_we    ,  //40:37
                       rf_waddr ,  //36:32
                       rf_wdata    //31:0,fixed
                      } & {`WS_TO_RF_BUS_WD{ws_valid}};
assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid || ws_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ws_valid <= 1'b0;
    end
    else if (ws_allowin) begin
        ws_valid <= ms_to_ws_valid;
    end

    if (ms_to_ws_valid && ws_allowin) begin
        ms_to_ws_bus_r <= ms_to_ws_bus;
    end
end
CP0 myCP0(
    .clk        (clk)       ,
    .reset      (reset)     ,
    .dest       (ws_dstcp0) ,
    .sel        (ws_sel)    ,
    .exc        (ws_ex)     ,
    .we         (ws_mtc0_op),
    .re         (ws_mfc0_op),
    .eret       (ws_eret_op),
    .bd         (ws_bd)     ,
    .pc         (ws_pc)     ,
    .excode     (ws_excode) ,
    .ext_in     (ws_ext_in) ,
    .wdata      (ws_cp0datw),
    .regread    (cp0regread),
    .badVaddr   (ws_bad_addr),
    .inst_adel  (ws_adel_inst),
    .mem_adel   (ws_adel_mem ),
    .mem_ades   (ws_ades_mem ),
    .has_int    (has_int   )
);

    
assign rf_we    = ws_gr_we & {4{ws_valid & ~ws_ex}};
assign rf_waddr = ws_dest;
assign rf_wdata = ws_mfc0_op ? cp0regread : 
                  ws_final_result;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_wen   = rf_we;
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = rf_wdata;

endmodule

