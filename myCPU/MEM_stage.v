`include "mycpu.h"

module mem_stage(
    input                          clk           ,
    input                          reset         ,
    input                          flush         ,
    //allowin
    input                          ws_allowin    ,
    output                         ms_allowin    ,
    //from es
    input                          es_to_ms_valid,
    input  [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    //to ws
    output                         ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus  ,
    //ex
    output                         ms_to_es_bus  ,
    //from data-sram
    input  [31                 :0] data_sram_rdata,
    //forward
    output [`MS_FWD_BUS_WD   -1:0] ms_to_ds_bus
);

reg         ms_valid;
wire        ms_ready_go;

reg [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus_r;

wire        ms_ex          ;
wire        ms_has_int     ;
wire        ms_alu_overflow;
wire [31:0] ms_bad_addr    ;
wire        ms_bad_inst    ;
wire        ms_break       ;
wire        ms_adel_inst   ;
wire        ms_adel_mem    ;
wire        ms_ades_mem    ;
wire        ms_res_from_mem;
wire        ms_lh_op       ;
wire        ms_lb_op       ;
wire        ms_lwl_op      ;
wire        ms_lwr_op      ;
wire        ms_load_ext    ;
wire        ms_mfc0_op     ;
wire        ms_mtc0_op     ;
wire        ms_eret_op     ;
wire        ms_sysc_ex     ;
wire [ 1:0] ms_load_offset ;
wire [ 3:0] ms_gr_we;
wire [ 4:0] ms_dest;
wire [ 4:0] ms_dstcp0;
wire [ 2:0] ms_sel;
wire [31:0] ms_alu_result; //in fact from ALU or HILO
wire [31:0] ms_pc;
wire [31:0] ms_rt_value;
wire        ms_bd;

assign {ms_has_int     ,  //164:164
        ms_alu_overflow,  //163:163
        ms_bad_addr    ,  //162:131
        ms_bad_inst    ,  //130:130
        ms_break       ,  //129:129
        ms_adel_inst   ,  //128:128
        ms_adel_mem    ,  //127:127
        ms_ades_mem    ,  //126:126
        ms_bd          ,  //125:125
        ms_rt_value    ,  //124:93
        ms_dstcp0      ,  //92:88
        ms_res_from_mem,  //87:87
        ms_gr_we       ,  //86:83
        ms_lh_op       ,  //82:82
        ms_lb_op       ,  //81:81
        ms_lwl_op      ,  //80:80
        ms_lwr_op      ,  //79:79
        ms_eret_op     ,  //78:78
        ms_mfc0_op     ,  //77:77
        ms_mtc0_op     ,  //76:76
        ms_sysc_ex     ,  //75:75
        ms_load_offset ,  //74:73
        ms_load_ext    ,  //72:72
        ms_dest        ,  //71:67
        ms_sel         ,  //66:64
        ms_alu_result  ,  //63:32
        ms_pc             //31:0
       } = es_to_ms_bus_r;
assign ms_to_es_bus = ms_ex | ms_eret_op;

wire [31:0] mem_result;
wire [31:0] mem_result_fin;
wire [15:0] mem_result_lh ;
wire [ 7:0] mem_result_lb ;
wire [31:0] mem_result_lwl;
wire [31:0] mem_result_lwr;
wire [31:0] mem_result_lh_ext;
wire [31:0] mem_result_lb_ext;
wire [31:0] ms_final_result;

assign ms_to_ws_bus = flush ? 157'h0:
                      {ms_has_int     ,  //156:156
                       ms_alu_overflow,  //155:155
                       ms_bad_addr    ,  //154:123
                       ms_bad_inst    ,  //122:122
                       ms_break       ,  //121:121
                       ms_adel_inst   ,  //120:120
                       ms_adel_mem    ,  //119:119
                       ms_ades_mem    ,  //118:118
                       ms_bd          ,  //117:117
                       ms_rt_value    ,  //116:85
                       ms_dstcp0      ,  //84:80
                       ms_gr_we       ,  //79:76
                       ms_eret_op     ,  //75:75
                       ms_mfc0_op     ,  //74:74
                       ms_mtc0_op     ,  //73:73
                       ms_sysc_ex     ,  //72:72
                       ms_dest        ,  //71:67
                       ms_sel         ,  //66:64
                       ms_final_result,  //63:32
                       ms_pc             //31:0
                      };

assign ms_ready_go    = 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
    end
    else if (ms_allowin) begin
        ms_valid <= es_to_ms_valid;
    end

    if (es_to_ms_valid && ms_allowin) begin
        es_to_ms_bus_r <= es_to_ms_bus; //bug 2: blocking assignment to non_blocking assignment
    end
end

assign mem_result = data_sram_rdata;
assign mem_result_lh = (ms_load_offset==2'b00)? mem_result[15:0]:
                       (ms_load_offset==2'b10)? mem_result[31:16]: 16'b0;
assign mem_result_lb = (ms_load_offset==2'b00)? mem_result[  7:0]:
                       (ms_load_offset==2'b01)? mem_result[ 15:8]:
                       (ms_load_offset==2'b10)? mem_result[23:16]:
                       (ms_load_offset==2'b11)? mem_result[31:24]: 8'b0;
assign mem_result_lh_ext = ms_load_ext? {{16{mem_result_lh[15]}},mem_result_lh}: {{16{1'b0}},mem_result_lh};
assign mem_result_lb_ext = ms_load_ext? {{24{mem_result_lb[7]}}, mem_result_lb}: {{24{1'b0}},mem_result_lb};
assign mem_result_lwl    = (ms_load_offset==2'b00)? {mem_result[7:0], 24'b0}:
                           (ms_load_offset==2'b01)? {mem_result[15:8], mem_result[7:0], 16'b0}:
                           (ms_load_offset==2'b10)? {mem_result[23:16], mem_result[15:8], mem_result[7:0], 8'b0}:
                           mem_result;
assign mem_result_lwr    = (ms_load_offset==2'b00)? mem_result:
                           (ms_load_offset==2'b01)? {8'b0, mem_result[31:24], mem_result[23:16], mem_result[15:8]}:
                           (ms_load_offset==2'b10)? {16'b0, mem_result[31:24], mem_result[23:16]}:
                           {24'b0, mem_result[31:24]};
assign mem_result_fin    = ms_lh_op? mem_result_lh_ext: 
                           ms_lb_op? mem_result_lb_ext: 
                           ms_lwl_op?   mem_result_lwl:
                           ms_lwr_op?   mem_result_lwr: mem_result;   
//bug: ms_res_from_mem passed as Z, unable to tell data to ws from mem/alu
assign ms_final_result = ms_res_from_mem ? mem_result_fin
                                         : ms_alu_result;
//forward
assign ms_ex = ms_alu_overflow | ms_adel_mem | ms_ades_mem | ms_adel_inst
             | ms_break    | ms_bad_inst | ms_sysc_ex
             | ms_has_int;
assign ms_to_ds_bus = flush? 43'h0:
                      {ms_ex,     //42:42
                       ms_mfc0_op,     //41:41
                       ms_gr_we,       //40:37
                       ms_dest,        //36:32
                       ms_final_result //31:0
                       } & {`MS_FWD_BUS_WD{ms_valid}};
endmodule

