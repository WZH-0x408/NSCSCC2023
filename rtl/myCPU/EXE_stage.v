`include "mycpu.h"

module exe_stage(
    input                          clk           ,
    input                          reset         ,
    input                          flush         ,
    //allowin
    input                          ms_allowin    ,
    output                         es_allowin    ,
    //from ds
    input                          ds_to_es_valid,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //ex
    input                          ms_to_es_bus  ,
    input                          ws_to_es_bus  ,
    //to ms
    output                         es_to_ms_valid,
    output [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    // data sram interface
    output        data_sram_en   ,
    output [ 3:0] data_sram_wen  ,
    output [31:0] data_sram_addr ,
    output [31:0] data_sram_wdata,
    //forwarding
    output [`ES_FWD_BUS_WD   -1:0] es_to_ds_bus  
);

reg         es_valid      ;
wire        es_ready_go   ;

reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire        es_ex          ;
wire        es_has_int     ;
wire        es_alu_overflow;
wire [31:0] es_bad_addr    ;
wire        es_signed_op   ;
wire        es_bad_inst    ;
wire        es_break       ;
wire        es_adel_inst   ;
wire        es_adel_mem    ;
wire        es_ades_mem    ;
wire [15:0] es_alu_op      ;
wire        es_load_op     ;
wire        es_lh_op       ;
wire        es_lb_op       ;
wire        es_lwl_op      ;
wire        es_lwr_op      ;
wire        es_load_ext    ;
wire        es_eret_op     ;
wire        es_mfc0_op     ;
wire        es_mtc0_op     ;
wire        es_sysc_ex     ; 
wire [ 1:0] es_load_offset ;
wire [ 1:0] es_read_hilo   ;
wire        es_src1_is_sa  ;  
wire        es_src1_is_pc  ;
wire        es_src2_is_imm ; 
wire        es_src2_is_8   ;
wire        es_src2_is_exti;
wire [ 1:0] es_hilo_we     ;
wire [ 3:0] es_gr_we       ;
wire        es_mem_we      ;
wire [ 4:0] es_dest        ;
wire [ 4:0] es_dstcp0      ;
wire [15:0] es_imm         ;
wire [31:0] es_rs_value    ;
wire [31:0] es_rt_value    ;
wire [31:0] es_pc          ;
wire        es_sh_op       ;
wire        es_sb_op       ;
wire        es_swl_op      ;
wire        es_swr_op      ;
wire        es_bd          ;
wire [ 2:0] es_sel         ;
wire        ms_ex          ;
wire        ws_ex          ;
wire [ 1:0] hilo_we        ;
assign      ws_ex = ws_to_es_bus;
assign      ms_ex = ms_to_es_bus;
assign {es_has_int    ,   //174:174
        es_signed_op   ,  //173:173
        es_bad_inst    ,  //172:172
        es_break       ,  //171:171
        es_adel_inst   ,  //170:170
        es_bd          ,  //169:169
        es_dstcp0      ,  //168:164
        es_alu_op      ,  //163:148
        es_read_hilo   ,  //147:146
        es_load_op     ,  //145:145
        es_load_ext    ,  //144:144
        es_lh_op       ,  //143:143
        es_lb_op       ,  //142:142
        es_lwl_op      ,  //141:141
        es_lwr_op      ,  //140:140
        es_eret_op     ,  //139:139
        es_mfc0_op     ,  //138:138
        es_mtc0_op     ,  //137:137
        es_sysc_ex     ,  //136:136
        es_src1_is_sa  ,  //135:135
        es_src1_is_pc  ,  //134:134
        es_src2_is_imm ,  //133:133
        es_src2_is_8   ,  //132:132
        es_src2_is_exti,  //131:131
        es_hilo_we     ,  //130:129
        es_gr_we       ,  //128:125
        es_sh_op       ,  //124:124
        es_sb_op       ,  //123:123
        es_swl_op      ,  //122:122
        es_swr_op      ,  //121:121
        es_mem_we      ,  //120:120
        es_dest        ,  //119:115
        es_sel         ,  //114:112
        es_imm         ,  //111:96
        es_rs_value    ,  //95 :64
        es_rt_value    ,  //63 :32
        es_pc             //31 :0
       } = ds_to_es_bus_r; //bug3: loadop unassigned, refer to ID stage to fix

wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;
wire [31:0] es_alu_result2;
wire [31:0] es_alu_result_fin;
wire [31:0] es_alu_result2_fin;

//div&divu control
wire  es_dividend_tready;
wire  es_divisor_tready ;
wire  es_dividend_tvalid;
wire  es_divisor_tvalid ; 
wire  es_dout_tvalid    ;
wire  es_dout_tvalid_u  ;
//for control handshake flip-flop
reg   es_dividend_tvalid_r;
reg   es_divisor_tvalid_r ;
//div&divu data
wire  [63:0] es_dout_tdata_div ;
wire  [63:0] es_dout_tdata_divu;
wire  [63:0] es_dout_tdata     ;

//MFHI/MFLO support
wire [31:0] es_hi_read       ;
wire [31:0] es_lo_read       ;

//final result
wire [31:0] es_final_result  ;
wire        es_res_from_mem  ;
wire        instj0;
wire        instj1;
wire [ 3:0] instget;

//interface with sram
wire [ 1:0] es_store_offset;   
wire [ 3:0] es_sb_wen   ;
wire [ 3:0] es_sh_wen   ;
wire [ 3:0] es_swl_wen  ;
wire [ 3:0] es_swr_wen  ;
wire [31:0] es_swl_data ;
wire [31:0] es_swr_data ;
wire [ 3:0] es_gr_we_lwl;
wire [ 3:0] es_gr_we_lwr;
wire [ 3:0] es_gr_we_fin;   
//load_op decides final result for writeback from HILO & ALU or MEM
assign es_res_from_mem = es_load_op;

//reading from HI/LO is irrelevant to ALU
assign es_final_result = es_read_hilo[0]? es_lo_read:
                         es_read_hilo[1]? es_hi_read:
                         es_alu_result_fin ;
                         
assign es_to_ms_bus = flush ? 165'h0:
                      {es_has_int       ,  //164:164
                       es_alu_overflow  ,  //163:163
                       es_bad_addr      ,  //162:131
                       es_bad_inst      ,  //130:130
                       es_break         ,  //129:129
                       es_adel_inst     ,  //128:128
                       es_adel_mem      ,  //127:127
                       es_ades_mem      ,  //126:126
                       es_bd            ,  //125:125
                       es_rt_value      ,  //124:93
                       es_dstcp0        ,  //92:88
                       es_res_from_mem  ,  //87:87
                       es_gr_we_fin     ,  //86:83
                       es_lh_op         ,  //82:82
                       es_lb_op         ,  //81:81
                       es_lwl_op        ,  //80:80
                       es_lwr_op        ,  //79:79
                       es_eret_op       ,  //78:78
                       es_mfc0_op       ,  //77:77
                       es_mtc0_op       ,  //76:76
                       es_sysc_ex       ,  //75:75
                       es_load_offset   ,  //74:73
                       es_load_ext      ,  //72:72
                       es_dest          ,  //71:67
                       es_sel           ,  //66:64
                       es_final_result  ,  //63:32
                       es_pc               //31:0
                      }&{`ES_TO_MS_BUS_WD{es_valid}};
assign instget = es_final_result[31:28];
assign instj0  = (instget==4'h8)||(instget==4'ha);
assign instj1  = (instget==4'h9)||(instget==4'hb);
assign es_ready_go    = es_alu_op[14]? es_dout_tvalid: 
                        es_alu_op[15]? es_dout_tvalid_u: 
                        1'b1; //stall formulti-clock period division
assign es_allowin     = !es_valid || es_ready_go && ms_allowin;
assign es_to_ms_valid =  es_valid && es_ready_go;
always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
    end
    else if (es_allowin) begin
        es_valid <= ds_to_es_valid;
    end

    if (ds_to_es_valid && es_allowin) begin
        ds_to_es_bus_r <= ds_to_es_bus;
    end
end

assign hilo_we = {!es_sysc_ex && !ms_ex && !ws_ex && es_hilo_we[1],!es_sysc_ex && !ms_ex && !ws_ex && es_hilo_we[0]};
assign es_alu_src1 = es_src1_is_sa  ? {27'b0, es_imm[10:6]} : 
                     es_src1_is_pc  ? es_pc[31:0] :
                                      es_rs_value;                           
//ALU: unsigned extension added
assign es_alu_src2 = es_src2_is_imm ? {{16{es_imm[15]}}, es_imm[15:0]} : 
                     es_src2_is_8   ? 32'd8 :
                     es_src2_is_exti ? {{16{1'b0}}     , es_imm[15:0]} :
                                      es_rt_value;
wire alu_overflow_ind;
alu u_alu(
    .alu_op      (es_alu_op     ),
    .alu_src1    (es_alu_src1   ),
    .alu_src2    (es_alu_src2   ),
    .alu_result  (es_alu_result ),
    .alu_result2 (es_alu_result2), 
    .overflow    (alu_overflow_ind)//higer 32 bits for mult/div
    );
assign es_alu_overflow = es_signed_op & alu_overflow_ind;

//HILO interface
hilo myhilo(
    .clk        (clk           ),
    .we         (hilo_we       ),
    .hi_in      (es_alu_result2_fin),
    .lo_in      (es_alu_result_fin ),
    .hi_read    (es_hi_read    ),
    .lo_read    (es_lo_read    )
    );

//non-blocking
assign es_dividend_tready = 1'b1;
assign es_divisor_tready  = 1'b1;
//flip-flop for access control, 1 when ongoing division
reg    div_ongo = 1'b0;
always @(posedge clk)
begin
    if((es_alu_op[14] | es_alu_op[15]) & ~div_ongo)
    begin
        es_divisor_tvalid_r  <= 1;
        es_dividend_tvalid_r <= 1;
    end
    else if(es_dividend_tvalid && es_divisor_tvalid)
    begin
        es_divisor_tvalid_r  <= 0;
        es_dividend_tvalid_r <= 0;       
    end
end
//control handshake and di
always @(posedge clk)
begin
    if((es_alu_op[14] | es_alu_op[15]) & ~div_ongo)
    begin //when performing div(u), dout_valid = division not in process
        div_ongo <= 1;
    end
    else if(div_ongo)
    begin
        div_ongo <= ~es_dout_tvalid;
    end
    else begin
        div_ongo <= 0;
    end
end
assign  es_divisor_tvalid  = es_divisor_tvalid_r ;
assign  es_dividend_tvalid = es_dividend_tvalid_r; 

//IP use
div_gen_0 mydiv(
    .aclk(clk),
    .s_axis_divisor_tvalid(es_divisor_tvalid),
    .s_axis_divisor_tdata(es_alu_src2),
    .s_axis_dividend_tvalid(es_dividend_tvalid),
    .s_axis_dividend_tdata(es_alu_src1),
    .m_axis_dout_tvalid(es_dout_tvalid),
    .m_axis_dout_tdata(es_dout_tdata_div)
);

div_gen_1 mydivu(
    .aclk(clk),
    .s_axis_divisor_tvalid(es_divisor_tvalid),
    .s_axis_divisor_tdata(es_alu_src2),
    .s_axis_dividend_tvalid(es_dividend_tvalid),
    .s_axis_dividend_tdata(es_alu_src1),
    .m_axis_dout_tvalid(es_dout_tvalid_u),
    .m_axis_dout_tdata(es_dout_tdata_divu)
);
//signed/unsigned MUX
assign es_dout_tdata = es_alu_op[14] ? es_dout_tdata_div :
                       es_alu_op[15] ? es_dout_tdata_divu:
                                       64'b0;
assign es_alu_result2_fin = ((es_alu_op[14] & es_dout_tvalid) | (es_alu_op[15] & es_dout_tvalid_u)) ? es_dout_tdata[31:0]:
                            (es_alu_op[12] | es_alu_op[13]) ? es_alu_result2 : es_alu_result;
assign es_alu_result_fin  = ((es_alu_op[14] & es_dout_tvalid) | (es_alu_op[15] & es_dout_tvalid_u)) ? es_dout_tdata[63:32]:
                            (es_lh_op | es_lb_op | es_sh_op | es_sb_op | es_swl_op | es_swr_op | es_lwl_op | es_lwr_op) ? {es_alu_result[31:2],2'b00}: //                                       
                            es_alu_result;      
assign es_load_offset     = (es_lh_op | es_lb_op | es_lwl_op | es_lwr_op) ? es_alu_result[1:0] : 2'b00;

                                                                       
assign data_sram_en    = 1'b1;
assign data_sram_addr  = instj0?{4'h0,es_final_result[27:0]}:
                         instj1?{4'h1,es_final_result[27:0]}:
                         es_final_result;
//for SB/SH
assign es_store_offset = es_alu_result[1:0];
assign es_sb_wen    = (es_store_offset==2'b00)? 4'b0001:
                      (es_store_offset==2'b01)? 4'b0010:
                      (es_store_offset==2'b10)? 4'b0100:
                      (es_store_offset==2'b11)? 4'b1000: 4'b0000;
assign es_sh_wen    = (es_store_offset==2'b00)? 4'b0011:
                      (es_store_offset==2'b10)? 4'b1100: 4'b0000;
assign es_swl_wen   = (es_store_offset==2'b00)? 4'b0001:
                      (es_store_offset==2'b01)? 4'b0011:
                      (es_store_offset==2'b10)? 4'b0111:
                      (es_store_offset==2'b11)? 4'b1111: 4'b0000;
assign es_swr_wen   = (es_store_offset==2'b11)? 4'b1000:
                      (es_store_offset==2'b10)? 4'b1100:
                      (es_store_offset==2'b01)? 4'b1110:
                      (es_store_offset==2'b00)? 4'b1111: 4'b0000;                                             
assign data_sram_wen   = !flush && !es_ex && !ws_ex && !ms_ex &&es_mem_we && es_valid ? (es_sb_op?  es_sb_wen:
                                                  es_sh_op?  es_sh_wen: 
                                                  es_swl_op? es_swl_wen:
                                                  es_swr_op? es_swr_wen: 4'hf) : 4'h0;
assign es_swl_data     = (es_store_offset==2'b00)? {24'b0, es_rt_value[31:24]}:
                         (es_store_offset==2'b01)? {16'b0, es_rt_value[31:24], es_rt_value[23:16]}:
                         (es_store_offset==2'b10)? {8'b0,  es_rt_value[31:24], es_rt_value[23:16], es_rt_value[15:8]}:
                         es_rt_value;
assign es_swr_data     = (es_store_offset==2'b00)? es_rt_value:
                         (es_store_offset==2'b01)? {es_rt_value[23:16], es_rt_value[15:8], es_rt_value[7:0], 8'b0}:
                         (es_store_offset==2'b10)? {es_rt_value[ 15:8], es_rt_value[ 7:0], 16'b0}:
                         {es_rt_value[7:0], 24'b0};
assign data_sram_wdata = es_sb_op ? {4{es_rt_value[7:0]}} : 
                         es_sh_op ? {2{es_rt_value[15:0]}}:
                         es_swl_op? es_swl_data:
                         es_swr_op? es_swr_data:
                         es_rt_value;
assign es_gr_we_lwl    = (es_load_offset==2'b00)? 4'b1000:
                         (es_load_offset==2'b01)? 4'b1100:
                         (es_load_offset==2'b10)? 4'b1110:
                         (es_load_offset==2'b11)? 4'b1111: 4'b0000;
assign es_gr_we_lwr    = (es_load_offset==2'b11)? 4'b0001:
                         (es_load_offset==2'b10)? 4'b0011:
                         (es_load_offset==2'b01)? 4'b0111:
                         (es_load_offset==2'b00)? 4'b1111: 4'b0000;
assign es_gr_we_fin    = es_lwl_op? es_gr_we_lwl: 
                         es_lwr_op? es_gr_we_lwr:
                         es_gr_we;
//badVaddr exception
wire   es_lw_op;
wire   es_sw_op;
assign es_lw_op = es_load_op & !es_lh_op & !es_lb_op & !es_lwl_op & !es_lwr_op;
assign es_sw_op = es_mem_we  & !es_sh_op & !es_sb_op & !es_swl_op & !es_swr_op;
assign es_adel_mem = es_lw_op? (es_alu_result[1:0]!=2'b00):
                     es_lh_op? (es_alu_result[0]  != 1'b0):1'b0;
assign es_ades_mem = es_sw_op? (es_alu_result[1:0]!=2'b00):
                     es_sh_op? (es_alu_result[0]  != 1'b0):1'b0;
assign es_bad_addr = es_alu_result;
assign es_ex       = es_alu_overflow | es_adel_mem | es_ades_mem | es_adel_inst
                   | es_break    | es_bad_inst | es_sysc_ex
                   | es_has_int;
//forward
assign es_to_ds_bus = flush? 44'h0:
                      {es_ex,   //43:43
                       es_mfc0_op,   //42:42
                       es_load_op,   //41:41
                       es_gr_we_fin, //40:37, to tell if write to rf needed
                       es_dest,      //36:32
                       es_final_result //31:0
                       } & {`ES_FWD_BUS_WD{es_valid}};
endmodule
