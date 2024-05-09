`include "mycpu.h"

module id_stage(
    input                          clk           ,
    input                          reset         ,
    input                          flush         ,
    //allowin
    input                          es_allowin    ,
    output                         ds_allowin    ,
    //from fs
    input                          fs_to_ds_valid,
    input  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus  ,
    //to es
    output                         ds_to_es_valid,
    output [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //to fs
    output [`BR_BUS_WD       -1:0] br_bus        ,
    //to rf: for write back
    input  [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus  ,
    //forward
    input  [`ES_FWD_BUS_WD   -1:0] es_to_ds_bus  ,
    input  [`MS_FWD_BUS_WD   -1:0] ms_to_ds_bus  ,
    //interruption mark
    input                          ds_has_int
);

reg         ds_valid   ; //unassigned ds_valid
wire        ds_ready_go;

wire [31                 :0] fs_pc;
reg  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus_r;
assign fs_pc = fs_to_ds_bus[31:0];

wire [31:0] ds_inst;
wire [31:0] ds_pc  ;
wire        ds_bd  ;//delay slot identifier
wire        ds_adel_inst;
wire        ds_bad_inst ;
wire        ds_break    ;
wire        ds_signed   ;

assign {ds_adel_inst,
        ds_bd,
        ds_inst,
        ds_pc
        } = fs_to_ds_bus_r;

//forwarding: from ws
wire        ws_ex;
wire        ws_mfc0_op;
wire [ 3:0] rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
assign {ws_ex     ,//42:42
        ws_mfc0_op,//41:41
        rf_we   ,  //40:37
        rf_waddr,  //36:32
        rf_wdata   //31:0
       } = ws_to_rf_bus;

wire        br_taken;
wire [31:0] br_target;
wire        br      ;

wire [15:0] alu_op;
wire [ 1:0] read_hilo_op;
wire        load_op;
wire        load_ext  ;
wire        src1_is_sa;
wire        src1_is_pc;
wire        src2_is_imm;
wire        src2_is_8;
wire        src2_is_exti;
wire [ 1:0] hilo_we;
wire        res_from_mem;
wire        lh_op;
wire        lb_op;
wire        lwl_op;
wire        lwr_op;
wire [ 3:0] gr_we;
wire        mem_we;
wire        sh_op ;
wire        sb_op ;
wire        swl_op;
wire        swr_op;
wire        mtc0_op;
wire        mfc0_op;
wire        eret_op;      
wire [ 4:0] dest;
wire [ 4:0] dstcp0;
wire [15:0] imm;
wire [31:0] rs_value;
wire [31:0] rt_value;

wire [ 5:0] op;
wire [ 4:0] rs;
wire [ 4:0] rt;
wire [ 4:0] rd;
wire [ 4:0] sa;
wire [ 5:0] func;
wire [25:0] jidx;
wire        er;
wire [18:0] er_c;
wire [ 7:0] mcode;
wire [ 2:0] sel ;
wire [63:0] op_d;
wire [31:0] rs_d;
wire [31:0] rt_d;
wire [31:0] rd_d;
wire [31:0] sa_d;
wire [63:0] func_d;

//control code
//basic calculation
wire        inst_addu;
wire        inst_subu;
wire        inst_slt;
wire        inst_sltu;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_nor;
//shift
wire        inst_sll;
wire        inst_srl;
wire        inst_sra;
wire        inst_sllv;
wire        inst_srlv;
wire        inst_srav;
wire        inst_addiu;
//load immediate
wire        inst_lui;
//load/save byte
wire        inst_lw;
wire        inst_sw;
//jump/branch
wire        inst_beq;
wire        inst_bne;
wire        inst_jal;
wire        inst_jr;
wire        inst_j ;
wire        inst_jalr;
wire        inst_bgez;
wire        inst_bgtz;
wire        inst_blez;
wire        inst_bltz;
wire        inst_bgezal;
wire        inst_bltzal;
//multiply/division
wire        inst_mult;
wire        inst_multu;
wire        inst_div;
wire        inst_divu;
wire        inst_mfhi;
wire        inst_mflo;
wire        inst_mthi;
wire        inst_mtlo;
//load/store byte/half-word
wire        inst_lb ;
wire        inst_lbu;
wire        inst_lh ;
wire        inst_lhu;
wire        inst_sb;
wire        inst_sh;
//unaligned load/store
wire        inst_lwl;
wire        inst_lwr;
wire        inst_swl;
wire        inst_swr;

//exception and CP0 interface
wire        inst_mfc0;
wire        inst_mtc0;
wire        inst_eret;
wire        inst_sysc;
wire        inst_break;      

wire        dst_is_r31;  
wire        dst_is_rt;   

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;

wire        rs_eq_rt;
//forwarding: from es
wire        es_ex     ;
wire        es_mfc0_op;
wire        es_load_op;
wire [ 3:0] es_wb     ;
wire [ 4:0] es_addr;
wire [31:0] es_data;
//forwarding: from ms
wire        ms_ex     ;
wire        ms_mfc0_op;
wire [ 3:0] ms_wb     ;
wire [ 4:0] ms_addr;
wire [31:0] ms_data;
//forwarding logic variables
wire        rs_clash_es;
wire        rs_clash_ms;
wire        rt_clash_es;
wire        rt_clash_ms;
//branch control logic variables
wire        rs_noless_zero ;
wire        rs_large_zero  ;
wire        rs_nolarge_zero;
wire        rs_less_zero   ;

//signed comparison
assign rs_noless_zero  = !rs_value[31]; //positive number(0 included)
assign rs_large_zero   = (!rs_value[31]) && (rs_value!=32'b0);//positive non-zero
assign rs_nolarge_zero = ( rs_value[31]) || (rs_value==32'b0);
assign rs_less_zero    =  rs_value[31];
assign br_bus          = flush ? 34'h0:
                        {br,br_taken,br_target}&{`BR_BUS_WD{ds_valid}};
//bug: load_op unassigned
assign ds_to_es_bus = flush ? 175'h0:
                      {ds_has_int  ,  //174:174
                       ds_signed   ,  //173:173
                       ds_bad_inst ,  //172:172
                       ds_break    ,  //171:171
                       ds_adel_inst,  //170:170
                       ds_bd       ,  //169:169
                       dstcp0      ,  //168:164
                       alu_op      ,  //163:148
                       read_hilo_op,  //147:146
                       load_op     ,  //145:145
                       load_ext    ,  //144:144
                       lh_op       ,  //143:143
                       lb_op       ,  //142:142
                       lwl_op      ,  //141:141
                       lwr_op      ,  //140:140
                       eret_op     ,  //139:139
                       mfc0_op     ,  //138:138
                       mtc0_op     ,  //137:137
                       sysc_ex     ,  //136:136
                       src1_is_sa  ,  //135:135
                       src1_is_pc  ,  //134:134
                       src2_is_imm ,  //133:133
                       src2_is_8   ,  //132:132
                       src2_is_exti,  //131:131
                       hilo_we     ,  //130:129
                       gr_we       ,  //128:125
                       sh_op       ,  //124:124
                       sb_op       ,  //123:123
                       swl_op      ,  //122:122
                       swr_op      ,  //121:121
                       mem_we      ,  //120:120
                       dest        ,  //119:115
                       sel         ,  //114:112
                       imm         ,  //111:96
                       rs_value    ,  //95 :64
                       rt_value    ,  //63 :32
                       ds_pc          //31 :0
                      }&{`DS_TO_ES_BUS_WD{ds_valid}};

assign ds_allowin     = !ds_valid || ds_ready_go && es_allowin;
assign ds_to_es_valid = ds_valid && ds_ready_go;
//stall logic
assign ds_ready_go = !((es_load_op  && (rs_clash_es||rt_clash_es))||es_mfc0_op||ms_mfc0_op||ws_mfc0_op);
always @(posedge clk) begin
    if(reset) begin
        ds_valid <= 0;
    end
    else if(ds_allowin) begin
        ds_valid <= fs_to_ds_valid;
    end
end

always @(posedge clk) begin
    if (fs_to_ds_valid && ds_allowin) begin
        fs_to_ds_bus_r <= fs_to_ds_bus;
    end
end

assign op   = ds_inst[31:26];
assign rs   = ds_inst[25:21];
assign rt   = ds_inst[20:16];
assign rd   = ds_inst[15:11];
assign sa   = ds_inst[10: 6];
assign func = ds_inst[ 5: 0];
assign imm  = ds_inst[15: 0];
assign jidx = ds_inst[25: 0];
assign er   = ds_inst[25   ];
assign er_c = ds_inst[24: 6];
assign mcode= ds_inst[10: 3];
assign sel  = ds_inst[ 2: 0];

decoder_6_64 u_dec0(.in(op  ), .out(op_d  ));
decoder_6_64 u_dec1(.in(func), .out(func_d));
decoder_5_32 u_dec2(.in(rs  ), .out(rs_d  ));
decoder_5_32 u_dec3(.in(rt  ), .out(rt_d  ));
decoder_5_32 u_dec4(.in(rd  ), .out(rd_d  ));
decoder_5_32 u_dec5(.in(sa  ), .out(sa_d  ));

assign inst_add    = op_d[6'h00] & func_d[6'h20] & sa_d[5'h00];
assign inst_addi   = op_d[6'h08];
assign inst_addiu  = op_d[6'h09];
assign inst_addu   = op_d[6'h00] & func_d[6'h21] & sa_d[5'h00];
assign inst_sub    = op_d[6'h00] & func_d[6'h22] & sa_d[5'h00];
assign inst_subu   = op_d[6'h00] & func_d[6'h23] & sa_d[5'h00];
assign inst_mult   = op_d[6'h00] & func_d[6'h18] & sa_d[5'h00];
assign inst_multu  = op_d[6'h00] & func_d[6'h19] & sa_d[5'h00];
assign inst_div    = op_d[6'h00] & func_d[6'h1a] & sa_d[5'h00];
assign inst_divu   = op_d[6'h00] & func_d[6'h1b] & sa_d[5'h00];

assign inst_slt    = op_d[6'h00] & func_d[6'h2a] & sa_d[5'h00];
assign inst_slti   = op_d[6'h0a];
assign inst_sltiu  = op_d[6'h0b];
assign inst_sltu   = op_d[6'h00] & func_d[6'h2b] & sa_d[5'h00];

assign inst_and    = op_d[6'h00] & func_d[6'h24] & sa_d[5'h00];
assign inst_or     = op_d[6'h00] & func_d[6'h25] & sa_d[5'h00];
assign inst_xor    = op_d[6'h00] & func_d[6'h26] & sa_d[5'h00];
assign inst_nor    = op_d[6'h00] & func_d[6'h27] & sa_d[5'h00];
assign inst_andi   = op_d[6'h0c];
assign inst_ori    = op_d[6'h0d];
assign inst_xori   = op_d[6'h0e];

assign inst_sll    = op_d[6'h00] & func_d[6'h00] & rs_d[5'h00];
assign inst_srl    = op_d[6'h00] & func_d[6'h02] & rs_d[5'h00];
assign inst_sra    = op_d[6'h00] & func_d[6'h03] & rs_d[5'h00];
assign inst_sllv   = op_d[6'h00] & func_d[6'h04] & sa_d[5'h00];
assign inst_srlv   = op_d[6'h00] & func_d[6'h06] & sa_d[5'h00];
assign inst_srav   = op_d[6'h00] & func_d[6'h07] & sa_d[5'h00];

assign inst_lui    = op_d[6'h0f] & rs_d[5'h00];
assign inst_lw     = op_d[6'h23];
assign inst_sw     = op_d[6'h2b];
assign inst_beq    = op_d[6'h04];
assign inst_bne    = op_d[6'h05];
assign inst_jal    = op_d[6'h03];
assign inst_jr     = op_d[6'h00] & func_d[6'h08] & rt_d[5'h00] & rd_d[5'h00] & sa_d[5'h00];
assign inst_j      = op_d[6'h02];
assign inst_jalr   = op_d[6'h00] & func_d[6'h09] & rt_d[5'h00] & sa_d[5'h00];

assign inst_mfhi   = op_d[6'h00] & sa_d[5'h00] & func_d[6'h10];
assign inst_mflo   = op_d[6'h00] & sa_d[5'h00] & func_d[6'h12];
assign inst_mthi   = op_d[6'h00] & sa_d[5'h00] & func_d[6'h11];
assign inst_mtlo   = op_d[6'h00] & sa_d[5'h00] & func_d[6'h13];

assign inst_lb     = op_d[6'h20];
assign inst_lbu    = op_d[6'h24];
assign inst_lh     = op_d[6'h21];
assign inst_lhu    = op_d[6'h25];
assign inst_sb     = op_d[6'h28];
assign inst_sh     = op_d[6'h29];

assign inst_lwl    = op_d[6'h22];
assign inst_lwr    = op_d[6'h26];
assign inst_swl    = op_d[6'h2a];
assign inst_swr    = op_d[6'h2e];

assign inst_bgez   = op_d[6'h01] & rt_d[5'h01];
assign inst_bgtz   = op_d[6'h07] & rt_d[5'h00];
assign inst_blez   = op_d[6'h06] & rt_d[5'h00];
assign inst_bltz   = op_d[6'h01] & rt_d[5'h00];

assign inst_bltzal = op_d[6'h01] & rt_d[5'h10];
assign inst_bgezal = op_d[6'h01] & rt_d[5'h11];

assign inst_mfc0   = op_d[6'h10] & rs_d[5'h00] & mcode==8'h00;
assign inst_mtc0   = op_d[6'h10] & rs_d[5'h04] & mcode==8'h00;
assign inst_eret   = op_d[6'h10] & er==1    & er_c==19'h00000 & func_d[6'h18];
assign inst_sysc   = op_d[6'h00] & func_d[6'h0c];
assign inst_break  = op_d[6'h00] & func_d[6'h0d];


assign alu_op[ 0] = inst_addu | inst_addiu | inst_add | inst_addi
                    | inst_lw | inst_sw 
                    | inst_lb | inst_lbu | inst_lh | inst_lhu
                    | inst_sb | inst_sh
                    | inst_lwl | inst_lwr | inst_swl | inst_swr
                    | inst_jal |inst_jalr | inst_bltzal | inst_bgezal
                    | inst_mthi |inst_mtlo;
assign alu_op[ 1] = inst_subu | inst_sub;
assign alu_op[ 2] = inst_slt  | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltiu;
assign alu_op[ 4] = inst_and  | inst_andi;
assign alu_op[ 5] = inst_nor            ;
assign alu_op[ 6] = inst_or   | inst_ori;
assign alu_op[ 7] = inst_xor  | inst_xori;
assign alu_op[ 8] = inst_sll  | inst_sllv;
assign alu_op[ 9] = inst_srl  | inst_srlv;
assign alu_op[10] = inst_sra  | inst_srav;
assign alu_op[11] = inst_lui;
assign alu_op[12] = inst_mult;
assign alu_op[13] = inst_multu;
assign alu_op[14] = inst_div;
assign alu_op[15] = inst_divu;

assign src1_is_sa      = inst_sll | inst_srl | inst_sra;
assign src1_is_pc      = inst_jal | inst_bgezal | inst_bltzal | inst_jalr;
assign src2_is_imm     = inst_addiu | inst_addi | inst_lui 
                         | inst_lw | inst_sw 
                         | inst_lb | inst_lbu |inst_lh |inst_lhu
                         | inst_lwl | inst_lwr | inst_swl | inst_swr
                         | inst_sb | inst_sh
                         | inst_sltiu | inst_slti;
assign src2_is_8       = inst_jal | inst_bgezal | inst_bltzal | inst_jalr;
assign src2_is_exti    = inst_andi | inst_ori | inst_xori;
assign res_from_mem    = inst_lw | inst_lb | inst_lbu | inst_lh | inst_lhu | inst_lwl | inst_lwr;
assign dst_is_r31      = inst_jal | inst_bgezal | inst_bltzal;
assign dst_is_rt       = inst_addiu | inst_addi | inst_lui 
                         | inst_andi | inst_ori | inst_xori
                         | inst_lw | inst_lb | inst_lbu | inst_lh | inst_lhu
                         | inst_lwl | inst_lwr
                         | inst_sltiu | inst_slti
                         | inst_mfc0 | inst_mtc0;//?????
assign gr_we           = (~inst_sw     & ~inst_sb  & ~inst_sh 
                         & ~inst_swl  & ~inst_swr
                         & ~inst_beq  & ~inst_bne & ~inst_jr
                         & ~inst_j & ~inst_bgez & ~inst_bgtz & ~inst_blez & ~inst_bltz
                         & ~inst_mthi & ~inst_mtlo
                         & ~inst_mult & ~inst_multu & ~inst_div & ~inst_divu
                         & ~inst_mtc0 & ~inst_eret & ~inst_break
                         )? 4'hf: 4'h0; //MULT/DIV instructions doesn't need to be written back
assign mem_we          = inst_sw | inst_sb | inst_sh | inst_swl | inst_swr;
assign sh_op           = inst_sh;
assign sb_op           = inst_sb;
assign swl_op          = inst_swl;
assign swr_op          = inst_swr;
//HILO read and write, lower bit enable LO read/write, higher bit enable HI read/write
assign read_hilo_op[0] = inst_mflo;
assign read_hilo_op[1] = inst_mfhi;
assign hilo_we[0]      = inst_mult | inst_multu | inst_div | inst_divu | inst_mtlo;
assign hilo_we[1]      = inst_mult | inst_multu | inst_div | inst_divu | inst_mthi;

assign load_op      = inst_lw | inst_lb | inst_lbu | inst_lh | inst_lhu | inst_lwl | inst_lwr;
assign lh_op        = inst_lh | inst_lhu;
assign lb_op        = inst_lb | inst_lbu;
assign lwl_op       = inst_lwl;
assign lwr_op       = inst_lwr;
assign eret_op      = inst_eret;
assign mfc0_op      = inst_mfc0;
assign mtc0_op      = inst_mtc0;
assign sysc_ex      = inst_sysc;
assign load_ext     = inst_lh | inst_lb;
assign dest         = dst_is_r31 ? 5'd31 :
                      dst_is_rt  ? rt    : 
                                   rd;
assign dstcp0       = rd;

assign rf_raddr1 = rs;
assign rf_raddr2 = rt;

assign ds_break  = inst_break;
assign ds_bad_inst = (  ~inst_add  & ~inst_addi  & ~inst_addu & ~inst_addiu & ~inst_sub & ~inst_subu
                      & ~inst_mult & ~inst_multu & ~inst_div  & ~inst_divu
                      & ~inst_slt  & ~inst_slti  & ~inst_sltu & ~inst_sltiu
                      & ~inst_and  & ~inst_or    & ~inst_nor  & ~inst_xor & ~inst_andi & ~inst_ori & ~inst_xori
                      & ~inst_srl  & ~inst_srlv  & ~inst_sra  & ~inst_sll & ~inst_sllv & ~inst_srav
                      & ~inst_lui  & ~inst_lw    & ~inst_sw
                      & ~inst_lh   & ~inst_lhu   & ~inst_lb   & ~inst_lbu & ~inst_sh   & ~inst_sb
                      & ~inst_lwl  & ~inst_lwr   & ~inst_swl  & ~inst_swr
                      & ~inst_j    & ~inst_jr    & ~inst_jal  & ~inst_jalr
                      & ~inst_beq  & ~inst_bne   & ~inst_bgez & ~inst_bgtz& ~inst_blez & ~inst_bltz
                      & ~inst_bltzal & ~inst_bgezal
                      & ~inst_mfhi & ~inst_mflo  & ~inst_mthi & ~inst_mtlo
                      & ~inst_mfc0 & ~inst_mtc0  & ~inst_sysc & ~inst_break & ~inst_eret);
assign  ds_signed  = (inst_add | inst_sub | inst_addi) & ds_valid;
//register file
regfile u_regfile(
    //id stage read data
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    //wb stage write data
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

//forwarding logic
assign rs_clash_es = (es_wb!=4'b0) && (es_addr==rf_raddr1) && (es_addr!=5'd0);
assign rt_clash_es = (es_wb!=4'b0) && (es_addr==rf_raddr2) && (es_addr!=5'd0);
assign rs_clash_ms = (ms_wb!=4'b0) && (ms_addr==rf_raddr1) && (ms_addr!=5'd0);
assign rt_clash_ms = (ms_wb!=4'b0) && (ms_addr==rf_raddr2) && (ms_addr!=5'd0);
//forward data
assign {es_ex     , //43:43
        es_mfc0_op, //42:42
        es_load_op, //41:41
        es_wb     , //40:37
        es_addr   , //36:32
        es_data     //31:0
        } = es_to_ds_bus;
assign {ms_ex     , //42:42
        ms_mfc0_op, //41:41
        ms_wb     , //40:37
        ms_addr   , //36:32
        ms_data     //31:0
        } = ms_to_ds_bus;
//forward value transfer, unaligned load forward from ms added
wire [31:0] ms_data_fin;
wire [31:0] data_org ;
wire [ 7:0] ms_data0;
wire [ 7:0] ms_data1;
wire [ 7:0] ms_data2;
wire [ 7:0] ms_data3;
//forward byte(s) write support
assign data_org = rs_clash_ms? rf_rdata1: 
                  rt_clash_ms? rf_rdata2: 32'b0;
assign ms_data0 = ({8{ms_wb[0]}} & ms_data[  7:0]) | ({8{(!ms_wb[0])}} & data_org[  7:0]);
assign ms_data1 = ({8{ms_wb[1]}} & ms_data[ 15:8]) | ({8{(!ms_wb[1])}} & data_org[ 15:8]);
assign ms_data2 = ({8{ms_wb[2]}} & ms_data[23:16]) | ({8{(!ms_wb[2])}} & data_org[23:16]);
assign ms_data3 = ({8{ms_wb[3]}} & ms_data[31:24]) | ({8{(!ms_wb[3])}} & data_org[31:24]);

assign ms_data_fin = {ms_data3, ms_data2, ms_data1, ms_data0};
assign rs_value = rs_clash_es? es_data:
                  rs_clash_ms? ms_data_fin:
                  rf_rdata1;
assign rt_value = rt_clash_es? es_data:
                  rt_clash_ms? ms_data_fin:
                  rf_rdata2;

assign rs_eq_rt = (rs_value == rt_value);
assign br       = (   inst_beq
                    || inst_bne
                    || inst_bgez
                    || inst_bgtz
                    || inst_blez
                    || inst_bltz
                    || inst_bltzal
                    || inst_bgezal
                    || inst_jal
                    || inst_jalr
                    || inst_jr
                    || inst_j
                    ) && ds_valid;
assign br_taken = (   inst_beq  &&  rs_eq_rt
                   || inst_bne  && !rs_eq_rt
                   || inst_bgez && rs_noless_zero
                   || inst_bgtz && rs_large_zero
                   || inst_blez && rs_nolarge_zero
                   || inst_bltz && rs_less_zero
                   || inst_bltzal && rs_less_zero
                   || inst_bgezal && rs_noless_zero
                   || inst_jal
                   || inst_jalr
                   || inst_jr
                   || inst_j
                    ) && ds_valid 
                    && !es_ex && !ms_ex && !ws_ex;
assign br_target = (inst_beq || inst_bne || inst_bgez || inst_bgtz || inst_blez || inst_bltz || inst_bltzal || inst_bgezal) ? (fs_pc + {{14{imm[15]}}, imm[15:0], 2'b0}) :
                   (inst_jr || inst_jalr)              ? rs_value :
                  /*inst_jal, inst_j*/              {fs_pc[31:28], jidx[25:0], 2'b0};

endmodule