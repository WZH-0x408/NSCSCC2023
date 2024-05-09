`include "mycpu.h"
module mycpu_top(
    input         clk,
    input         resetn,
    input  [5:0]  ext_int,
    // inst sram interface
    output        inst_sram_en,
    output [ 3:0] inst_sram_wen,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,
    // data sram interface
    output        data_sram_en,
    output [ 3:0] data_sram_wen,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,
    input  [31:0] data_sram_rdata,
    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_wen,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);
reg         reset;
always @(posedge clk) reset <= ~resetn;
wire        stall;
wire         ds_allowin;
wire         es_allowin;
wire         ms_allowin;
wire         ws_allowin;
wire         fs_to_ds_valid;
wire         ds_to_es_valid;
wire         es_to_ms_valid;
wire         ms_to_ws_valid;
wire [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus;
wire [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus;
wire [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus;
wire [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus;
wire [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus;
wire [`WS_TO_FS_BUS_WD -1:0] ws_to_fs_bus;
wire                         ms_to_es_bus;
wire                         ws_to_es_bus;
//forwarding
wire [`ES_FWD_BUS_WD-1:0] es_to_ds_bus;
wire [`MS_FWD_BUS_WD-1:0] ms_to_ds_bus;
wire [`BR_BUS_WD       -1:0] br_bus;
//flush
wire                      ws_to_fs_flush;
wire                      ws_to_ds_flush;
wire                      ws_to_es_flush;
wire                      ws_to_ms_flush;
wire                      ws_back;
wire                      has_int;
// IF stage
if_stage if_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    .flush          (ws_to_fs_flush ),
    //allowin
    .ds_allowin     (ds_allowin     ),
    //brbus
    .br_bus         (br_bus         ),
    .ws_to_fs_bus   (ws_to_fs_bus   ),
    //outputs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),
    // inst sram interface
    .inst_sram_en   (inst_sram_en   ),
    .inst_sram_wen  (inst_sram_wen  ),
    .inst_sram_addr (inst_sram_addr ),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_rdata(inst_sram_rdata),
    .stall          (stall          ),
    .ws_back        (ws_back        )
);
// ID stage
id_stage id_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    .flush          (ws_to_ds_flush ),
    //allowin
    .es_allowin     (es_allowin     ),
    .ds_allowin     (ds_allowin     ),
    //from fs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),
    //to es
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),
    //to fs
    .br_bus         (br_bus         ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
    //forward
    .es_to_ds_bus   (es_to_ds_bus   ),
    .ms_to_ds_bus   (ms_to_ds_bus   ),
    .ds_has_int     (has_int        )
);
// EXE stage
exe_stage exe_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    .flush          (ws_to_es_flush ),
    //allowin
    .ms_allowin     (ms_allowin     ),
    .es_allowin     (es_allowin     ),
    //from ds
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),
    //flush
    .ms_to_es_bus   (ms_to_es_bus   ),
    .ws_to_es_bus   (ws_to_es_bus   ),
    //to ms
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),
    // data sram interface
    .data_sram_en   (data_sram_en   ),
    .data_sram_wen  (data_sram_wen  ),
    .data_sram_addr (data_sram_addr ),
    .data_sram_wdata(data_sram_wdata),
    //forward
    .es_to_ds_bus   (es_to_ds_bus   )
);
// MEM stage
mem_stage mem_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    .flush          (ws_to_ms_flush ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    .ms_allowin     (ms_allowin     ),
    //from es
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),
    //to ws
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    //flush
    .ms_to_es_bus   (ms_to_es_bus   ),
    //from data-sram
    .data_sram_rdata(data_sram_rdata),
    .ms_to_ds_bus   (ms_to_ds_bus   )
);
// WB stage
wb_stage wb_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    //from ms
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
    .ws_to_fs_bus   (ws_to_fs_bus   ),
    .ws_to_es_bus   (ws_to_es_bus   ),
    //flush to ex
    .ws_to_fs_flush   (ws_to_fs_flush   ),
    .ws_to_ds_flush   (ws_to_ds_flush   ),
    .ws_to_es_flush   (ws_to_es_flush   ),
    .ws_to_ms_flush   (ws_to_ms_flush   ),

    //trace debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_wen  (debug_wb_rf_wen  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),
    .stall            (stall            ),
    .ws_back          (ws_back          ),
    .has_int          (has_int          ),
    .ws_ext_in        (ext_int          )
);
endmodule

