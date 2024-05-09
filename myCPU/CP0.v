//�洢�����쳣�������õ��ļĴ�������ʵ��mfc0��mtc0��eret
module CP0(
   input clk            ,
   input reset          ,
   input [ 4:0]dest     ,
   input [ 2:0]sel      ,
   input exc            ,//�Ƿ��������ж�
   input we             ,
   input re             , 
   input eret           ,
   input bd             ,//�Ƿ����ӳٲ���
   input  [31:0]pc      ,//wb_pc
   input  [31:0]wdata   ,
   input  [ 4:0]excode  ,//д�ض�Ӧ��code
   input  [ 5:0]ext_in  ,//���������ж������ź�,������Ϊ0��
   input  [31:0]badVaddr,
   output [31:0]regread ,
   input        inst_adel,
   input        mem_adel ,
   input        mem_ades ,
   output       has_int  //�ⲿ�ж��ж�
   );
reg [31: 0] out;
wire [4: 0] getdest         ;
wire [11:0] degetdest       ;
wire count_eq_compare       ;
reg       c0_status_exl    ;
reg       c0_status_bev    ;
reg [ 7:0]c0_status_im     ;
reg       c0_status_ie     ;
reg [31:0]c0_epc           ;
reg       c0_cause_bd      ;
reg       c0_cause_ti      ;
reg [ 7:0]c0_cause_ip      ;
reg [ 4:0]c0_cause_excode  ;
reg [31:0]c0_badVaddr      ;
reg [31:0]c0_count         ;
reg [31:0]c0_compare       ;
assign getdest = (dest==5'h00) ? 5'h00:
                 (dest==5'h02) ? 5'h01:
                 (dest==5'h03) ? 5'h02:
                 (dest==5'h08) ? 5'h03:
                 (dest==5'h09) ? 5'h04:
                 (dest==5'h0a) ? 5'h05:
                 (dest==5'h0b) ? 5'h06:
                 (dest==5'h0c) ? 5'h07:
                 (dest==5'h0d) ? 5'h08: 
                 (dest==5'h0e) ? 5'h09:
                 (dest==5'h10) & (sel==3'h0) ? 5'h0a:
                 5'h0b;
assign degetdest[0]= (dest==5'h00);
assign degetdest[1]= (dest==5'h02);
assign degetdest[2]= (dest==5'h03);
assign degetdest[3]= (dest==5'h08);
assign degetdest[4]= (dest==5'h09);
assign degetdest[5]= (dest==5'h0a);
assign degetdest[6]= (dest==5'h0b);
assign degetdest[7]= (dest==5'h0c);
assign degetdest[8]= (dest==5'h0d);
assign degetdest[9]= (dest==5'h0e);
assign degetdest[10] = (dest==5'h10) & (sel==3'h0);
assign degetdest[11] = (dest==5'h10) & (sel==3'h1);
assign regread     = degetdest[4] ? c0_count:       //count   
                     degetdest[6] ? c0_compare:       //compare
                     degetdest[7] ? {9'h0,c0_status_bev,6'h0,c0_status_im,6'h0,c0_status_exl,c0_status_ie}:       //status   
                     degetdest[8] ? {c0_cause_bd,c0_cause_ti,14'h0,c0_cause_ip,1'h0,c0_cause_excode,2'h0}:        //cause
                     (degetdest[9]||eret) ? c0_epc   : //epc
                     degetdest[3] ? c0_badVaddr:   
                     32'h00;
assign count_eq_compare = (c0_count==c0_compare);

//status exl
always @(posedge clk)
begin
   if(reset)
      c0_status_exl<=1'b0;
   else if(eret)
      c0_status_exl<=1'b0;
   else if(exc)
      c0_status_exl<=1'b1;
   else if(we && degetdest[7])
      c0_status_exl<=wdata[1];
end

//status bev
always @(posedge clk)
begin
   if(reset)
      c0_status_bev<=1'b1;
end

//status im
always @(posedge clk)
begin
   if(we && degetdest[7])
      c0_status_im<=wdata[15:8];
end

//status ie
always @(posedge clk)
begin
   if(reset)
      c0_status_ie<=1'b0;
   else if(we && degetdest[7])
      c0_status_ie<=wdata[0];
end

//epc
always @(posedge clk)
begin
   if(exc && !c0_status_exl)
      c0_epc <= bd ? pc-3'h4 : pc;
   else if(we && degetdest[9])
      c0_epc <= wdata;
end

//cause bd
always @(posedge clk)
begin
   if(reset)
      c0_cause_bd <= 1'b0;
   else if( exc && !c0_status_exl)
      c0_cause_bd <= bd;
end

//cause ti
always @(posedge clk)
begin
   if(reset)
      c0_cause_ti <= 1'b0;
   else if( we && degetdest[6])
      c0_cause_ti <= 1'b0;
   else if(count_eq_compare)
      c0_cause_ti <= 1'b1;
end

//cause ip
always @(posedge clk)
begin
   if(reset)
      c0_cause_ip[7:2] <= 6'b0;
   else begin
      c0_cause_ip[7]   <= ext_in[5] | c0_cause_ti;
      c0_cause_ip[6:2] <= ext_in[4:0];
   end
end
always @(posedge clk)
begin
   if(reset)
      c0_cause_ip[1:0] <= 2'b0;
   else if(we && degetdest[8])
      c0_cause_ip[1:0] <= wdata[9:8];
end

//cause ex
always @(posedge clk)
begin
   if(reset)
      c0_cause_excode <= 5'b0;
   else if( exc)
      c0_cause_excode <= excode;
end

//count
reg     tic_toc;
always @(posedge clk) begin
    //tick FF
    if(reset) tic_toc <= 1'b0;
    else      tic_toc <= !tic_toc;
    
    if( we && degetdest[4])
        c0_count <= wdata;
    else if(tic_toc)
        c0_count <= c0_count + 1'b1;
end

//compare
always @(posedge clk)
begin
   if(reset)
      c0_compare <= 32'h0;
   else if( we && degetdest[6] )
      c0_compare <= wdata;
end

//badVaddr
always @(posedge clk)
begin
    if(exc && (mem_adel | mem_ades))
        c0_badVaddr <= badVaddr;
    else if(exc && inst_adel)
        c0_badVaddr <= pc;
end

//Interruption generate
assign has_int = ((c0_cause_ip[7:0] & c0_status_im[7:0])!=8'h0) && c0_status_ie==1'b1 && c0_status_exl==1'b0;
endmodule
