`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2023/07/25 21:24:45
// Design Name: 
// Module Name: hilo
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

module hilo(
    input         clk,

    input[ 1:0]   we,
    input[31:0]   hi_in,
    input[31:0]   lo_in,
    
    output[31:0]  hi_read,
    output[31:0]  lo_read
);

reg[31:0]  hi;
reg[31:0]  lo;

//register write
always @ (posedge clk) begin
    if(we==2'b01) begin
        lo <= lo_in;
    end
    else if(we==2'b10) begin
        hi <= hi_in;
    end
    else if(we==2'b11) begin
        hi <= hi_in;
        lo <= lo_in;
    end
end

//register read
assign hi_read = hi;
assign lo_read = lo;
endmodule
