
// Dual-Port Block RAM with Two Write Ports

module BlackBoxBRAMTDP #(
                         parameter ADDR_WIDTH_A = 10,
                         parameter DATA_WIDTH_A = 36,
                         parameter ADDR_WIDTH_B = 10,
                         parameter DATA_WIDTH_B = 36,
                         parameter SIZE_A       = 1024,
                         parameter SIZE_B       = 1024)
   (clk,enA,enB,weA,weB,addrA,addrB,diA,diB,doA,doB);

   input clk,enA,enB,weA,weB;
   input [ADDR_WIDTH_A-1:0] addrA;
   input [ADDR_WIDTH_B-1:0] addrB;
   input [DATA_WIDTH_A-1:0] diA;
   input [DATA_WIDTH_B-1:0] diB;
   output [DATA_WIDTH_A-1:0] doA;
   output [DATA_WIDTH_B-1:0] doB;

`define max(a,b) {(a) > (b) ? (a) : (b)}
`define min(a,b) {(a) < (b) ? (a) : (b)}

   function integer log2;
      input integer          value;
      reg [31:0]             shifted;
      integer                res;
      begin
         if (value < 2)
  	       log2 = value;
         else
           begin
  	          shifted = value-1;
  	          for (res=0; shifted>0; res=res+1)
  		          shifted = shifted>>1;
  	          log2 = res;
           end
      end
   endfunction

   localparam maxSIZE  = `max(SIZE_A, SIZE_B);
   localparam maxWIDTH = `max(DATA_WIDTH_A, DATA_WIDTH_B);
   localparam minWIDTH = `min(DATA_WIDTH_A, DATA_WIDTH_B);

   localparam RATIO = maxWIDTH / minWIDTH;
   localparam log2RATIO = log2(RATIO);

   reg[minWIDTH-1:0] ram [0:maxSIZE-1];
   reg [DATA_WIDTH_A-1:0] readA;
   reg [DATA_WIDTH_B-1:0] readB;

   always @(posedge clk)
     begin
        if (enB)
          begin
             if (weB)
               ram[addrB] <= diB;
             readB <= ram[addrB];
          end
     end

   always @(posedge clk)
     begin
        if (enA)
          begin
             if (weA)
               ram[addrA] <= diA;
             readA <= ram[addrA];
          end
     end

   assign doA = readA;
   assign doB = readB;

endmodule
  