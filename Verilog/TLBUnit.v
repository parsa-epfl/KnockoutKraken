module TLBUnit( // @[:@469.2]
  input  [63:0] io_vaddr, // @[:@472.4]
  output [63:0] io_paddr // @[:@472.4]
);
  assign io_paddr = io_vaddr >> 2'h2; // @[TLB.scala 49:12:@707.4]
endmodule
