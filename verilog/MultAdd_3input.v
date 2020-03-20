
  // This module describes a Multiplier,3 input adder (a*b + c + p(feedback))
  // This can be packed into 1 DSP block (Ultrascale architecture)
  // Make sure the widths are less than what is supported by the architecture
 module MultAdd_3input #(
  parameter AWIDTH = 16,  // Width of multiplier's 1st input
  parameter BWIDTH = 16,  // Width of multiplier's 2nd input
  parameter CWIDTH = 32,  // Width of Adder input
  parameter PWIDTH = 33   // Output Width
 ) (
  input clk, // Clock
  input rst, // Reset
  input en, // Reg enable
  input signed [AWIDTH-1:0] a, // Multiplier input
  input signed [BWIDTH-1:0] b, // Mutiplier input
  input signed [CWIDTH-1:0] c, // Adder input
 output signed [PWIDTH-1:0] p// Result
 );

  wire signed [AWIDTH-1:0] a_r; // Multiplier input
  wire signed [BWIDTH-1:0] b_r; // Mutiplier input
  wire signed [CWIDTH-1:0] c_r; // Adder input
  wire signed [PWIDTH-1:0] p_r; // Result

  assign a_r = a;
  assign b_r = b;
  assign c_r = c;
  assign p_r = a_r * b_r + c_r;
  assign p = p_r;
 endmodule
  