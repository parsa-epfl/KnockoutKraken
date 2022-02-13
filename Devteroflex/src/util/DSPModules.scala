package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._

class MACC(WIDTH: Int, bypass: Boolean = true) extends Module {
  val io = IO(new Bundle(){
    val mult1 = Input(SInt(WIDTH.W))
    val mult2 = Input(SInt(WIDTH.W))
    val add = Input(SInt((2*WIDTH).W))
    val res = Output(SInt((2*WIDTH+1).W))
  })
  private val macc = Module(new MultAdd_3input(WIDTH, true))
  macc.io.clk <> this.clock.asUInt
  macc.io.rst <> this.reset.asUInt
  macc.io.en <> true.B
  macc.io.a <> io.mult1
  macc.io.b <> io.mult2
  macc.io.c <> io.add
  macc.io.p <> io.res
}

class MultAdd_3input(WIDTH: Int, bypass: Boolean = true) extends BlackBox(Map(
  "AWIDTH" -> WIDTH, // Width of multiplier's 1st input
  "BWIDTH" -> WIDTH, // Width of multiplier's 2nd input
  "CWIDTH" -> 2*WIDTH, // Width of Adder input
  "PWIDTH" -> (2*WIDTH + 1), // Output Width
)) with HasBlackBoxInline {
  val io = IO(new Bundle(){
    val clk = Input(Bool())
    val rst = Input(Bool())
    val en = Input(Bool())
    val a = Input(SInt(WIDTH.W))
    val b = Input(SInt(WIDTH.W))
    val c = Input(SInt((2*WIDTH).W))
    val p = Output(SInt((2*WIDTH+1).W))
  })

  if (bypass) {
    setInline("MultAdd_3input.v",
      s"""
        |  // This module describes a Multiplier,3 input adder (a*b + c + p(feedback))
        |  // This can be packed into 1 DSP block (Ultrascale architecture)
        |  // Make sure the widths are less than what is supported by the architecture
        | module MultAdd_3input #(
        |  parameter AWIDTH = 16,  // Width of multiplier's 1st input
        |  parameter BWIDTH = 16,  // Width of multiplier's 2nd input
        |  parameter CWIDTH = 32,  // Width of Adder input
        |  parameter PWIDTH = 33   // Output Width
        | ) (
        |  input clk, // Clock
        |  input rst, // Reset
        |  input en, // Reg enable
        |  input signed [AWIDTH-1:0] a, // Multiplier input
        |  input signed [BWIDTH-1:0] b, // Mutiplier input
        |  input signed [CWIDTH-1:0] c, // Adder input
        | output signed [PWIDTH-1:0] p// Result
        | );
        |
        |  wire signed [AWIDTH-1:0] a_r; // Multiplier input
        |  wire signed [BWIDTH-1:0] b_r; // Mutiplier input
        |  wire signed [CWIDTH-1:0] c_r; // Adder input
        |  wire signed [PWIDTH-1:0] p_r; // Result
        |
        |  assign a_r = a;
        |  assign b_r = b;
        |  assign c_r = c;
        |  assign p_r = a_r * b_r + c_r;
        |  assign p = p_r;
        | endmodule
  """.stripMargin)
  } else {
    setInline("MultAdd_3input.v",
      s"""
        |  // This module describes a Multiplier,3 input adder (a*b + c + p(feedback))
        |  // This can be packed into 1 DSP block (Ultrascale architecture)
        |  // Make sure the widths are less than what is supported by the architecture
        | module MultAdd-3input #(
        |  parameter AWIDTH = 16,  // Width of multiplier's 1st input
        |  parameter BWIDTH = 16,  // Width of multiplier's 2nd input
        |  parameter CWIDTH = 32,  // Width of Adder input
        |  parameter PWIDTH = 33   // Output Width
        | ) (
        |  input clk, // Clock
        |  input rst, // Reset
        |  input en; // Reg enable
        |  input signed [AWIDTH-1:0] a, // Multiplier input
        |  input signed [BWIDTH-1:0] b, // Mutiplier input
        |  input signed [CWIDTH-1:0] c, // Adder input
        |  output signed [PWIDTH-1:0] p;// Result
        | );
        |
        |  reg signed [AWIDTH-1:0] a_r, // Multiplier input
        |  reg signed [BWIDTH-1:0] b_r, // Mutiplier input
        |  reg signed [CWIDTH-1:0] c_r, // Adder input
        |  reg signed [PWIDTH-1:0] p_r; // Result
        |
        |  always @ (posedge clk)
        |  begin
        |   if(rst)
        |   begin
        |    a_r <= 0;
        |    b_r <= 0;
        |    c_r <= 0;
        |    p_r <= 0;
        |   end
        |   else
        |    begin
        |     if(en)
        |     begin
        |      a_r <= a;
        |      b_r <= b;
        |      c_r <= c;
        |      p_r <= a_r * b_r + c_r + p_r;
        |     end
        |    end
        |  end
        |  assign p = p_r;
        | endmodule
  """.stripMargin)
  }
  // */
}
