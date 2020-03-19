# KnockoutKraken

KnockoutKraken brings FPGA-accelerated simulation to the QFlex family.

KnockoutKraken is composed of three main components: a modified version of QEMU, an instrumented ARM softcore (ARMFlex), and a driver that handles the communication between QEMU and ARMFlex. The vast majority of developers will work on QEMU and/or ARMFlex. QEMU is written in C and can be developed in most Linux machines. ARMFlex is written in Chisel, and while basic testing can be done in most Linux machines, fully simulating and synthesizing the softcore requires an extensive toolchain.

As such, the easiest way to simulate and synthesize KnockoutKraken is by using the [Amazon FPGA Developer AMI](https://aws.amazon.com/marketplace/pp/B06VVYBLZZ). This image has all the software necessary to synthesize and simulate a bitstream for an AWS F1 node. You can also develop on-premise. Please look [here](https://github.com/aws/aws-fpga) for a discussion on how to develop for AWS F1 nodes on-premise.

In the following sections, we will describe how to simulate and synthesize KnockoutKraken

## Run the QEMU+Chisel testbench

The first step is to download and build QEMU. The QEMU repository is located [here](https://github.com/parsa-epfl/qemu/tree/knockoutkraken). Please refer to that repository for instructions on how to build QEMU.


## Generate Verilog

## Simulate KnockoutKraken

## Synthesize ARMFlex
