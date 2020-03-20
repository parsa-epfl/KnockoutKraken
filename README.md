# KnockoutKraken

KnockoutKraken brings FPGA-accelerated simulation to the QFlex family.

KnockoutKraken is composed of three main components: a modified version of QEMU, an instrumented ARM softcore (ARMFlex), and a driver that handles the communication between QEMU and ARMFlex. The vast majority of developers will work on QEMU and/or ARMFlex. QEMU is written in C and can be developed in most Linux machines. ARMFlex is written in Chisel, and while basic testing can be done in most Linux machines, fully simulating and synthesizing the softcore requires an extensive toolchain.

As such, the easiest way to simulate and synthesize KnockoutKraken is by using the [Amazon FPGA Developer AMI](https://aws.amazon.com/marketplace/pp/B06VVYBLZZ). This image has all the software necessary to synthesize and simulate a bitstream for an AWS F1 node. You can also develop on-premise. Please look [here](https://github.com/aws/aws-fpga) for a discussion on how to develop for AWS F1 nodes on-premise.

In the following sections, we will describe how to simulate, synthesize, and run KnockoutKraken

# Develop KnockoutKraken

## Run the QEMU+Chisel testbench

The first step is to download and build QEMU. The QEMU repository is located [here](https://github.com/parsa-epfl/qemu/tree/knockoutkraken). Please refer to that repository for instructions on how to build QEMU.

Now start QEMU. We assume that `QFLEX_DIR` is the root folder where the QEMU folder is located.
```
$ $QFLEX_DIR/qemu/aarch64-softmmu/qemu-system-aarch64 --machine virt -cpu cortex-a57\
    -smp 1 -m 1G -global virtio-blk-device.scsi=off -device virtio-scsi-device,id=scsi\
    -nographic -rtc clock=vm -icount shift=0,sleep=off\
    -drive if=none,file=$QFLEX_DIR/images/ubuntu16/ubuntu.qcow2,id=hd0\
    -pflash $QFLEX_DIR/images/ubuntu16/flash0.img\
    -pflash $QFLEX_DIR/images/ubuntu16/flash1.img\
    -device scsi-hd,drive=hd0 -device virtio-scsi-device\
    -netdev user,id=net1,hostfwd=tcp::2230-:22\
    -device virtio-net-device,mac=52:54:00:00:00:00,netdev=net1\
    -exton -D /dev/shm/output\
    -singlestep -qflex_d gen,magic_insn\
    -qflex ff=on -fa_qflex enable=on,mode=magic,sim=on -loadext testbench
```

In the command above, most options are default QEMU options, but a few are specific to KnockoutKraken.
```
-singlestep: This option is required to force QEMU to translate instructions individually, 
             as opposed to translating basic blocks. This modification enables us to transplant
             execution to QEMU at a particular step
     
-qflex_d gen,magic_insn: This option enables magic instructions, enabling QEMU to identify instrumented
                         code.

-qflex ff=on : This option enables the QFlex modifications.
-fa_qflex enable=on,mode=magic,sim=on: This option enalbles the modifications for FPGA accelerated execution.
                                       Here we are specifying magic mode and simulation mode which means that 
                                       (magic mode meaning here) and that QEMU will try to communicate with a 
                                       simulator instead of with an FPGA.
-drive if=none,file=$QFLEX_DIR/images/ubuntu16/ubuntu.qcow2,id=hd0 : This is a regular QEMU option that specifies
                                                                     the image. You will have to modify it if you use another
                                                                     image.
-loadext testbench: This is a regular QEMU option that loads a checkpoint. "testbench" is the name of the checkpoint.
```

This command will start QEMU, and lead the user to a shell in the target machine. Any commands send to this shell will be executed in the target machine.

To start the instrumented test, run:
```
$ ./matmul
```

## Simulate KnockoutKraken
Open another therminal and, on the same machine, go to the KnockoutKraken repository and start the test using `sbt`.

```
$ cd armflex
$ sbt # start SBT Shell
$(in sbt shell) test:runMain armflex.SimulatorMain SIM_STATE SIM_LOCK SIM_CMD QEMU_STATE QEMU_LOCK QEMU_CMD PROGRAM_PAGE 4096 /dev/shm/qflex

```

This will start the simulation. The simulator generates a lot of output, so we recomend you keep track of the output in 
different terminals. To do so, open three terminals. To observe the output from QEMU, run:
```
$ tail -f /dev/shm/output
```

On the second, to observe the output of the Chisel simulation run:
```
$ tail -f /dev/shm/outputSim
```

And on the third to see the output of all the simulations, run:
```
$ tail -f /dev/shm/output >> /dev/merged $
$ tail -f /dev/shm/outputSim >> /dev/merged $
$ tail -f /dev/merged # pretty output
```

The output will show all the events in the simulation. The simulation runs every instruction on both QEMU and 
the softcore, and reports on inconsistencies. Here is a guide on the output format:
```
RLT:OUT: Shows the commited PC and Instruction code of an instruction commited in Chisel
IN[0] : Shows the commited instruction in QEMU
```

Here is an example of an instruction executed correctly. In the example below, `tbz` executed
and the result of the RTL and QEMU matched.

```
RTL:OUT:0x0000ffffac213aa4:  36180062
IN[0]  :0x0000ffffac213aa4:  36180062      tbz w2, #3, #+0xc (addr 0xffffac213ab0)
```

Whenever there is a mismatch, you will see a message with the following format:
```
RTL:PState didn't match, differences FPGA - QEMU:
 X1:0x0000000029650aa0 != 0x0000ffffabf12028
```

This message indicates which register did not match, and the values found in Chisel and in QEMU. 

In the example below, we have a load that misses in the FPGA memory, and fetches data from QEMU. After
the data is fetched, there is still a mismatch. The `QEMU:REQ:` message shows the page that was fetched
from QEMU and the first `RTL:` message indicates the page that missed on Chisel.
```
 QEMU:REQ:    PAGE:0x0000ffffabf12020
 RTL:0000ffffabf12020:BRAM:104:MISSED:DATA_LOAD :0
 RTL:OUT:0x0000ffffac213aa8:  f8408423
 IN[0]  :0x0000ffffac213aa8:  f8408423      ldr x3, [x1], #8
      LDST:0x0000ffffabf12020
 RTL:PState didn't match, differences FPGA - QEMU:
 X1:0x0000000029650aa0 != 0x0000ffffabf12028
 ```
Overall, simulation speed in this mode is very slow, we are activelly working to improve simulation speed.

## Generate Verilog

## Synthesize ARMFlex
Once your simulation works fine, you can synthesize ARMFlex. You will have to do it on a machine that has all the tools required by AMS F1. See above for a description.

After generating verilog files in the `<armflex repo>/Verilog` folder you can synthesize the design to create design check-point and AFI image. Go to the directory `<armflex repo>/aws`

```
$ cd <armflex repo>/aws
$ ./aws_build_dcp.sh
```
This will take some time (several hours) and generate a design checkpoint in the `<armflex repo>/aws/armflex.runs/faas_1/build/checkpoints/to_aws/` folder.

# Run KnockoutKraken

The easiest way to run KnockoutKraken is to get it through our Amazon AMI image, already has QEMU built and the FPGA image of ARMFlex, our instrumented ARM softcore. To do so, send an email to `qflex_knockoutkraken@groupes.epfl.ch` with your AWS user ID and we will give you access to the image.

## Start an FPGA instance
Once you received a confirmation from us, go to the AWS EC2 service, then to the AMI image section and choose a private image. You should be able to see the shared AMI (i.e. <armflex_dev_v1 ami-0891cda4dca10d171> in the figure below).

IMAGE GOES HERE

Launch the AMI image and select f1.2xlarge as the instance type. This instance type is only available in some regions. Also, sometimes AWS requires additional information before allowing users to launch f1.XX nodes. While the process is quick, you might not get immediate access to the nodes the first time you try. Please plan accordingly.

While configuring your image, do not forget to choose or create a key pair for login. See [here](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html) for further instructions on AWS key pairs.

Once the instance is initialized, you can ssh to the image using the key you just created:
```
$ ssh -i <location to key> centos@<node name>
 ```
 
## Generate an AFI image from a binary

Create an s3 bucket for the FPGA image if you do not have one already. You can more find instructions [here](https://github.com/aws/aws-fpga/blob/master/SDAccel/docs/Setup_AWS_CLI_and_S3_Bucket.md).
```
$ aws s3 mb s3://<bucket_name eg: armflex> --region <region name eg: eu-west-1>
 ```
 
Modify the `generate_afi.sh` script located in `/home/centos`. Change the `bucket_name` variable (line 2) with the bucket name you picked. Run `generate_afi.sh` to generate an AFI image with the following command:
```
$ /home/centos/generate_afi.sh
```

## Load the AFI image
After `generate_afi.sh` completes, a new file will appear in the folder `/home/centos` with the name `afi_info`. Open that file and note down the ID of the image generated.
```
$ cat /home/centos/afi_info
>>> agfi-XXX afi-XXX
# You want the value of "agfi-xxx"
```

Now you can load the image on the FPGA. First, clear any FPGA image in socket 0
```
$ sudo fpga-clear-local-image  -S 0
```

Now, load the image on the FPGA.
```
$ sudo fpga-load-local-image -S 0 -I <agfi-ID>
```

Check whether the image is properly loaded.
```
$ sudo fpga-describe-local-image -S 0 -H
```

## Launch the driver/ARMFlex shell
Open a terminal in your AWS F1 node and source the AWS SDK.
```
$ source /home/centos/aws_sdk_source
```

Run the armflex driver shell.

```
$ sudo armflex_shell/armflex_shell
```

The driver is now waiting for commands from QFlex

## Launch QFlex
Start another terminal in your AWS F1 node. You can also use `tmux`, which is already installed in our image.
Run Qflex with the script `/home/centos/run_qflex.sh`. 
```
$ /home/centos/run_qflex.sh
```

This script starts the instrumented QEMU on a pre-copied image. You can run your own image if needed. Any QEMU aarch64 image snapshot can be used instead of the one already provided. After QEMU starts, press enter a few times to get access to the target machine terminal.

We provide a sample program that is already instrumented for KnockoutKraken. You can run it from the target machine terminal.
```
$ ./a.out
```

To abort QEMU execution, run the following command on another terminal on the AWS F1 node.
```
$ pgrep "qemu" | xargs sudo kill -9
```


