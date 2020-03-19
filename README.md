# KnockoutKraken

KnockoutKraken brings FPGA-accelerated simulation to the QFlex family.

KnockoutKraken is composed of three main components: a modified version of QEMU, an instrumented ARM softcore (ARMFlex), and a driver that handles the communication between QEMU and ARMFlex. The vast majority of developers will work on QEMU and/or ARMFlex. QEMU is written in C and can be developed in most Linux machines. ARMFlex is written in Chisel, and while basic testing can be done in most Linux machines, fully simulating and synthesizing the softcore requires an extensive toolchain.

As such, the easiest way to simulate and synthesize KnockoutKraken is by using the [Amazon FPGA Developer AMI](https://aws.amazon.com/marketplace/pp/B06VVYBLZZ). This image has all the software necessary to synthesize and simulate a bitstream for an AWS F1 node. You can also develop on-premise. Please look [here](https://github.com/aws/aws-fpga) for a discussion on how to develop for AWS F1 nodes on-premise.

In the following sections, we will describe how to simulate and synthesize KnockoutKraken

# Develop KnockoutKraken

## Run the QEMU+Chisel testbench

The first step is to download and build QEMU. The QEMU repository is located [here](https://github.com/parsa-epfl/qemu/tree/knockoutkraken). Please refer to that repository for instructions on how to build QEMU.


## Generate Verilog

## Simulate KnockoutKraken

## Synthesize ARMFlex

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
Now that you created the image, you can load it on the FPGA. First, clear any FPGA image in socket 0
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

To abort QFlex execution, run the following command on another terminal on the AWS F1 node.
```
$ pgrep "qemu" | xargs sudo kill -9
```


