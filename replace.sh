#!/bin/bash

# This script will correct the name for AXI Bus so that they can be automatically recognized by Vivado.
# Example: M_AXI_aw_awready -> M_AXI_awready
sed -r -i 's/([MS]_AXI[A-Z_]*_)(aw|w|b|r|ar)_/\1/g' ARMFlexTop_AWS.v 

