#pragma once

#include "dut.hh"
#include <mutex>

class TopDUT;
class IPCServer;

/**
 * @file define all subroutines.
 */

/**
 * Subroute for accessing DRAM. 
 * 
 * @param dut the DUT
 * 
 * @note it's highly recommend to register this coroutine all the time.
 */
void handleDRAMSubroute(TopDUT &dut);

/**
 * Subroute for handle AXIL request.
 * 
 * @param dut the DUT
 * 
 * @note this will open and listen to a specific unix domain socket.
 */
void AXILRoutine(TopDUT &dut, IPCServer &ipc);

/**
 * Subroutine for handle AXI request.
 * 
 * @param dut the DUT
 * 
 * @note this will open and listen to a specific unix domain socket.
 */
void AXIRoutine(TopDUT &dut, IPCServer &ipc);

