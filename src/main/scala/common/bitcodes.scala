/* Source: ARM Architecture Reference Manual
 *          ARMv8, for ARMv8-A architecture profile
 *
 * A64 Instruction Set Encoding
**/

//----------------------------------------------------------------------
// Bitcodes
//----------------------------------------------------------------------

/* Table C3-1 A64 main encoding table */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 04 03 02 01 00 | Encoding Group                                       */
/*  -  -  -  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | UNALLOCATED                                          */
/*  -  -  -  1  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Data processing - immediate                          */
/*  -  -  -  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Branch, exception generation and system instructions */
/*  -  -  -  -  1  -  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Loads and stores                                     */
/*  -  -  -  -  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Data processing - register                           */
/*  -  -  -  0  1  1  1  -  -  - it -  -  -  -  -  -  -  -  -  -  -  |  -  -  -  -  -  -  -  -  -  - | Data processing - SIMD and floating point            */
/*  -  -  -  1  1  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Data processing - SIMD and flotating point           */

/************************************************************************************************************************************************************/
/************************************************************************************************************************************************************/
/************************************************************************************************************************************************************/


/* Branch, exception genereation and system instructions */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 04 03 02 01 00 | */
/*  -  -  -  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | */

/* Table C3-2 Encoding table for the Branches, Exception Generating and System instructions functional group */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 04 03 02 01 00 |                                  */
/*  -  0  0  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Uncoditional branch (immediate)  */
/*  -  0  1  1  0  1  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Compare & branch (immediate)     */
/*  -  0  1  1  0  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Test & branch (immediate)        */
/*  0  1  0  1  0  1  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Conditional branch (immediate)   */
/*  1  1  0  1  0  1  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Exception generation             */
/*  1  1  0  1  0  1  0  1  0  0  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | System                           */
/*  1  1  0  1  0  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |  -  -  -  -  -  -  -  -  -  - | Unconditional branch (register)  */

/* Unconditional branch (immediate)      Bits[31:26] */
/* 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 | Instruction Page | Variant */
/* op |  0  0  1  0  1 |                                   imm26                                       |                  |    -    */
/*  0 |  0  0  1  0  1 |                                   imm26                                       | B                |    -    */
/*  1 |  0  0  1  0  1 |                                   imm26                                       | BL               |    -    */

/* Compare & branch (immediate)          Bits[31:24] */
/* 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant   */
/* sf |  0  0  1  0  1  0 | op |                             imm19                        |      Rt        |                  |           */
/*  0 |  0  0  1  0  1  0 |  0 |                             imm19                        |      Rt        | CBZ              | 32-bit    */
/*  0 |  0  0  1  0  1  0 |  1 |                             imm19                        |      Rt        | CBNZ             | 32-bit    */
/*  1 |  0  0  1  0  1  0 |  0 |                             imm19                        |      Rt        | CBZ              | 64-bit    */
/*  1 |  0  0  1  0  1  0 |  1 |                             imm19                        |      Rt        | CBNZ             | 64-bit    */

/* Test & branch (immediate)             Bits[31:24] */
/* Comments : b5 - 64 vs 32 bit address */
/* 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 | 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* b5 |  1  0  1  0  1  0 | op |      b40       |              imm14                        |       Rt       |                  |    -    */
/* b5 |  1  0  1  0  1  0 |  0 |      b40       |              imm14                        |       Rt       | TBZ              |    -    */
/* b5 |  1  0  1  0  1  0 |  1 |      b40       |              imm14                        |       Rt       | TBNZ             |    -    */

/* Compare & branch (immediate)            Bits[31:24] */
/* 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 | 03 02 01 00 | Instruction Page | Variant   */
/*  0  1  0  1  0  1  0 | o1 |                             imm19                        | o0 |    cond     |                  |           */
/*  0  1  0  1  0  1  0 |  0 |                             imm19                        |  0 |    cond     | B.cond           |    -      */

/* Exception generation                  Bits[31:24] */
/* 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 | 01 00 | Instruction Page | Variant */
/*  1  1  0  1  0  1  0  0 |    opc   |                    imm16                        |    op2   |  L  L |                  |    -    */
/*  1  1  0  1  0  1  0  0 |  0  0  0 |                    imm16                        |  0  0  0 |  0  1 | SVC              |    -    */
/*  1  1  0  1  0  1  0  0 |  0  0  0 |                    imm16                        |  0  0  0 |  1  0 | HVC              |    -    */
/*  1  1  0  1  0  1  0  0 |  0  0  0 |                    imm16                        |  0  0  0 |  1  1 | SMC              |    -    */
/*  1  1  0  1  0  1  0  0 |  0  0  1 |                    imm16                        |  0  0  0 |  0  0 | BRK              |    -    */
/*  1  1  0  1  0  1  0  0 |  0  1  0 |                    imm16                        |  0  0  0 |  0  0 | HLT              |    -    */
/*  1  1  0  1  0  1  0  0 |  1  0  1 |                    imm16                        |  0  0  0 |  0  1 | DCPS1            |    -    */
/*  1  1  0  1  0  1  0  0 |  1  0  1 |                    imm16                        |  0  0  0 |  1  0 | DCPS2            |    -    */
/*  1  1  0  1  0  1  0  0 |  1  0  1 |                    imm16                        |  0  0  0 |  1  1 | DCPS3            |    -    */

/* Unconditional branch (register) */
/* 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page */
/*  1  1  0  1  0  1  1  0 |   opc    |      op2       |        op3        |       Rn       |       op4      |                  */
/*  1  1  0  1  0  1  1  0 |  0  0  0 |  1  1  1  1  1 |  0  0  0  0  0  0 |  -  -  -  -  - |  0  0  0  0  0 | BR               */
/*  1  1  0  1  0  1  1  0 |  0  0  1 |  1  1  1  1  1 |  0  0  0  0  0  0 |  -  -  -  -  - |  0  0  0  0  0 | BLR              */
/*  1  1  0  1  0  1  1  0 |  0  1  0 |  1  1  1  1  1 |  0  0  0  0  0  0 |  -  -  -  -  - |  0  0  0  0  0 | RET              */
/*  1  1  0  1  0  1  1  0 |  1  0  0 |  1  1  1  1  1 |  0  0  0  0  0  0 |  1  1  1  1  1 |  0  0  0  0  0 | ERET             */
/*  1  1  0  1  0  1  1  0 |  1  0  1 |  1  1  1  1  1 |  0  0  0  0  0  0 |  1  1  1  1  1 |  0  0  0  0  0 | DRPS             */

/**********************************************************************************************************************************************************/
/**********************************************************************************************************************************************************/
/**********************************************************************************************************************************************************/

/* Loads and stores */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 */
/*  -  -  -  -  1  -  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - */

/* Table C3-3 Encoding table for the Loads and Stores functional group */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 */
/*  -  -  0  0  1  0  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store exclusive                                 */
/*  -  -  0  1  1  -  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load register (literal)                              */
/*  -  -  1  0  1  -  0  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store no-allocate pair (offset)                 */
/*  -  -  1  0  1  -  0  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store register pair (post-indexed)              */
/*  -  -  1  0  1  -  0  1  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store register pair (offset)                    */
/*  -  -  1  0  1  -  0  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store register pair (pre-indexed)               */
/*  -  -  1  1  1  -  0  0  -  -  0  -  -  -  -  -  -  -  -  -  0  0  -  -  -  -  -  -  -  -  -  - Load/store register (unscaled immediate)             */
/*  -  -  1  1  1  -  0  0  -  -  0  -  -  -  -  -  -  -  -  -  0  1  -  -  -  -  -  -  -  -  -  - Load/store register (immediate post-indexed)         */
/*  -  -  1  1  1  -  0  0  -  -  0  -  -  -  -  -  -  -  -  -  1  0  -  -  -  -  -  -  -  -  -  - Load/store register (unprivileged)                   */
/*  -  -  1  1  1  -  0  0  -  -  0  -  -  -  -  -  -  -  -  -  1  1  -  -  -  -  -  -  -  -  -  - Load/store register (immediate pre-indexed)          */
/*  -  -  1  1  1  -  0  0  -  -  1  -  -  -  -  -  -  -  -  -  1  0  -  -  -  -  -  -  -  -  -  - Load/store register (register offset)                */
/*  -  -  1  1  1  -  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - Load/store register (unsigned immediate)             */
/*  0  -  0  0  1  1  0  0  0  -  0  0  0  0  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - AdvSIMD load/store multiple structures               */
/*  0  -  0  0  1  1  0  0  1  -  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - AdvSIMD load/store multiple structures (post-indexed)*/
/*  0  -  0  0  1  1  0  1  0  -  -  0  0  0  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - AdvSIMD load/store single structure                  */
/*  0  -  0  0  1  1  0  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - AdvSIMD load/store single structure (post-indexed)   */

/* Load/store exclusive            Bits[31:24] */
/* Byte and Half Word and 32-bit and 64-bit variants */
/* Comment : size - 00 for byte / 01 for halfwrod / 10 for 32-bit / 11 for 64-bit */
/* 31 30 | 29 28 27 26 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/*  size |  0  0  1  0  0  0 | o2 |  L | o1 |        Rs      | o0 |      Rt2       |       Rn       |       Rt       |                  |         */
/*  size |  0  0  1  0  0  0 |  0 |  0 |  0 |        Rs      |  0 |      Rt2       |       Rn       |       Rt       | STXR             |         */
/*  size |  0  0  1  0  0  0 |  0 |  0 |  0 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | STLXR            |         */
/*  size |  0  0  1  0  0  0 |  0 |  0 |  1 |        Rs      |  0 |      Rt2       |       Rn       |       Rt       | STXP             |         */
/*  size |  0  0  1  0  0  0 |  0 |  0 |  1 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | STLXP            |         */
/*  size |  0  0  1  0  0  0 |  0 |  1 |  0 |        Rs      |  0 |      Rt2       |       Rn       |       Rt       | LDXR             |         */
/*  size |  0  0  1  0  0  0 |  0 |  1 |  0 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | LDAXR            |         */
/*  size |  0  0  1  0  0  0 |  0 |  1 |  1 |        Rs      |  0 |      Rt2       |       Rn       |       Rt       | LDXP             |         */
/*  size |  0  0  1  0  0  0 |  0 |  1 |  1 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | LDAXP            |         */
/*  size |  0  0  1  0  0  0 |  1 |  0 |  0 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | STLR             |         */
/*  size |  0  0  1  0  0  0 |  1 |  1 |  0 |        Rs      |  1 |      Rt2       |       Rn       |       Rt       | LDAR             |         */

/* Load register (literal) */
/* V = 1 bit is for SIMD, we ignore */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/*  opc  |  0  1  1 |  V |  0  0 |                           imm19                          |       Rt       |                  |         */
/*  0  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 32-bit  */
/*  0  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 64-bit  */
/*  1  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDRSW            |         */
/*  1  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | PRFM             |         */

/* Load/store no-allocate pair (offset) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  opc  |  1  0  1 |  V |  0  1  0 |  L |         imm7         |      Rt2       |       Rn       |       Rt       |                  |            */

/* Load/store register pair (post-indexed) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  opc  |  1  0  1 |  V |  0  0  1 |  L |         imm7         |      Rt2       |       Rn       |       Rt       |                  |            */
/*  0  0 |  1  0  1 |  0 |  0  0  1 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 32-bit     */
/*  0  0 |  1  0  1 |  0 |  0  0  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 32-bit     */
/*  0  1 |  1  0  1 |  0 |  0  0  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDPSW            | post-index */
/*  1  0 |  1  0  1 |  0 |  0  0  1 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 64-bit     */
/*  1  0 |  1  0  1 |  0 |  0  0  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 64-bit     */

/* Load/store register pair (offset) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant         */
/*  opc  |  1  0  1 |  V |  0  1  0 |  L |         imm7         |      Rt2       |       Rn       |       Rt       |                  |                 */
/*  0  0 |  1  0  1 |  0 |  0  1  0 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 32-bit          */
/*  0  0 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 32-bit          */
/*  0  1 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDPSW            | Signed offset   */
/*  1  0 |  1  0  1 |  0 |  0  1  0 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 64-bit          */
/*  1  0 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 64-bit          */

/* Load/store register pair (pre-indexed) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  opc  |  1  0  1 |  V |  0  1  1 |  L |         imm7         |      Rt2       |       Rn       |       Rt       |                  |            */
/*  0  0 |  1  0  1 |  0 |  0  1  1 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 32-bit     */
/*  0  0 |  1  0  1 |  0 |  0  1  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 32-bit     */
/*  0  1 |  1  0  1 |  0 |  0  1  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDPSW            | pre-index  */
/*  1  0 |  1  0  1 |  0 |  0  1  1 |  0 |         imm7         |      Rt2       |       Rn       |       Rt       | STP              | 64-bit     */
/*  1  0 |  1  0  1 |  0 |  0  1  1 |  1 |         imm7         |      Rt2       |       Rn       |       Rt       | LDP              | 64-bit     */

/* Load/store register (unscaled immediate) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 8-bit */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 |11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant  */
/*  size |  1  1  1 |  V |  0  0 |  opc  |  0 |             imm9           | 0  0 |       Rn       |       Rt       |                  |          */
/* 8-bit */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | STURB            |    -     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURB            |    -     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURSB           | 64-bit   */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURSB           | 32-bit   */
/* 16-bit */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | STURH            |    -     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURH            |    -     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURSH           | 64-bit   */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURSH           | 32-bit   */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | STUR             | 32-bit   */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDUR             | 32-bit   */
/*  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDURSW           |    -     */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | STUR             | 64-bit   */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | LDUR             | 64-bit   */
/*  1  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  0 |       Rn       |       Rt       | PRFUM            |    -     */

/* Load/store register (immediate post-indexed) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 |11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  size |  1  1  1 |  V |  0  0 |  opc  |  0 |             imm9           | 0  1 |       Rn       |       Rt       |                  |            */
/* B      */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | STRB             | Post-index */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRB             | Post-index */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRSB            | 64-bit     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRSB            | 32-bit     */
/* W      */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | STRH             | Post-index */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRH             | Post-index */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRSH            | 64-bit     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRSH            | 32-bit     */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | STR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDRSW            | Post-index */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | STR              | 64-bit     */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 0  1 |       Rn       |       Rt       | LDR              | 64-bit     */

/* Load/store register (unprivileged) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 |11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant  */
/*  size |  1  1  1 |  V |  0  0 |  opc  |  0 |             imm9           | 1  0 |       Rn       |       Rt       |                  |          */
/* 8-bit */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | STTRB            |    -     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRB            |    -     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRSB           | 64-bit   */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRSB           | 32-bit   */
/* 16-bit */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | STTRH            |    -     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRH            |    -     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRSH           | 64-bit   */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRSH           | 32-bit   */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | STTR             | 32-bit   */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTR             | 32-bit   */
/*  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTRSW           |    -     */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | STTR             | 64-bit     */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  0 |       Rn       |       Rt       | LDTR             | 64-bit     */

/* Load/store register (immediate pre-indexed) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 |11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  size |  1  1  1 |  V |  0  0 |  opc  |  0 |             imm9           | 1  1 |       Rn       |       Rt       |                  |            */
/* 8-bit */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | STRB             |  Pre-index */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRB             |  Pre-index */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRSB            | 64-bit     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRSB            | 32-bit     */
/* 16-bit */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | STRH             |  Pre-index */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRH             |  Pre-index */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRSH            | 64-bit     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRSH            | 32-bit     */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | STR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDRSW            |  Pre-index */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | STR              | 64-bit     */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |             imm9           | 1  1 |       Rn       |       Rt       | LDR              | 64-bit     */

/* Load/store register (register offset) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 8-bit */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant    */
/*  size |  1  1  1 |  V |  0  0 |  opc  |  1 |      Rm        |  option  |  S |  1  0 |       Rn       |       Rt       |                  |            */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | STRB             |    -       */
/*  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRB             |    -       */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRSB            | 64-bit     */
/*  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRSB            | 32-bit     */
/* 16-bit */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | STRB             |    -       */
/*  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRB             |    -       */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRSB            | 64-bit     */
/*  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRSB            | 32-bit     */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | STR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDR              | 32-bit     */
/*  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDRSW            |    -       */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | STR              | 64-bit     */
/*  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | LDR              | 64-bit     */
/*  1  1 |  1  1  1 |  0 |  0  0 |  1  0 |  1 |      Rm        |    -     |  S |  1  0 |       Rn       |       Rt       | PRFM             |    -       */

/* Load/store register (unsigned immediate) */
/* V = 1 bit is for SIMD, we take non SIMD instructons */
/* 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant         */
/*  size |  1  1  1 |  V |  0  1 |  opc  |                imm12                |       Rn       |       Rt       |                  |                 */
/* 8-bit */
/*  0  0 |  1  1  1 |  0 |  0  1 |  0  0 |                imm12                |       Rn       |       Rt       | STRB             | unsigned offset */
/*  0  0 |  1  1  1 |  0 |  0  1 |  0  1 |                imm12                |       Rn       |       Rt       | LDRB             | unsigned offset */
/*  0  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSB            | 64-bit          */
/*  0  0 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSB            | 32-bit          */
/* 16-bit */
/*  0  1 |  1  1  1 |  0 |  0  1 |  0  0 |                imm12                |       Rn       |       Rt       | STRH             | unsigned offset */
/*  0  1 |  1  1  1 |  0 |  0  1 |  0  1 |                imm12                |       Rn       |       Rt       | LDRH             |    -            */
/*  0  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSH            | 64-bit          */
/*  0  1 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSH            | 32-bit          */
/* 32-bit */
/*  1  0 |  1  1  1 |  0 |  0  1 |  0  0 |                imm12                |       Rn       |       Rt       | STR              | 32-bit          */
/*  1  0 |  1  1  1 |  0 |  0  1 |  0  1 |                imm12                |       Rn       |       Rt       | LDR              | 32-bit          */
/*  1  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSW            | unsigned offset */
/* 64-bit */
/*  1  1 |  1  1  1 |  0 |  0  1 |  0  0 |                imm12                |       Rn       |       Rt       | STR              | 64-bit          */
/*  1  1 |  1  1  1 |  0 |  0  1 |  0  1 |                imm12                |       Rn       |       Rt       | LDR              | 64-bit          */
/*  1  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | PRFUM            | unsigned offset */

/***************************************************************************************************/
/***************************************************************************************************/
/***************************************************************************************************/

/* Data processing - immediate */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 */
/*  -  -  -  1  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - */

/* Table C3-4 Encoding table for the Data Processing - Immediate functional group */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00  |                          */
/*  -  -  -  1  0  0  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | PC-rel. addressing       */
/*  -  -  -  1  0  0  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | Add/subtract (immediate) */
/*  -  -  -  1  0  0  1  0  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | Logical (immediate)      */
/*  -  -  -  1  0  0  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | Move wide (immediate)    */
/*  -  -  -  1  0  0  1  1  0  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | Bitfield                 */
/*  -  -  -  1  0  0  1  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  | Extract                  */

/* PC-rel. addressing */
/* 31 | 30 29 | 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* op | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      |                  |         */
/*  0 | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      | ADR              |    -    */
/*  1 | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      | ADRP             |    -    */

/* Add/subtract (immediate) */
/* 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* sf | op |  S |  1  0  0  0  1 | shift |               imm12                 |      Rn        |        Rd      |                  |         */
/*  0 |  0 |  0 |  1  0  0  0  1 | shift |               imm12                 |      Rn        |        Rd      | ADD              | 32-bit  */
/*  0 |  0 |  1 |  1  0  0  0  1 | shift |               imm12                 |      Rn        |        Rd      | ADDS             | 32-bit  */
/*  0 |  1 |  0 |  1  0  0  0  1 | shift |               imm12                 |      Rn        |        Rd      | SUB              | 32-bit  */
/*  0 |  1 |  1 |  1  0  0  0  1 | shift |               imm12                 |      Rn        |        Rd      | SUBS             | 32-bit  */
//  1 |  0 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
//  1 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADDS             | 64-bit
//  1 |  1 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUB              | 64-bit
//  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUBS             | 64-bit
// Note on shift : 01 -> LSL #12, nothing else possible

/* Logical (immediate) */
/* 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* sf |  opc  |  1  0  0  1  0  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |         */
/*  0 |  0  0 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | AND              | 32-bit  */
/*  0 |  0  1 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | ORR              | 32-bit  */
/*  0 |  1  0 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | EOR              | 32-bit  */
/*  0 |  1  1 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | ANDS             | 32-bit  */
/*  1 |  0  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | AND              | 64-bit  */
/*  1 |  0  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ORR              | 64-bit  */
/*  1 |  1  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | EOR              | 64-bit  */
/*  1 |  1  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ANDS             | 64-bit  */

/* Move wide (immediate) */
/* 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* sf |  opc  |  1  0  0  1  0  1 |  hw   |                     imm16                       |        Rd      |                  |         */
/*  0 |  0  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVN             | 32-bit  */
/*  0 |  0  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVZ             | 32-bit  */
/*  0 |  1  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVK             | 32-bit  */
/*  1 |  0  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVN             | 64-bit  */
/*  1 |  0  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVZ             | 64-bit  */
/*  1 |  1  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVK             | 64-bit  */

/* Bitfield */
/* 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* sf |  opc  |  1  0  0  1  1  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |         */
/*  0 |  0  0 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 32-bit  */
/*  0 |  0  1 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 32-bit  */
/*  0 |  1  0 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 32-bit  */
/*  1 |  0  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 64-bit  */
/*  1 |  0  1 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 64-bit  */
/*  1 |  1  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 64-bit  */

/* Extract */
/* 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant */
/* sf |  op21 |  1  0  0  1  1  1 |  N | o0 |      Rm        |      imms         |      Rn        |        Rd      |                  |         */
/*  0 |  0  0 |  1  0  0  1  1  1 |  0 |  0 |      Rm        |      imms         |      Rn        |        Rd      | EXTR             | 32-bit  */
/*  1 |  0  1 |  1  0  0  1  1  1 |  1 |  0 |      Rm        |      imms         |      Rn        |        Rd      | EXTR             | 64-bit  */

/***************************************************************************************************/
/***************************************************************************************************/
/***************************************************************************************************/

/*  Data processing - register */
/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 */
/*  -  -  -  -  1  0  1  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - */

/* 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 |                                  */
/*  -  -  -  0  1  0  1  0  -  -  -  -  -  -  -  -  -  -  -  -  -  - | Logical (shifted register)       */
/*  -  -  -  0  1  0  1  1  -  -  0  -  -  -  -  -  -  -  -  -  -  - | Add/subtract (shifted register)  */
/*  -  -  -  0  1  0  1  1  -  -  1  -  -  -  -  -  -  -  -  -  -  - | Add/subtract (extended register) */
/*  -  -  -  1  1  0  1  0  0  0  0  -  -  -  -  -  -  -  -  -  -  - | Add/subtract (with carry)        */
/*  -  -  -  1  1  0  1  0  0  1  0  -  -  -  -  -  -  -  -  -  0  - | Conditional compare (register)   */
/*  -  -  -  1  1  0  1  0  0  1  0  -  -  -  -  -  -  -  -  -  1  - | Conditional compare (immediate)  */
/*  -  -  -  1  1  0  1  0  1  0  0  -  -  -  -  -  -  -  -  -  -  - | Conditional select               */
/*  -  -  -  1  1  0  1  1  -  -  -  -  -  -  -  -  -  -  -  -  -  - | Data-processing (3 source)       */
/*  -  0  -  1  1  0  1  0  1  1  0  -  -  -  -  -  -  -  -  -  -  - | Data-processing (2 source)       */
/*  -  1  -  1  1  0  1  0  1  1  0  -  -  -  -  -  -  -  -  -  -  - | Data-processing (1 source)       */

/* Logical (shifted register) */
/* 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |         */
/*  0 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 32-bit  */
/*  0 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 32-bit  */
/*  0 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 32-bit  */
/*  0 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 32-bit  */
/*  0 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 32-bit  */
/*  0 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 32-bit  */
/*  0 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 32-bit  */
/*  0 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 32-bit  */
/*  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit  */
/*  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 64-bit  */

/* Add/subtract (shifted register) */
/* 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      |                  |         */
/*  0 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADD              | 32-bit  */
/*  0 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADDS             | 32-bit  */
/*  0 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUB              | 32-bit  */
/*  0 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUBS             | 32-bit  */
/*  1 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADD              | 64-bit  */
/*  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADDS             | 64-bit  */
/*  1 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUB              | 64-bit  */
/*  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUBS             | 64-bit  */

/* Add/subtract (extended register) */
/* 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  0  1  0  1  1 |  opt  |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      |                  |         */
/*  0 |  0 |  0 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | ADD              | 32-bit  */
/*  0 |  0 |  1 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | ADDS             | 32-bit  */
/*  0 |  1 |  0 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | SUB              | 32-bit  */
/*  0 |  1 |  1 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | SUBS             | 32-bit  */
/*  1 |  0 |  0 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | ADD              | 64-bit  */
/*  1 |  0 |  1 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | ADDS             | 64-bit  */
/*  1 |  1 |  0 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | SUB              | 64-bit  */
/*  1 |  1 |  1 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd      | SUBS             | 64-bit  */


/* Add/subtract (with carry) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  0  1  0  1  0  0  0  0 |      Rm        |     opcode2       |       Rn       |       Rd      |                  |         */
/*  0 |  0 |  0 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | ADC              | 32-bit  */
/*  0 |  0 |  1 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | ADCS             | 32-bit  */
/*  0 |  1 |  0 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | SBC              | 32-bit  */
/*  0 |  1 |  1 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | SBCS             | 32-bit  */
/*  1 |  0 |  0 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | ADC              | 64-bit  */
/*  1 |  0 |  1 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | ADCS             | 64-bit  */
/*  1 |  1 |  0 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | SBC              | 64-bit  */
/*  1 |  1 |  1 |  0  1  0  1  0  0  0  0 |      Rm        |  0  0  0  0  0  0 |       Rn       |       Rd      | SBCS             | 64-bit  */

/* Conditional compare (register) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 | 09 08 07 06 05 | 04 | 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 | o2 |       Rn       | o3 |   nzcv     |                  |         */
/*  0 |  0 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 32-bit  */
/*  0 |  1 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 32-bit  */
/*  1 |  0 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 64-bit  */
/*  1 |  1 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 64-bit  */

/* Conditional compare (immediate) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 | 09 08 07 06 05 | 04 | 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 | o2 |       Rn       | o3 |   nzcv     |                  |         */
/*  0 |  0 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 32-bit  */
/*  0 |  1 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 32-bit  */
/*  1 |  0 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 64-bit  */
/*  1 |  1 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 64-bit  */

/* Conditional compare (select) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf | op |  S |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  op2  |       Rn       |       Rd      |                  |         */
/*  0 |  0 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  0 |       Rn       |       Rd      | CSEL             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  1 |       Rn       |       Rd      | CSINC            | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  0 |       Rn       |       Rd      | CSINV            | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  1 |       Rn       |       Rd      | CSNEG            | 32-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  0 |       Rn       |       Rd      | CSEL             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  1 |       Rn       |       Rd      | CSINC            | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  0 |       Rn       |       Rd      | CSINV            | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |     imm5       |      cond   |  0  1 |       Rn       |       Rd      | CSNEG            | 64-bit  */

/* Data-Processing (3 source) */
/* 31 | 30 29 | 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  op54 |  1  1  0  1  1 |   op31   |       Rm       | o0 |     Ra         |       Rn       |      Rd       |                  |         */
/*  0 |  0  0 |  1  1  0  1  1 |  0  0  0 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | MADD             | 32-bit  */
/*  0 |  0  0 |  1  1  0  1  1 |  0  0  0 |       Rm       |  1 |     Ra         |       Rn       |      Rd       | MSUB             | 32-bit  */
/*  1 |  0  0 |  1  1  0  1  1 |  0  0  0 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | MADD             | 64-bit  */
/*  1 |  0  0 |  1  1  0  1  1 |  0  0  0 |       Rm       |  1 |     Ra         |       Rn       |      Rd       | MSUB             | 64-bit  */
/*  1 |  0  0 |  1  1  0  1  1 |  0  0  1 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | SMADDL           |   -     */
/*  1 |  0  0 |  1  1  0  1  1 |  0  0  1 |       Rm       |  1 |     Ra         |       Rn       |      Rd       | SMSUBL           |   -     */
/*  1 |  0  0 |  1  1  0  1  1 |  0  1  0 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | SMULH            |   -     */
/*  1 |  0  0 |  1  1  0  1  1 |  1  0  1 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | UMADDL           |   -     */
/*  1 |  0  0 |  1  1  0  1  1 |  1  0  1 |       Rm       |  1 |     Ra         |       Rn       |      Rd       | UMSUBL           |   -     */
/*  1 |  0  0 |  1  1  0  1  1 |  1  1  0 |       Rm       |  0 |     Ra         |       Rn       |      Rd       | UMULH            |   -     */

/* Data-Processing (2 source) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |      opcode       |       Rn       |      Rd       |                  |         */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  0  0  1  0 |       Rn       |      Rd       | UDIV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  0  0  1  1 |       Rn       |      Rd       | SDIV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  0 |       Rn       |      Rd       | LSLV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  1 |       Rn       |      Rd       | LSRV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  0 |       Rn       |      Rd       | ASRV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  1 |       Rn       |      Rd       | RORV             | 32-bit  */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  0  0  0 |       Rn       |      Rd       | CRC32B           |   -     */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  0  0  1 |       Rn       |      Rd       | CRC32H           |   -     */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  0  1  0 |       Rn       |      Rd       | CRC32W           |   -     */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  1  0  0 |       Rn       |      Rd       | CRC32CB          |   -     */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  1  0  1 |       Rn       |      Rd       | CRC32CH          |   -     */
/*  0 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  1  1  0 |       Rn       |      Rd       | CRC32CW          |   -     */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  0  0  1  0 |       Rn       |      Rd       | UDIV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  0  0  1  1 |       Rn       |      Rd       | SDIV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  0 |       Rn       |      Rd       | LSLV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  1 |       Rn       |      Rd       | LSRV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  0 |       Rn       |      Rd       | ASRV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  1 |       Rn       |      Rd       | RORV             | 64-bit  */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  0  1  1 |       Rn       |      Rd       | CRC32X           |   -     */
/*  1 |  0 |  0 |  1  1  0  1  0  1  1  0 |       Rm       |  0  1  0  1  1  1 |       Rn       |      Rd       | CRC32CX          |   -     */

/* Data-Processing (1 source) */
/* 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  1 |  S |  1  1  0  1  0  1  1  0 |     opcode     |      opcode       |       Rn       |      Rd       |                  |         */
/*  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  0 |       Rn       |      Rd       | RBIT             | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  1 |       Rn       |      Rd       | REV16            | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  0 |       Rn       |      Rd       | REV              | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  0 |       Rn       |      Rd       | CLZ              | 32-bit  */
/*  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  1 |       Rn       |      Rd       | CLS              | 32-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  0 |       Rn       |      Rd       | RBIT             | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  1 |       Rn       |      Rd       | REV16            | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  0 |       Rn       |      Rd       | REV32            |   -     */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  1 |       Rn       |      Rd       | REV              | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  0 |       Rn       |      Rd       | CLZ              | 64-bit  */
/*  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  1 |       Rn       |      Rd       | CLS              | 64-bit  */
