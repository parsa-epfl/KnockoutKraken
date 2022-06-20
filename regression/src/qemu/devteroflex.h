/* Include these in a c program inside the guest.
 * Once the helper is executed inside the guest, 
 * qemu will detect the event and trigger the command
 */
#define STR(x)  #x                                                                                                                               
#define XSTR(s) STR(s)                                                                                                                           
#if defined(__aarch64__) && !defined(CONFIG_FOR_LOADER)
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t"  )                                                                      
#else 
#define magic_inst(val) 
#endif
                                                                                                                                                 
#define DO_QFLEX_OP(op) magic_inst(QFLEX_OP); magic_inst(op)                                                                                     
                                                                                                                                                 
#define DEVTEROFLEX_OP    (94)

#define DEVTEROFLEX_FLOW_START (90)
#define DEVTEROFLEX_FLOW_STOP  (91)
#define DO_DEVTEROFLEX_OP(op) magic_inst(DEVTEROFLEX_OP); magic_inst(op) 

#define PAGE_SIZE (4096L)
#define PAGE_MASK (0xFFFL)

#define DUMMY_FILE_DIR "dummy_file"

#include <stdio.h>
#include <assert.h>
#define do_assert(expr) if(!(expr)) {         \
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP); \
  assert(expr);                             \
  }

#if defined(CONFIG_FOR_LOADER)
#undef printf
#define printf(words...)
#endif