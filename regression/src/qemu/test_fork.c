/* Include these in a c program inside the guest.
 * Once the helper is executed inside the guest, 
 * qemu will detect the event and trigger the command
 */
#define STR(x)  #x
#define XSTR(s) STR(s)
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t"  )

#define DO_QFLEX_OP(op) magic_inst(QFLEX_OP); magic_inst(op)

#define DEVTEROFLEX_OP    (94)

#define DEVTEROFLEX_FLOW_START (90)
#define DEVTEROFLEX_FLOW_STOP  (91)
#define DO_DEVTEROFLEX_OP(op) magic_inst(DEVTEROFLEX_OP); magic_inst(op)

#include <stdio.h>
#include <assert.h>

int main() {
	DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
	if (fork() == 0){
		argx += 1;
	} else {
		argx += 2;
	}
	DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
	return 0;
}

