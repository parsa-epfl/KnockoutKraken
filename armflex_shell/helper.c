#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <byteswap.h>
#include <string.h> 

//forward declaration
int wr32b(uint64_t addr, uint64_t offset, uint32_t data);
int wr64b(uint64_t addr, uint64_t offset, uint64_t data);
uint32_t rd32b(uint64_t addr, uint64_t offset);
uint64_t rd64b(uint64_t addr, uint64_t offset);


#define FA_QFLEX_ROOT_DIR "/dev/shm/qflex"
#define SIM_CMD_FILE "SIM_CMD"
#define QEMU_CMD_FILE "QEMU_CMD"
#define SIM_STATE_FILE "SIM_STATE"
#define QEMU_STATE_FILE "QEMU_STATE"
#define PAGE_FILE "PROGRAM_PAGE"
#define PATH_MAX 4096
#define FA_QFLEX_ARCH_STATE_SIZE ((66) * sizeof(uint32_t))
#define ARCH_NUM_REGS           (32)
#define ARCH_XREGS_SIZE         (ARCH_NUM_REGS * sizeof(uint64_t))
#define ARCH_XREGS_OFFST        (0)                             // 0
#define ARCH_PC_OFFST           (32 * 2)                        // 64
#define ARCH_SP_OFFST           (ARCH_PC_OFFST + 2)             // 66
#define ARCH_PSTATE_FLAG_OFFST  (ARCH_SP_OFFST + 2)             // 68
#define ARCH_PSTATE_NF_MASK     (3)    // 64bit 3
#define ARCH_PSTATE_ZF_MASK     (2)    // 64bit 2
#define ARCH_PSTATE_CF_MASK     (1)    // 64bit 1
#define ARCH_PSTATE_VF_MASK     (0)    // 64bit 0
// #define errno (*__errno_location ())
// #define ULONG_HEX_MAX   16
// #define UINT_HEX_MAX    8
// #define STR(x) #x
// #define XSTR(s) STR(s)

typedef enum FA_QFlexCmds_t {
    // Commands SIM->QEMU
    DATA_LOAD   = 0,
    DATA_STORE  = 1,
    INST_FETCH  = 2,
    INST_UNDEF  = 3,
    INST_EXCP   = 4,
    // Commands QEMU->SIM
    SIM_START  = 5, // Load state from QEMU
    SIM_STOP   = 6, //
    // Commands QEMU<->SIM
    LOCK_WAIT   = 7,
    CHECK_N_STEP = 8,
    FA_QFLEXCMDS_NR
} FA_QFlexCmds_t;

typedef struct FA_QFlexCmd_short_t {
    FA_QFlexCmds_t cmd;
    uint64_t addr;
} FA_QFlexCmd_short_t;

FA_QFlexCmd_short_t* sim_cmd,*qemu_cmd;

FA_QFlexCmd_short_t* fa_qflfex_open_cmd_file(const char* name){
	const int SIZE = sizeof(FA_QFlexCmd_short_t);
	int shm_fd; 
	shm_fd = shm_open(name, O_CREAT | O_RDWR, 0666); 
    if (ftruncate(shm_fd, SIZE) < 0) {
    }
	FA_QFlexCmd_short_t* cmd = (FA_QFlexCmd_short_t*) mmap(0, SIZE, PROT_READ|PROT_WRITE, MAP_SHARED, shm_fd, 0); 
	return cmd;
}


void* fa_qflex_read_file(const char* filename, size_t *size) {
    int ret, fr;
    void *buffer;
    size_t fsize;
    char filepath[PATH_MAX];

    // printf("READ FILE %s\n", filename);
    snprintf(filepath, PATH_MAX, FA_QFLEX_ROOT_DIR"/%s", filename);

    fr = open(filepath, O_RDONLY, 0666);
    assert(fr);

    lseek(fr, 0, SEEK_END);
    fsize = lseek(fr, 0, SEEK_CUR);
    lseek(fr, 0, SEEK_SET);
    buffer = malloc(fsize + 1);

    ret = pread(fr, buffer, fsize, 0);
    assert(ret);
    close(fr);
    *size = fsize;
    return buffer;
}


int fa_qflex_write_file(const char *filename, void* buffer, size_t size) {
    char filepath[PATH_MAX];
    int fd = -1;
    void *region;
    printf("Writing file : "FA_QFLEX_ROOT_DIR"/%s\n", filename);
    if (mkdir(FA_QFLEX_ROOT_DIR, 0777) && errno != EEXIST) {
        printf("'mkdir "FA_QFLEX_ROOT_DIR"' failed\n");
        return 1;
    }
    snprintf(filepath, PATH_MAX, FA_QFLEX_ROOT_DIR"/%s", filename);
    if((fd = open(filepath, O_RDWR | O_CREAT | O_TRUNC, 0666)) == -1) {
        printf("Program Page dest file: open failed\n"
                       "    filepath:%s\n", filepath);
        return 1;
    }
    if (lseek(fd, size-1, SEEK_SET) == -1) {
        close(fd);
        printf("Error calling lseek() to 'stretch' the file\n");
        return 1;
    }
    if (write(fd, "", 1) != 1) {
        close(fd);
        printf(
            "Error writing last byte of the file\n");
        return 1;
    }

    region = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(region == MAP_FAILED) {
        close(fd);
        printf("Error dest file: mmap failed");
        return 1;
    }

    memcpy(region, buffer, size);
    msync(region, size, MS_SYNC);
    munmap(region, size);

    close(fd);
    return 0;
}

int writePage(uint64_t addr){
    size_t size;
    printf("Write programe page to FPGA \n");
    uint32_t* page = fa_qflex_read_file(PAGE_FILE, &size);
    int i = 0;
    for (; i < size/(sizeof(uint32_t)); ++i){
        // printf("[%016x]\n", __bswap_32 (*(page + i)));
        wr32b(addr, i, __bswap_32 (*(page + i)));
    }
    return 0;
}

void writeState(uint64_t addr){
    size_t size;
    uint32_t* xregs = fa_qflex_read_file(QEMU_STATE_FILE, &size);;
    int i =0;
    for (; i < ARCH_NUM_REGS; i+=2){
        // printf("[%016lx]\n", xregs[i] );
    	wr32b(addr, i, *(xregs + i + 1)); //msb
    	wr32b(addr, i + 1, *(xregs + i)); //lsb
    }
    //pc
	wr32b(addr, ARCH_PC_OFFST , *(xregs + ARCH_PC_OFFST + 1));
	wr32b(addr, ARCH_PC_OFFST + 1, *(xregs + ARCH_PC_OFFST));
    // sp
    wr32b(addr, ARCH_SP_OFFST , *(xregs + ARCH_SP_OFFST + 1));
	wr32b(addr, ARCH_SP_OFFST + 1, *(xregs + ARCH_SP_OFFST));
    //nzcv
	wr32b(addr, ARCH_PSTATE_FLAG_OFFST, *(xregs + ARCH_PSTATE_FLAG_OFFST));
}

void writeStateBack(uint64_t addr){
    printf("Read state from FPGA....\n");
    uint32_t *xregs = calloc(FA_QFLEX_ARCH_STATE_SIZE, sizeof(uint32_t));

    int i =0;
    for (; i < ARCH_NUM_REGS; i+=2){
    	*(xregs + i + 1) = rd32b(addr, i); //msb
    	*(xregs + i) = rd32b(addr, i + 1); //lsb
    }
	*(xregs + ARCH_PC_OFFST + 1) = rd32b(addr, ARCH_PC_OFFST );
	*(xregs + ARCH_PC_OFFST)= rd32b(addr, ARCH_PC_OFFST + 1);
    // sp
    *(xregs + ARCH_SP_OFFST + 1) = rd32b(addr, ARCH_SP_OFFST );
	*(xregs + ARCH_SP_OFFST) = rd32b(addr, ARCH_SP_OFFST + 1);
    //nzcv
	*(xregs + ARCH_PSTATE_FLAG_OFFST) = rd32b(addr, ARCH_PSTATE_FLAG_OFFST);

    fa_qflex_write_file(SIM_STATE_FILE, xregs, FA_QFLEX_ARCH_STATE_SIZE);
    free(xregs);
}

void openCMDFiles(){
    sim_cmd = fa_qflfex_open_cmd_file(SIM_CMD_FILE);
    qemu_cmd = fa_qflfex_open_cmd_file(QEMU_CMD_FILE);
}

void waitForStart(){
    printf("Waiting for START command from QEMU...\n");
	while (sim_cmd->cmd == LOCK_WAIT)usleep(1);
    sim_cmd->cmd = LOCK_WAIT;
}

void writeUndefined(){
    printf("Sending UNDEFINED command to QEMU...\n");
    qemu_cmd->cmd = INST_UNDEF;
}

void writeCheckNStep(){
    printf("Sending CHECK_N_STEP command to QEMU...\n");
    qemu_cmd->cmd = CHECK_N_STEP;
}
