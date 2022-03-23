int create_request_server(void);
int do_dma_axi_read(uint8_t *buffer, size_t size, uint64_t address);

#define GET_CHANNEL(address) ((address >> 34L) & 0b11)