ENTRY(_start);

SECTIONS
{
  .text 0x0 : { 
    start.o(.text)
    *(.text) 
  }
}