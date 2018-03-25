/* For extracting the assembly and hex instructions :
 * aarch64-linux-gnu-gcc -g -c -O0 *.c
 * aarch64-linux-gnu-objdump -d -M amd64 -S *.c
 */


int main(void)
{
  int a = 0xF0;
  int b = 0x0F;
  int and = a, bic = a, orr = a, orn = a, eor = a, eon = a;
  int add = a, sub = a;

  and &=  b;
  bic &= ~b;
  orr |=  b;
  orn |= ~b;
  eor ^=  b;
  eon ^= ~b;
  add +=  b;
  sub -=  b;
  if(sub) return 1;

  return 0;
}
