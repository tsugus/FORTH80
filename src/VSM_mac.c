/**************************************************************/
/*                                                            */
/*                                                            */
/*         V i r t u a l   S t a c k   M a c h i n e          */
/*                                                            */
/*                            for                             */
/*                                                            */
/*                       F O R T H 8 0                        */
/*                                                            */
/*                                                            */
/*                 A FORTH langage proccessor                 */
/*               conformiting FORTH-79 Standard               */
/*                                                            */
/*                          for Mac                           */
/*                                                            */
/*                       Version 0.6.1                        */
/*                                                            */
/*                                       (C) 2023-2024 Tsugu  */
/*                                                            */
/*                                                            */
/*  This program is released under the                        */
/*                                                            */
/*                       MIT License.                         */
/*     (https://opensource.org/licenses/mit-license.php)      */
/*                                                            */
/**************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "conio.h" // "homemade" conio.h
#define LIMIT 0x8000
#define CLD1 0x0009
#define WRM1 0x000C
#define UVR 0x000E
#define UP 0x7BB8
#define SECTOR_SIZE 512

unsigned char Memory[LIMIT] = {0};
int PC = 0;                   // Program Counter
unsigned short IP, SP, RP, W; // "Register"
unsigned short AX, BX, CX;    // "Register"

FILE *infp = NULL, *outfp = NULL;

unsigned short read_word(int n)
{
  short r = Memory[n + 1];
  r <<= 8;
  r |= Memory[n];
  return r;
}

void write_word(int n, unsigned short x)
{
  Memory[n] = x;
  Memory[n + 1] = x >> 8;
}

unsigned short pop()
{
  unsigned short r = read_word(SP);
  SP += 2;
  return r;
}

void push(unsigned short x)
{
  SP -= 2;
  write_word(SP, x);
}

void exchange(unsigned short *x, unsigned short *y)
{
  unsigned short temp = *x;
  *x = *y;
  *y = temp;
}

void NEXT();

void APUSH();

// #1
void CLD_()
{
  IP = CLD1;
  SP = read_word(UVR + 6);
  RP = read_word(UVR + 8);
  NEXT();
}

// #2
void WRM()
{
  IP = WRM1;
  NEXT();
}

// #3 (environment dependent)
void CTST()
{
  if (kbhit())
    AX = 1;
  else
    AX = 0;
  APUSH();
}

// #4 (environment dependent)
void CIN()
{
  AX = 0x00FF & (unsigned short)getch();
  if (AX == 0x7F) // for Mac
    AX = 0x08;    // fix of backspace key code
  APUSH();
}

// #61 (3Dh) (environment dependent)
void STIN()
{
  if (infp != NULL)
    infp = feof(infp) ? stdin : infp;
  AX = 0x00FF & (unsigned short)getc(infp);
  APUSH();
}

// #5 (environment dependent)
void COUT()
{
  putc(pop(), stderr);
  NEXT();
}

// #6 (environment dependent)
void POUT()
{
  putc(pop(), outfp);
  NEXT();
}

// #7 (environment dependent)
void READ() // 1 sector only
{
  W = pop();  // starting logical sector
  BX = pop(); // address of buffer
  AX = pop(); // drive number

  FILE *fp;
  char *file;

  switch (AX)
  {
  case 0:
    file = "DRIVE0.img";
    break;
  case 1:
    file = "DRIVE1.img";
    break;
  default:
    AX = 1; // Error Flag
    return;
  }

  fp = fopen(file, "rb");
  if (fp == NULL)
  {
    AX = 1; // Error Flag
    return;
  }

  int r1 = fseek(fp, SECTOR_SIZE * W, SEEK_SET);
  if (!r1)
  {
    int r2 = fread(Memory + BX, sizeof(unsigned char), SECTOR_SIZE, fp);
    if (r2 < SECTOR_SIZE)
      AX = 1; // Error Flag
    else
      AX = 0;
  }
  else
    AX = 1;
  fclose(fp);
  APUSH();
}

// #8 (environment dependent)
void WRITE() // 1 sctor only
{
  W = pop();  // starting logical sector
  BX = pop(); // address of buffer
  AX = pop(); // drive number

  FILE *fp;
  char *file;

  switch (AX)
  {
  case 0:
    file = "DRIVE0.img";
    break;
  case 1:
    file = "DRIVE1.img";
    break;
  default:
    AX = 1; // Error Flag
    return;
  }

  fp = fopen(file, "r+b");
  if (fp == NULL)
  {
    AX = 1; // Error Flag
    return;
  }

  int r1 = fseek(fp, SECTOR_SIZE * W, SEEK_SET);
  if (!r1)
  {
    int r2 = fwrite(Memory + BX, sizeof(unsigned char), SECTOR_SIZE, fp);
    if (r2 < SECTOR_SIZE)
      AX = 1; // Error Flag
    else
      AX = 0;
  }
  else
    AX = 1;
  fclose(fp);
  APUSH();
}

// #9
void WPUSH()
{
  push(W);
  APUSH();
}

// #10 (0Ah)
void APUSH()
{
  push(AX);
  NEXT();
}

void NEXT1();

// #11 (0Bh)
void NEXT()
{
  BX = AX = read_word(IP);
  IP += 2;
  NEXT1();
}

// 12 (0Ch)
void NEXT1()
{
  W = BX;
  W++;
  PC = read_word(BX);
}

// #13 (0Dh)
void LIT()
{
  AX = read_word(IP);
  IP += 2;
  APUSH();
}

// #14 (0Eh)
void EXEC()
{
  BX = pop();
  NEXT1();
}

// #15 (0Fh)
void BRAN()
{
  IP += read_word(IP);
  NEXT();
}

// #16 (10h)
void ZBRAN()
{
  if (!pop())
    BRAN();
  else
  {
    IP += 2;
    NEXT();
  }
}

void loop(unsigned short x)
{
  write_word(RP, read_word(RP) + x);
  AX = read_word(RP) - read_word(RP + 2);
  if ((AX ^ x) >> 15)
    BRAN();
  else
  {
    RP += 4;
    IP += 2;
    NEXT();
  }
}

// #17 (11h)
void XLOOP()
{
  loop(1);
}

// #18 (12h)
void XPLOO()
{
  loop(pop());
}

// #19 (13h)
void XDO()
{
  W = pop();
  AX = pop();
  exchange(&RP, &SP);
  push(AX);
  push(W);
  exchange(&RP, &SP);
  NEXT();
}

// #20 (14h)
void ANDD()
{
  AX = pop();
  AX &= pop();
  APUSH();
}

// #21 (15h)
void ORR()
{
  AX = pop();
  AX |= pop();
  APUSH();
}

// #22 (16h)
void XORR()
{
  AX = pop();
  AX ^= pop();
  APUSH();
}

// #23 (17h)
void SPAT()
{
  AX = SP;
  APUSH();
}

// #24 (18h)
void SPSTO()
{
  SP = read_word(UP + 6);
  NEXT();
}

// #25 (19h)
void RPAT()
{
  AX = RP;
  APUSH();
}

// #26 (1Ah)
void RPSTO()
{
  RP = read_word(UP + 8);
  NEXT();
}

// #27 (1Bh)
void SEMIS()
{
  IP = read_word(RP);
  RP += 2;
  NEXT();
}

// #28 (1Ch)
void TOR()
{
  RP -= 2;
  write_word(RP, pop());
  NEXT();
}

// #29 (1Dh)
void FROMR()
{
  AX = read_word(RP);
  RP += 2;
  APUSH();
}

// #30 (1Eh)
void RAT()
{
  AX = read_word(RP);
  APUSH();
}

// #31 (1Fh)
void ZEQU()
{
  if (pop())
    AX = 0;
  else
    AX = 1;
  APUSH();
}

// #32 (20h)
void ZLESS()
{
  AX = pop() >> 15;
  APUSH();
}

// #33 (21h)
void PLUS()
{
  AX = pop();
  AX += pop();
  APUSH();
}

// #34 (22h)
void SUBB()
{
  AX = pop();
  AX = pop() - AX;
  APUSH();
}

// #35 (23h)
void DPLUS()
{
  unsigned int d2 = pop();
  d2 = d2 << 16 | pop();
  unsigned int d1 = pop();
  d1 = d1 << 16 | pop();
  d1 += d2;
  W = d1;
  AX = d1 >> 16;
  WPUSH();
}

// #36 (24h)
void DSUB()
{
  unsigned int d2 = pop();
  d2 = d2 << 16 | pop();
  unsigned int d1 = pop();
  d1 = d1 << 16 | pop();
  d1 -= d2;
  W = d1;
  AX = d1 >> 16;
  WPUSH();
}

// #37 (25h)
void OVER()
{
  W = pop();
  AX = pop();
  push(AX);
  WPUSH();
}

// #38 (26h)
void DROP()
{
  AX = pop();
  NEXT();
}

// #39 (27h)
void SWAP()
{
  W = pop();
  AX = pop();
  WPUSH();
}

// #40 (28h)
void DUPE()
{
  AX = pop();
  push(AX);
  APUSH();
}

// #41 (29h)
void ROT()
{
  W = pop();
  BX = pop();
  AX = pop();
  push(BX);
  WPUSH();
}

// #42 (2Ah)
void USTAR()
{
  unsigned int u2 = pop();
  unsigned int u1 = pop();
  u1 *= u2;
  W = u1;
  AX = u1 >> 16;
  WPUSH();
}

// #43 (2Bh)
void USLAS()
{
  unsigned short u = pop();
  unsigned int ud = pop();
  if (ud >= u)
  {
    W = AX = -1;
    pop();
  }
  else
  {
    ud <<= 16;
    ud |= pop();
    AX = ud / u;
    W = ud % u;
  }
  WPUSH();
}

// #44 (2Ch)
void TDIV()
{
  AX = pop();
  AX = 0x8000 & AX | AX >> 1;
  APUSH();
}

// #45 (2Dh)
void TOGGL()
{
  AX = pop();
  Memory[pop()] ^= AX;
  NEXT();
}

// #46 (2Eh)
void ATT()
{
  AX = read_word(pop());
  APUSH();
}

// #47 (2Fh)
void STORE()
{
  BX = pop();
  write_word(BX, pop());
  NEXT();
}

// #48 (30h)
void CSTOR()
{
  BX = pop();
  Memory[BX] = pop();
  NEXT();
}

// #49 (31h)
void CMOVE()
{
  int n = pop();
  unsigned short a2 = pop();
  unsigned short a1 = pop();
  for (int i = 0; i < n; i++)
    Memory[a2 + i] = Memory[a1 + i];
  NEXT();
}

// #50 (32h)
void LCMOVE()
{
  int n = pop();
  unsigned short a2 = pop();
  unsigned short a1 = pop();
  for (int i = n - 1; i >= 0; i--)
    Memory[a2 + i] = Memory[a1 + i];
  NEXT();
}

// #51 (33h)
void FILL()
{
  unsigned char b = pop();
  int n = pop();
  unsigned short a = pop();
  for (int i = 0; i < n; i++)
    Memory[a + i] = b;
  NEXT();
}

// #52 (34h)
void DOCOL()
{
  W++;
  RP -= 2;
  write_word(RP, IP);
  IP = W;
  NEXT();
}

// #53 (35h)
void DOCON()
{
  AX = read_word(++W);
  APUSH();
}

// #54 (36h)
void DOVAR()
{
  push(++W);
  NEXT();
}

// #55 (37h)
void DOTCON()
{
  AX = read_word(++W);
  W = read_word(W + 2);
  WPUSH();
}

// #56 (38h)
void DOTVAR()
{
  push(++W);
  NEXT();
}

// #57 (39h)
void DOUSE()
{
  BX = read_word(++W);
  BX &= 0x00FF;
  AX = BX + UP;
  APUSH();
}

// #58 (3Ah)
void XDOES()
{
  exchange(&RP, &SP);
  push(IP);
  exchange(&RP, &SP);
  IP = read_word(BX) + 3;
  push(++W);
  NEXT();
}

// #59 (3Bh)
void DOCREA()
{
  push(++W);
  NEXT();
}

// #60 (3Ch)
void BYE()
{
  exit(EXIT_SUCCESS);
}

// #62 (3Eh)
void POPEN()
{
  unsigned short a2 = pop();
  unsigned short a1 = pop();
  char command[256], mode[3];
  FILE *r;

  for (int i = 0; i < Memory[a1]; i++)
    command[i] = Memory[a1 + i + 1];
  command[Memory[a1]] = '\0';

  int max = Memory[a2] > 2 ? 2 : Memory[a2];
  for (int i = 0; i < max; i++)
    mode[i] = Memory[a2 + i + 1];
  mode[max] = '\0';

  if ((r = popen(command, mode)) == NULL)
    perror("can not exec commad");

  push((unsigned long long)r);
  push((unsigned long long)r >> 16);
  push((unsigned long long)r >> 32);
  push((unsigned long long)r >> 48);

  NEXT();
}

// 63 (3Fh)
void PCLOSE()
{
  unsigned long long u4 = pop();
  unsigned long long u3 = pop();
  unsigned long long u2 = pop();
  unsigned long long u1 = pop();
  int r;

  r = pclose((FILE *)(u4 << 48 | u3 << 32 | u2 << 16 | u1));

  push((unsigned)r);
  push((unsigned)r >> 16);

  NEXT();
}

// 64 (40h)
void SETIN()
{
  unsigned long long u4 = pop();
  unsigned long long u3 = pop();
  unsigned long long u2 = pop();
  unsigned long long u1 = pop();

  FILE *fp = (FILE *)(u4 << 48 | u3 << 32 | u2 << 16 | u1);

  infp = fp == NULL ? stdin : fp;

  NEXT();
}

// 65 (41h)
void SETOUT()
{
  unsigned long long u4 = pop();
  unsigned long long u3 = pop();
  unsigned long long u2 = pop();
  unsigned long long u1 = pop();

  FILE *fp = (FILE *)(u4 << 48 | u3 << 32 | u2 << 16 | u1);

  outfp = fp == NULL ? stdin : fp;

  NEXT();
}

// 90h
void NOP()
{
  PC++;
}

// E9h
void JMP()
{
  PC = read_word(++PC);
}

/****************************************/

void (*func_table[])() = {
    NULL,
    CLD_,
    WRM,
    CTST,
    CIN,
    COUT,
    POUT,
    READ,
    WRITE,
    WPUSH,
    APUSH,
    NEXT,
    NEXT1,
    LIT,
    EXEC,
    BRAN,
    ZBRAN,
    XLOOP,
    XPLOO,
    XDO,
    ANDD,
    ORR,
    XORR,
    SPAT,
    SPSTO,
    RPAT,
    RPSTO,
    SEMIS,
    TOR,
    FROMR,
    RAT,
    ZEQU,
    ZLESS,
    PLUS,
    SUBB,
    DPLUS,
    DSUB,
    OVER,
    DROP,
    SWAP,
    DUPE,
    ROT,
    USTAR,
    USLAS,
    TDIV,
    TOGGL,
    ATT,
    STORE,
    CSTOR,
    CMOVE,
    LCMOVE,
    FILL,
    DOCOL,
    DOCON,
    DOVAR,
    DOTCON,
    DOTVAR,
    DOUSE,
    XDOES,
    DOCREA,
    BYE,
    STIN,
    POPEN,
    PCLOSE,
    SETIN,
    SETOUT};

/****************************************/

int main(int argc, char *argv[])
{
  FILE *fp = NULL;
  char *file;

  file = "FORTH80.bin";
  fp = fopen(file, "rb");
  if (fp == NULL)
  {
    fprintf(stderr, "To open %s is failed.\n", file);
    return 1;
  }
  fread(Memory, sizeof(unsigned char), sizeof(Memory) / sizeof(Memory[0]), fp);
  fclose(fp);

  Memory[UVR + 26 * 2] = 1; // the initial value of the user variable "UTF-8"
  infp = stdin, outfp = stdout;

  if (argc > 1)
  {
    if (!strcmp(argv[1], "-v"))
      printf("Virtual Stack Machine for FORTH80 Version 0.6.1\n");
    return 0;
  }

  while (1)
  {
    int opecode = Memory[PC];
    if (opecode == 0xE9)
      JMP();
    else if (1 <= opecode && opecode <= 65)
      (*func_table[opecode])();
    else if (opecode == 0x90)
      NOP();
    else
    {
      fprintf(stderr, "%02X: undefined code.\n", opecode);
      break;
    }
  }

  return 0;
}
