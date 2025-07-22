/*************************************************************/
/*                                                           */
/*                                                           */
/*         V i r t u a l   S t a c k   M a c h i n e         */
/*                                                           */
/*                            for                            */
/*                                                           */
/*                F O R T H 8 0  ver. 0.8.0 ~                */
/*                                                           */
/*                                                           */
/*                A FORTH language processor                 */
/*               conforming FORTH-79 Standard                */
/*                                                           */
/*                          for Mac                          */
/*                                                           */
/*                       Version 0.8.1                       */
/*                                                           */
/*                                      (C) 2023-2024 Tsugu  */
/*                                                           */
/*                                                           */
/*  This program is released under the                       */
/*                                                           */
/*                       MIT License.                        */
/*     (https://opensource.org/licenses/mit-license.php)     */
/*                                                           */
/**************************************************************/

#define MAJOR_V 0
#define MINOR_V 8
#define USER_V 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "conio.h" // "homemade" conio.h
#define LIMIT 0x8000
#define CLD1 0x0009
#define WRM1 0x000C
#define UVR 0x000E
#define UP 0x77B8
#define BPS 512

unsigned char Memory[LIMIT];
int PC = 0;                   // Program Counter
unsigned short IP, SP, RP, W; // "Register"
unsigned short AX, BX;        // "Register"

FILE *infp = NULL, *outfp = NULL;
int update_flag = 0;

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

// #0  90h
void NOP()
{
  PC++;
}

// #1  E9h
void JMP()
{
  PC = read_word(++PC);
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

// #2
void CLD_()
{
  IP = CLD1;
  SP = read_word(UVR + 6);
  RP = read_word(UVR + 8);
  NEXT();
}

// #3
void WRM()
{
  IP = WRM1;
  NEXT();
}

// #4
void CTST()
{
  if (kbhit())
    AX = 1;
  else
    AX = 0;
  APUSH();
}

// #5
void CIN()
{
  AX = 0x00FF & (unsigned short)getch();
  if (AX == 0x7F) // for Mac
    AX = 0x08;    // fix of backspace key code
  APUSH();
}

// #6
void STIN()
{
  if (infp != NULL)
    infp = feof(infp) ? stdin : infp;
  AX = 0x00FF & (unsigned short)getc(infp);
  APUSH();
}

// #7
void COUT()
{
  putc(pop(), stderr);
  NEXT();
}

// #8
void POUT()
{
  putc(pop(), outfp);
  NEXT();
}

// #9
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
    APUSH();
    return;
  }

  fp = fopen(file, "rb");
  if (fp == NULL)
  {
    AX = 1; // Error Flag
    APUSH();
    return;
  }

  int r1 = fseek(fp, BPS * W, SEEK_SET);
  if (!r1)
  {
    int r2 = fread(Memory + BX, sizeof(unsigned char), BPS, fp);
    if (r2 < BPS)
      AX = 1; // Error Flag
    else
      AX = 0;
  }
  else
    AX = 1;
  fclose(fp);
  APUSH();
}

// #10
void WRITE() // 1 sector only
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

  int r1 = fseek(fp, BPS * W, SEEK_SET);
  if (!r1)
  {
    int r2 = fwrite(Memory + BX, sizeof(unsigned char), BPS, fp);
    if (r2 < BPS)
      AX = 1; // Error Flag
    else
      AX = 0;
  }
  else
    AX = 1;
  fclose(fp);
  APUSH();
}

// WPUSH
void WPUSH()
{
  push(W);
  APUSH();
}

// APUSH
void APUSH()
{
  push(AX);
  NEXT();
}

void NEXT1();

// NEXT
void NEXT()
{
  BX = read_word(IP);
  IP += 2;
  NEXT1();
}

// NEXT1
void NEXT1()
{
  W = BX;
  W++;
  PC = read_word(BX);
}

// #11
void LIT()
{
  AX = read_word(IP);
  IP += 2;
  APUSH();
}

// #12
void EXEC()
{
  BX = pop();
  NEXT1();
}

// #13
void BRAN()
{
  IP += read_word(IP);
  NEXT();
}

// #14
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

// #15
void XLOOP()
{
  loop(1);
}

// #16
void XPLOO()
{
  loop(pop());
}

// #17
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

// #18
void ANDD()
{
  AX = pop();
  AX &= pop();
  APUSH();
}

// #19
void ORR()
{
  AX = pop();
  AX |= pop();
  APUSH();
}

// #20
void XORR()
{
  AX = pop();
  AX ^= pop();
  APUSH();
}

// #21
void SPAT()
{
  AX = SP;
  APUSH();
}

// #22
void SPSTO()
{
  SP = read_word(UP + 6);
  NEXT();
}

// #23
void RPAT()
{
  AX = RP;
  APUSH();
}

// #24
void RPSTO()
{
  RP = read_word(UP + 8);
  NEXT();
}

// #25
void SEMIS()
{
  IP = read_word(RP);
  RP += 2;
  NEXT();
}

// #26
void TOR()
{
  RP -= 2;
  write_word(RP, pop());
  NEXT();
}

// #27
void FROMR()
{
  AX = read_word(RP);
  RP += 2;
  APUSH();
}

// #28
void RAT()
{
  AX = read_word(RP);
  APUSH();
}

// #29
void ZEQU()
{
  if (pop())
    AX = 0;
  else
    AX = 1;
  APUSH();
}

// #30
void ZLESS()
{
  AX = pop() >> 15;
  APUSH();
}

// #31
void PLUS()
{
  AX = pop();
  AX += pop();
  APUSH();
}

// #32
void SUBB()
{
  AX = pop();
  AX = pop() - AX;
  APUSH();
}

// #33
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

// #34
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

// #35
void OVER()
{
  W = pop();
  AX = pop();
  push(AX);
  WPUSH();
}

// #36
void DROP()
{
  AX = pop();
  NEXT();
}

// #37
void SWAP()
{
  W = pop();
  AX = pop();
  WPUSH();
}

// #38
void DUPE()
{
  AX = pop();
  push(AX);
  APUSH();
}

// #39
void ROT()
{
  W = pop();
  BX = pop();
  AX = pop();
  push(BX);
  WPUSH();
}

// #40
void USTAR()
{
  unsigned int u2 = pop();
  unsigned int u1 = pop();
  u1 *= u2;
  W = u1;
  AX = u1 >> 16;
  WPUSH();
}

// #41
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

// #42
void TDIV()
{
  AX = pop();
  AX = 0x8000 & AX | AX >> 1;
  APUSH();
}

// #43
void TOGGL()
{
  AX = pop();
  Memory[pop()] ^= AX;
  NEXT();
}

// #44
void ATT()
{
  AX = read_word(pop());
  APUSH();
}

// #45
void STORE()
{
  BX = pop();
  write_word(BX, pop());
  NEXT();
}

// #46
void CSTOR()
{
  BX = pop();
  Memory[BX] = pop();
  NEXT();
}

// #47
void CMOVE()
{
  int n = pop();
  unsigned short a2 = pop();
  unsigned short a1 = pop();
  for (int i = 0; i < n; i++)
    Memory[a2 + i] = Memory[a1 + i];
  NEXT();
}

// #48
void LCMOVE()
{
  int n = pop();
  unsigned short a2 = pop();
  unsigned short a1 = pop();
  for (int i = n - 1; i >= 0; i--)
    Memory[a2 + i] = Memory[a1 + i];
  NEXT();
}

// #49
void FILL()
{
  unsigned char b = pop();
  int n = pop();
  unsigned short a = pop();
  for (int i = 0; i < n; i++)
    Memory[a + i] = b;
  NEXT();
}

// #50
void DOCOL()
{
  W++;
  RP -= 2;
  write_word(RP, IP);
  IP = W;
  NEXT();
}

// #51
void XDOES()
{
  exchange(&RP, &SP);
  push(IP);
  exchange(&RP, &SP);
  IP = read_word(BX) + 3;
  push(++W);
  NEXT();
}

// #52
void DOCREA()
{
  push(++W);
  NEXT();
}

// #53
void BYE()
{
  if (update_flag)
  {
    char ch;
    printf("\nDo you update FORTH80.bin ? (Y/N): ");
    scanf("%c", &ch);
    if (ch == 'Y' || ch == 'y')
    {
      FILE *fp = NULL;
      char *file = "FORTH80.bin";
      fp = fopen(file, "wb");
      if (fp != NULL)
      {
        Memory[UVR + 18] = Memory[UP + 18];       // DP
        write_word(UVR + 18, read_word(UP + 18)); // DP
        Memory[UVR + 20] = Memory[UP + 20];       // VOC-LINK
        write_word(UVR + 20, read_word(UP + 20)); // VOC-LINK
        fwrite(Memory, 1, read_word(UVR + 18), fp);
        fclose(fp);
      }
      else
        fprintf(stderr, "To open %s is failed.\n", file);
    }
  }

  exit(EXIT_SUCCESS);
}

// #54
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

// #55
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

// #56
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

// #57
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

// #58
void DOCON()
{
  AX = read_word(++W);
  APUSH();
}

// #59
void DOVAR()
{
  push(++W);
  NEXT();
}

// #60
void DOTCON()
{
  AX = read_word(++W);
  W = read_word(W + 2);
  WPUSH();
}

// #61
void DOTVAR()
{
  push(++W);
  NEXT();
}

// #62
void DOUSE()
{
  BX = read_word(++W);
  BX &= 0x00FF;
  AX = BX + UP;
  APUSH();
}

/****************************************/

void (*func_table[])() = {
    NOP,
    JMP,
    CLD_,
    WRM,
    CTST,
    CIN,
    STIN,
    COUT,
    POUT,
    READ,
    WRITE,
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
    XDOES,
    DOCREA,
    BYE,
    POPEN,
    PCLOSE,
    SETIN,
    SETOUT,
    DOCON,
    DOVAR,
    DOTCON,
    DOTVAR,
    DOUSE};

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

  if (Memory[0] == 0x90)
    Memory[0] = 0; // NOP (0x90) of i8086 -> my NOP (0x00)

  if (argc > 1)
  {
    if (!strcmp(argv[1], "-h"))
    {
      printf("\noption\n\n");
      printf("  -h   :   Show the help of options.\n\n");
      printf("  -s   :   Assigning 1 to user variable STDIN, the program uses \"stdin\" instead of \"getch()\".\n");
      printf("           (Note. ");
      printf("This program uses \"stderr\" as the default output destination. ");
      printf("To change the output destination to \"stdout\", the value of FORTH's user variable \"PFLAG\" must be set to any non-zero number.)\n\n");
      printf("  -u   :   Changes made to the system are saved to FORTH80.bin.\n");
      printf("  -v   :   Display the version of this program.\n");
      return 0;
    }
    if (!strcmp(argv[1], "-s"))
      Memory[UVR + 56] = 1; // STDIN = 1
    if (!strcmp(argv[1], "-u"))
      update_flag = 1;
    if (!strcmp(argv[1], "-v"))
    {
      printf("Virtual Stack Machine for FORTH80 Version %d.%d.%d\n", MAJOR_V, MINOR_V, USER_V);
      return 0;
    }
  }

  while (1)
  {
    int opecode = Memory[PC];
    if (opecode <= 62)
      (*func_table[opecode])();
    else
    {
      fprintf(stderr, "%02X: undefined code.\n", opecode);
      break;
    }
  }

  return 0;
}
