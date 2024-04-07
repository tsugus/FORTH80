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
/*                       Version 0.5.6                        */
/*                                                            */
/*                                       (C) 2023-2024 Tsugu  */
/*                                                            */
/*                                                            */
/*  This program is released under                            */
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

FILE *printout, *cmdfp;

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

int stdin_flag = 0;

// #4 (environment dependent)
void CIN()
{
  switch (stdin_flag)
  {
  case 0:
    AX = (unsigned short)getch();
    if (AX == 0x7F) // for Mac
      AX = 0x08;    // fix of backspace key code
    break;
  case 1:
    AX = (unsigned short)getchar();
    break;
  default:
    AX = (unsigned short)getc(cmdfp);
    if (feof(cmdfp))
    {
      stdin_flag -= 2;
      pclose(cmdfp);
    }
  }

  AX &= 0x00FF;
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
  putchar(pop());
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
  d2 = (d2 << 16) | pop();
  unsigned int d1 = pop();
  d1 = (d1 << 16) | pop();
  d1 += d2;
  W = d1;
  AX = d1 >> 16;
  WPUSH();
}

// #36 (24h)
void DSUB()
{
  unsigned int d2 = pop();
  d2 = (d2 << 16) | pop();
  unsigned int d1 = pop();
  d1 = (d1 << 16) | pop();
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
  AX = (0x8000 & AX) | (AX >> 1);
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

// #61 (3Ch)
void POPENRCLOSE()
{
  unsigned short a = pop();
  printf("\n");
  char str[256];
  int i = 0;
  for (i = 0; i < Memory[a]; i++)
    str[i] = Memory[a + i + 1];
  str[i] = '\0';

  if ((cmdfp = popen(str, "r")) == NULL)
  {
    perror("can not exec commad");
    //    exit(EXIT_FAILURE);
  }
  else
  {
    if (!feof(cmdfp) && stdin_flag < 2)
      stdin_flag += 2;
  }

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

  if (argc > 1)
  {
    if (!strcmp(argv[1], "-s"))
      stdin_flag = 1;
    if (!strcmp(argv[1], "-h"))
    {
      printf("\noption\n\n");
      printf("  -h   :   Show the help of options.\n\n");
      printf("  -s   :   The program use \"stdin\" instead of \"getch()\".\n");
      printf("           (Note. ");
      printf("This program uses \"stderr\" as the default output destination. ");
      printf("To change the output destination to \"stdout\", the value of FORTH's user variable \"PFLAG\" must be set to any non-zero number.)\n\n");
      return 0;
    }
  }

  int loopf = 1;
  do
  {
    switch (Memory[PC])
    {
    case 0:
      loopf = 0;
      break;
    case 1:
      CLD_();
      break;
    case 2:
      WRM();
      break;
    case 3:
      CTST();
      break;
    case 4:
      CIN();
      break;
    case 5:
      COUT();
      break;
    case 6:
      POUT();
      break;
    case 7:
      READ();
      break;
    case 8:
      WRITE();
      break;
    case 9:
      WPUSH();
      break;
    case 10:
      APUSH();
      break;
    case 11:
      NEXT();
      break;
    case 12:
      NEXT1();
      break;
    case 13:
      LIT();
      break;
    case 14:
      EXEC();
      break;
    case 15:
      BRAN();
      break;
    case 16:
      ZBRAN();
      break;
    case 17:
      XLOOP();
      break;
    case 18:
      XPLOO();
      break;
    case 19:
      XDO();
      break;
    case 20:
      ANDD();
      break;
    case 21:
      ORR();
      break;
    case 22:
      XORR();
      break;
    case 23:
      SPAT();
      break;
    case 24:
      SPSTO();
      break;
    case 25:
      RPAT();
      break;
    case 26:
      RPSTO();
      break;
    case 27:
      SEMIS();
      break;
    case 28:
      TOR();
      break;
    case 29:
      FROMR();
      break;
    case 30:
      RAT();
      break;
    case 31:
      ZEQU();
      break;
    case 32:
      ZLESS();
      break;
    case 33:
      PLUS();
      break;
    case 34:
      SUBB();
      break;
    case 35:
      DPLUS();
      break;
    case 36:
      DSUB();
      break;
    case 37:
      OVER();
      break;
    case 38:
      DROP();
      break;
    case 39:
      SWAP();
      break;
    case 40:
      DUPE();
      break;
    case 41:
      ROT();
      break;
    case 42:
      USTAR();
      break;
    case 43:
      USLAS();
      break;
    case 44:
      TDIV();
      break;
    case 45:
      TOGGL();
      break;
    case 46:
      ATT();
      break;
    case 47:
      STORE();
      break;
    case 48:
      CSTOR();
      break;
    case 49:
      CMOVE();
      break;
    case 50:
      LCMOVE();
      break;
    case 51:
      FILL();
      break;
    case 52:
      DOCOL();
      break;
    case 53:
      DOCON();
      break;
    case 54:
      DOVAR();
      break;
    case 55:
      DOTCON();
      break;
    case 56:
      DOTVAR();
      break;
    case 57:
      DOUSE();
      break;
    case 58:
      XDOES();
      break;
    case 59:
      DOCREA();
      break;
    case 60: // BYE
      loopf = 0;
      break;
    case 61:
      POPENRCLOSE();
      break;
    case 0x90:
      NOP();
      break;
    case 0xE9:
      JMP();
      break;
    default:
      printf("%02X: undefined code.\n", Memory[PC]);
      loopf = 0;
    }
  } while (loopf);

  fclose(printout);

  return 0;
}
