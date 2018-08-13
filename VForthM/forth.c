/* SOD32 the Stack Oriented Design computer.
   Copyright 1994, L.C. Benschop, Eindhoven, The Netherlands.
   The program is released under the GNU General Public License version 2.
   There is NO WARRANTY.
*/
#include "stdio.h"
#include "stdint.h"
#include "ctype.h"
#include <linux/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <readline/readline.h>
#include <readline/history.h>

//#include <syslimits.h>
#include <fcntl.h>

#define INT32 int32_t

#define MEMSIZE (1<<22) /* must be a power of two */
#define MEMMASK MEMSIZE-1 /* mask for addresses to force them into range */

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

#define  TIB_SAZE 256
#define  vtst (__u32*)&(mem[0x7fbc])

typedef struct {
	__u32 oSTART;
	__u32 oSP;
	__u32 oTO_IN;
	__u32 oNTIB;
	__u8  oTTIB[TIB_SAZE];
	__u32 oATIB;
	__u32 oSTATE;
	__u32 oHANDLER;
	__u32 ooverflow;
	__u32 oDPOINT;
	__u32 oLAST;
	__u32 oLFORTH;
	__u32 oVFORTH;
	__u32 oCONTEXT[17];
	__u32 oCURRENT;
	__u32 oOUTPUT;
	__u32 oINPUT;
/*
#define LST	(*(__u32*)(mem+4*2))
#define VFORTH	(*(__u32*)(mem+4*3))
	*/
}  f_boot_t ;

f_boot_t * f_boot;
extern void ttwrite ( int __fd, const void *__buf, size_t __n);
extern __u32  ttgetchar(int __fd);
__u32 ttkbhit(int __fd);


#define farrd(reg)  ((int)&(f_boot->reg) - (int)(f_boot))

#define SP		farrd(oSP)
#define TO_IN		farrd(oTO_IN)
#define NTIB		farrd(oNTIB)
#define TTIB		farrd(oTTIB)
#define ATIB		farrd(oATIB)
#define STATE		farrd(oSTATE)
#define HANDLER 	farrd(oHANDLER)
#define overflow	farrd(ooverflow)
#define DPOINT		farrd(oDPOINT)
#define LAST		farrd(oLAST)
#define LFORTH		farrd(oLFORTH)
#define VFORTH		farrd(oVFORTH)
#define CONTEXT		farrd(oCONTEXT)
#define CURRENT		farrd(oCURRENT)
#define OUTPUT		farrd(oOUTPUT)
#define INPUT		farrd(oINPUT)

#define vocs_beg 	sizeof(*f_boot)

__u8 mem[MEMSIZE+3+4];

__u8 * DescriptorTable [16];

__u8 * v2r(__u32 VAddr)
{
//	return &mem[VAddr];
	return &  (DescriptorTable [ VAddr >> (32-4) ])[VAddr];
}

#define BYTE(reg) *v2r(reg)
#define WCELL(reg) (*(__u16*)v2r(reg))
#define CELL(reg)  (*(__u32*)v2r(reg))
#define DCELL(reg) (*(__u64*)v2r(reg))

#define FSTART	(*(__u32*)&mem[0])
char * HPATH="HOME";

#define HERE	CELL(DPOINT)
//   HERE ((*(f_boot_t*)&mem).oDPOINT)

#define last 	CELL(LAST)
//#define vforth	CELL(VFORTH)
#define context		CELL(CELL(CONTEXT))
#define current		CELL(CELL(CURRENT))
#define TIB	CELL(ATIB)

#define TIB	CELL(ATIB)


#define sp (*(__u32*)&mem[SP])

__u32 save_ip,interrupt;
extern char * vatr;

char filename[256];
char FileErrName[256];

int make_name(char *addr,__u32 len)
{
 int i;
 for(i=0;i<len && i<256;i++) {
//    if(addr[i]=='.' && addr[i+1]=='.') return 0;
    filename[i]=addr[i];
   /* Prohibit filenames in other directories */
 }
 filename[i]='\0';
 return 1;
}

//#define PUSH(n)	(sp-=4,CELL(sp)=n)
//#define POP()	(CELL(sp),sp+=4)

#define FNBSize   (1<<5)

__u32 FNBI[FNBSize];

__u8 FNB[FNBSize][256];
__u32  FNBCount;

/* Multiply 32-bit unsigned numbers *a and *b.
   High half of 64-bit result in *a, low half in *b
*/
static void umul(__u32 *a,__u32 *b)
{
 __u32 ah,al,bh,bl,ph,pm1,pm2,pl;
 char cc[44];
 ah=*a>>16;al=*a&0xffff;
 bh=*b>>16;bl=*b&0xffff;
 //sprintf(cc,"umul=%x %x\n",*b,vtst); write(1,cc,strlen(cc));

 pl=al*bl;
 if((ah|bh)==0) {
  ph=0;
 } else {
  pm1=al*bh;
  pm2=ah*bl;
  ph=ah*bh;
  pl=pl+(pm1<<16);
  ph+=(pl<(pm1<<16));
  pl=pl+(pm2<<16);
  ph+=(pl<(pm2<<16));
  ph=ph+(pm1>>16)+(pm2>>16);
 }
 *a=ph;*b=pl;
}

/* Divide 64-bit unsigned number (high half *b, low half *c) by
   32-bit unsigend number in *a. Quotient in *b, remainder in *c.
*/
static void udiv(__u32 *a,__u32 *b,__u32 *c)
{
 __u32 d,qh,ql;
 int i,cy;
 qh=*b;ql=*c;d=*a;
 if(qh==0) {
  *b=ql/d;
  *c=ql%d;
 } else {
  for(i=0;i<32;i++) {
   cy=qh&0x80000000;
   qh<<=1;
   if(ql&0x80000000)qh++;
   ql<<=1;
   if(qh>=d||cy) {
    qh-=d;
    ql++;
    cy=0;
   }
   *c=qh;
   *b=ql;
  }
 }
}

enum {
sw_nop,
sw_swap,
sw_rot,
sw_rrot,
sw_0equ,
sw_0Nequ,
sw_0LT,
sw_0GT,
sw_equ,
sw_nequ,
sw_negate,
sw_invert,
sw_ummul,
sw_Cload,
sw_Wload,
sw_load,
sw_1add,
sw_2add,
sw_1sub,
sw_2sub,
sw_2mul,
sw_2div,
sw_u2div,
sw_add,
sw_sub,
sw_mul,
sw_div,
sw_mod,
sw_celldiv,
sw_cells,
sw_cellp,
sw_cellm,
sw_and,
sw_or,
sw_xor,
sw_uLT,
sw_LT,
sw_uGT,
sw_GT,
sw_umin,
sw_umax,
sw_min,
sw_max,
sw_lshift,
sw_rshift,
sw_umSmod,
sw_addcy,
sw_Dadd,
sw_scan1,
sw_drop,
sw_2drop,
sw_on,
sw_0ST,
sw_ST,
sw_addST,
sw_cST,
sw_wST,
sw_2ST,
sw_2load,
sw_2swap,
sw_cSTa,
sw_VFMAdr,
sw_PLoad,
sw_PST,
sw_CPLoad,
sw_PDump,
sw_qdup,
sw_dup,
sw_2dup,
sw_over,
sw_2over,
sw_rload,
sw_rGT,
sw_rdrop,
sw_dupGTr,
sw_GTr,
sw_2rload,
sw_2rGT,
sw_2rdrop,
sw_2GTr,
sw_ip,
sw_lit,
sw_litcomp,
sw_comp,
sw_Wcomp,
sw_Ccomp,
sw_allot,
sw_perform,
sw_execute,
sw_compile,
sw_immediate,
sw_FromName,
sw_ret,
sw_femd,
sw_spload,
sw_spST,
sw_rpload,
sw_rpST,
sw_to_catch,
sw_from_catch,
sw_throw,
sw_setalarm,
sw_branch,
sw_0branch,
sw_BEGIN,
sw_AGAIN,
sw_UNTIL,
sw_AHEAD,
sw_IF,
sw_WHILE,
sw_THEN,
sw_ELSE,
sw_REPEAT,
sw_port,
sw_portq,
sw_key,
sw_keyq,
sw_emit,
sw_type,
sw_hdot,
sw_decdot,
sw_count,
sw_cstr,
sw_str,
sw_slitComp,
sw_StrComp,
sw_Compare,
sw_NextWord,
sw_openfile,
sw_closefile,
sw_FileErrType,
sw_File2err,
sw_writeline,
sw_readline,
sw_writefile,
sw_readfile,
sw_repositionfile,
sw_fileposition,
sw_filesize,
sw_Parse,
sw_CQ,
sw_SQ,
sw_PQ,
sw_Source,
sw_SourceST,
sw_SFind,
sw_SearchWordList,
sw_SHeader,
sw_alloc,
sw_free,
sw_vatr,
sw_FNType,
sw_halt,
   sw_ms,
  sw_hi
};


void COM(__u32 dd)
{ 
 CELL(HERE)=dd;
HERE+=4;
}

void WCOM(__u16 dd)
{ 
	WCELL(HERE)=dd;
	HERE+=2;
}

void CCOM(__u8 dd)
{ 
 BYTE(HERE)=dd;
  HERE+=1;
}

void StrComp(__u8 * word,__u32 size)
{ __u32 addr1,addr0;
  
addr0=HERE;
addr1=addr0+1;
while(size--)
	   BYTE(addr1++)=*word++;
BYTE(addr0)=addr1-addr0-1;
HERE=addr1;
if(HERE&1)CCOM(0);
}

void SHEADER(__u8 * word,__u32 size)
{ //    if(   size  & ( ~0xff  )  )	 printf(  "\nSHEADER size = %d\n",size );
	COM(current);
   	last=HERE;
	StrComp(word,size);
}


void HEADER(__u8 * word)
{  
	__u32 i;
    for (i = 0; word[i] != 0; i ++);
	SHEADER(word,i);
}

void IMMED()
{       
	BYTE(last)=BYTE(last)|0x80;
}

void PRIM(__u8 * word,__u32 cod)
{	
	HEADER(word);
	current=last;
	COM(cod<<1|0x8001);
}

void CONSTANT(__u8 * word,__u32 dd)
{	
	HEADER(word);
	current=last;
	COM(sw_lit<<1|0x4001);
	COM(dd);
}

void CREATE(__u8 * word)
	{	
	HEADER(word);
	current=last;
	COM(sw_ip<<1|0x4001);
}

void VARIABLE(__u8 * word)
{	
	CREATE(word);
	COM(0);
}

#ifndef memcasecmp
__u32 memcasecmp (const void *vs1, const void *vs2, __u32 n)
{
  unsigned int i;
  unsigned char const *s1 = (unsigned char const *) vs1;
  unsigned char const *s2 = (unsigned char const *) vs2;
  for (i = 0; i < n; i++)
    {
      unsigned char u1 = *s1++;
      unsigned char u2 = *s2++;
      if (toupper (u1) != toupper (u2))
        return toupper (u1) - toupper (u2);
    }
  return 0;
}
#endif

__u32 SEARCH(__u32 wid,  __u8 * word)
{ __u32 addr=wid;
  for(;;)
  {	if( !memcasecmp(word,&BYTE(addr+1),BYTE(addr)&0x7F) )
	return addr;
	addr-=4;
	addr=CELL(addr);
	if(!addr) return 0;
  }
}

__u32 FromName_(__u32 addr)
{ 
 addr+=(BYTE(addr)&0x7f)+1;
  if(addr&1)addr++;
  return addr;
}

void Compile(__u32 addr)
{	__u32 cod;
	
	  cod=CELL(addr);
	  if((cod&0x8001)==0x8001)
		  {COM(cod&0x3FFF); return;}
	  COM(addr);

}


void CC(__u8 * word)
{       
	__u32 addr=current;
	__u8 size;

	for (size = 0; word[size] != 0; size ++);

	do	{
		addr=SEARCH(addr,word);
//		if(!addr){ printf("\nmake_image: %s dont found\n " ,word);  }
	   	if((BYTE(addr)&0x3f)==size ) break;
		addr=CELL(addr-4);
	}while(addr);

	  if(!addr)
	    for(;;);
	  Compile(FromName_(addr));
}

void STR(char * str)
{	
	char * str0=str;
	__u32 addr0;
	COM(sw_str<<1|1);
	addr0=HERE;
	CCOM(0);
	while(*str)  CCOM(*str++);
        BYTE(addr0)=(__u8)(str-str0);
	if(HERE&1)CCOM(0);
}

void TP(char * str)
{ 
 STR(str);
 COM(sw_type<<1|1);
}

void F_END()
{	
	COM(0x8001);
	current=last;
}

void BEGIN()
{	
	sp-=4;CELL(sp)=HERE;
}

void AGAIN()
{	
	COM(sw_branch<<1|1);
	COM(CELL(sp));
	sp+=4;
}

void UNTIL()
{	
	COM(sw_0branch<<1|1);
	COM(CELL(sp));
	sp+=4;
}

void AHEAD()
{       
	sp-=4;CELL(sp)=0;
	AGAIN();
	sp-=4;CELL(sp)=HERE-4;
}

void IF()
{       
	sp-=4;CELL(sp)=0;
	UNTIL();
	sp-=4;CELL(sp)=HERE-4;
}

void WHILE()
{ 
  __u32 t;
	IF();
	t=CELL(sp);
	CELL(sp)=CELL(sp+4);
	CELL(sp+4)=t;
}

void THEN()
{       
	CELL(CELL(sp))=HERE;
	sp+=4;
}

void ELSE()
{ 
  __u32 t;
	AHEAD();
	t=CELL(sp);
	CELL(sp)=CELL(sp+4);
	CELL(sp+4)=t;
	THEN();
}

void REPEAT()
{	
	AGAIN();
	THEN();
}

void LIT(__u32 dd)
{	
	COM(sw_lit<<1|1);
	COM(dd);
}

void NextWord()
{ 
  __u32 addr,Waddr,Eaddr;
addr=CELL(TO_IN) + TIB ;
Eaddr=TIB+CELL(NTIB);
sp-=8;
	while ( (BYTE(addr) <= ' ') & (addr<Eaddr ) )addr++;
	CELL(sp+4)=Waddr=addr;
	while ( (BYTE(addr++)  > ' ') & (addr<=Eaddr )	);
	CELL(sp)=addr-Waddr-1;
	CELL(TO_IN)=addr - TIB;
}


void Parse()
{ 
  __u32 addr,Waddr,Eaddr;
   __u8 ch;
addr=CELL(TO_IN) + TIB ;
Eaddr=TIB+CELL(NTIB);
ch=BYTE(sp);
sp-=4;
	CELL(sp+4)=Waddr=addr;
	while ( (BYTE(addr)  != ch) & (addr<Eaddr )){addr++;};
	addr++;
	CELL(sp)=addr-Waddr-1;
	CELL(TO_IN)=addr - TIB;
}

void SQ()
{	
	sp-=4; CELL(sp)=(__u32)'"';Parse();
	if(CELL(STATE))
	{		__u32 addr,size;
	COM(sw_str<<1|1);
	addr=CELL(sp+4);
 	   size=CELL(sp);
// 	   size=MIN(size, MEMSIZE-addr);
 	   StrComp(&BYTE(addr),size);
  sp+=8;
	}
}

void CQ()
{	
	sp-=4; CELL(sp)=(__u32)'"';Parse();
	if(CELL(STATE))
	{		__u32 addr,size;
	COM(sw_cstr<<1|1);
	addr=CELL(sp+4);
 	   size=CELL(sp);
// 	   size=MIN(size, MEMSIZE-addr);
 	   StrComp(&BYTE(addr),size);
  sp+=8;
	}
}


void Source()
{	
	sp-=8;
	CELL(sp+4)=TIB;
	CELL(sp)=CELL(NTIB);
}

void SourceST()
{	
	__u32 len,addr;
	CELL(NTIB)=len=CELL(sp);
	addr=CELL(sp+4);
	while(len--)	BYTE(TIB+len)=BYTE(addr+len);
	sp+=8;
}

__u32 FromName(__u32 addr)
{ __u32 ixt;
	   ixt=FromName_(addr);
	  if(CELL(ixt)==0x7ff) // tag of code/voc separation 
		return CELL(ixt+4);
    	  else	return ixt; }

void SearchWordList() // ( c-addr u wid --- 0 | xt 1 xt -1 )
{	
  __u32 addr=CELL(CELL(sp));
   __u8  size=CELL(sp+4);
   __u8 * word=&BYTE(CELL(sp+8));
    	if(!addr)
    	{
    		sp+=8; CELL(sp)=0; return;
    	}
   for(;;)
    {
    	if(!addr)
    	{
    		sp+=8; CELL(sp)=0; return;
     	}
	   addr=SEARCH(addr,word);
    	if(!addr)
    	{
    		sp+=8; CELL(sp)=0; return;
    	}
    	if((BYTE(addr)&0x3f)==size )
    	{
	  sp+=4;
    	  if(BYTE(addr)&0x80)
    	   CELL(sp)=1;
    	  else
		CELL(sp)=-1;
		CELL(sp+4)=FromName(addr);
    	  return;
    	}
		addr-=4;
		addr=CELL(addr);
    }

}

void SFind()
{	int voc=CONTEXT;
	
	sp-=4; CELL(sp)=0;
	while( CELL(voc) )
	{	sp+=4;
	    sp-=4;CELL(sp)=CELL(sp+8);
	    sp-=4;CELL(sp)=CELL(sp+8);
		sp-=4;CELL(sp)=CELL(voc);
		SearchWordList();
		if(CELL(sp))
		{
			CELL(sp+8)=CELL(sp);
			CELL(sp+12)=CELL(sp+4);
			sp+=8;
			return;
		}	 voc+=4;
	}
}

#if 1
/* A static variable for holding the line. */
static char *line_read = (char *)NULL;
// http://oco.org.ua/sp-forth-%D0%B2-linux
/* Read a string, and return a pointer to it.  Returns NULL on EOF. */
char *
rl_gets ()
{
  /* If the buffer has already been allocated, return the memory
     to the free pool. */
  if (line_read)
    {
      free (line_read);
      line_read = (char *)NULL;
    }

  /* Get a line from the user. */
  line_read = readline ("");

  /* If the line has any text in it, save it on the history. */
  if (line_read && *line_read)
    add_history (line_read);

  return (line_read);
}
#endif

void virtual_machine()
{
	
 __u32 rp,ip,ireg,t,tmp;
 __u32 addr,len,fp,ior;
 __u32  res;
    char cc[44];
 __u32 zdbg=0;
 __u64 dtmp;
 interrupt=0;
 rp=MEMSIZE;
 sp=rp-32*444;
 ip=FSTART;
 for(;;) {
  if(interrupt) {
   ireg=1;
//  doint:
   rp-=4;
  CELL(rp)=ip;
   sp-=4;CELL(sp)=ireg;
   ip=CELL(interrupt);
   interrupt=0;
  }
  ireg=CELL(ip);
  ip+=4;
  
//  restart: /* Instruction restart after interrupt */
  if(!(ireg&1)) {
            rp-=4;
            CELL(rp)=ip;
            ip= ireg;
  } else {
	  if(zdbg)  {  printf("z=%x %x %d\n",ip,ireg&0xc000,(ireg&0x3fff)>>1); }
    switch( (ireg&0x3fff)>>1) {
     case sw_swap:	/*swap*/ t=CELL(sp);CELL(sp)=CELL(sp+4);
                      CELL(sp+4)=t;
     case sw_nop:	/*nop*/
    	 break;
     case sw_rot:	/*rot*/  t=CELL(sp+8);CELL(sp+8)=CELL(sp+4);
                      CELL(sp+4)=CELL(sp);CELL(sp)=t;break;
     case sw_rrot:	/*rot*/  t=CELL(sp);CELL(sp)=CELL(sp+4);
                      CELL(sp+4)=CELL(sp+8);CELL(sp+8)=t;break;
     case sw_0equ:	/*0=*/   CELL(sp)=-!CELL(sp);break;
     case sw_0Nequ:	/*0<>*/   CELL(sp)=-!!CELL(sp);break;
     case sw_0LT:	/*0<*/   CELL(sp)=-((INT32)CELL(sp)<0);break;
     case sw_0GT:	/*0>*/   CELL(sp)=-((INT32)CELL(sp)>0);break;
     case sw_equ:	/*=*/  	 CELL(sp+4)=-(CELL(sp)==CELL(sp+4));sp+=4;break;
     case sw_nequ:	/*=*/  	 CELL(sp+4)=-(CELL(sp)!=CELL(sp+4));sp+=4;break;
     case sw_negate:	/*negate*/ CELL(sp)=-CELL(sp);break;
     case sw_invert:	/*invert*/ CELL(sp)=~CELL(sp);break;
     case sw_ummul:	/* um* */ umul(&CELL(sp),&CELL(sp+4));break;
     case sw_Cload:	/* C@ */  CELL(sp)=BYTE(CELL(sp));break;
     case sw_Wload: /* W@ */  CELL(sp)=WCELL(CELL(sp));break;
     case sw_load:	/* @ */   CELL(sp)=CELL(CELL(sp));break;
     case sw_1add:	/* 1+ */   CELL(sp)++;break;
     case sw_2add:	/* 2+ */   CELL(sp)+=2;break;
     case sw_1sub:	/* 1- */   CELL(sp)--;break;
     case sw_2sub:	/* 2+ */   CELL(sp)-=2;break;
     case sw_2mul:	/* 2* */   CELL(sp)*=2;break;
     case sw_2div:	/* 2/ */   CELL(sp)=(INT32)CELL(sp)/2;break;
     case sw_u2div:	/* 2/ */   CELL(sp)/=2;break;
     case sw_add:	/* + */   CELL(sp+4)=CELL(sp)+CELL(sp+4);sp+=4;break;
     case sw_sub:	/* - */   CELL(sp+4)-=CELL(sp);sp+=4;break;
     case sw_mul:	/* * */   CELL(sp+4)=(INT32)CELL(sp)*(INT32)CELL(sp+4);sp+=4;break;
     case sw_div:	/* / */   if(CELL(sp))
     	 	 	 { CELL(sp+4)=(INT32)CELL(sp+4)/(INT32)CELL(sp);sp+=4;break;
     	 	 	 } CELL(sp+4)=(__u32)-1; sp+=4;break;
     case sw_mod:	/* mod */ if(CELL(sp))
	 	 { CELL(sp+4)=(INT32)CELL(sp+4)%(INT32)CELL(sp);sp+=4;break;
	 	 } CELL(sp+4)=0; sp+=4;break;
     case sw_celldiv:	/* cell/ */ CELL(sp)>>=2;break;
     case sw_cells:	/* cells */ CELL(sp)<<=2;break;
     case sw_cellp:	/* cells */ CELL(sp)+=4;break;
     case sw_cellm:	/* cells */ CELL(sp)-=4;break;
     case sw_and:	/* and */ CELL(sp+4)=CELL(sp)&CELL(sp+4);sp+=4;break;
     case sw_or:	/* or  */ CELL(sp+4)=CELL(sp)|CELL(sp+4);sp+=4;break;
     case sw_xor:	/* xor */ CELL(sp+4)=CELL(sp)^CELL(sp+4);sp+=4;break;
     case sw_uLT:	/* u<  */ CELL(sp+4)=-(CELL(sp+4)<CELL(sp));sp+=4;break;
     case sw_LT:	/* <   */ CELL(sp+4)=-((INT32)CELL(sp+4)<(INT32)CELL(sp));sp+=4;break;
     case sw_uGT:	/* u>  */ CELL(sp+4)=-(CELL(sp+4)>CELL(sp));sp+=4;break;
     case sw_GT:	/* >  */ CELL(sp+4)=-((INT32)CELL(sp+4)>(INT32)CELL(sp));sp+=4;break;
     case sw_umin:	/* umin */ CELL(sp+4)=MIN(CELL(sp+4),CELL(sp));sp+=4;break;
     case sw_umax:	/* umax */ CELL(sp+4)=MAX(CELL(sp+4),CELL(sp));sp+=4;break;
     case sw_min:	/* min */ CELL(sp+4)=MIN((INT32)CELL(sp+4),(INT32)CELL(sp));sp+=4;break;
     case sw_max:	/* max */ CELL(sp+4)=MAX((INT32)CELL(sp+4),(INT32)CELL(sp));sp+=4;break;
     case sw_lshift:/*lshift*/CELL(sp+4)<<=CELL(sp);sp+=4;break;
     case sw_rshift:/*rshift*/CELL(sp+4)>>=CELL(sp);sp+=4;break;
     case sw_umSmod:/*um/mod*/if(CELL(sp)<=CELL(sp+4)) { /*overflow */
//                         interrupt=overflow;  goto doint;
    	 	 	 	 sp+=4;CELL(sp)=(__u32)-1;CELL(sp+4)=(__u32)-1; break;
                       }
                        udiv(&CELL(sp),&CELL(sp+4),&CELL(sp+8));
                        sp+=4;
                       break;
     case sw_addcy:	/* +cy */ { __u32 sum; if(CELL(sp)) { /* carry in */
                          sum=CELL(sp+8)+CELL(sp+4)+1;
                          sp+=4;
                          CELL(sp)=(sum<=CELL(sp));
                          CELL(sp+4)=sum;
                         } else {
                          sum=CELL(sp+8)+CELL(sp+4);
                          sp+=4;
                          CELL(sp)=(sum<CELL(sp));
                          CELL(sp+4)=sum;
                         }
                       }break;
     case sw_Dadd:	/* d+ */
     	 	 	 { __u64 sum= ((__u64)CELL(sp)<<32) + (__u64)CELL(sp+4) +
     	 	 		 ((__u64)CELL(sp+8)<<32) + (__u64)CELL(sp+12);
                 CELL(sp+8)=sum>>32;
                 CELL(sp+12)=sum; sp+=8;break;
     	 	 	 }
     case sw_scan1:/*scan1*/ if(CELL(sp)){ /* scan from right */
                         t=0;while(!(CELL(sp+4)&1) && t<32) {
                          CELL(sp+4)>>=1;t++;
                         }
                       } else {
                         t=0;while(!(CELL(sp+4)&0x80000000) && t<32) {
                          CELL(sp+4)<<=1;t++;
                         }
                       }sp+=4;CELL(sp)=t;break;
     case sw_drop:/* drop */ sp+=4;break;
     case sw_2drop:/* drop */ sp+=8;break;
     case sw_on:/* 0! */   CELL(CELL(sp))=(__u32)-1;sp+=4;break;
     case sw_0ST:/* 0! */  CELL(CELL(sp))=0;sp+=4;break;
     case sw_ST: /*  ! */  CELL(CELL(sp))=CELL(sp+4);sp+=8;break;
     case sw_addST: /*  +! */  CELL(CELL(sp))+=CELL(sp+4);sp+=8;break;
     case sw_cST:/* C! */  BYTE(CELL(sp))=CELL(sp+4);sp+=8;break;
     case sw_wST:/* W! */  WCELL(CELL(sp))=CELL(sp+4);sp+=8;break;
     case sw_2ST:/* 2! */    DCELL(CELL(sp))=DCELL(sp+4);sp+=12;break;
     case sw_2load:/* 2@ */ sp-=4; DCELL(sp)=DCELL(CELL(sp+4));break;
     case sw_2swap:/* 2swap */    dtmp=DCELL(sp);
			 DCELL(sp)=DCELL(sp+8);
			 DCELL(sp+8)=dtmp;break;
     case sw_cSTa:/* c!a  */ t=CELL(sp);BYTE(t)=CELL(sp+4);sp+=4;
                        CELL(sp)=t;break;

     case sw_VFMAdr:/* VFMADR */        sp-=4;
     CELL(sp)= (__u32)&mem; break;

     case sw_PLoad: 	/* P@ */
    	 tmp=CELL(sp);
    	 CELL(sp)=  *(__u32*)tmp; break;

     case sw_PST: /*  P! */
    	 *(__u32*)(CELL(sp))=CELL(sp+4);
    	 sp+=8;break;

     case sw_CPLoad: 	/* P!@ */
    	 tmp=CELL(sp);
    	 CELL(sp)= (__u32) *(__u8*)tmp; break;

     case sw_PDump: 	/* pdump */
     {
#if 1
	 __u8 * BB ;  char  buffer[111]; int len;
     BB= (__u8*)  CELL(sp+4);
     len=CELL(sp);
    	 while(len>0)
    	 {
         sprintf (buffer,
     "%X\t%2X %2X %2X %2X %2X %2X %2X %2X  %2X %2X %2X %2X %2X %2X %2X %2X | ",
        		 &BB[0],
	 BB[0], BB[1], BB[2], BB[3],  BB[4], BB[5], BB[6], BB[7],	 BB[8], BB[9], BB[10], BB[11], BB[12], BB[13], BB[14], BB[15]	  );
         for (t = 0; buffer[t] != 0; t ++); t--;
    	ttwrite(CELL(OUTPUT),buffer, t  );
        for (t = 0; t<16; t ++)
        	{  if(  BB[t]<' ')      ttwrite(CELL(OUTPUT),".", 1  );
        	else  ttwrite(CELL(OUTPUT),&BB[t], 1  );
        	}  ttwrite(CELL(OUTPUT),"\n", 1  );


    	 len-=16;    	 BB+=16;
         }    	 sp+=8;
#endif
     }    	 break;
     case sw_qdup:/* dup  */  if(CELL(sp)==0)break;
     case sw_dup:/* dup  */ sp-=4;CELL(sp)=CELL(sp+4);break;
     case sw_2dup:/* 2dup */ sp-=4;CELL(sp)=CELL(sp+8);
     case sw_over:/* over */ sp-=4;CELL(sp)=CELL(sp+8);break;

     case sw_2over:/* 2over */	sp-=4;CELL(sp)=CELL(sp+16);
				sp-=4;CELL(sp)=CELL(sp+16);break;

     case sw_rload:	/* r@   */   sp-=4;CELL(sp)=CELL(rp);break;
     case sw_rGT:	/* r>   */   sp-=4;CELL(sp)=CELL(rp);
     case sw_rdrop:	/* rdrop */  rp+=4;break;
     case sw_dupGTr:	/* dup>r   */ rp-=4;CELL(rp)=CELL(sp);break;
     case sw_GTr:	/* >r   */   rp-=4;CELL(rp)=CELL(sp);sp+=4;break;
     case sw_2rload:	/* 2r@   */  sp-=8;CELL(sp)=CELL(rp);
		CELL(sp+4)=CELL(rp+4);break;
     case sw_2rGT:	/* 2r>   */  sp-=8;CELL(sp)=CELL(rp);
		CELL(sp+4)=CELL(rp+4);
     case sw_2rdrop:	/* 2rdrop */ rp+=8;break;
     case sw_2GTr:	/* 2>r   */  rp-=8;CELL(rp)=CELL(sp);
		CELL(rp+4)=CELL(sp+4);
	sp+=8;break;
     case sw_ip:/*ip*/
    	 sp-=4;CELL(sp)=ip;ip+=4;break;
     case sw_lit:/*lit*/
    	 sp-=4;CELL(sp)=CELL(ip);ip+=4;break;

     case sw_litcomp:/*lit*/	COM(sw_lit<<1|1);
     case sw_comp:/* , */	COM(CELL(sp));	sp+=4;break;
     case sw_Wcomp:/* W, */	WCOM(CELL(sp));	sp+=4;break;
     case sw_Ccomp:/* C, */	CCOM(CELL(sp));	sp+=4;break;
     case sw_allot:/* allot */   HERE+=CELL(sp); sp+=4;break;
     case sw_perform:	/* perform */   CELL(sp)=CELL(CELL(sp));
     case sw_execute:/* execute */
    	 rp-=4;CELL(rp)=ip; ip=CELL(sp); sp+=4;break;
     case sw_compile:/* compile */
    	 Compile(CELL(sp));
    	 sp+=4;break;
     case sw_immediate:/* immediate */
    	 IMMED();break;
     case sw_femd:/*return*/  F_END();break;
     case sw_FromName:/*return*/ CELL(sp)=FromName(CELL(sp));break;


     case sw_spload: /* sp@ */ sp-=4;CELL(sp)=sp+4;break;
     case sw_spST: /* sp! */ sp=CELL(sp);break;
     case sw_rpload: /* rp@ */ sp-=4;CELL(sp)=rp;break;
     case sw_rpST: /* rp! */ rp=CELL(sp);sp+=4;break;

//     case sw_setalarm: /* setalarm */ setalarm(CELL(sp));sp+=4;break;

     case sw_to_catch:
   	 	 	    rp-=12;
   	 	 	    CELL(rp+8)=CELL(HANDLER);
   	 	 	    CELL(rp+4)=sp;
   	 	 	    CELL(rp)=ip;
   	 	 	    CELL(HANDLER)=rp;
   	 	 	 	ip=CELL(sp);
     	 	 	sp+=4;
     	 	 	 break;
     case sw_from_catch:
 	 	  	  	CELL(HANDLER)=CELL(rp+8-4);
 	 	  	  	rp+=12-4;
 	 	  	  	sp-=4;CELL(sp)=0;
 	 	  	  	break;

     case sw_throw:
    	 	 	 	if( !CELL(sp)) { sp+=4; break; }
    	 	 	 	if( !CELL(HANDLER)) {for(;;);}
    	 	 	 	{ __u32 savesp=CELL(sp);
    	 	 	 	  rp=CELL(HANDLER);
    	 	 	 	  CELL(HANDLER)=CELL(rp+8);
    	 	 	 	  sp=CELL(rp+4);
    	 	 	 	  CELL(sp)=savesp;
    	 	 	 	  ip=CELL(rp)+4; // over from_catch
    	 	 	 	  rp+=12;
    	 	 	 	 break;
    	 	 	 	}

     case sw_branch: /*branch*/
    	 ip = CELL(ip);break;
     case sw_0branch: /*0branch*/
    	 if(CELL(sp)) ip+=4; else ip= CELL(ip); sp+=4; break;

    	 case sw_BEGIN:  BEGIN();break;
    	 case sw_AGAIN:  AGAIN();break;
    	 case sw_UNTIL:  UNTIL();break;
    	 case sw_AHEAD:  AHEAD();break;
    	 case sw_IF:     IF();break;
    	 case sw_WHILE:  WHILE();break;
    	 case sw_THEN:   THEN();break;
    	 case sw_ELSE:   ELSE();break;
    	 case sw_REPEAT: REPEAT();break;


     case sw_key: /*read char */	sp-=4;CELL(sp)=(__u32)ttgetch(0);break;
     case sw_keyq: /*check key */	sp-=4;CELL(sp)=-(INT32)ttkbhit(0);break;
     case sw_port: /*read char */	CELL(sp)=(__u32)ttgetch(CELL(sp));break;
     case sw_portq: /*check key */	CELL(sp)=-(INT32)ttkbhit(CELL(sp));break;
     case sw_emit: /*print char*/
    	ttwrite(CELL(OUTPUT),&BYTE(sp),1);
    	 sp+=4;break;
     case sw_type: /*type*/
//         if( CELL(sp+4)+CELL(sp) < MEMSIZE )
        	 ttwrite(CELL(OUTPUT),&BYTE(CELL(sp+4)),CELL(sp));
        sp+=8;break;

     case sw_hdot: /*print hex*/
     {   char buffer[44];
     	 tmp= CELL(sp);
         sprintf (buffer, "%X ",tmp);
         for (t = 0; buffer[t] != 0; t ++);
    	 ttwrite(CELL(OUTPUT),buffer, t  );
     }
     sp+=4;break;
     case sw_decdot: /*print decimal*/
     {   char buffer[44];
         sprintf (buffer, "%d ", CELL(sp));
     }
     sp+=4;break;

     case sw_count: /*count*/ sp-=4;
	     CELL(sp)= BYTE(CELL(sp+4)); 
	     CELL(sp+4)++; break;
     case sw_cstr: /*(C")*/ sp-=4;  CELL(sp)=ip; t=BYTE(ip++);
			ip+=t; if(ip&1)ip++; break;
     case sw_str: /*(S")*/ sp-=8; t=BYTE(ip++); CELL(sp)=t; CELL(sp+4)=ip;
			ip+=t; if(ip&1)ip++; break;

     case sw_slitComp: /* sliteral */
			COM(sw_str<<1|1);
     case sw_StrComp: /* S, */
			StrComp( v2r(CELL(sp+4)),CELL(sp));
			sp+=8;break;
     case sw_Compare: /* Compare */
     {
    	     int addr1,len1,addr2,len2;
    	     len1=CELL(sp);    addr1=CELL(sp+4);
    	     len2=CELL(sp+8);  addr2=CELL(sp+12); sp+=12;
    	          if(len1!=len2) { CELL(sp)=len1-len2; break; }
//    	          if( addr1+len1 < MEMSIZE )
//        	      if( addr2+len2 < MEMSIZE )
    	          CELL(sp)=memcmp(&BYTE(addr1),&BYTE(addr2),len1);
    	 break;
     }

     case sw_NextWord:
    	 NextWord(); break;

     case sw_openfile: /*open-file*/
    	 sp+=4;len=CELL(sp);addr=CELL(sp+4); // ior=0;
     	 if(!make_name(v2r(addr),len)) {ior=-202; CELL(sp)=ior;break;}
		ior=open(filename,CELL(sp-4),S_IRUSR | S_IWUSR );
		strcpy( &FNB[ior&(FNBSize-1)], filename );
		CELL(sp+4)=ior;
		 CELL(sp)=-((ior+1)==0);break;

     case sw_closefile: /*close-file*/ 	 CELL(sp)=close(CELL(sp));break;;
     case sw_File2err:
     			strcpy( FileErrName, &FNB[CELL(sp)&(FNBSize-1)] );
     	   sp+=4;

    	 break;

     case sw_FileErrType:
    	 ttwrite(2,FileErrName,strlen(FileErrName));


     	 break;

     case sw_writeline: /*write-line*/ sp+=8;fp=CELL(sp-8); len=CELL(sp-4);
     	 	 	 addr=CELL(sp);
				 write(fp,v2r(addr),len);
	    		 write(fp,"\n",1);    ior=0;
                 CELL(sp)=ior;break;

     case sw_readline: /*read-line*/
     { __u32 ior; 
      //__u32 len; __u32 addr;
     __u32 rrr;
	 fp=CELL(sp);
	 len=CELL(sp+4);
     addr=CELL(sp+8);
     res=0;

     if(fp)
		while(len--)
		{  int rrr=0;
			do{     rrr=read(fp,v2r(addr+res),1);
				}while(rrr==-1);
				  if(rrr!=1) break;
				if(BYTE(addr+res)=='\n') { res++; break; }
                               	res++;
		}
     else {
     		 char * zzz;
     		 zzz=rl_gets();
     		write_history("rlhistory.cfg");
     		 while( (*zzz!=0) & (len!=0) )
     		 { BYTE(addr+res)=*zzz; res++; zzz++;len--; }
     		 strlen(&BYTE(addr));
     		 BYTE(addr+res)='\n';
     		 res++;
     }

     	ior=0;
		 BYTE(addr+res)=0;
               if(res) { CELL(sp+4)=0xffffffff;
         		while((BYTE(addr+res-1)<' ')&(res>1)) res--; }
               else  CELL(sp+4)=0;
                         CELL(sp+8)=res;
                         CELL(sp)=ior;

    } break;

     case sw_writefile: /*write-file*/sp+=8;fp=CELL(sp-8); len=CELL(sp-4);
          addr=CELL(sp);
          res=write(fp,v2r(addr),len);
          CELL(sp)=res;break;
     case sw_readfile: /*read-file*/ sp+=4;fp=CELL(sp-4); len=CELL(sp);
     	  addr=CELL(sp+4);
     	  res=read(fp,mem+addr,len);ior=0;
     	  CELL(sp+4)=res;
		  CELL(sp)=ior;break;

     case sw_repositionfile: /*reposition-file*/ sp+=8;fp=CELL(sp-8);
                             lseek(fp,CELL(sp),SEEK_SET); ior=0;
				CELL(sp)=ior;break;

     case sw_fileposition: /*file-position*/ sp-=8;fp=CELL(sp+8);
                             CELL(sp+8)=lseek(fp, (off_t) 0, SEEK_CUR);CELL(sp+4)=0;
				 ior=0;
                             CELL(sp)=ior;break;
     case sw_filesize: /* file-size*/ sp-=8;fp=CELL(sp-8);
     { int fpt;
         fpt=lseek(fp, (off_t) 0, SEEK_CUR);
         CELL(sp+8)=lseek(fp, (off_t) 0, SEEK_END);
         CELL(sp+4)=0;
         CELL(sp)=0;
            lseek(fp, (off_t) fpt, SEEK_SET);
      }break;

     case sw_Parse:		Parse(); break;
     case sw_CQ: /* C" */ CQ();       break;
     case sw_SQ: /* S" */ SQ();       break;
     case sw_PQ: /* ." */ SQ();	 COM(sw_type<<1|1); break;
     case sw_SourceST:		SourceST(); break;
     case sw_Source:		Source(); break;
     case sw_SearchWordList:	SearchWordList(); break;
     case sw_SFind:		SFind(); break;
     case sw_SHeader: //	if( CELL(sp+4)+CELL(sp) < MEMSIZE )
    	                  SHEADER( &BYTE(CELL(sp+4)), CELL(sp) );
    	 	 	 	 sp+=8; break;

    case sw_alloc:
	 {	int ii;
		__u32 a_size =  CELL(sp);

		sp-=4;
		for(ii=1;ii<17;ii++)
		{ if( DescriptorTable[ii] == (__u8 *)0) break;
		}

		if(ii==17) CELL(sp) = -1;
		else
		{ __u8 * a_addr = malloc(a_size);
			DescriptorTable[ii]= & a_addr[-(ii<<(32-4))] ;
			CELL(sp)=0;
			CELL(sp+4)=ii<<(32-4);
		}

	 }
 break;

    case sw_free:
	{ __u8 ii = CELL(sp)>>(32-4);
		free( &(DescriptorTable[ii]) [(ii<<(32-4))] );
		DescriptorTable[ii]=(__u8 * ) 0;
		CELL(sp) = 0 ;
	 } break;

     case sw_vatr:
    	 strcpy( &BYTE(CELL(sp)+1) , vatr );
    	 BYTE(CELL(sp))=strlen(vatr);

         break;
    case  sw_FNType:
	{char filePath[222];
/*
	 if (fcntl(CELL(sp), F_GETPATH, filePath) != -1)
		{    
        	 ttwrite(CELL(OUTPUT),&filePath,sizeof(filePath));
		}
*/
	} sp+=4;
         break;

     case sw_halt: exit(CELL(sp));
     case sw_ms: /* ms*/ usleep(CELL(sp)*1000);sp+=4;break;

     case sw_hi:
         write(1,"Hello!!!\n",9);
         break;

   }

   if(ireg&0xC000)/*return*/ {
            ip=CELL(rp);
            rp+=4;
           } 

  }
 }
}

make_image()
{ 	
	__u32 shere;
  HERE=0;
 HERE=vocs_beg;
 CELL(HANDLER)=0;
 CELL(ATIB)=TTIB;
 CELL(CONTEXT)=VFORTH;
 CELL(CONTEXT+4)=0;
 CELL(CURRENT)=VFORTH;
 CELL(INPUT)=0;
 CELL(OUTPUT)=1;
 CELL(LFORTH)=0;
 current=0;
 sp = MEMSIZE-32*2;

PRIM("NOOP",sw_nop);
PRIM("CHARS",sw_nop);
PRIM("SWAP",sw_swap);
PRIM("ROT",sw_rot);
PRIM("-ROT",sw_rrot);
PRIM("0=",sw_0equ);
PRIM("0<>",sw_0Nequ);
PRIM("0<",sw_0LT);
PRIM("0>",sw_0GT);
PRIM("=",sw_equ);
PRIM("<>",sw_nequ);
PRIM("NEGATE",sw_negate);
PRIM("INVERT",sw_invert);
PRIM("UM*",sw_ummul);
PRIM("C@",sw_Cload);
PRIM("W@",sw_Wload);
PRIM("@",sw_load);
PRIM("ON",sw_on);
PRIM("0!",sw_0ST);
PRIM("1+",sw_1add);
PRIM("2+",sw_2add);
PRIM("1-",sw_1sub);
PRIM("2-",sw_2sub);
PRIM("2*",sw_2mul);
PRIM("2/",sw_2div);
PRIM("U2/",sw_u2div);
PRIM("+",sw_add);
PRIM("-",sw_sub);
PRIM("*",sw_mul);
PRIM("/",sw_div);
PRIM("MOD",sw_mod);
PRIM("CELL/",sw_celldiv);
PRIM("CELLS",sw_cells);
PRIM("CELL+",sw_cellp);
PRIM("CELL-",sw_cellm);
PRIM("AND",sw_and);
PRIM("OR",sw_or);
PRIM("XOR",sw_xor);
PRIM("U<",sw_uLT);
PRIM("<",sw_LT);
PRIM("U>",sw_uGT);
PRIM(">",sw_GT);
PRIM("UMIN",sw_umin);
PRIM("UMAX",sw_umax);
PRIM("MIN",sw_min);
PRIM("MAX",sw_max);
PRIM("LSHIFT",sw_lshift);
PRIM("RSHIFT",sw_rshift);
PRIM("UM/MOD",sw_umSmod);
PRIM("+CY",sw_addcy);
PRIM("D+",sw_Dadd);
PRIM("SCAN1",sw_scan1);
PRIM("DROP",sw_drop);
PRIM("2DROP",sw_2drop);
PRIM("MLIT,",sw_litcomp);
PRIM(",",sw_comp);
PRIM("W,",sw_Wcomp);
PRIM("C,",sw_Ccomp);
PRIM("ALLOT",sw_allot);
PRIM("PERFORM",sw_perform);
PRIM("EXECUTE",sw_execute);
PRIM("COMPILE,",sw_compile);
PRIM("IMMEDIATE",sw_immediate);
PRIM("NAME>",sw_FromName);
PRIM(">BODY",sw_cellp);
PRIM("SP@",sw_spload);
PRIM("SP!",sw_spST);
PRIM("RP@",sw_rpload);
PRIM("RP!",sw_rpST);

PRIM("THROW",sw_throw);
PRIM("!",sw_ST);
PRIM("+!",sw_addST);
PRIM("C!",sw_cST);
PRIM("W!",sw_wST);
PRIM("2!",sw_2ST);
PRIM("2@",sw_2load);
PRIM("2SWAP",sw_2swap);
PRIM("C!A",sw_cSTa);
PRIM("VFMADR",sw_VFMAdr);
PRIM("P@",sw_PLoad);
PRIM("P!",sw_PST);
PRIM("PC@",sw_CPLoad);
PRIM("PDUMP",sw_PDump);
PRIM("?DUP",sw_qdup);
PRIM("DUP",sw_dup);
PRIM("2DUP",sw_2dup);
PRIM("OVER",sw_over);
PRIM("2OVER",sw_2over);
PRIM("R@",sw_rload);
PRIM("R>",sw_rGT);
PRIM("RDROP",sw_rdrop);
PRIM("DUP>R",sw_dupGTr);
PRIM(">R",sw_GTr);
PRIM("2R@",sw_2rload);
PRIM("2R>",sw_2rGT);
PRIM("2RDROP",sw_2rdrop);
PRIM("2>R",sw_2GTr);
PRIM("KEY",sw_key);
PRIM("KEY?",sw_keyq);
PRIM("PORT@",sw_port);
PRIM("PORT?",sw_portq);
PRIM("EMIT",sw_emit);
PRIM("TYPE",sw_type);
PRIM("MS",sw_ms);
PRIM("HI",sw_hi);
PRIM("H.",sw_hdot);
PRIM("DEC.",sw_decdot);
PRIM("COUNT",sw_count);
PRIM("SLIT,",sw_slitComp);
PRIM("S\",",sw_StrComp);
PRIM("COMPARE",sw_Compare);
PRIM("PARSE-NAME",sw_NextWord);
PRIM("PARSE",sw_Parse);
PRIM("C\"", sw_CQ); IMMED();
PRIM("S\"", sw_SQ); IMMED();
PRIM(".\"", sw_PQ); IMMED();
PRIM("SOURCE",sw_Source);
PRIM("SOURCE!",sw_SourceST);
PRIM("SEARCH-WORDLIST",sw_SearchWordList);
PRIM("SFIND",sw_SFind);
PRIM("SHEADER_",sw_SHeader);

PRIM("BEGIN",sw_BEGIN); IMMED();
PRIM("AGAIN",sw_AGAIN); IMMED();
PRIM("UNTIL",sw_UNTIL); IMMED();
PRIM("AHEAD",sw_AHEAD); IMMED();
PRIM("IF",sw_IF); IMMED();
PRIM("WHILE",sw_WHILE); IMMED();
PRIM("THEN",sw_THEN); IMMED();
PRIM("ELSE",sw_ELSE); IMMED();
PRIM("REPEAT",sw_REPEAT); IMMED();

PRIM("OPEN-FILE",sw_openfile);
PRIM("CLOSE-FILE",sw_closefile);

PRIM("FILE2ERR",sw_File2err);
PRIM("FE-TYPE",sw_FileErrType);

PRIM("WRITE-LINE",sw_writeline);
PRIM("READ-LINE",sw_readline);

PRIM("WRITE-FILE",sw_writefile);
PRIM("READ-FILE",sw_readfile);

PRIM("REPOSITION-FILE",sw_repositionfile);
PRIM("FILE-POSITION",sw_fileposition);
PRIM("FILE-SIZE",sw_filesize);

PRIM("ALLOCATE",sw_alloc);
PRIM("FREE",sw_free);
PRIM("VATR",sw_vatr);
PRIM("FNT",sw_FNType);
PRIM("HALT",sw_halt);

HEADER("EXECUTE"); current=last; COM(sw_GTr<<1|0x4001);

CONSTANT("CELL",4);
CONSTANT(">IN",TO_IN);
CONSTANT("STATE",STATE);
CONSTANT("[EXIT]",0x8001);
CONSTANT("C/L",TIB_SAZE);
CONSTANT("ATIB",ATIB);
CONSTANT("TTIB",TTIB);
CONSTANT("#TIB",NTIB);
CONSTANT("DP",DPOINT);
CONSTANT("CURRENT",CURRENT);
CONSTANT("CONTEXT",CONTEXT);
CONSTANT("LAST",LAST);
CONSTANT("FORTH-WORDLIST",VFORTH);
CONSTANT("CREATE-CODE",sw_ip<<1|0x4001);
CONSTANT("CONSTANT-CODE",sw_lit<<1|0x4001);
CONSTANT("BRANCH-CODE",sw_branch<<1|1);
CONSTANT("PPPP",0x55);


CONSTANT("R/O",O_RDONLY);
CONSTANT("W/O",O_WRONLY);
CONSTANT("R/W",O_RDWR);
CONSTANT("O_CREAT",O_CREAT);
CONSTANT("O_TRUNC",O_TRUNC);

HERE+=0x300;

CONSTANT("PAD",HERE-0x100);

VARIABLE("DPL");
VARIABLE("BASE");
VARIABLE("SP0");
VARIABLE("CURSTR");
VARIABLE("SAVEERR?");

CONSTANT("STDOUT",OUTPUT);

CONSTANT("STDIN",INPUT);
CONSTANT("SID",INPUT);

HEADER("LIT,"); COM(sw_litcomp<<1|1);
F_END();

HEADER("SHEADER"); CC("SHEADER_"); F_END();
HEADER("DECIMAL");
// TP(" DECIMAL");
 LIT(10); CC("BASE");  CC("!"); F_END();
HEADER("HEX");     LIT(16); CC("BASE");  CC("!"); F_END();
HEADER("["); CC("STATE");  CC("0!"); F_END();  IMMED();
HEADER("]"); CC("STATE");  CC("ON"); F_END();
HEADER("TIB"); CC("ATIB");  CC("@"); F_END();


HEADER("CATCH");
COM(sw_to_catch<<1|1);
COM(sw_from_catch<<1|1);
F_END();

 //CC("PARSE-NAME");   CC("2DUP"); CC("TYPE");   CC("2DROP");
  // CC("SHEADER");  CC("LAST"); CC("@"); CC("CURRENT"); CC("@"); CC("!");
// F_END();

 HEADER("DNEGATE"); // ( d1 --- d2)
 // Negate the top double number on the stack.
 CC(">R");
 CC("NEGATE");
 CC("R>");
 CC("NEGATE");
 CC("OVER");
 CC("0<>");
 CC("+");
 F_END();


 HEADER("DIGIT?"); // ( c -- 0| c--- n -1)
 // Convert character c to its digit value n and return true if c is a
 // digit in the current base. Otherwise return false.
 LIT('0'); CC("-"); CC("DUP"); CC("0<"); IF(); CC("DROP"); LIT(0); COM(0x8001); THEN();
 CC("DUP"); LIT(9); CC(">"); CC("OVER"); LIT(17); CC("<"); CC("AND"); IF(); CC("DROP"); LIT(0); COM(0x8001); THEN();
 CC("DUP"); LIT(9);  CC(">"); IF();  LIT(7); CC("-"); THEN();
 CC("DUP"); LIT(41); CC(">"); IF();  LIT(32); CC("-"); THEN();
 CC("DUP"); CC("BASE"); CC("@"); CC("<"); CC("0="); IF(); CC("DROP"); LIT(0); COM(0x8001); THEN();
 LIT((__u32)-1);
 F_END();

 HEADER(">NUMBER"); // ( ud1 c-addr1 u1 --- ud2 c-addr2 u2 )
 // Convert the string at c-addr with length u1 to binary, multiplying ud1
 // by the number in BASE and adding the digit value to it for each digit.
 // c-addr2 u2 is the remainder of the string starting at the first character
 // that is no digit.
   BEGIN();
   CC("DUP");
   WHILE();
    CC("1-"); CC(">R");
    CC("COUNT"); CC("DIGIT?"); CC("0=");
    IF();
    CC("R>"); CC("1+"); CC("SWAP"); CC("1-"); CC("SWAP");  COM(0x8001);
    THEN();
    CC("SWAP"); CC(">R");
    CC(">R");
    CC("SWAP"); CC("BASE"); CC("@");CC("UM*"); CC("ROT");CC("BASE");CC("@");
    CC("*"); LIT(0); CC("SWAP"); CC("D+"); // Multiply ud by base.
    CC("R>"); LIT(0); CC("D+");           // Add new digit.
    CC("R>"); CC("R>");
   REPEAT();
   F_END();

 HEADER("CONVERT"); // ( ud1 c-addr1 --- ud2 c-addr2)
 // Convert the string starting at c-addr1 + 1 to binary. c-addr2 is the
 // address of the first non-digit. Digits are added into ud1 as in >NUMBER
 CC("1-"); LIT((__u32)-1); CC(">NUMBER"); CC("DROP");
 F_END();

 HEADER("NUMBER?"); // ( c-addr n ---- d f)
 // Convert the counted string at c-addr to a double binary number.
 // f is true if and only if the conversion was successful. DPL contains
 // -1 if there was no point in the number, else the position of the point
 // from the right. Special prefixes: # means decimal, $ means hex.
 LIT((__u32)-1); CC("DPL"); CC("!");
 CC("BASE"); CC("@"); CC(">R");
// CC("COUNT");
 CC("OVER"); CC("C@"); LIT(45); CC("="); CC("DUP"); CC(">R"); IF(); CC("1-"); CC("SWAP"); CC("1+"); CC("SWAP"); THEN(); // Get any - sign
 CC("OVER"); CC("C@"); LIT(36); CC("="); IF(); LIT(16); CC("BASE"); CC("!"); CC("1-"); CC("SWAP"); CC("1+"); CC("SWAP"); THEN();   // $ sign for hex.
 CC("OVER"); CC("C@"); LIT(35); CC("="); IF(); LIT(10); CC("BASE"); CC("!"); CC("1-"); CC("SWAP"); CC("1+"); CC("SWAP"); THEN();   // # sign for decimal
 CC("DUP");  CC("0>"); CC("0=");
 IF();  CC("R>"); CC("DROP"); CC("R>"); CC("BASE"); CC("!"); LIT(0); COM(0x8001);
 THEN();   // Length 0 or less?
 CC(">R"); CC(">R"); LIT(0); LIT(0); CC("R>"); CC("R>");
   BEGIN();
 CC(">NUMBER");
 CC("DUP"); IF(); CC("OVER"); CC("C@"); LIT(46); CC("="); IF(); CC("1-"); CC("DUP");
 CC("DPL"); CC("!"); CC("SWAP"); CC("1+"); CC("SWAP"); ELSE(); // handle point.
 CC("R>"); CC("DROP"); CC("R>"); CC("BASE"); CC("!"); LIT(0); COM(0x8001); THEN();  // Error if anything but point
        THEN();
   CC("DUP"); CC("0="); UNTIL(); CC("2DROP"); CC("R>"); IF(); CC("DNEGATE"); THEN();
   CC("R>"); CC("BASE"); CC("!"); LIT(-1);
   F_END();

HEADER("DEPTH");
  CC("SP0"); CC("@"); CC("SP@");  CC("-");
  CC("CELL/"); CC("1-");
  F_END();

HEADER("?STACK");
CC("DEPTH");  CC("0<");
IF(); LIT((__u32)-4); CC("THROW");
THEN();
F_END();

HEADER("?SLITERAL");
//        CC("DUP>R");
        CC("NUMBER?");
        IF(); //  CC("RDROP");

    		CC("DPL"); CC("@"); CC("1+");
    	    IF(); 	CC("STATE"); CC("@");
    		   IF(); CC("SWAP"); CC("LIT,"); CC("LIT,");
    		   THEN();
            ELSE(); CC("DROP"); CC("STATE"); CC("@"); IF(); CC("LIT,"); THEN();
            THEN();
       ELSE();
//  CC("2SWAP"); CC("2DROP");  CC("R@"); CC("-");  CC("+");  CC("R>");
//             CC("NUMBER?"); CC("0=");  IF();  LIT((__u32)-13); CC("THROW"); THEN();
//             CC("STATE"); CC("@");  IF(); CC("LIT,"); THEN();
		LIT((__u32)-13); CC("THROW");
       THEN();
F_END();
 
 HEADER("INTERPRET_");
 LIT(-1);  CC("SAVEERR?");  CC("!");
 BEGIN();
 CC("PARSE-NAME");

 CC("DUP");
 WHILE();
 CC("SFIND"); CC("?DUP");
    IF();    CC("STATE"); CC("@"); CC("=");
    	IF();  CC("COMPILE,");
    	ELSE();    	CC("EXECUTE");
    	THEN();
    ELSE(); CC("?SLITERAL");
    THEN();

 CC("?STACK");
 REPEAT();
 CC("2DROP");
 F_END();

 VARIABLE("&INTERPRET");

 HEADER("INTERPRET");
 CC("&INTERPRET"); CC("PERFORM");
 F_END();


 HEADER("EVAL");
 //TP("\nEVAL=<"); CC("2DUP"); CC("TYPE"); TP(">\n");
 CC("SOURCE!");
 CC(">IN");   CC("0!");
 CC("INTERPRET_");
 F_END();

 HEADER("HERE");
 CC("DP");   CC("@");
 F_END();

 HEADER("\:");
 CC("STATE"); CC("ON");
 CC("PARSE-NAME");
 //TP("PARSE-NAME=");  CC("2DUP"); CC("TYPE");
 CC("SHEADER");
 F_END();

 HEADER("EXIT");  IMMED();
 CC("[EXIT]"); CC("W,");
 F_END();

 HEADER(";");  IMMED();
// COM(sw_femd<<1|1);
 CC("[EXIT]"); CC("W,");
 CC("STATE"); CC("0!");
 CC("LAST"); CC("@");   CC("CURRENT"); CC("@");  CC("!");
 F_END();

 HEADER("CREATE");
 CC("\:"); 
//   TP(">CRE\n");
	LIT(sw_ip<<1|0x4001); CC(",");
 CC("STATE"); CC("0!");
  CC("LAST"); CC("@");   CC("CURRENT"); CC("@");  CC("!");
//  CC("STATE"); CC("@"); TP("ST="); CC("H.");
  F_END();

 HEADER("'");
 CC("PARSE-NAME"); CC("SFIND");  CC("0=");
 IF(); LIT((__u32)-321); CC("THROW");
 THEN();
 F_END();

 HEADER("[']"); IMMED();
 CC("'"); CC("LIT,");
 F_END();

// HEADER("(");   IMMED();
//  LIT(')'); CC("PARSE"); CC("2DROP");
// F_END();

 HEADER(".(");   IMMED();
  LIT(')'); CC("PARSE"); CC("TYPE");
 F_END();

 HEADER("\\");   IMMED();
  LIT(0); CC("PARSE"); CC("2DROP");
 F_END();

 HEADER("CR");
 LIT('\n');  CC("EMIT");
 //LIT('\r');  CC("EMIT");
 F_END();

 HEADER("SPACE");
  LIT(' ');  CC("EMIT");
 F_END();

 HEADER("MU/MOD");
 // ( ud u --- urem udquot )  Divide unsigned double number
 //ud by u and return a double quotient and G a single remainder.
 CC(">R"); LIT(0); CC("R@"); CC("UM/MOD"); CC("R>");
       CC("SWAP"); CC(">R"); CC("UM/MOD"); CC("R>");
F_END();

 HEADER("DABS"); // ( d --- ud)  ud is the absolute value of d.
		 CC("DUP"); CC("0<"); IF(); CC("DNEGATE"); THEN();
F_END();

 VARIABLE("HLD");

 HEADER("HOLD"); // ( c ---)  Insert character c into the  numerical  conver- sion buffer.
 LIT(-1);  CC("HLD"); CC("+!"); CC("HLD"); CC("@"); CC("C!");
 F_END();

 HEADER("#");  // ( ud1 --- ud2)  Extract the rightmost digit of ud1 and put
 // it into the numerical G conversion buffer.
 CC("BASE"); CC("@"); CC("MU/MOD"); CC("ROT"); CC("DUP");
  LIT(9); CC(">");
  IF(); LIT(7); CC("+");
  THEN(); LIT(48); CC("+"); CC("HOLD");
 F_END();

 HEADER("#S");  // ( ud --- 0 0 )  Convert ud by repeated use of # until ud is zero.
   BEGIN(); CC("#"); CC("2DUP"); CC("OR"); CC("0="); UNTIL();
 F_END();

 HEADER("SIGN"); // (  n  ---)  Insert a - sign in the numerical conversion buffer if n is negative.
		 CC("0<"); IF(); LIT(45); CC("HOLD"); THEN();
 F_END();

 HEADER("<#"); // ( --- )  Reset the numerical conversion buffer.
 CC("PAD"); CC("HLD"); CC("!");
 F_END();

 HEADER("#>"); // ( ud --- addr u )  Discard ud  and  give  the  address  and  length of the numerical conversion G buffer.
 CC("2DROP");  CC("HLD"); CC("@"); CC("PAD"); CC("OVER"); CC("-");
     F_END();

 HEADER("(D.)");
 CC("SWAP"); CC("OVER"); CC("DABS"); CC("<#"); CC("#S"); CC("ROT"); CC("SIGN");
 CC("#>");
     F_END();

HEADER("D."); // ( d --- )  Type the double number d to the terminal.
  CC("(D.)"); CC("TYPE"); CC("SPACE");
  F_END();

 HEADER("U."); // ( u ---)  Type the unsigned number u to the terminal.
     LIT(0); CC("D.");
     F_END();

 HEADER("S>D");
   CC("DUP");  CC("0<");
 F_END();

 HEADER("."); // ( n ---)  Type the signed number n to the terminal.
		 CC("S>D"); CC("D.");
 F_END();



 HEADER("SOURCE-ID"); CC("SID");  CC("@");
 F_END();


 HEADER("ACCEPT");
  LIT(0); CC("READ-LINE"); CC("2DROP");
 F_END();

 HEADER("FORPRE");
 CC("NOOP");
 F_END();

 HEADER("REFILL");
 CC("TIB"); CC("C/L"); CC("SOURCE-ID");
   CC("READ-LINE");  CC("DROP");
   IF();  CC("TIB"); CC("SWAP");
 CC("SOURCE!");
    LIT(1); CC("CURSTR"); CC("+!");   CC("FORPRE");
  LIT((__u32)-1);
   ELSE(); CC("DROP"); LIT(0);
   THEN();
   CC(">IN"); CC("0!");
   F_END();

HEADER("(");   IMMED();
     BEGIN();
      	  LIT(')'); CC("DUP"); CC("PARSE"); CC("+");
      	  CC("C@"); CC("="); CC("0=");
     WHILE(); CC("REFILL"); CC("0=");
     	 IF(); COM(0x8001); THEN();
     REPEAT();
  F_END();

   HEADER("FQUIT");
   BEGIN();    CC("REFILL");
   WHILE();    CC("INTERPRET");
   REPEAT();
   F_END();

   HEADER("QUIT");
   BEGIN();    CC("REFILL");
   WHILE();    CC("INTERPRET");   CC("STATE"); CC("@");   CC("0=");   	   	   IF();  TP("OK"); CC("CR");   	   	   THEN();
//   WHILE();  CC("CR"); CC("SOURCE"); TP("<" ); CC("TYPE");  TP(">" );   //  CC("INTERPRET");

   REPEAT();
   F_END();

   HEADER("TST"); // ( d --- )  Type the double number d to the terminal.
   TP("TST<");  CC("PPPP"); CC("U.");   TP(">TST\n");
     F_END();

  HEADER("ECHOX");
  BEGIN(); CC("KEY"); CC("H.");
  AGAIN();
  F_END();

  shere=HERE;
  HERE+=1000;

 FSTART=HERE;
 TP("Forth");
 COM(sw_lit<<1|1); CC("INTERPRET_");  TP(" sys\n");  CC("&INTERPRET"); CC("!");

 //TP("Save version 2 \n");
  CC("STATE");
 CC("0!");
 CC("SID"); CC("0!");
 CC("SP@"); CC("SP0"); CC("!");

CC("DECIMAL");

STR(".( DEPTH=) DEPTH . CR");  CC("EVAL");

STR(" : CDUMP  SWAP   VFMADR + SWAP  PDUMP ; ");  CC("EVAL");
//STR(" : UTST  BEGIN  S\" U\" UTYPE  AGAIN  ; ");  CC("EVAL");

STR(": FERR^ NOOP ;");  CC("EVAL");
STR(": PERR .\" v ERR=\". ;");  CC("EVAL");
STR(": SAVEERR NOOP ;");  CC("EVAL");

STR(": INCLUDE-FILE");  CC("EVAL"); // ( fid --- )
// Read lines from the file identified by fid and interpret them.
// INCLUDE and EVALUATE nest in arbitrary order.
STR("SID @ >R >IN @ >R ATIB @ >R #TIB @ >R CURSTR @ >R RP@ C/L - RP!");  CC("EVAL");
STR("SID ! RP@ ATIB ! CURSTR 0! ");  CC("EVAL");
STR("['] FQUIT CATCH SAVEERR");  CC("EVAL");
STR("DUP IF CR SOURCE TYPE CR FERR^ .\" :\" CURSTR @ . THEN");  CC("EVAL");

STR("RP@ C/L + RP! R> CURSTR ! R> #TIB ! R> ATIB ! R> >IN ! R> SID ! THROW ;");  CC("EVAL");

STR(": INCLUDED");  CC("EVAL"); // ( c-addr u ---- )
// Open the file with name c-addr u and interpret all lines contained in it.
STR("R/O OPEN-FILE IF -38 THROW THEN");  CC("EVAL");
STR("DUP >R ['] INCLUDE-FILE CATCH");  CC("EVAL");
STR("R> CLOSE-FILE DROP THROW ;");  CC("EVAL");

STR(": FLOAD PARSE-NAME INCLUDED ;");  CC("EVAL");
STR("CREATE TATR 55 ALLOT TATR VATR DROP");  CC("EVAL");
STR(": AUTOEXEC S\" autoexec.4\" ['] INCLUDED CATCH .\" ERR=\" . SP0 @ SP! STATE 0! ;"); CC("EVAL");
STR(": MAIN .\" MAIN\" CR  BEGIN STATE 0! ['] QUIT CATCH .\" ERR=\"  .  SP0 @ SP! AGAIN ;");  CC("EVAL");
STR(": TATRDO TATR COUNT ['] EVAL CATCH PERR  SP0 @ SP! STATE 0! MAIN ;"); CC("EVAL");
STR("TATRDO"); CC("EVAL");
 HERE=shere;
}

void vfm_do()
{  int ii;
	DescriptorTable[0]= mem;
	for(ii=1;ii<16;ii++)
	DescriptorTable[ii] = (__u8 * ) 0;

	make_image();
	  read_history("rlhistory.cfg");
	printf("make_image ok\n");
	virtual_machine();
}

