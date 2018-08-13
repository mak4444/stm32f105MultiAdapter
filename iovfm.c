#include <stdlib.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/usb/usbd.h>
#include <libopencm3/usb/cdc.h>
#include <libopencm3/cm3/scb.h>

//<mmo usb_fx07_common.c
#include <string.h>
#include <libopencm3/cm3/common.h>
#include <libopencm3/stm32/tools.h>
#include <libopencm3/stm32/otg_fs.h>
#include <libopencm3/stm32/otg_hs.h>
#include <libopencm3/usb/usbd.h>
//#include "usb_private.h"
//#include "usb_fx07_common.h"
#include <../lib/usb/usb_private.h>
#include <../lib/usb/usb_fx07_common.h>

extern usbd_device *usbd_dev;
extern uint8_t ep;
extern char buf[];


int ttkbhit()	{ // usbd_poll(); // stm32fx07_poll_mmo();
	return USBkbhit();}

//void UsbType(char* buf , unsigned len )
void ttwrite(char* buf , unsigned len )
{
   while (usbd_ep_write_packet(usbd_dev, 0x82, buf, len) == 0);
}

void USB_PUT(char c)
{
	ttwrite(&c , 1 );
}

//char UsbKey()
unsigned ttgetch()
{
	while (!ttkbhit());
//	do{ stm32fx07_poll_mmo();} while(ep!=1);
		return USBDDGet();
}

static uint8_t ccbuf[111];
static int ppOut=0;
static int ppIn=0;

unsigned ttgetchar()
{ uint8_t cc;
	if(ppOut)
	{ cc=ccbuf[ppOut++];
	if(cc=='\n') ppOut=ppIn=0;
	if(cc=='\r') ppOut=ppIn=0;
		if(ppIn==111) ppOut=ppIn=0;
		return (unsigned)cc;
  }
	do
	{  // cc
		cc=ttgetch();
		if(cc==0x1b)  cc=ccbuf[ppIn];
		ttwrite(&cc,1);
		if(cc=='\t') {  cc=' ';
			do
			{
				ccbuf[ppIn++]=cc;
			}
			while( ppIn&7);   continue;
		}
		if(cc==8) { if(ppIn) ppIn--; continue;}
		ccbuf[ppIn++]=cc;
	}while( (cc!='\n') & (cc!='\r')& ( ppIn<111)  );
	cc=ccbuf[ppOut++];
	if(cc=='\n') ppOut=ppIn=0;
	if(cc=='\r') ppOut=ppIn=0;
	return (unsigned)cc;
}






