/*
 * uart1.c
 *
 *  Created on: Apr 11, 2018
 *      Author: mak
 */

#include <stdlib.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/cm3/nvic.h>
#include "uart1.h"

int nndebcc;
void nndeb()
{
	NOOP();
	nndebcc++;
}
int nndeb0cc;
void nndeb0()
{
	NOOP();
	nndeb0cc++;
}


#define  ComBufSize ( 1 << 9 )
char ComBuf1[ComBufSize];
volatile int ComBufIn1=1;
volatile int ComBufOut1=0;

void ComClr1()
{ ComBufIn1=1; ComBufOut1=0;
}

volatile int  ComCnt1()  /*  get how many bytes are in the buffer*/
{ return ( (ComBufIn1-ComBufOut1-1) & (ComBufSize-1) );
}

unsigned char GetPort1() /*  get a character from the buffer*/
{  return  ComBuf1[ (++ComBufOut1) & (ComBufSize-1) ];
}

void ComPut1(unsigned char Data)
{ ComBuf1[ (ComBufIn1++) & (ComBufSize-1) ] = Data;
}

unsigned char WGetPort1() /*  get a character from the buffer*/
{  while(!ComCnt1());

  return  GetPort1();
}

void UART1_Transmit(unsigned char * buf, int len)
{ 
  while(len--) usart_send_blocking(USART1,*buf++);
}

void clock_setup(void)
{
	rcc_clock_setup_in_hse_8mhz_out_72mhz();

	/* Enable GPIOA clock (for LED GPIOs). */
	rcc_periph_clock_enable(RCC_GPIOC);

	/* Enable clocks for GPIO port A (for GPIO_USART1_TX) and USART1. */
	rcc_periph_clock_enable(RCC_GPIOA);
	rcc_periph_clock_enable(RCC_AFIO);
	rcc_periph_clock_enable(RCC_USART1);
}

void usart_setup(void)
{
	/* Enable the USART1 interrupt. */
	nvic_enable_irq(NVIC_USART1_IRQ);

	/* Setup GPIO pin GPIO_USART1_RE_TX on GPIO port B for transmit. */
	gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_50_MHZ,
		      GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, GPIO_USART1_TX);

	/* Setup GPIO pin GPIO_USART1_RE_RX on GPIO port B for receive. */
	gpio_set_mode(GPIOA, GPIO_MODE_INPUT,
		      GPIO_CNF_INPUT_FLOAT, GPIO_USART1_RX);

	/* Setup UART parameters. */
//	usart_set_baudrate(USART1, 230400);
	usart_set_baudrate(USART1, 115200);
	usart_set_databits(USART1, 8);
	usart_set_stopbits(USART1, USART_STOPBITS_1);
	usart_set_parity(USART1, USART_PARITY_NONE);
	usart_set_flow_control(USART1, USART_FLOWCONTROL_NONE);
	usart_set_mode(USART1, USART_MODE_TX_RX);

	/* Enable USART1 Receive interrupt. */
	USART_CR1(USART1) |= USART_CR1_RXNEIE;

	/* Finally enable the USART. */
	usart_enable(USART1);
}

void usart1_isr(void)
{
	static uint8_t data = 'A';

	/* Check if we were called because of RXNE. */
	if (((USART_CR1(USART1) & USART_CR1_RXNEIE) != 0) &&
	    ((USART_SR(USART1) & USART_SR_RXNE) != 0)) {

		/* Indicate that we got data. */
		gpio_toggle(GPIOC, GPIO12);

		/* Retrieve the data from the peripheral. */
		data = usart_recv(USART1);

		/* Enable transmit interrupt so it sends back the data. */
		USART_CR1(USART1) |= USART_CR1_TXEIE;
	}

	/* Check if we were called because of TXE. */
	if (((USART_CR1(USART1) & USART_CR1_TXEIE) != 0) &&
	    ((USART_SR(USART1) & USART_SR_TXE) != 0)) {

		/* Indicate that we are sending out data. */
		// gpio_toggle(GPIOA, GPIO7);

		/* Put data into the transmit register. */
//		usart_send(USART1, data);
//		ttwrite(&data , 1);
		ComPut1(data);

		/* Disable the TXE interrupt as we don't need it anymore. */
		USART_CR1(USART1) &= ~USART_CR1_TXEIE;
	}
}


void slcan_usart1_write( uint8_t * adr, int len)
{
	while(len--) usart_send_blocking(USART1,*adr++);
}


void  hht(uint32_t hh )
{  uint8_t pp;
   uint8_t ii=8;

   for(;;)
   {
	   pp=(hh>>28)&0xf;

	   if(!--ii) break;
	   if(pp)	break;
//   	   usart_send_blocking(USART1,'_');
	   hh<<=4;
   }

   do
   {
	pp+=0x30;
   	if(pp>0x39)pp+= 'A' - 0x3a;
   	   usart_send_blocking(USART1,pp);
	hh<<=4;
	pp=(hh>>28)&0xf;
   }   while(ii--);

//	   usart_send_blocking(USART1,'#');

}


