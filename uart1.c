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
char ComBuf2[ComBufSize];
volatile int ComBufIn2=1;
volatile int ComBufOut2=0;

void ComClr2()
{ ComBufIn2=1; ComBufOut2=0;
}

volatile int  ComCnt2()  /*  get how many bytes are in the buffer*/
{ return ( (ComBufIn2-ComBufOut2-1) & (ComBufSize-1) );
}

unsigned char GetPort2() /*  get a character from the buffer*/
{  return  ComBuf2[ (++ComBufOut2) & (ComBufSize-1) ];
}

void ComPut2(unsigned char Data)
{ ComBuf2[ (ComBufIn2++) & (ComBufSize-1) ] = Data;
}

unsigned char WGetPort2() /*  get a character from the buffer*/
{  while(!ComCnt2());

  return  GetPort2();
}

void UART2_Transmit(unsigned char * buf, int len)
{ 
  while(len--) usart_send_blocking(USART2,*buf++);
}

void clock_setup(void)
{
	rcc_clock_setup_in_hse_8mhz_out_72mhz();

	/* Enable GPIOA clock (for LED GPIOs). */
	rcc_periph_clock_enable(RCC_GPIOC);

	/* Enable clocks for GPIO port A (for GPIO_USART2_TX) and USART2. */
	rcc_periph_clock_enable(RCC_GPIOA);
	rcc_periph_clock_enable(RCC_AFIO);
	rcc_periph_clock_enable(RCC_USART1);
	rcc_periph_clock_enable(RCC_USART2);
	rcc_periph_clock_enable(RCC_USART3);
}

void usart1_setup(void)
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
//		ComPut1(data);

		/* Disable the TXE interrupt as we don't need it anymore. */
		USART_CR1(USART1) &= ~USART_CR1_TXEIE;
	}
}

void usart2_isr(void)
{
	static uint8_t data = 'A';

	/* Check if we were called because of RXNE. */
	if (((USART_CR1(USART2) & USART_CR1_RXNEIE) != 0) &&
	    ((USART_SR(USART2) & USART_SR_RXNE) != 0)) {

		/* Indicate that we got data. */
		gpio_toggle(GPIOA, GPIO8);

		/* Retrieve the data from the peripheral. */
		data = usart_recv(USART2);

		/* Enable transmit interrupt so it sends back the data. */
		USART_CR1(USART2) |= USART_CR1_TXEIE;
	}

	/* Check if we were called because of TXE. */
	if (((USART_CR1(USART2) & USART_CR1_TXEIE) != 0) &&
	    ((USART_SR(USART2) & USART_SR_TXE) != 0)) {

		/* Indicate that we are sending out data. */
		gpio_toggle(GPIOC, GPIO15);

		/* Put data into the transmit register. */
// mmo		usart_send(USART2, data);
		ComPut2(data);

		/* Disable the TXE interrupt as we don't need it anymore. */
		USART_CR1(USART2) &= ~USART_CR1_TXEIE;
	}
}

void slcan_usart2_write( uint8_t * adr, int len)
{
	while(len--) usart_send_blocking(USART2,*adr++);
}

void usart2_setup(void)
{
	/* Enable the USART2 interrupt. */
	nvic_enable_irq(NVIC_USART2_IRQ);

	/* Setup GPIO pin GPIO_USART2_TX on GPIO port A for transmit. */
	gpio_set_mode(GPIO_BANK_USART2_TX, GPIO_MODE_OUTPUT_50_MHZ,
		      GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, GPIO_USART2_TX);

	/* Setup GPIO pin GPIO_USART2_RX on GPIO port A for receive. */
	gpio_set_mode(GPIO_BANK_USART2_RX, GPIO_MODE_INPUT,
		      GPIO_CNF_INPUT_FLOAT, GPIO_USART2_RX);

	/* Setup UART parameters. */
	usart_set_baudrate(USART2, 115200);
	usart_set_databits(USART2, 8);
	usart_set_stopbits(USART2, USART_STOPBITS_1);
	usart_set_parity(USART2, USART_PARITY_NONE);
	usart_set_flow_control(USART2, USART_FLOWCONTROL_NONE);
	usart_set_mode(USART2, USART_MODE_TX_RX);

	/* Enable USART2 Receive interrupt. */
	USART_CR1(USART2) |= USART_CR1_RXNEIE;

	/* Finally enable the USART. */
	usart_enable(USART2);
}

void usart3_isr(void)
{
	static uint8_t data = 'A';

	/* Check if we were called because of RXNE. */
	if (((USART_CR1(USART3) & USART_CR1_RXNEIE) != 0) &&
	    ((USART_SR(USART3) & USART_SR_RXNE) != 0)) {

		/* Indicate that we got data. */
		gpio_toggle(GPIOA, GPIO8);

		/* Retrieve the data from the peripheral. */
		data = usart_recv(USART3);

		/* Enable transmit interrupt so it sends back the data. */
		USART_CR1(USART3) |= USART_CR1_TXEIE;
	}

	/* Check if we were called because of TXE. */
	if (((USART_CR1(USART3) & USART_CR1_TXEIE) != 0) &&
	    ((USART_SR(USART3) & USART_SR_TXE) != 0)) {

		/* Indicate that we are sending out data. */
		gpio_toggle(GPIOC, GPIO15);

		/* Put data into the transmit register. */
// mmo		usart_send(USART3, data);
		ComPut2(data);

		/* Disable the TXE interrupt as we don't need it anymore. */
		USART_CR1(USART3) &= ~USART_CR1_TXEIE;
	}
}

void usart3_setup(void)
{
	/* Enable the USART3 interrupt. */
	nvic_enable_irq(NVIC_USART3_IRQ);

	/* Setup GPIO pin GPIO_USART3_TX on GPIO port A for transmit. */
	gpio_set_mode(GPIO_BANK_USART3_TX, GPIO_MODE_OUTPUT_50_MHZ,
		      GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, GPIO_USART3_TX);

	/* Setup GPIO pin GPIO_USART3_RX on GPIO port A for receive. */
	gpio_set_mode(GPIO_BANK_USART3_RX, GPIO_MODE_INPUT,
		      GPIO_CNF_INPUT_FLOAT, GPIO_USART3_RX);

	/* Setup UART parameters. */
	usart_set_baudrate(USART3, 115200);
	usart_set_databits(USART3, 8);
	usart_set_stopbits(USART3, USART_STOPBITS_1);
	usart_set_parity(USART3, USART_PARITY_NONE);
	usart_set_flow_control(USART3, USART_FLOWCONTROL_NONE);
	usart_set_mode(USART3, USART_MODE_TX_RX);

	/* Enable USART3 Receive interrupt. */
	USART_CR1(USART3) |= USART_CR1_RXNEIE;

	/* Finally enable the USART. */
	usart_enable(USART3);
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
   	   usart_send_blocking(USART3,pp);
	hh<<=4;
	pp=(hh>>28)&0xf;
   }   while(ii--);

//	   usart_send_blocking(USART1,'#');

}


