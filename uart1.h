/*
 * uart1.h
 *
 *  Created on: Apr 11, 2018
 *      Author: mak
 */
extern char ComBuf1[];
extern volatile int ComBufIn1;
extern volatile int ComBufOut1;

void ComClr1(void);

int ComCnt1(void);  /*  get how many bytes are in the buffer*/

unsigned char GetPort1(); /*  get a character from the buffer*/

void ComPut1(unsigned char Data);


void clock_setup(void);

void usart_setup(void);

unsigned char WGetPort1(void);

void UART1_Transmit(unsigned char * buf, int len);

void slcan_usart1_write(uint8_t * adr, int len);
