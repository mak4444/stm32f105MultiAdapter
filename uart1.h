/*
 * uart1.h
 *
 *  Created on: Apr 11, 2018
 *      Author: mak
 */
extern char ComBuf2[];
extern volatile int ComBufIn2;
extern volatile int ComBufOut2;

void ComClr2(void);

int ComCnt2(void);  /*  get how many bytes are in the buffer*/

unsigned char GetPort2(); /*  get a character from the buffer*/

void ComPut2(unsigned char Data);


void clock_setup(void);

void usart1_setup(void);
void usart2_setup(void);
void usart3_setup(void);

unsigned char WGetPort2(void);

void UART2_Transmit(unsigned char * buf, int len);

void slcan_usart2_write(uint8_t * adr, int len);
