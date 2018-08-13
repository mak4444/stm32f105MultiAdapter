
#include <stdlib.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/flash.h>

#include <libopencm3/cm3/common.h>
#include <libopencm3/stm32/tools.h>
#include <libopencm3/stm32/otg_fs.h>
#include <libopencm3/usb/usbd.h>
#include "uart1.h"


//mmo>

#define sp_size 0x80
#define dp_size 0x1244+sp_size
int sp_buff[dp_size];

int * CDSTK;

ctest()
{
	*--CDSTK=0x77;
}

//void tstsss(void);

/** Initialize the USB device controller hardware of the STM32. */
void stm32f107_USBdInit(void)
{
	rcc_periph_clock_enable(RCC_OTGFS);
	OTG_FS_GUSBCFG |= OTG_GUSBCFG_PHYSEL;

	/* Wait for AHB idle. */
	while (!(OTG_FS_GRSTCTL & OTG_GRSTCTL_AHBIDL));
	/* Do core soft reset. */
	OTG_FS_GRSTCTL |= OTG_GRSTCTL_CSRST;
	while (OTG_FS_GRSTCTL & OTG_GRSTCTL_CSRST);

	if (OTG_FS_CID >= OTG_CID_HAS_VBDEN) {
		/* Enable VBUS detection in device mode and power up the PHY. */
		OTG_FS_GCCFG |= OTG_GCCFG_VBDEN | OTG_GCCFG_PWRDWN;
	} else {
		/* Enable VBUS sensing in device mode and power up the PHY. */
		OTG_FS_GCCFG |= OTG_GCCFG_VBUSBSEN | OTG_GCCFG_PWRDWN;
	}
	/* Explicitly enable DP pullup (not all cores do this by default) */
	OTG_FS_DCTL &= ~OTG_DCTL_SDIS;

	/* Force peripheral only mode. */
	OTG_FS_GUSBCFG |= OTG_GUSBCFG_FDMOD | OTG_GUSBCFG_TRDT_MASK;

	OTG_FS_GINTSTS = OTG_GINTSTS_MMIS;

	/* Full speed device. */
	OTG_FS_DCFG |= OTG_DCFG_DSPD;

	/* Restart the PHY clock. */
	OTG_FS_PCGCCTL = 1;

//	OTG_FS_GRXFSIZ = 0; // mmo
//	OTG_FS_GRXFSIZ = stm32f107_usb_driver.rx_fifo_size;
//	usbd_dev.fifo_mem_top = stm32f107_usb_driver.rx_fifo_size;

	/* Unmask interrupts for TX and RX. */
	OTG_FS_GAHBCFG |= OTG_GAHBCFG_GINT;
	OTG_FS_GINTMSK = OTG_GINTMSK_ENUMDNEM |
			 OTG_GINTMSK_RXFLVLM |
			 OTG_GINTMSK_IEPINT |
			 OTG_GINTMSK_USBSUSPM |
			 OTG_GINTMSK_WUIM;
	OTG_FS_DAINTMSK = 0xF;
	OTG_FS_DIEPMSK = OTG_DIEPMSK_XFRCM;

}

void FORTH_MAIN(int *);

int tttt;
int main(void)
{

	rcc_clock_setup_in_hse_8mhz_out_72mhz();

	clock_setup();
	usart_setup();

	nndeb();
//	flash_unlock();
//	stm32f107_usb_init();

	CDSTK = &sp_buff[dp_size];
	*--CDSTK=0x77;
	hht(0);
	hht(1);
	hht(0x78665544);
	hht(0xabc);
	FFMAIN(CDSTK);

}

//https://community.arm.com/processors/b/blog/posts/how-to-load-constants-in-assembly-for-arm-architecture
//movw    r0, #:lower16:label
//movt    r0, #:upper16:label
//sudo openocd -f /usr/local/share/openocd/scripts/interface/stlink-v2.cfg -f /usr/local/share/openocd/scripts/target/stm32f1x_stlink.cfg

// flash erase_address ./mecrisp-stellaris-stm32f103.bin 0x8000000 0x10000

// sudo slcand -o -c -F -S  115200 /dev/ttyUSB0
// sudo ifconfig slcan0 up
// sudo ip link set up slcan0



