/*
 * This file is part of the libopencm3 project.
 *
 * Copyright (C) 2010 Gareth McMullin <gareth@blacksphere.co.nz>
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/usb/usbd.h>
#include <libopencm3/usb/cdc.h>

#include <libopencm3/cm3/common.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/tools.h>
#include <libopencm3/stm32/st_usbfs.h>
#include <libopencm3/usb/usbd.h>
#include <../lib/usb/usb_private.h>
#include <../lib/stm32/common/st_usbfs_core.h>
#include <libopencm3/stm32/usart.h>
//#include <libopencm3/cm3/nvic.h>
#include "uart1.h"

static const struct usb_device_descriptor dev = {
	.bLength = USB_DT_DEVICE_SIZE,
	.bDescriptorType = USB_DT_DEVICE,
	.bcdUSB = 0x0200,
	.bDeviceClass = USB_CLASS_CDC,
	.bDeviceSubClass = 0,
	.bDeviceProtocol = 0,
	.bMaxPacketSize0 = 64,
//	.idVendor = 0x0483,
//	.idProduct = 0x5746,
	.idVendor = 0x0403,
	.idProduct = 0xffa8,

	.bcdDevice = 0x0200,
	.iManufacturer = 1,
	.iProduct = 2,
	.iSerialNumber = 3,
	.bNumConfigurations = 1,
};

/*
 * This notification endpoint isn't implemented. According to CDC spec its
 * optional, but its absence causes a NULL pointer dereference in Linux
 * cdc_acm driver.
 */
static const struct usb_endpoint_descriptor comm_endp[] = {{
	.bLength = USB_DT_ENDPOINT_SIZE,
	.bDescriptorType = USB_DT_ENDPOINT,
	.bEndpointAddress = 0x83,
	.bmAttributes = USB_ENDPOINT_ATTR_INTERRUPT,
	.wMaxPacketSize = 16,
	.bInterval = 255,
}};

static const struct usb_endpoint_descriptor data_endp[] = {{
	.bLength = USB_DT_ENDPOINT_SIZE,
	.bDescriptorType = USB_DT_ENDPOINT,
	.bEndpointAddress = 0x01,
	.bmAttributes = USB_ENDPOINT_ATTR_BULK,
	.wMaxPacketSize = 64,
	.bInterval = 1,
}, {
	.bLength = USB_DT_ENDPOINT_SIZE,
	.bDescriptorType = USB_DT_ENDPOINT,
	.bEndpointAddress = 0x82,
	.bmAttributes = USB_ENDPOINT_ATTR_BULK,
	.wMaxPacketSize = 64,
	.bInterval = 1,
}};

static const struct {
	struct usb_cdc_header_descriptor header;
	struct usb_cdc_call_management_descriptor call_mgmt;
	struct usb_cdc_acm_descriptor acm;
	struct usb_cdc_union_descriptor cdc_union;
} __attribute__((packed)) cdcacm_functional_descriptors = {
	.header = {
		.bFunctionLength = sizeof(struct usb_cdc_header_descriptor),
		.bDescriptorType = CS_INTERFACE,
		.bDescriptorSubtype = USB_CDC_TYPE_HEADER,
		.bcdCDC = 0x0110,
	},
	.call_mgmt = {
		.bFunctionLength =
			sizeof(struct usb_cdc_call_management_descriptor),
		.bDescriptorType = CS_INTERFACE,
		.bDescriptorSubtype = USB_CDC_TYPE_CALL_MANAGEMENT,
		.bmCapabilities = 0,
		.bDataInterface = 1,
	},
	.acm = {
		.bFunctionLength = sizeof(struct usb_cdc_acm_descriptor),
		.bDescriptorType = CS_INTERFACE,
		.bDescriptorSubtype = USB_CDC_TYPE_ACM,
		.bmCapabilities = 0,
	},
	.cdc_union = {
		.bFunctionLength = sizeof(struct usb_cdc_union_descriptor),
		.bDescriptorType = CS_INTERFACE,
		.bDescriptorSubtype = USB_CDC_TYPE_UNION,
		.bControlInterface = 0,
		.bSubordinateInterface0 = 1,
	 },
};

static const struct usb_interface_descriptor comm_iface[] = {{
	.bLength = USB_DT_INTERFACE_SIZE,
	.bDescriptorType = USB_DT_INTERFACE,
	.bInterfaceNumber = 0,
	.bAlternateSetting = 0,
	.bNumEndpoints = 1,
	.bInterfaceClass = USB_CLASS_CDC,
	.bInterfaceSubClass = USB_CDC_SUBCLASS_ACM,
	.bInterfaceProtocol = USB_CDC_PROTOCOL_AT,
	.iInterface = 0,

	.endpoint = comm_endp,

	.extra = &cdcacm_functional_descriptors,
	.extralen = sizeof(cdcacm_functional_descriptors),
}};

static const struct usb_interface_descriptor data_iface[] = {{
	.bLength = USB_DT_INTERFACE_SIZE,
	.bDescriptorType = USB_DT_INTERFACE,
	.bInterfaceNumber = 1,
	.bAlternateSetting = 0,
	.bNumEndpoints = 2,
	.bInterfaceClass = USB_CLASS_DATA,
	.bInterfaceSubClass = 0,
	.bInterfaceProtocol = 0,
	.iInterface = 0,

	.endpoint = data_endp,
}};

static const struct usb_interface ifaces[] = {{
	.num_altsetting = 1,
	.altsetting = comm_iface,
}, {
	.num_altsetting = 1,
	.altsetting = data_iface,
}};

static const struct usb_config_descriptor config = {
	.bLength = USB_DT_CONFIGURATION_SIZE,
	.bDescriptorType = USB_DT_CONFIGURATION,
	.wTotalLength = 0,
	.bNumInterfaces = 2,
	.bConfigurationValue = 1,
	.iConfiguration = 0,
	.bmAttributes = 0x80,
	.bMaxPower = 0x32,

	.interface = ifaces,
};

static const char *usb_strings[] = {
	"Black Sphere Technologies",
	"CDC-ACM Demo",
	"DEMO",
};

/* Buffer to be used for control requests. */
uint8_t usbd_control_buffer[128];

static int cdcacm_control_request(usbd_device *usbd_dev, struct usb_setup_data *req, uint8_t **buf,
		uint16_t *len, void (**complete)(usbd_device *usbd_dev, struct usb_setup_data *req))
{
	(void)complete;
	(void)buf;
	(void)usbd_dev;

	switch (req->bRequest) {
	case USB_CDC_REQ_SET_CONTROL_LINE_STATE: {
		/*
		 * This Linux cdc_acm driver requires this to be implemented
		 * even though it's optional in the CDC spec, and we don't
		 * advertise it in the ACM functional descriptor.
		 */
		char local_buf[10];
		struct usb_cdc_notification *notif = (void *)local_buf;

		/* We echo signals back to host as notification. */
		notif->bmRequestType = 0xA1;
		notif->bNotification = USB_CDC_NOTIFY_SERIAL_STATE;
		notif->wValue = 0;
		notif->wIndex = 0;
		notif->wLength = 2;
		local_buf[8] = req->wValue & 3;
		local_buf[9] = 0;
		// usbd_ep_write_packet(0x83, buf, 10);
		return 1;
		}
	case USB_CDC_REQ_SET_LINE_CODING:
		if (*len < sizeof(struct usb_cdc_line_coding))
			return 0;
		return 1;
	}
	return 0;
}

uint8_t CBType;
volatile uint16_t istr2;
//uint16_t istr;
usbd_device *usbd_dev;
uint8_t ep;
char buf[64];
char ccep;

int poll_end()
{
	if (istr2 & USB_ISTR_SUSP) {
		USB_CLR_ISTR_SUSP();
		if (usbd_dev->user_callback_suspend) {
			usbd_dev->user_callback_suspend();
		}
	}

	if (istr2 & USB_ISTR_WKUP) {
		USB_CLR_ISTR_WKUP();
		if (usbd_dev->user_callback_resume) {
			usbd_dev->user_callback_resume();
		}
	}

	if (istr2 & USB_ISTR_SOF) {
		USB_CLR_ISTR_SOF();
		if (usbd_dev->user_callback_sof) {
			usbd_dev->user_callback_sof();
		}
	}

	if (usbd_dev->user_callback_sof) {
		*USB_CNTR_REG |= USB_CNTR_SOFM;
	} else {
		*USB_CNTR_REG &= ~USB_CNTR_SOFM;
	}

}

static void cdcacm_data_rx_cb(usbd_device *usbd_dev, uint8_t ep)
{
	(void)ep;
//	(void)usbd_dev;

//	char buf[64];
	int len = usbd_ep_read_packet(usbd_dev, 0x01, buf, 64);

	if (len) {
		if(ep!=1)
		{	usbd_ep_write_packet(usbd_dev, 0x82, buf, len);
		} else

		{
			while (usbd_ep_write_packet(usbd_dev, 0x82, ".", 0)==1) ;
		}
	}
}

static void cdcacm_set_config(usbd_device *usbd_dev, uint16_t wValue)
{
	(void)wValue;
	(void)usbd_dev;

	usbd_ep_setup(usbd_dev, 0x01, USB_ENDPOINT_ATTR_BULK, 64, cdcacm_data_rx_cb);
	usbd_ep_setup(usbd_dev, 0x82, USB_ENDPOINT_ATTR_BULK, 64, NULL);
	usbd_ep_setup(usbd_dev, 0x83, USB_ENDPOINT_ATTR_INTERRUPT, 16, NULL);

	usbd_register_control_callback(
				usbd_dev,
				USB_REQ_TYPE_CLASS | USB_REQ_TYPE_INTERFACE,
				USB_REQ_TYPE_TYPE | USB_REQ_TYPE_RECIPIENT,
				cdcacm_control_request);
}


int USBkbhit()
{
	istr2 = *USB_ISTR_REG;

	if (istr2 & USB_ISTR_RESET) {
		USB_CLR_ISTR_RESET();
		usbd_dev->pm_top = USBD_PM_TOP;
		_usbd_reset(usbd_dev);
		return 0;
	}

	return (istr2 & USB_ISTR_CTR);
}

int poll_iq()
{

/* Note: RX and TX handled differently in this device. */
if (USBkbhit()) {
	ep = istr2 & USB_ISTR_EP_ID;
	CBType;

	if (istr2 & USB_ISTR_DIR) {
		/* OUT or SETUP? */
		if (*USB_EP_REG(ep) & USB_EP_SETUP) {
			CBType = USB_TRANSACTION_SETUP;
		} else {
			CBType = USB_TRANSACTION_OUT;
		}
	} else {
		CBType = USB_TRANSACTION_IN;
		USB_CLR_EP_TX_CTR(ep);
	}
		return 1;
} else	return 0;
}

void st_usbfs_poll_mmo()
{
	if (poll_iq())
	{
		if (usbd_dev->user_callback_ctr[ep][CBType]) {
			usbd_dev->user_callback_ctr[ep][CBType] (usbd_dev, ep);
		} else {
			USB_CLR_EP_RX_CTR(ep);
		}
//		poll_end();
	}
}

#if 1

//volatile uint32_t istr2;

void st_usbfs_poll_mmo_()
{
	//	uint16_t	istr2 = *USB_ISTR_REG;
		istr2 = *USB_ISTR_REG;

	if (istr2 & USB_ISTR_RESET) {
		USB_CLR_ISTR_RESET();
		usbd_dev->pm_top = USBD_PM_TOP;
		_usbd_reset(usbd_dev);
		return;
	}

	if (istr2 & USB_ISTR_CTR) {
		ep = istr2 & USB_ISTR_EP_ID;
		CBType;

		if (istr2 & USB_ISTR_DIR) {
			/* OUT or SETUP? */
			if (*USB_EP_REG(ep) & USB_EP_SETUP) {
				CBType = USB_TRANSACTION_SETUP;
			} else {
				CBType = USB_TRANSACTION_OUT;
			}
		} else {
			CBType = USB_TRANSACTION_IN;
			USB_CLR_EP_TX_CTR(ep);
		}

		if (usbd_dev->user_callback_ctr[ep][CBType]) {
			usbd_dev->user_callback_ctr[ep][CBType] (usbd_dev, ep);
		} else {
			USB_CLR_EP_RX_CTR(ep);
		}
	}

	if (istr2 & USB_ISTR_SUSP) {
		USB_CLR_ISTR_SUSP();
		if (usbd_dev->user_callback_suspend) {
			usbd_dev->user_callback_suspend();
		}
	}

	if (istr2 & USB_ISTR_WKUP) {
		USB_CLR_ISTR_WKUP();
		if (usbd_dev->user_callback_resume) {
			usbd_dev->user_callback_resume();
		}
	}

	if (istr2 & USB_ISTR_SOF) {
		USB_CLR_ISTR_SOF();
		if (usbd_dev->user_callback_sof) {
			usbd_dev->user_callback_sof();
		}
	}

	if (usbd_dev->user_callback_sof) {
		*USB_CNTR_REG |= USB_CNTR_SOFM;
	} else {
		*USB_CNTR_REG &= ~USB_CNTR_SOFM;
	}
}
#endif


int USB_RS232(void)
{
	int i, j = 0, c = 0;

#if 0
	while (1) {
//		gpio_toggle(GPIOC, GPIO12);	/* LED on/off */
		usart_send_blocking(USART1, c + '0');	/* USART1: Send byte. */
//		usart_send_blocking(USART2, c + '0');	/* USART2: Send byte. */
//		usart_send_blocking(USART3, c + '0');	/* USART3: Send byte. */
		c = (c == 9) ? 0 : c + 1;	/* Increment c. */
		if ((j++ % 80) == 0) {	/* Newline after line full. */
			usart_send_blocking(USART1, '*');
			usart_send_blocking(USART1, '\r');
			usart_send_blocking(USART1, '\n');
/*
			usart_send_blocking(USART2, '\r');
			usart_send_blocking(USART2, '\n');
			usart_send_blocking(USART3, '\r');
			usart_send_blocking(USART3, '\n');
*/
		}
		for (i = 0; i < 800000; i++)	/* Wait a bit. */
			__asm__("nop");
	}
#endif
//	usbd_dev = usbd_init(&st_usbfs_v1_usb_driver, &dev, &config, usb_strings, 3, usbd_control_buffer, sizeof(usbd_control_buffer));
	usbd_dev = usbd_init(&stm32f107_usb_driver, &dev, &config, usb_strings, 3, usbd_control_buffer, sizeof(usbd_control_buffer));
	usbd_register_set_config_callback(usbd_dev, cdcacm_set_config);





	do{  		  st_usbfs_poll_mmo();
	}while(ep!=1);

//	vfm_do();
	while (1)
	{
//		usbd_poll(usbd_dev);
//	st_usbfs_poll_mmo();	if(ep==1)
//		while (usbd_ep_write_packet(usbd_dev, 0x82, "<$>", 3) == 0); // ep=0;
		  if(USBkbhit()) {c=ttgetch();  usart_send_blocking(USART1, c);  }
		  if(ComCnt1){ c=GetPort1(); ttwrite(&c , 1); }
//		  if(!usart_get_flag(USART1,USART_SR_RXNE)){  c=usart_recv_blocking(USART1); ttwrite(&c , 1);  }
//		ttwrite( &c , 1 )

//	poll_end();
	}

}
