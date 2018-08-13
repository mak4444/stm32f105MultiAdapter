#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
//mmo #include "version.h"
#include "can_driver.h"
#include "slcan.h"
//mmo #include "bus_power.h"

#define MAX_FRAME_LEN (sizeof("T1111222281122334455667788EA5F\r")+1)

int slcan_serial_write(void *arg, const char *buf, size_t len);
//char *slcan_getline(void *arg);
char *slcan_getline();

static void slcan_ack(char *buf);
static void slcan_nack(char *buf);

static char hex_digit(const uint8_t b)
{
    static const char *hex_tbl = "0123456789abcdef";
    return hex_tbl[b & 0x0f];
}

static void hex_write(char **p, const uint8_t *data, uint8_t len)
{
    unsigned int i;
    for (i = 0; i < len; i++) {
        *(*p)++ = hex_digit(data[i]>>4);
        *(*p)++ = hex_digit(data[i]);
    }
}

static uint8_t hex_val(char c)
{
    if (c >= 'A' && c <= 'F') {
        return c - 'A' + 0xA;
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 0xa;
    } else {
        return (c - '0') & 0xf;
    }
}

static uint32_t hex_to_u32(const char *str, uint8_t len)
{
    uint32_t val = 0;
    unsigned int i;
    for (i = 0; i < len; i++) {
        val = (val<<4) | hex_val(str[i]);
    }
    return val;
}

static uint8_t hex_to_u8(const char *str)
{
    uint8_t val;
    val = hex_val(*str++);
    val = (val<<4) | hex_val(*str);
    return val;
}

void hex_to_u8_array(const char *str, uint8_t *buf, size_t len)
{
    while (len-- > 0) {
        *buf++ = hex_to_u8(str);
        str += 2;
    }
}

size_t slcan_frame_to_ascii(char *buf, const struct can_frame_s *f, bool timestamp)
{
    char *p = buf;
    uint32_t id = f->id;

    // type
    if (f->remote) {
        if (f->extended) {
            *p++ = 'R';
        } else {
            *p++ = 'r';
        }
    } else {
        if (f->extended) {
            *p++ = 'T';
        } else {
            *p++ = 't';
        }
    }

    // ID
    if (f->extended) {
        int i;
        for (i = 3; i >= 0; i--) {
            uint8_t b = id>>(8*i);
            hex_write(&p, &b, 1);
        }
    } else {
        *p++ = hex_digit(id>>8);
        *p++ = hex_digit(id>>4);
        *p++ = hex_digit(id);
    }

    // DLC
    *p++ = hex_digit(f->length);

    // data
    if (!f->remote) {
        hex_write(&p, f->data, f->length);
    }

    // timestamp
    if (timestamp) {
        uint16_t t = f->timestamp;
        uint8_t b[2] = {t>>8, t};
        hex_write(&p, b, 2);
    }

    *p++ = '\r';
    *p = 0;

    return (size_t)(p - buf);
}

#define SLC_STD_ID_LEN 3
#define SLC_EXT_ID_LEN 8

can_send_tst(uint32_t id, bool extended,bool remote,uint8_t * data,uint8_t len)
{
volatile	static uint32_t sid;
volatile	static bool sextended;
volatile   static bool sremote;
volatile 	static uint8_t  sdata;
volatile	static uint8_t slen;
	sid=id;
	sextended=extended;
	sremote=remote;
	sdata=data;
	slen=len;

}

void slcan_send_frame(char *line)
{
    char *out = line;
    uint8_t data[8];
    uint8_t len;
    uint32_t id;
    bool remote = false;
    bool extended = false;

    switch (*line++) {
    case 'r':
        remote = true;
        /* fallthrought */
    case 't':
        id = hex_to_u32(line, SLC_STD_ID_LEN);
        line += SLC_STD_ID_LEN;
        break;
    case 'R':
        remote = true;
        /* fallthrought */
    case 'T':
        extended = true;
        id = hex_to_u32(line, SLC_EXT_ID_LEN);
        line += SLC_EXT_ID_LEN;
        break;
    default:
        slcan_nack(out);
        return;
    };

    len = hex_val(*line++);

    if (len > 8) {
        slcan_nack(out);
        return;
    }

    if (!remote) {
        hex_to_u8_array(line, data, len);
    }
//    can_send_tst(id, extended, remote, data, len);

    if (can_send(id, extended, remote, data, len)) {
        slcan_ack(out);
    } else {
        slcan_nack(out);
    }

}

static void set_bitrate(char* line)
{
    static const uint32_t br_tbl[10] = {10000, 20000, 50000, 100000, 125000,
                                        250000, 500000, 800000, 1000000};
    unsigned char i = line[1];
    if (i < '0' || i > '8') {
        slcan_nack(line);
        return;
    }
    i -= '0';
/* mmo
    if (can_set_bitrate(br_tbl[i])) {
        slcan_ack(line);
    } else {
        slcan_nack(line);
    }
*/
}

static void slcan_open(char *line, int mode)
{
	slcan_ack(line);

    if (can_open(mode)) {
        slcan_ack(line);
    } else {
        slcan_nack(line);
    }
}

static void slcan_close(char *line)
{
//mmo    can_close();
    slcan_ack(line);
}

/** wirtes a NULL terminated ACK response */
static void slcan_ack(char *buf)
{
    *buf++ = '\r'; // CR
    *buf = 0;
}

/** wirtes a NULL terminated NACK response */
static void slcan_nack(char *buf)
{
    *buf++ = '\a'; // BELL
    *buf = 0;
}

/*
reference:
http://www.fischl.de/usbtin/
http://www.can232.com/docs/canusb_manual.pdf
*/
void slcan_decode_line(char *line)
{
    switch (*line) {
    case 'T': // extended frame
    case 't': // standard frame
    case 'R': // extended remote frame
    case 'r': // standard remote frame
        slcan_send_frame(line);
        break;
    case 'S': // set baud rate, S0-S9
        set_bitrate(line);
        break;
    case 'O': // open CAN channel
        slcan_open(line, CAN_MODE_NORMAL);
        break;
    case 'l': // open in loop back mode
        slcan_open(line, CAN_MODE_LOOPBACK);
        break;
    case 'L': // open in silent mode (listen only)
        slcan_open(line, CAN_MODE_SILENT);
        break;
    case 'C': // close CAN channel
        slcan_close(line);
        break;
    case 'V': // hardware version
//        line = stpcpy(line, hardware_version_str);
        line = stpcpy(line,"hui");
        slcan_ack(line);
        break;
    case 'v': // firmware version
        line = stpcpy(line,"hui2");
//        line = stpcpy(line, software_version_str);
        slcan_ack(line);
        break;
    case 'F': // read & clear status/error flags
        line[1] = '0'; // no error
        line[2] = '0';
        slcan_ack(line);
        break;
    case '\0': // Empty line, requires an ACK to be sent back
        slcan_ack(line);
        break;
    // 'N': // serial number
    // 'F': // read status byte
    // 'Z': // timestamp on/off, Zx[CR]
    // 'm': // acceptance mask, mxxxxxxxx[CR]
    // 'M': // acceptance code, Mxxxxxxxx[CR]

    /* CVRA Proprietary extensions */
    case 'P': // Enable bus power
    	//mmo        bus_power(true);
        slcan_ack(line);
        break;
    case 'p': // Disable bus power
//mmo        bus_power(false);
slcan_ack(line);
        break;
    default:
        slcan_nack(line);
        break;
    };
}
#ifdef mmo
char *slcan_getline()
{
    static char line_buffer[500];
    static size_t pos = 0;
    size_t i;
    for (i = pos; i < sizeof(line_buffer); i++) {
        int c = WGetPort1();
        if (c == '\n' || c == '\r' || c == '\0') {
            /* line found */
            line_buffer[i] = 0;
            pos = 0;
            return line_buffer;
        } else {
            line_buffer[i] = c;
        }
    }

    /* reset */
    pos = 0;
    return NULL;
}
#endif


char txbuf[MAX_FRAME_LEN];

size_t slcan_rx_spin_rx()
{
    struct can_frame_s *rxf;
    rxf = CanGet();
    return slcan_frame_to_ascii(txbuf, rxf, false);
}

#if 1

char *slcan_getline()
{
    static char line_buffer[500];
    static size_t pos = 0;
    if(ComCnt1())
    {
		int c = GetPort1();
        if (c == '\n' || c == '\r' || c == '\0') {
            /* line found */
            line_buffer[pos] = 0;
            pos = 0;
            return line_buffer;
        } else {
        	if(pos>=sizeof(line_buffer)) pos = 0;
            line_buffer[pos++] = c;
            return NULL;
        }
    }

    return NULL;
}

void slcan_spin()
{
    char *line = slcan_getline();
    if (line) {
        slcan_decode_line(line);
        slcan_usart1_write( line, strlen(line));
    }
}


void slcan_rx_spin_mmo()
{
    if (CanCnt()) {
        size_t len;
        len = slcan_rx_spin_rx();
        slcan_usart1_write(txbuf, len);
    }
}

void slcan_loop()
{    while (1) {
    	slcan_rx_spin_mmo();
    	slcan_spin();
    }
}

#endif

