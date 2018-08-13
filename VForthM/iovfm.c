
#include <termios.h>
#include <sys/ioctl.h>

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <linux/types.h>
#include <unistd.h>

__u32 ttwrite ( int __fd, const void *__buf, size_t __n)
{

 return write (__fd, __buf, __n);
}

__u32 ttgetchar ()
{  return  getchar ();
}   

char * vatr;

__u32 ttgetch(int __fd)
{
	  char ch;
	  int n;
	  struct termios t1, t2;

	      tcgetattr(__fd, &t1);
	      t2 = t1;
	      t2.c_lflag &= ~ICANON;
	      t2.c_lflag &= ~ECHO;
	      t2.c_lflag |= ISIG;
	      t2.c_cc[VMIN] = 1;
	      t2.c_cc[VTIME] = 0;
	      tcsetattr(__fd, TCSANOW, &t2);

		n = read(__fd, &ch, 1);

	      tcsetattr(__fd, TCSANOW, &t1);

	  return ch;
}

__u32 ttkbhit(int __fd) {
	  struct termios t1, t2;
	    int bytesWaiting;

    tcgetattr(__fd, &t1);

      	t2 = t1;
        t2.c_lflag &= ~ICANON;

    tcsetattr(__fd, TCSANOW, &t2);

    	ioctl(__fd, FIONREAD, &bytesWaiting);

    tcsetattr(__fd, TCSANOW, &t1);

    return bytesWaiting;
}


void ttputch(int __fd, int c)
{
	char k=c;
	while(write(__fd,&k,1)<0);
}

