#include <stdlib.h>
#include <unistd.h>
int main()
{
	int i, r;
	fork(); /*    2 processes */
	fork(); /*    4 processes */
	fork(); /*    8 processes */
	fork(); /*   16 processes */
	fork(); /*   32 processes */
	fork(); /*   64 processes */
	fork(); /*  128 processes */
	fork(); /*  256 processes */
	fork(); /*  512 processes */
	fork(); /* 1024 processes */
	fork(); /* 2048 processes */
	fork(); /* 4096 processes */
	sleep(1);
	return 0;
}
