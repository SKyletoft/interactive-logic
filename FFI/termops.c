#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

#include "termops.h"

// https://stackoverflow.com/questions/1798511/how-to-avoid-pressing-enter-with-getchar-for-reading-a-single-character-only
// https://en.wikipedia.org/wiki/C_signal_handling

static struct termios oldt, newt;

void setup_terminal() {
	/*tcgetattr gets the parameters of the current terminal
	STDIN_FILENO will tell tcgetattr that it should write the settings
	of stdin to oldt*/
	tcgetattr(STDIN_FILENO, &oldt);
	/*now the settings will be copied*/
	newt = oldt;

	/*ICANON normally takes care that one line at a time will be processed
	that means it will return if it sees a "\n" or an EOF or an EOL*/
	newt.c_lflag &= ~(ICANON | ECHO);

	/*Those new settings will be set to STDIN
	TCSANOW tells tcsetattr to change attributes immediately. */
	tcsetattr( STDIN_FILENO, TCSANOW, &newt);

	signal(SIGINT, deinit_terminal);
}

void deinit_terminal() {
	tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
	exit(0);
}
