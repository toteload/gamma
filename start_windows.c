#include <Windows.h>
#include <stdint.h>

// Need to link with kernel32.lib

// This is the Gamma main function
extern int32_t main();

void start(void) {
    ExitProcess(main());
}
