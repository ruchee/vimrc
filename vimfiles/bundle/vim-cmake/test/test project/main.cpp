#include <iostream>
#include "configure.h"

int main() {
    std::cout << "Hello World" << std::endl;
#ifdef WITH_BYE
    std::cout << "Bye World" << std::endl;
#endif
    return 0;
}
