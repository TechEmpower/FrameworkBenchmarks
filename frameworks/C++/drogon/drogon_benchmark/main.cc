#include <drogon/drogon.h>
#include <iostream>

int main(int argc, char const *argv[])
{
    if (argc < 2)
    {
        std::cout << "please input the config file name" << std::endl;
        return -1;
    }
    drogon::app().loadConfigFile(argv[1]).run();
    return 0;
}
