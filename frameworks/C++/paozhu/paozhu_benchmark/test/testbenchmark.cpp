#include <string>
#include <iostream>
#include <thread>
#include <chrono>
#include <filesystem>
#include "server.h"

static sigjmp_buf env_startacs;
static void sig_child(int signo);
namespace fs = std::filesystem;
int main(int argc, char *argv[])
{
    std::string argv_str;
    if (argc > 1)
    {
        // server.conf filepath or confpath
        argv_str.append(argv[1]);
        fs::path conf_path = argv_str;
        if (fs::is_regular_file(conf_path))
        {
        }
        else
        {
            if (argv_str.back() == '/')
            {
                argv_str = argv_str + "server.conf";
            }
            else
            {
                argv_str = argv_str + "/server.conf";
            }
            conf_path = argv_str;
            if (fs::is_regular_file(conf_path))
            {
            }
            else
            {
                std::cout << "Not found server.conf file.";
                return 0;
            }
        }
    }
    else
    {

        fs::path conf_path = fs::current_path();
        argv_str           = conf_path.string() + "/conf/server.conf";
        conf_path          = argv_str;
        if (fs::is_regular_file(conf_path))
        {
        }
        else
        {
            argv_str  = "/usr/local/etc/paozhu/server.conf";
            conf_path = argv_str;
            if (fs::is_regular_file(conf_path))
            {
            }
            else
            {
                std::cout << "Not found server.conf file. Please copy conf Directory rename to /usr/local/etc/paozhu\n";
                return 0;
            }
        }
    }

    pid_t pid;//, subpid = 0;
    signal(SIGCHLD, sig_child);
    if (sigsetjmp(env_startacs, 1) == 0)// 设置记号
    {
        printf("setjmp ok.....\n");
    }
    else
    {
        printf("longjmp ok.....\n");
    }

    pid = fork();
    printf("fork id %d \n", pid);
    if (pid < 0)
    {
        perror("fork error:");
        exit(1);
    }
    else if (pid == 0)
    {

        try
        {
            http::httpserver &httpmy = http::get_server_app();
            httpmy.run(argv_str);
        }
        catch (std::exception &e)
        {
            std::printf("Exception: %s\n", e.what());
        }
        exit(0);
    }
    else
    {

        while (1)
        {
            std::this_thread::sleep_for(std::chrono::seconds(10));
            //Future features are added here
        }
        return 0;
    }
}
static void sig_child(int signo)
{
    /*pid_t */ int pid;
    int stat;
    // 处理僵尸进程

    switch (signo)
    {
    case SIGCHLD:

        pid = wait(&stat);
        printf("SIGCHLD...farter id %d..%d\n", getpid(), pid);
        siglongjmp(env_startacs, 1);// jump setjmp begin
        break;
    }
    exit(0);
}
