#include <iostream>

#include <import.hpp>

int main(int argc, char** argv) try
{
	framework::utility::initializeWebFramework();

	framework::WebFramework server("config.json");

	server.start(true, []() { std::cout << "Server is running..." << std::endl; });

	return 0;
}
catch (const std::exception& e)
{
	std::cerr << e.what() << std::endl;

	return 1;
}
