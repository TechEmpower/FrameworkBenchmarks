
#include "fortune_mysql.h"
#include "fortunebase.h"
#include "Fortune.h"

/* 如果此文件存在不会自动覆盖，没有则会自动生成。
*If this file exists, it will not be overwritten automatically. If not, it will be generated automatically. */

	 
 namespace orm{
 
			 Fortune::Fortune(std::string dbtag):fortune_mysql(dbtag){ mod=this; }
			 Fortune::Fortune():fortune_mysql(){ mod=this; }


	  }
