#include "cpoll.H"
#include <sys/types.h>
       #include <sys/stat.h>
       #include <fcntl.h>

using namespace std;
using namespace CP;
int main()
{
	Poll p;
	File f(0); //stdin
	File f2(1); //stdout
	File f3(open("/etc/passwd", O_RDONLY));
	
	/*char buf[4096];
	f.read(buf, 4096, [&](int br)
	{
		cerr << br << " bytes read;" << endl;
		f3.read(buf, 4096, [](int br){cout << br << " bytes read from file" << endl;});
		
	});*/
	
	
	//generate troll
	stringstream ss1;
	ss1 << "troll";
	for(int i=0;i<30;i++)
		ss1 << " lol";
	string str1=ss1.str();
	stringstream ss2;
	for(int i=0;i<5000;i++)
		ss2 << str1 << endl;
	string str2=ss2.str();
	const char* cstr2=str2.data();
	int cstr2len=str2.length();
	


	const char* httphdr="HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n";
	int httphdr_len=strlen(httphdr);
	
	Socket* s1=new Socket();
	s1->bind(IPEndPoint(IPAddress("0.0.0.0"), 12345));
	s1->listen();
	//cout << (void*)s1->accept();
	s1->repeatAccept([&](Socket* s) {
		if(s==NULL)return;
		cout << "accepted socket " << s->handle << endl;
		//s.retain();
		/*char* buf=new char[4096];
		s->repeatRead(buf, 4096, [buf, &f2, s](int l) {
			if(l<=0) {
				cerr << "closed socket " << s->handle << endl;
				free(buf);
				s->release();
				return;
			}
			f2.write(buf, l);
		});*/
		s->writeAll(httphdr, httphdr_len, [s,&cstr2,cstr2len](int l) {
			s->writeAll(cstr2, cstr2len, [s](int l) {
				char* buf=new char[4096];
				s->repeatRead(buf, 4096, [buf](int br) {
					if(br<=0)
						delete[] buf;
				});
			});
		});
		p.add(*s);
		s->release();
	});
	p.add(*s1);
	
	
	f2.write("aaaaa\n", 6, [](int bw){cerr << bw << " bytes written" << endl;});
	//f.loop();
	
	p.add(f);
	p.add(f2);
	p.add(f3);
	p.loop();
	
	/*File f(0); //stdin
	char buf[4096];
	f.read(buf,4096,[](int br){cout << "read " << br << endl;});
	Poll poll;
	poll.add(f);
	Poll poll2;
	poll2.add(poll);
	poll2.loop();*/
}
