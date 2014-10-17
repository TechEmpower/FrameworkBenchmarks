all: cppsp_0.2.3

clean:
	rm -f www/*.so www/*.txt
	rm -f www/forcedynamic.cppsm.*
	cd $(IROOT)/cppsp_0.2.3 all && $(MAKE) clean

cppsp_0.2.3:
	$(MAKE) CXX=g++-4.8 -C $(IROOT)/cppsp_0.2.3 all


