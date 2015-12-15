/*
 * Http2HPACKContext.h
 *
 *  Created on: 09-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2HPACKCONTEXT_H_
#define HTTP2HPACKCONTEXT_H_
#include "map"
#include "string"
#include "bitset"
#include "vector"
#include <math.h>
#include <stdio.h>
#include "cstring"
#include "CommonUtils.h"
using namespace std;

class Bits {
	vector<bitset<8> > bitv;
	bitset<30> bits30v;
	int lastbssize;
	uint32_t numbits;
	uint32_t code;
	unsigned char symbol;
	friend class Http2HPACKContext;
	friend class Http2HPACKHeaderTable;
public:
	void print();
	bool matches(const unsigned char& v, const int& nbits);
	void appendBits(string& out, const int& last);
	Bits();
	Bits(const uint32_t& code, const uint32_t& rembits, const unsigned char& sym);
	virtual ~Bits();
};


class Http2HPACKHeaderTable {
	static map<int, map<string, string> > HPACK_TABLE;
	static vector<Bits> HUFFMAN_TABLE;

	static map<string, map<int, Bits> > HUFFMAN_LK_STRINDX_NUMINDX_TABLE;
	static map<string, map<int, int> > HUFFMAN_LK_STRINDX_NUMINDX_BL_TABLE;

	static map<int, Bits> HUFFMAN_LK_NUMINDX_TABLE;
	static map<int, int> HUFFMAN_LK_NUMINDX_BL_TABLE;

	static map<string, Bits> cib;
	static map<uint32_t, bitset<38> > masks;

	static void init();

	map<int, map<string, string> > reqContextTable;
	map<int, map<string, string> > resContextTable;
	map<string, int> reqhnIndexTable;
	map<string, int> reqhnvIndexTable;
	int reqcurrentSize;
	map<string, int> reshnIndexTable;
	map<string, int> reshnvIndexTable;
	int rescurrentSize;
	Http2HPACKHeaderTable();
	friend class Http2HPACKContext;
public:
	virtual ~Http2HPACKHeaderTable();
	int getIndexByNameAndValue(const string& name, const string& value, const bool& isRequest);
	int getIndexByName(const string& name, const bool& isRequest);
	map<string, string> getNameAndValueByIndex(const int& index, const bool& isRequest);
	string getNameByIndex(const int& index, const bool& isRequest);
	void addHeader(const string& name, const string& value, const bool& isRequest);
};

class Http2HPACKContext {
public:
	Http2HPACKHeaderTable table;
	bool huffmanEncoding;
	bool decipherHuffmanValue(const int& bitnum, bitset<8> obvm, bitset<8>& bvm, string &out, bitset<8>& prev, int& last, uint32_t& indx, string& key, string& value, int& totbits, int& cub);
	long decodeNumber(const string& data, const int& prefixSize, bitset<8> bits, size_t& index);
	string encodeNumber(long number, const vector<bool>& prefixBits);
	string encodeString(string value);
	string decodeString(const string& data, size_t& indx);
	string encodeHuffman(const string& value);
	string decodeHuffman(const string& value);
	string decodeHuffmanOld(string value);
	Http2HPACKContext();
	string encode(const map<string, string>& headers);
	map<string, string> decode(const string& data);
	virtual ~Http2HPACKContext();
};

#endif /* HTTP2HPACKCONTEXT_H_ */
