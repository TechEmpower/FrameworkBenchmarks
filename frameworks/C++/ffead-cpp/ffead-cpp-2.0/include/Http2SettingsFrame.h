/*
 * Http2SettingsFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2SETTINGSFRAME_H_
#define HTTP2SETTINGSFRAME_H_
#include "Http2Frame.h"
#include "map"

class Http2SettingsFrame : public Http2Frame {
	map<uint16_t, uint32_t> settings;
	Http2SettingsFrame(const string& data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	static uint16_t SETTINGS_HEADER_TABLE_SIZE;
	static uint16_t SETTINGS_ENABLE_PUSH;
	static uint16_t SETTINGS_MAX_CONCURRENT_STREAMS;
	static uint16_t SETTINGS_INITIAL_WINDOW_SIZE;
	static uint16_t SETTINGS_MAX_FRAME_SIZE;
	static uint16_t SETTINGS_MAX_HEADER_LIST_SIZE;

	//Not the final spec yet, change this when the real rfc is finalized
	//https://tools.ietf.org/html/draft-hirano-httpbis-websocket-over-http2-01
	static uint16_t SETTINGS_WEBSOCKET_CAPABLE;
	Http2SettingsFrame();
	virtual ~Http2SettingsFrame();
	const map<uint16_t, uint32_t>& getSettings() const;
	string getFrameData();
};

#endif /* HTTP2SETTINGSFRAME_H_ */
