--!native
local COMMA = 44
local QUOTATION = 34
local OPEN_BRACKET = 91
local CLOSE_BRACKET = 93
local OPEN_BRACE = 123
local CLOSE_BRACE = 125
local BACKSLASH = 92

-- used for string serialization
local ESCAPE_MAP = {
	[0x5C] = string.byte("\\"), -- 5C = '\'
	[0x08] = string.byte("b"),
	[0x0C] = string.byte("f"),
	[0x0A] = string.byte("n"),
	[0x0D] = string.byte("r"),
	[0x09] = string.byte("t"),
}

local types = require("./types")

local buff: buffer = buffer.create(1024)
local size = 1024
local cursor = 0

local function alloc(len: number)
	if cursor + len < size then
		return
	end

	while cursor + len < size do
		size *= 2
	end

	local newbuff = buffer.create(size)
	buffer.copy(newbuff, 0, buff)
	buff = newbuff
end

local function WRITE_QUOTATION()
	buffer.writeu8(buff, cursor, QUOTATION)
	cursor += 1
end

local function WRITE_COMMA()
	buffer.writeu8(buff, cursor, COMMA)
	cursor += 1
end

local function writeString(str: string)
	local len = #str
	alloc(len)

	buffer.writestring(buff, cursor, str)

	cursor += len
end

local function serializeUnicode(codepoint: number)
	if codepoint >= 0x10000 then
		local high = ((codepoint - 0x10000) // 0x400) + 0xD800
		local low = ((codepoint - 0x10000) % 0x400) + 0xDC00
		return string.format("\\u%04x\\u%04x", high, low)
	end

	return string.format("\\u%04x", codepoint)
end

local function serializeString(str: string)
	-- covers the quotations & utf
	alloc((#str * 4) + 2)

	WRITE_QUOTATION()

	for pos, codepoint in utf8.codes(str) do
		if ESCAPE_MAP[codepoint] then
			-- write \
			buffer.writeu8(buff, cursor, BACKSLASH)
			cursor += 1

			buffer.writeu8(buff, cursor, ESCAPE_MAP[codepoint])
			cursor += 1
		elseif codepoint < 32 or codepoint > 126 then
			writeString(serializeUnicode(codepoint))
		else
			-- we are in ascii

			buffer.writeu8(buff, cursor, codepoint)
			cursor += 1
		end
	end

	WRITE_QUOTATION()
end

local serializeAny: (value: types.Value) -> ()

local function serializeArray(array: types.Array)
	-- open close bracket
	-- 1 comma for each array element
	alloc(2 + #array)

	-- write [
	buffer.writeu8(buff, cursor, OPEN_BRACKET)
	cursor += 1

	for _, value in array do
		serializeAny(value)
		WRITE_COMMA()
	end

	-- get rid of comma
	cursor -= 1

	-- write ]
	buffer.writeu8(buff, cursor, CLOSE_BRACKET)
	cursor += 1
end

local function serializeTable(object: types.Object)
	-- openbrace, newline
	alloc(2)

	-- write {
	buffer.writeu8(buff, cursor, OPEN_BRACE)
	cursor += 1

	for key, value in object do
		-- quotation 2x, colon and comma
		alloc(4)

		WRITE_QUOTATION()
		buffer.writestring(buff, cursor, key)
		cursor += #key + 2

		buffer.writestring(buff, cursor - 2, '":')

		serializeAny(value)

		WRITE_COMMA()
	end

	-- get rid of the comma
	cursor -= 1

	-- write }
	buffer.writeu8(buff, cursor, CLOSE_BRACE)
	cursor += 1
end

serializeAny = function(value: types.Value)
	local valueType = type(value)

	if valueType == "string" then
		serializeString(value :: string)
	elseif valueType == "table" then
		if #(value :: {}) == 0 then
			serializeTable(value :: types.Object)
		else
			serializeArray(value :: types.Array)
		end
	elseif valueType == "number" then
		local numstr = tostring(value)

		buffer.writestring(buff, cursor, numstr)
		cursor += #numstr
	elseif value == types.NULL then
		-- null as u32
		buffer.writeu32(buff, cursor, 1819047278)
		cursor += 4
	elseif value == true then
		-- true as u32
		buffer.writeu32(buff, cursor, 1702195828)
		cursor += 4
	elseif value == false then
		-- false as u32 + u8
		buffer.writeu32(buff, cursor, 1936482662)
		buffer.writeu8(buff, cursor + 4, 101)
		cursor += 5
	else
		error("Unknown value", 2)
	end
end

local function serialize(data: types.Value)
	buff = buffer.create(1024)
	size = 1024
	cursor = 0

	serializeAny(data)

	return buffer.readstring(buff, 0, cursor)
end

return {
	serialize = serialize,
}
