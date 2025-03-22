--!strict

local json = {
    --- Not actually a nil value, a newproxy stand-in for a null value since Luau has no actual representation of `null`
    NULL = newproxy() :: nil,
}

type JSONPrimitive = nil | number | string | boolean
type Object = { [string]: Value }
type Array = { Value }
export type Value = JSONPrimitive | Array | Object

-- serialization

type SerializerState = {
    buf: buffer,
    cursor: number,
    prettyPrint: boolean,
    depth: number,
}

local function checkState(state: SerializerState, len: number)
    local curLen = buffer.len(state.buf)

    if state.cursor + len < curLen then
        return
    end

    local newBuffer = buffer.create(curLen * 2)

    buffer.copy(newBuffer, 0, state.buf)

    state.buf = newBuffer
end

local function writeByte(state: SerializerState, byte: number)
    checkState(state, 1)

    buffer.writeu8(state.buf, state.cursor, byte)

    state.cursor += 1
end

local function writeSpaces(state: SerializerState)
    if state.depth == 0 then
        return
    end

    if state.prettyPrint then
        checkState(state, state.depth * 4)

        for i = 0, state.depth do
            buffer.writeu32(state.buf, state.cursor, 0x_20_20_20_20)
            state.cursor += 4
        end
    else
        buffer.writeu8(state.buf, state.cursor, string.byte(" "))

        state.cursor += 1
    end
end

local function writeString(state: SerializerState, str: string)
    checkState(state, #str)

    buffer.writestring(state.buf, state.cursor, str)

    state.cursor += #str
end

local serializeAny

local ESCAPE_MAP = {
    [0x5C] = string.byte("\\"), -- 5C = '\'
    [0x08] = string.byte("b"),
    [0x0C] = string.byte("f"),
    [0x0A] = string.byte("n"),
    [0x0D] = string.byte("r"),
    [0x09] = string.byte("t"),
}

local function serializeUnicode(codepoint: number)
    if codepoint >= 0x10000 then
        local high = math.floor((codepoint - 0x10000) / 0x400) + 0xD800
        local low = ((codepoint - 0x10000) % 0x400) + 0xDC00
        return string.format("\\u%04x\\u%04x", high, low)
    end

    return string.format("\\u%04x", codepoint)
end

local function serializeString(state: SerializerState, str: string)
    checkState(state, #str)

    writeByte(state, string.byte('"'))

    for pos, codepoint in utf8.codes(str) do
        if ESCAPE_MAP[codepoint] then
            writeByte(state, string.byte("\\"))
            writeByte(state, ESCAPE_MAP[codepoint])
        elseif codepoint < 32 or codepoint > 126 then
            writeString(state, serializeUnicode(codepoint))
        else
            writeString(state, utf8.char(codepoint))
        end
    end

    writeByte(state, string.byte('"'))
end

local function serializeArray(state: SerializerState, array: Array)
    state.depth += 1

    writeByte(state, string.byte("["))

    if state.prettyPrint and #array ~= 0 then
        writeByte(state, string.byte("\n"))
    end

    for i, value in array do
        if i ~= 1 then
            writeByte(state, string.byte(","))

            if state.prettyPrint then
                writeByte(state, string.byte("\n"))
            end
        end

        if i ~= 1 or state.prettyPrint then
            writeSpaces(state)
        end

        serializeAny(state, value)
    end

    state.depth -= 1

    if state.prettyPrint and #array ~= 0 then
        writeByte(state, string.byte("\n"))
        writeSpaces(state)
    end

    writeByte(state, string.byte("]"))
end

local function serializeTable(state: SerializerState, object: Object)
    writeByte(state, string.byte("{"))

    if state.prettyPrint then
        writeByte(state, string.byte("\n"))
    end

    state.depth += 1

    local first = true
    for key, value in object do
        if not first then
            writeByte(state, string.byte(","))
            writeByte(state, string.byte(" "))

            if state.prettyPrint then
                writeByte(state, string.byte("\n"))
            end
        end

        first = false

        writeSpaces(state)

        writeByte(state, string.byte('"'))
        writeString(state, key)
        writeString(state, '": ')

        serializeAny(state, value)
    end

    if state.prettyPrint then
        writeByte(state, string.byte("\n"))
    end

    state.depth -= 1

    writeSpaces(state)

    writeByte(state, string.byte("}"))
end

serializeAny = function(state: SerializerState, value: Value)
    local valueType = type(value)

    if value == json.NULL then
        writeString(state, "null")
    elseif valueType == "boolean" then
        writeString(state, if value then "true" else "false")
    elseif valueType == "number" then
        writeString(state, tostring(value))
    elseif valueType == "string" then
        serializeString(state, value :: string)
    elseif valueType == "table" then
        if #(value :: {}) == 0 and next(value :: {}) ~= nil then
            serializeTable(state, value :: Object)
        else
            serializeArray(state, value :: Array)
        end
    else
        error("Unknown value", 2)
    end
end

-- deserialization

type DeserializerState = {
    src: string,
    cursor: number,
}

local function deserializerError(state: DeserializerState, msg: string): never
    return error(`JSON error - {msg} around {state.cursor}`)
end

local function skipWhitespace(state: DeserializerState): boolean
    state.cursor = string.find(state.src, "%S", state.cursor) :: number

    if not state.cursor then
        return false
    end

    return true
end

local function currentByte(state: DeserializerState)
    return string.byte(state.src, state.cursor)
end

local function deserializeNumber(state: DeserializerState)
    -- first "segment"
    local nStart, nEnd = string.find(state.src, "^[%-%deE]*", state.cursor)

    if not nStart then
        -- i dont think this is possible
        deserializerError(state, "Could not match a number literal?")
    end

    if string.byte(state.src, nEnd :: number + 1) == string.byte(".") then -- decimal!
        local decStart, decEnd = string.find(state.src, "^[eE%-+%d]+", nEnd :: number + 2)

        if not decStart then
            deserializerError(state, "Trailing '.' in number value")
        end

        nEnd = decEnd
    end

    local num = tonumber(string.sub(state.src, nStart :: number, nEnd))

    if not num then
        deserializerError(state, "Malformed number value")
    end

    state.cursor = nEnd :: number + 1

    return num
end

local function decodeSurrogatePair(high, low): string?
    local highVal = tonumber(high, 16)
    local lowVal = tonumber(low, 16)

    if not highVal or not lowVal then
        return nil -- Invalid
    end

    -- Calculate the actual Unicode codepoint
    local codepoint = 0x10000 + ((highVal - 0xD800) * 0x400) + (lowVal - 0xDC00)
    return utf8.char(codepoint)
end

local function deserializeString(state: DeserializerState): string
    state.cursor += 1

    local startPos = state.cursor

    if currentByte(state) == string.byte('"') then
        state.cursor += 1

        return ""
    end

    while state.cursor <= #state.src do
        if currentByte(state) == string.byte('"') then
            state.cursor += 1

            local source = string.sub(state.src, startPos, state.cursor - 2)

            source = string.gsub(
                source,
                "\\u([dD]83[dD])\\u(d[cC]%w%w)",
                function(high, low)
                    return decodeSurrogatePair(high, low)
                        or deserializerError(state, "Invalid unicode surrogate pair")
                end :: any
            )
            -- Handle regular Unicode escapes
            source = string.gsub(source, "\\u(%x%x%x%x)", function(code)
                return utf8.char(tonumber(code, 16) :: number)
            end)

            source = string.gsub(source, "\\\\", "\0")
            source = string.gsub(source, "\\b", "\b")
            source = string.gsub(source, "\\f", "\f")
            source = string.gsub(source, "\\n", "\n")
            source = string.gsub(source, "\\r", "\r")
            source = string.gsub(source, "\\t", "\t")
            source = string.gsub(source, '\\"', '"')
            source = string.gsub(source, '\0', '\\')

            return source
        end

        if currentByte(state) == string.byte("\\") then
            state.cursor += 1
        end

        state.cursor += 1
    end

    -- error

    state.cursor = startPos

    return deserializerError(state, "Unterminated string")
end

local deserialize

local function deserializeArray(state: DeserializerState): Array
    state.cursor += 1

    local current: Array = {}

    local expectingValue = false
    while state.cursor < #state.src do
        skipWhitespace(state)

        if currentByte(state) == string.byte(",") then
            expectingValue = true
            state.cursor += 1
        end

        skipWhitespace(state)

        if currentByte(state) == string.byte("]") then
            break
        end

        table.insert(current, deserialize(state))

        expectingValue = false
    end

    if expectingValue then
        deserializerError(state, "Trailing comma")
    end

    if not skipWhitespace(state) or currentByte(state) ~= string.byte("]") then
        deserializerError(state, "Unterminated array")
    end

    state.cursor += 1

    return current
end

local function deserializeObject(state: DeserializerState): Object
    state.cursor += 1

    local current = {}

    local expectingValue = false
    while state.cursor < #state.src do
        skipWhitespace(state)

        if currentByte(state) == string.byte("}") then
            break
        end

        skipWhitespace(state)

        if currentByte(state) ~= string.byte('"') then
            return deserializerError(state, "Expected a string key")
        end

        local key = deserializeString(state)

        skipWhitespace(state)

        if currentByte(state) ~= string.byte(":") then
            return deserializerError(state, "Expected ':' for key value pair")
        end

        state.cursor += 1

        local value = deserialize(state)

        current[key] = value

        if not skipWhitespace(state) then
            deserializerError(state, "Unterminated object")
        end

        skipWhitespace(state)

        if currentByte(state) == string.byte(",") then
            expectingValue = true
            state.cursor += 1
        else
            expectingValue = false
        end
    end

    if expectingValue then
        return deserializerError(state, "Trailing comma")
    end

    if not skipWhitespace(state) or currentByte(state) ~= string.byte("}") then
        deserializerError(state, "Unterminated object")
    end

    state.cursor += 1

    return current
end

deserialize = function(state: DeserializerState): Value
    skipWhitespace(state)

    local fourChars = string.sub(state.src, state.cursor, state.cursor + 3)

    if fourChars == "null" then
        state.cursor += 4
        return json.NULL
    elseif fourChars == "true" then
        state.cursor += 4
        return true
    elseif string.sub(state.src, state.cursor, state.cursor + 4) == "false" then
        state.cursor += 5
        return false
    elseif string.match(state.src, "^[%d%.]", state.cursor) then
        -- number
        return deserializeNumber(state)
    elseif string.byte(state.src, state.cursor) == string.byte('"') then
        return deserializeString(state)
    elseif string.byte(state.src, state.cursor) == string.byte("[") then
        return deserializeArray(state)
    elseif string.byte(state.src, state.cursor) == string.byte("{") then
        return deserializeObject(state)
    end

    return deserializerError(state, `Unexpected token '{string.sub(state.src, state.cursor, state.cursor)}'`)
end

-- user-facing

json.serialize = function(value: Value, prettyPrint: boolean?)
    local state: SerializerState = {
        buf = buffer.create(1024),
        cursor = 0,
        prettyPrint = prettyPrint or false,
        depth = 0,
    }

    serializeAny(state, value)

    return buffer.readstring(state.buf, 0, state.cursor)
end

json.deserialize = function(src: string)
    local state = {
        src = src,
        cursor = 0,
    }

    return deserialize(state)
end

return table.freeze(json)
