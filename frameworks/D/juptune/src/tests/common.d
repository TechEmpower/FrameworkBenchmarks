module tests.common;

import juptune.core.util : Result, then;
import juptune.http : Http1Writer;

enum ENABLE_LOGGING = false;

void log(Args...)(scope auto ref Args args)
{
    import std.exception : assumeWontThrow;
    import std.stdio : writeln;
    static if(ENABLE_LOGGING)
        writeln(args).assumeWontThrow;
}

Result putServerAndDate(scope ref Http1Writer writer) nothrow
{
    import std.datetime : Clock, DayOfWeek, Month;
    import std.exception : assumeWontThrow;

    char[256] dateBuffer;
    size_t dateCursor;

    void put(const(char)[] value)
    {
        dateBuffer[dateCursor..dateCursor+value.length] = value[0..$];
        dateCursor += value.length;
    }

    void putInt(int value)
    {
        import juptune.core.util.conv : toBase10, IntToCharBuffer;
        IntToCharBuffer buffer;
        put(toBase10(value, buffer));
    }

    const currTime = Clock.currTime.assumeWontThrow.toUTC();
    final switch(currTime.dayOfWeek) with(DayOfWeek)
    {
        case mon: put("Mon, "); break;
        case tue: put("Tue, "); break;
        case wed: put("Wed, "); break;
        case thu: put("Thu, "); break;
        case fri: put("Fri, "); break;
        case sat: put("Sat, "); break;
        case sun: put("Sun, "); break;
    }

    putInt(currTime.day);
    put(" ");

    final switch(currTime.month) with(Month)
    {
        case jan: put("Jan "); break;
        case feb: put("Feb "); break;
        case mar: put("Mar "); break;
        case apr: put("Apr "); break;
        case may: put("May "); break;
        case jun: put("Jun "); break;
        case jul: put("Jul "); break;
        case aug: put("Aug "); break;
        case sep: put("Sep "); break;
        case oct: put("Oct "); break;
        case nov: put("Nov "); break;
        case dec: put("Dec "); break;
    }

    putInt(currTime.year);
    put(" ");

    putInt(currTime.hour);
    put(":");
    putInt(currTime.minute);
    put(":");
    putInt(currTime.second);
    put(" GMT");

    return writer.putHeader("Server", "Juptune (Linux)").then!(
        () => writer.putHeader("Date", dateBuffer[0..dateCursor])
    );
}