package org.smartboot.http;

import java.util.ArrayList;
import java.util.List;

/**
 * @author 三刀
 * @version V1.0 , 2018/6/10
 */
public class BufferRanges {

    public List<BufferRange> headers = new ArrayList<>();

    public BufferRange getReadableRange() {
        BufferRange bufferRange;
        if (headers.size() == 0) {
            bufferRange = new BufferRange();
            headers.add(bufferRange);
            return bufferRange;
        }
        bufferRange = headers.get(headers.size() - 1);
        if (bufferRange.isOk) {
            bufferRange = new BufferRange();
            headers.add(bufferRange);
        }
        return bufferRange;
    }

    public void reset() {
        headers.clear();
    }
}
