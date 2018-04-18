/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.CompletionHandler;
import java.util.function.*;
import java.util.logging.Level;

/**
 * 协议响应对象
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <C> Context的子类型
 * @param <R> Request的子类型
 */
@SuppressWarnings("unchecked")
public abstract class Response<C extends Context, R extends Request<C>> {

    protected final C context;

    protected final R request;

    protected AsyncConnection channel;

    protected ByteBuffer writeHeadBuffer;

    protected ByteBuffer writeBodyBuffer;

    private boolean inited = true;

    protected Object output; //输出的结果对象

    protected BiConsumer<R, Response<C, R>> recycleListener;

    protected Filter<C, R, ? extends Response<C, R>> filter;

    protected Servlet<C, R, ? extends Response<C, R>> servlet;

    private Supplier<ByteBuffer> bodyBufferSupplier;

    private final CompletionHandler finishHandler = new CompletionHandler<Integer, ByteBuffer>() {

        @Override
        public void completed(Integer result, ByteBuffer attachment) {
            if (attachment.hasRemaining()) {
                channel.write(attachment, attachment, this);
            } else {
                offerResponseBuffer(attachment);
                finish();
            }
        }

        @Override
        public void failed(Throwable exc, ByteBuffer attachment) {
            offerResponseBuffer(attachment);
            finish(true);
        }

        private void offerResponseBuffer(ByteBuffer attachment) {
            if (writeHeadBuffer == null) {
                if (context.bufferPool.getRecyclerPredicate().test(attachment)) {
                    writeHeadBuffer = attachment;
                }
            } else if (writeBodyBuffer == null) {
                if (context.bufferPool.getRecyclerPredicate().test(attachment)) {
                    writeBodyBuffer = attachment;
                }
            } else {
                context.offerBuffer(attachment);
            }
        }

    };

    private final CompletionHandler finishHandler2 = new CompletionHandler<Integer, ByteBuffer[]>() {

        @Override
        public void completed(final Integer result, final ByteBuffer[] attachments) {
            int index = -1;
            for (int i = 0; i < attachments.length; i++) {
                if (attachments[i].hasRemaining()) {
                    index = i;
                    break;
                }
            }
            if (index >= 0) {
                channel.write(attachments, index, attachments.length - index, attachments, this);
            } else {
                offerResponseBuffer(attachments);
                finish();
            }
        }

        @Override
        public void failed(Throwable exc, final ByteBuffer[] attachments) {
            offerResponseBuffer(attachments);
            finish(true);
        }

        private void offerResponseBuffer(ByteBuffer[] attachments) {
            int start = 0;
            if (writeHeadBuffer == null && attachments.length > start) {
                if (context.bufferPool.getRecyclerPredicate().test(attachments[start])) {
                    writeHeadBuffer = attachments[start];
                    start++;
                }
            }
            if (writeBodyBuffer == null && attachments.length > start) {
                if (context.bufferPool.getRecyclerPredicate().test(attachments[start])) {
                    writeBodyBuffer = attachments[start];
                    start++;
                }
            }
            for (int i = start; i < attachments.length; i++) {
                context.offerBuffer(attachments[i]);
            }
        }
    };

    protected Response(C context, final R request) {
        this.context = context;
        this.request = request;
        this.writeHeadBuffer = context.pollBuffer();
        this.writeBodyBuffer = context.pollBuffer();
        this.bodyBufferSupplier = () -> {
            ByteBuffer buffer = writeBodyBuffer;
            if (buffer == null) return context.pollBuffer();
            writeBodyBuffer = null;
            return buffer;
        };
    }

    protected ByteBuffer pollWriteReadBuffer() {
        ByteBuffer buffer = this.writeHeadBuffer;
        this.writeHeadBuffer = null;
        if (buffer == null) buffer = context.pollBuffer();
        return buffer;
    }

    protected ByteBuffer pollWriteBodyBuffer() {
        ByteBuffer buffer = this.writeBodyBuffer;
        this.writeBodyBuffer = null;
        if (buffer == null) buffer = context.pollBuffer();
        return buffer;
    }

    protected Supplier<ByteBuffer> getBodyBufferSupplier() {
        return bodyBufferSupplier;
    }

    protected AsyncConnection removeChannel() {
        AsyncConnection ch = this.channel;
        this.channel = null;
        this.request.channel = null;
        return ch;
    }

    protected void prepare() {
        inited = true;
    }

    protected boolean recycle() {
        if (!inited) return false;
        boolean keepAlive = request.keepAlive;
        if (recycleListener != null) {
            try {
                recycleListener.accept(request, this);
            } catch (Exception e) {
                context.logger.log(Level.WARNING, "Response.recycleListener error, request = " + request, e);
            }
            recycleListener = null;
        }
        this.output = null;
        this.filter = null;
        this.servlet = null;
        request.recycle();
        if (channel != null) {
            if (keepAlive) {
                this.context.runAsync(new PrepareRunner(context, channel, null, null));
            } else {
                try {
                    if (channel.isOpen()) channel.close();
                } catch (Exception e) {
                }
            }
            channel = null;
        }
        this.inited = false;
        return true;
    }

    protected void refuseAlive() {
        this.request.keepAlive = false;
    }

    protected void init(AsyncConnection channel) {
        this.channel = channel;
        this.request.channel = channel;
        this.request.createtime = System.currentTimeMillis();
    }

    protected void setFilter(Filter<C, R, Response<C, R>> filter) {
        this.filter = filter;
    }

    protected void thenEvent(Servlet servlet) {
        this.servlet = servlet;
    }

    @SuppressWarnings("unchecked")
    public void nextEvent() throws IOException {
        if (this.filter != null) {
            Filter runner = this.filter;
            this.filter = this.filter._next;
            runner.doFilter(request, this);
            return;
        }
        if (this.servlet != null) {
            Servlet s = this.servlet;
            this.servlet = null;
            s.execute(request, this);
        }
    }

    public void recycleListener(BiConsumer<R, Response<C, R>> recycleListener) {
        this.recycleListener = recycleListener;
    }

    public Object getOutput() {
        return output;
    }

    /**
     * 是否已关闭
     *
     * @return boolean
     */
    public boolean isClosed() {
        return !this.inited;
    }

    public void finish() {
        this.finish(false);
    }

    public void finish(boolean kill) {
        if (!this.inited) return; //避免重复关闭
        //System.println("耗时: " + (System.currentTimeMillis() - request.createtime));
        if (kill) refuseAlive();
        this.context.responsePool.accept(this);
    }

    public void finish(final byte[] bs) {
        if (!this.inited) return; //避免重复关闭
        if (this.context.bufferCapacity == bs.length) {
            ByteBuffer buffer = this.context.pollBuffer();
            buffer.put(bs);
            buffer.flip();
            this.finish(buffer);
        } else {
            this.finish(ByteBuffer.wrap(bs));
        }
    }

    public void finish(ByteBuffer buffer) {
        if (!this.inited) return; //避免重复关闭
        this.channel.write(buffer, buffer, finishHandler);
    }

    public void finish(boolean kill, ByteBuffer buffer) {
        if (!this.inited) return; //避免重复关闭
        if (kill) refuseAlive();
        this.channel.write(buffer, buffer, finishHandler);
    }

    public void finish(ByteBuffer... buffers) {
        if (!this.inited) return; //避免重复关闭
        this.channel.write(buffers, buffers, finishHandler2);
    }

    public void finish(boolean kill, ByteBuffer... buffers) {
        if (!this.inited) return; //避免重复关闭
        if (kill) refuseAlive();
        this.channel.write(buffers, buffers, finishHandler2);
    }

    protected <A> void send(final ByteBuffer buffer, final A attachment, final CompletionHandler<Integer, A> handler) {
        this.channel.write(buffer, attachment, new CompletionHandler<Integer, A>() {

            @Override
            public void completed(Integer result, A attachment) {
                if (buffer.hasRemaining()) {
                    channel.write(buffer, attachment, this);
                } else {
                    context.offerBuffer(buffer);
                    if (handler != null) handler.completed(result, attachment);
                }
            }

            @Override
            public void failed(Throwable exc, A attachment) {
                context.offerBuffer(buffer);
                if (handler != null) handler.failed(exc, attachment);
            }

        });
    }

    protected <A> void send(final ByteBuffer[] buffers, A attachment, final CompletionHandler<Integer, A> handler) {
        this.channel.write(buffers, attachment, new CompletionHandler<Integer, A>() {

            @Override
            public void completed(Integer result, A attachment) {
                int index = -1;
                for (int i = 0; i < buffers.length; i++) {
                    if (buffers[i].hasRemaining()) {
                        index = i;
                        break;
                    }
                    context.offerBuffer(buffers[i]);
                }
                if (index == 0) {
                    channel.write(buffers, attachment, this);
                } else if (index > 0) {
                    ByteBuffer[] newattachs = new ByteBuffer[buffers.length - index];
                    System.arraycopy(buffers, index, newattachs, 0, newattachs.length);
                    channel.write(newattachs, attachment, this);
                } else if (handler != null) handler.completed(result, attachment);
            }

            @Override
            public void failed(Throwable exc, A attachment) {
                for (ByteBuffer buffer : buffers) {
                    context.offerBuffer(buffer);
                }
                if (handler != null) handler.failed(exc, attachment);
            }

        });
    }

    public C getContext() {
        return context;
    }
}
