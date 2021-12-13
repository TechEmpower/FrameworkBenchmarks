package cn.ibaijia.tfb.http;

public abstract class HttpEntity {

    public String charset = "UTF-8";

    public abstract byte[] getHeader(byte[] name);

    public abstract byte[] getHeader(String name);

    public abstract void setHeader(byte[] name, byte[] value);

    public abstract void setContentType(String contentType);

    public abstract void setContentType(byte[] contentType);

}
