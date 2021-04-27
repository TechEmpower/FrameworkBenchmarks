package cn.ibaijia.tfb.http;

public abstract class HttpEntity {

    public String charset = "UTF-8";

    public abstract String getHeader(String name);
    public abstract void setHeader(String name,String value);

}
