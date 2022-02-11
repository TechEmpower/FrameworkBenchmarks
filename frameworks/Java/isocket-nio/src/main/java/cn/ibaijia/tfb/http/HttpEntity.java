package cn.ibaijia.tfb.http;

/**
 * @author longzl
 */
public abstract class HttpEntity {

//    public static String charset = "UTF-8";

    /**
     * 获取Header
     * @param name
     * @return
     */
    public abstract byte[] getHeader(byte[] name);

    /**
     * 获取Header
     * @param name
     * @return
     */
    public abstract byte[] getHeader(String name);

    /**
     * 设置header
     * @param name
     * @param value
     */
    public abstract void setHeader(byte[] name, byte[] value);

    /**
     * @param contentType
     */
    public abstract void setContentType(String contentType);

    public abstract void setContentType(byte[] contentType);

}
