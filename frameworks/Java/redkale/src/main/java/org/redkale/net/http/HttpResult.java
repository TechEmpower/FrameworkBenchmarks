/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.Serializable;
import java.net.HttpCookie;
import java.util.*;
import org.redkale.convert.json.JsonConvert;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 结果对象的类型
 */
public class HttpResult<T> {

    public static final String SESSIONID_COOKIENAME = HttpRequest.SESSIONID_NAME;

    private Map<String, String> headers;

    private List<HttpCookie> cookies;

    private String contentType;

    private T result;

    private int status = 0; //不设置则为 200

    private String message;

    public HttpResult() {
    }

    public HttpResult(T result) {
        this.result = result;
    }

    public HttpResult(String contentType, T result) {
        this.contentType = contentType;
        this.result = result;
    }

    public HttpResult<T> header(String name, Serializable value) {
        if (this.headers == null) this.headers = new HashMap<>();
        this.headers.put(name, String.valueOf(value));
        return this;
    }

    public HttpResult<T> cookie(String name, Serializable value) {
        return cookie(new HttpCookie(name, String.valueOf(value)));
    }

    public HttpResult<T> cookie(HttpCookie cookie) {
        if (this.cookies == null) this.cookies = new ArrayList<>();
        this.cookies.add(cookie);
        return this;
    }

    public HttpResult<T> contentType(String contentType) {
        this.contentType = contentType;
        return this;
    }

    public HttpResult<T> result(T result) {
        this.result = result;
        return this;
    }

    public HttpResult<T> status(int status) {
        this.status = status;
        return this;
    }

    public HttpResult<T> message(String message) {
        this.message = message;
        return this;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public List<HttpCookie> getCookies() {
        return cookies;
    }

    public void setCookies(List<HttpCookie> cookies) {
        this.cookies = cookies;
    }

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public T getResult() {
        return result;
    }

    public void setResult(T result) {
        this.result = result;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }
}
