/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: Consts.java
 * Date: 2018-02-06
 * Author: sandao
 */

package org.smartboot.http;

import java.nio.charset.Charset;

public interface Consts {

    /**
     * Horizontal space
     */
    public static final byte SP = 32;


    /**
     * Carriage return
     */
    public static final byte CR = 13;


    /**
     * Line feed character
     */
    public static final byte LF = 10;

    /**
     * Colon ':'
     */
    public static final byte COLON = 58;


    public static final byte[] CRLF = {Consts.CR, Consts.LF};

    byte[] COLON_ARRAY = {COLON};

    byte[] SP_ARRAY = {SP};
}