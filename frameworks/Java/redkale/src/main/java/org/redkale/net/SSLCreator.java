/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.*;
import java.security.*;
import java.security.cert.*;
import javax.net.ssl.*;
import org.redkale.util.*;

/**
 * 根据配置生成SSLContext
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface SSLCreator {

    default SSLContext create(Server server, AnyValue sslConf) throws Exception {
        String keyfile = sslConf.getValue("keystorefile");
        String keypass = sslConf.getValue("keystorepass", "");
        KeyManager[] keyManagers = null;
        if (keyfile != null) {
            KeyManagerFactory kmf = KeyManagerFactory.getInstance("SunX509");
            KeyStore ks = KeyStore.getInstance("JKS");
            ks.load(new FileInputStream(keyfile), keypass.toCharArray());
            kmf.init(ks, keypass.toCharArray());
            keyManagers = kmf.getKeyManagers();
        }

        String trustfile = sslConf.getValue("truststorefile");
        String trustpass = sslConf.getValue("truststorepass", "");
        TrustManager[] trustManagers;
        if (trustfile != null) {
            KeyStore ts = KeyStore.getInstance("JKS");
            ts.load(new FileInputStream(trustfile), trustpass.toCharArray());
            TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
            tmf.init(ts);
            trustManagers = tmf.getTrustManagers();
        } else {
            trustManagers = new TrustManager[]{new X509TrustManager() {
                @Override
                public void checkClientTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
                }

                @Override
                public void checkServerTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
                }

                @Override
                public X509Certificate[] getAcceptedIssuers() {
                    return new X509Certificate[0];
                }
            }};
        }
        SSLContext sslContext = SSLContext.getInstance("TLS");
        sslContext.init(keyManagers, trustManagers, new SecureRandom());
        return sslContext;
    }
}
