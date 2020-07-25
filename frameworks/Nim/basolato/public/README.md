Public
===
By default Basolato looks for static files in this directory. This can be overriden using the `setStaticDir` function in main.nim. Files will be served like so:

./public/css/style.css -> http://example.com/css/style.css

Note: Basolato will only serve files, that are readable by others. On Unix/Linux you can ensure this with chmod o+r ./public/css/style.css.

In production enviroment, static files should be served by Web server application such as Nginx.

```
# sample_nginx.conf
http {
        ##
        # Gzip Settings
        ##

        gzip on;
        gzip_vary on;
        gzip_proxied any;
        gzip_comp_level 6;
        gzip_buffers 16 8k;
        gzip_http_version 1.1;
        gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;

        location ~ .*\.(js|css|ico|jpg|gif|png|svg|)$ {
            root    your/project/dir/public;
            expire  30d;
            access_log off;
        }
}
```