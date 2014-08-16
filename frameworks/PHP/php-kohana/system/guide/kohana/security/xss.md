# Cross-Site Scripting (XSS) Security

*This page is not comprehensive and should not be considered a complete guide to XSS prevention.*

The first step to preventing [XSS](http://wikipedia.org/wiki/Cross-Site_Scripting) attacks is knowing when you need to protect yourself. XSS can only be triggered when it is displayed within HTML content, sometimes via a form input or being displayed from database results. Any global variable that contains client information can be tainted. This includes `$_GET`, `$_POST`, and `$_COOKIE` data.

## Prevention

There are a few simple rules to follow to guard your application HTML against XSS. If you do not want HTML in a variable, use [strip_tags](http://php.net/strip_tags) to remove all unwanted HTML tags from a value.

[!!] If you allow users to submit HTML to your application, it is highly recommended to use an HTML cleaning tool such as [HTML Purifier](http://htmlpurifier.org/) or [HTML Tidy](http://php.net/tidy).

The second is to always escape data when inserting into HTML. The [HTML] class provides generators for many common tags, including script and stylesheet links, anchors, images, and email (mailto) links. Any untrusted content should be escaped using [HTML::chars].

## References

* [OWASP XSS Cheat Sheet](http://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet)