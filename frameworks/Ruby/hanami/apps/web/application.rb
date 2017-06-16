require 'hanami/helpers'
require 'hanami/assets'
require_relative 'config/controller'

module Web
  class Application < Hanami::Application
    configure do
      ##
      # BASIC
      #

      # Define the root path of this application.
      # All paths specified in this configuration are relative to path below.
      #
      root __dir__

      # Relative load paths where this application will recursively load the
      # code.
      #
      # When you add new directories, remember to add them here.
      #
      load_paths << [
        'controllers',
        'views'
      ]

      # Handle exceptions with HTTP statuses (true) or don't catch them (false).
      # Defaults to true.
      # See: http://www.rubydoc.info/gems/hanami-controller/#Exceptions_management
      #
      # handle_exceptions true

      ##
      # HTTP
      #

      # Routes definitions for this application
      # See: http://www.rubydoc.info/gems/hanami-router#Usage
      #
      routes 'config/routes'

      # URI scheme used by the routing system to generate absolute URLs
      # Defaults to "http"
      #
      # scheme 'https'

      # URI host used by the routing system to generate absolute URLs
      # Defaults to "localhost"
      #
      # host 'example.org'

      # URI port used by the routing system to generate absolute URLs
      # Argument: An object coercible to integer, defaults to 80 if the scheme
      # is http and 443 if it's https
      #
      # This should only be configured if app listens to non-standard ports
      #
      # port 443

      # Enable cookies
      # Argument: boolean to toggle the feature
      #           A Hash with options
      #
      # Options:
      #   :domain   - The domain (String - nil by default, not required)
      #   :path     - Restrict cookies to a relative URI
      #               (String - nil by default)
      #   :max_age  - Cookies expiration expressed in seconds
      #               (Integer - nil by default)
      #   :secure   - Restrict cookies to secure connections
      #               (Boolean - Automatically true when using HTTPS)
      #               See #scheme and #ssl?
      #   :httponly - Prevent JavaScript access (Boolean - true by default)
      #
      # cookies true
      # or
      # cookies max_age: 300

      # Enable sessions
      # Argument: Symbol the Rack session adapter
      #           A Hash with options
      #
      # See: http://www.rubydoc.info/gems/rack/Rack/Session/Cookie
      #
      # sessions :cookie, secret: ENV['WEB_SESSIONS_SECRET']

      # Configure Rack middleware for this application
      #
      # middleware.use Rack::Protection

      # Default format for the requests that don't specify an HTTP_ACCEPT header
      # Argument: A symbol representation of a mime type, defaults to :html
      #
      # default_request_format :html

      # Default format for responses that don't consider the request format
      # Argument: A symbol representation of a mime type, defaults to :html
      #
      # default_response_format :json

      # HTTP Body parsers
      # Parse non GET responses body for a specific mime type
      # Argument: Symbol, which represent the format of the mime type
      #             (only `:json` is supported)
      #           Object, the parser
      #
      # body_parsers :json

      # When it's true and the router receives a non-encrypted request (http),
      # it redirects to the secure equivalent (https). Disabled by default.
      #
      # force_ssl true

      ##
      # TEMPLATES
      #

      # The layout to be used by all views
      #
      layout :application # It will load Web::Views::ApplicationLayout

      # The relative path to templates
      #
      templates 'templates'

      ##
      # ASSETS
      #
      assets do
        # JavaScript compressor
        #
        # Supported engines:
        #
        #   * :builtin
        #   * :uglifier
        #   * :yui
        #   * :closure
        #
        # See: http://hanamirb.org/guides/assets/compressors
        #
        # In order to skip JavaScript compression comment the following line
        # javascript_compressor :builtin

        # Stylesheet compressor
        #
        # Supported engines:
        #
        #   * :builtin
        #   * :yui
        #   * :sass
        #
        # See: http://hanamirb.org/guides/assets/compressors
        #
        # In order to skip stylesheet compression comment the following line
        # stylesheet_compressor :builtin

        # Specify sources for assets
        #
        sources << [
          'assets'
        ]
      end

      ##
      # SECURITY
      #

      # X-Frame-Options is a HTTP header supported by modern browsers.
      # It determines if a web page can or cannot be included via <frame> and
      # <iframe> tags by untrusted domains.
      #
      # Web applications can send this header to prevent Clickjacking attacks.
      #
      # Read more at:
      #
      #   * https://developer.mozilla.org/en-US/docs/Web/HTTP/X-Frame-Options
      #   * https://www.owasp.org/index.php/Clickjacking
      #
      security.x_frame_options 'DENY'

      # X-Content-Type-Options prevents browsers from interpreting files as
      # something else than declared by the content type in the HTTP headers.
      #
      # Read more at:
      #
      #   * https://www.owasp.org/index.php/OWASP_Secure_Headers_Project#X-Content-Type-Options
      #   * https://msdn.microsoft.com/en-us/library/gg622941%28v=vs.85%29.aspx
      #   * https://blogs.msdn.microsoft.com/ie/2008/09/02/ie8-security-part-vi-beta-2-update
      #
      security.x_content_type_options 'nosniff'

      # X-XSS-Protection is a HTTP header to determine the behavior of the
      # browser in case an XSS attack is detected.
      #
      # Read more at:
      #
      #   * https://www.owasp.org/index.php/Cross-site_Scripting_(XSS)
      #   * https://www.owasp.org/index.php/OWASP_Secure_Headers_Project#X-XSS-Protection
      #
      security.x_xss_protection '1; mode=block'

      # Content-Security-Policy (CSP) is a HTTP header supported by modern
      # browsers. It determines trusted sources of execution for dynamic
      # contents (JavaScript) or other web related assets: stylesheets, images,
      # fonts, plugins, etc.
      #
      # Web applications can send this header to mitigate Cross Site Scripting
      # (XSS) attacks.
      #
      # The default value allows images, scripts, AJAX, fonts and CSS from the
      # same origin, and does not allow any other resources to load (eg object,
      # frame, media, etc).
      #
      # Inline JavaScript is NOT allowed. To enable it, please use:
      # "script-src 'unsafe-inline'".
      #
      # Content Security Policy introduction:
      #
      #  * http://www.html5rocks.com/en/tutorials/security/content-security-policy/
      #  * https://www.owasp.org/index.php/Content_Security_Policy
      #  * https://www.owasp.org/index.php/Cross-site_Scripting_%28XSS%29
      #
      # Inline and eval JavaScript risks:
      #
      #   * http://www.html5rocks.com/en/tutorials/security/content-security-policy/#inline-code-considered-harmful
      #   * http://www.html5rocks.com/en/tutorials/security/content-security-policy/#eval-too
      #
      # Content Security Policy usage:
      #
      #  * http://content-security-policy.com/
      #  * https://developer.mozilla.org/en-US/docs/Web/Security/CSP/Using_Content_Security_Policy
      #
      # Content Security Policy references:
      #
      #  * https://developer.mozilla.org/en-US/docs/Web/Security/CSP/CSP_policy_directives
      #
      security.content_security_policy %{
        form-action 'self';
        frame-ancestors 'self';
        base-uri 'self';
        default-src 'none';
        script-src 'self';
        connect-src 'self';
        img-src 'self' https: data:;
        style-src 'self' 'unsafe-inline' https:;
        font-src 'self';
        object-src 'none';
        plugin-types application/pdf;
        child-src 'self';
        frame-src 'self';
        media-src 'self'
      }

      ##
      # FRAMEWORKS
      #

      # Configure the code that will yield each time Web::Action is included
      # This is useful for sharing common functionality
      #
      # See: http://www.rubydoc.info/gems/hanami-controller#Configuration
      controller.prepare do
        # include MyAuthentication # included in all the actions
        # before :authenticate!    # run an authentication before callback
      end

      # Configure the code that will yield each time Web::View is included
      # This is useful for sharing common functionality
      #
      # See: http://www.rubydoc.info/gems/hanami-view#Configuration
      view.prepare do
        include Hanami::Helpers
        include Web::Assets::Helpers
      end
    end

    ##
    # DEVELOPMENT
    #
    configure :development do
      # Don't handle exceptions, render the stack trace
      handle_exceptions false
    end

    ##
    # TEST
    #
    configure :test do
      # Don't handle exceptions, render the stack trace
      handle_exceptions false
    end

    ##
    # PRODUCTION
    #
    configure :production do
      # scheme 'https'
      # host   'example.org'
      # port   443

      assets do
        # Don't compile static assets in production mode (eg. Sass, ES6)
        #
        # See: http://www.rubydoc.info/gems/hanami-assets#Configuration
        compile false

        # Use fingerprint file name for asset paths
        #
        # See: http://hanamirb.org/guides/assets/overview
        fingerprint false

        # Content Delivery Network (CDN)
        #
        # See: http://hanamirb.org/guides/assets/content-delivery-network
        #
        # scheme 'https'
        # host   'cdn.example.org'
        # port   443

        # Subresource Integrity
        #
        # See: http://hanamirb.org/guides/assets/content-delivery-network/#subresource-integrity
        subresource_integrity :sha256
      end
    end
  end
end
