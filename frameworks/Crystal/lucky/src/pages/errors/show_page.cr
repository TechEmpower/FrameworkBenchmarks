class Errors::ShowPage
  include Lucky::HTMLPage

  needs status : Int32
  needs title : String

  def render
    html_doctype
    html lang: "en" do
      head do
        utf8_charset
        title "Something went wrong"
        load_lato_font
        normalize_styles
        error_page_styles
      end

      body do
        div class: "container" do
          h2 @status, class: "status-code"
          h1 @title, class: "title"

          ul class: "helpful-links" do
            li do
              link "Try heading back to home", to: "/", class: "helpful-link"
            end
          end
        end
      end
    end
  end

  def load_lato_font
    css_link "https://fonts.googleapis.com/css?family=Lato"
  end

  def normalize_styles
    style <<-CSS
      /*! normalize.css v7.0.0 | MIT License | github.com/necolas/normalize.css */html{line-height:1.15;-ms-text-size-adjust:100%;-webkit-text-size-adjust:100%}body{margin:0}article,aside,footer,header,nav,section{display:block}h1{font-size:2em;margin:.67em 0}figcaption,figure,main{display:block}figure{margin:1em 40px}hr{box-sizing:content-box;height:0;overflow:visible}pre{font-family:monospace,monospace;font-size:1em}a{background-color:transparent;-webkit-text-decoration-skip:objects}abbr[title]{border-bottom:none;text-decoration:underline;text-decoration:underline dotted}b,strong{font-weight:inherit}b,strong{font-weight:bolder}code,kbd,samp{font-family:monospace,monospace;font-size:1em}dfn{font-style:italic}mark{background-color:#ff0;color:#000}small{font-size:80%}sub,sup{font-size:75%;line-height:0;position:relative;vertical-align:baseline}sub{bottom:-.25em}sup{top:-.5em}audio,video{display:inline-block}audio:not([controls]){display:none;height:0}img{border-style:none}svg:not(:root){overflow:hidden}button,input,optgroup,select,textarea{font-family:sans-serif;font-size:100%;line-height:1.15;margin:0}button,input{overflow:visible}button,select{text-transform:none}[type=reset],[type=submit],button,html [type=button]{-webkit-appearance:button}[type=button]::-moz-focus-inner,[type=reset]::-moz-focus-inner,[type=submit]::-moz-focus-inner,button::-moz-focus-inner{border-style:none;padding:0}[type=button]:-moz-focusring,[type=reset]:-moz-focusring,[type=submit]:-moz-focusring,button:-moz-focusring{outline:1px dotted ButtonText}fieldset{padding:.35em .75em .625em}legend{box-sizing:border-box;color:inherit;display:table;max-width:100%;padding:0;white-space:normal}progress{display:inline-block;vertical-align:baseline}textarea{overflow:auto}[type=checkbox],[type=radio]{box-sizing:border-box;padding:0}[type=number]::-webkit-inner-spin-button,[type=number]::-webkit-outer-spin-button{height:auto}[type=search]{-webkit-appearance:textfield;outline-offset:-2px}[type=search]::-webkit-search-cancel-button,[type=search]::-webkit-search-decoration{-webkit-appearance:none}::-webkit-file-upload-button{-webkit-appearance:button;font:inherit}details,menu{display:block}summary{display:list-item}canvas{display:inline-block}template{display:none}[hidden]{display:none}/*# sourceMappingURL=normalize.min.css.map */
    CSS
  end

  def error_page_styles
    style <<-CSS
      body {
        background-color: #F4F7F6;
        color: #000;
        font-family: 'Lato', sans-serif;
        padding-top: 100px;
      }

      .helpful-links {
        list-style-type: none;
        margin: 0;
        padding: 0;
      }

      .helpful-link {
        color: #15A38B;
      }

      .status-code {
        opacity: 0.4;
        font-size: 26px;
        font-weight: normal;
      }

      .title {
        font-size: 34px;
        line-height: 56px;
        font-weight: normal;
      }

      .container {
        margin: 0 auto;
        max-width: 450px;
        padding: 55px;
      }

      @media only screen and (max-width: 500px) {
        .status-code {
          font-size: 18px;
        }

        .title {
          font-size: 26px;
          line-height: 40px;
          margin: 20px 0 35px 0;
        }
      }
    CSS
  end
end
