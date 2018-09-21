module Shared::Layout
  macro included
    include Lucky::HTMLPage
    include Shared::FieldErrors
    include Shared::FlashMessages
    include Shared::Field
  end

  abstract def content

  def shared_layout_head
    head do
      utf8_charset
      title "My App - #{page_title}"
      css_link asset("css/app.css")
      js_link asset("js/app.js")
      csrf_meta_tags
      responsive_meta_tag
    end
  end

  abstract def page_title

  # This is the default page title. If you remove method this the compiler will
  # help you remember to include a `page_title` method for every page because
  # we used `abstract def page_title` above. Alternatively, you can leave this
  # here and override the `page_title` class only on the pages you care about.
  def page_title
    "Welcome"
  end
end
