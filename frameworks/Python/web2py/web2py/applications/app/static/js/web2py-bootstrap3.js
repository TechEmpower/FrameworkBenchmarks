(function($, undefined) {
  $.web2py.ajax_fields = function(target) {
    /*
     *this attaches something to a newly loaded fragment/page
     * Ideally all events should be bound to the document, so we can avoid calling
     * this over and over... all will be bound to the document
     */
    /*adds btn class to buttons*/
    $('button', target).addClass('btn btn-default');
    $("p.w2p-autocomplete-widget input").addClass('form-control');
    $('form input[type="submit"], form input[type="button"]', target).addClass('btn btn-default');
    /* javascript for PasswordWidget*/
    $('input[type=password][data-w2p_entropy]', target).each(function() {
      web2py.validate_entropy($(this));
    });
    /* javascript for ListWidget*/
    $('ul.w2p_list', target).each(function() {
      function pe(ul, e) {
        var new_line = ml(ul);
        rel(ul);
        if ($(e.target).parent().is(':visible')) {
          /* make sure we didn't delete the element before we insert after */
          new_line.insertAfter($(e.target).parent());
        } else {
          /* the line we clicked on was deleted, just add to end of list */
          new_line.appendTo(ul);
        }
        new_line.find(":text").focus();
        return false;
      }

      function rl(ul, e) {
        if ($(ul).children().length > 1) {
          /* only remove if we have more than 1 item so the list is never empty */
          $(e.target).parent().remove();
        }
      }

      function ml(ul) {
        /* clone the first field */
        var line = $(ul).find("li:first").clone(true);
        line.find(':text').val('');
        return line;
      }

      function rel(ul) {
        /* keep only as many as needed*/
        $(ul).find("li").each(function() {
          var trimmed = $.trim($(this.firstChild).val());
          if (trimmed == '') $(this).remove();
          else $(this.firstChild).val(trimmed);
        });
      }
      var ul = this;
      $(ul).find(":text").after('<a class="btn btn-default" href="#">+</a>&nbsp;<a class="btn btn-default" href="#">-</a>').keypress(function(e) {
        return (e.which == 13) ? pe(ul, e) : true;
      }).next().click(function(e) {
        pe(ul, e);
        e.preventDefault();
      }).next().click(function(e) {
        rl(ul, e);
        e.preventDefault();
      });
    });
  }

  $(function() {
    $(".nav ul.dropdown-menu").each(function() {
      var toggle = jQuery(this).parent();
      if (toggle.parent().hasClass("nav")) {
        toggle.attr("data-w2pmenulevel", "l0");
        toggle.children("a")
          .addClass("dropdown-toggle")
          .append('<span class="caret"> </span>')
          .attr("data-toggle", "dropdown");
      } else {
        toggle.addClass("dropdown-submenu").removeClass("dropdown");
      };
    });
  });

})(jQuery);