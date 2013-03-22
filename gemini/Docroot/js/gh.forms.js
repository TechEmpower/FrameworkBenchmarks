gh.provide('gh.forms');

/**
 * Focuses the first element in the given form.  If the form has been 
 * submitted and has invalid elements, the first invalid element is 
 * focused.  Invisible elements and submit buttons will not be focused.
 * 
 * @param formSelector A CSS selector for the form, e.g. '#LoginForm'.
 */
gh.forms.autofocus = function(formSelector) {
  
  var form = $(formSelector);
  var invalidElements = form.find('.invalid:visible:not([type="hidden"]):not([type="submit"])');
  
  if (invalidElements.length == 0) {
    var allElements = form.find('input:visible:not([type="hidden"]):not([type="submit"]), textarea:visible, select:visible');
    if (allElements.length > 0) {
      $(allElements[0]).focus();
    }
  } else {
    $(invalidElements[0]).focus();
  }
  
};

/**
 * Prevents the given form from being submitted more than once.  
 * 
 * @param formSelector A CSS selector for the form, e.g. '#LoginForm'.
 */
gh.forms.disableOnSubmit = function(formSelector) {

  var form = $(formSelector);
  
  form.submit(function(e) {
    if (form.data('submitted')) {
      e.preventDefault();
    } else {
      form.data('submitted', true);
    }
  });
  
};
