var gh = gh || {};

/**
 * Creates the given namespace if it does not already exist.  For example: 
 *   gh.provide('foo.bar.baz');
 *   
 * is equivalent to:
 *   foo = foo || {};
 *   foo.bar = foo.bar || {};
 *   foo.bar.baz = foo.bar.baz || {};
 * 
 * So the following code would execute safely:
 *   gh.provide('foo.bar.baz');
 *   foo.bar.baz.count = 0;
 * 
 * @param namespace The namespace to be created.
 */
gh.provide = function(namespace) {
  namespace = namespace.split('.');
  var object = window;
  for (var i = 0; i < namespace.length; i++) {
    object[namespace[i]] = object[namespace[i]] || {};
    object = object[namespace[i]];
  }
}
