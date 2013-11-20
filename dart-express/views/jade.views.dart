library jade_views;
import 'package:jaded/runtime.dart';
import 'package:jaded/runtime.dart' as jade;
Map<String,Function> JADE_TEMPLATES = {
'./fortunes-table.jade': ([Map locals]){///jade-begin
  if (locals == null) locals = {};
  var fortunes = locals['fortunes'];
var fortune = locals['fortune'];

var buf = [];
var self = locals; 
if (self == null) self = {};
buf.add("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table>");
// iterate fortunes
;((){
  var $$obj = fortunes;
  if ($$obj is Iterable) {

    for (var $index = 0, $$l = $$obj.length; $index < $$l; $index++) {
      var fortune = $$obj[$index];

buf.add("<tr><td>" + (jade.escape(null == (jade.interp = fortune.id) ? "" : jade.interp)) + "</td><td>" + (jade.escape(null == (jade.interp = fortune.message) ? "" : jade.interp)) + "</td></tr>");
    }

  } else {
    var $$l = 0;
    for (var $index in $$obj.keys) {
      $$l++;      var fortune = $$obj[$index];

buf.add("<tr><td>" + (jade.escape(null == (jade.interp = fortune.id) ? "" : jade.interp)) + "</td><td>" + (jade.escape(null == (jade.interp = fortune.message) ? "" : jade.interp)) + "</td></tr>");
    }

  }
})();

buf.add("</table></body></html>");;
return buf.join("");

},///jade-end
};
