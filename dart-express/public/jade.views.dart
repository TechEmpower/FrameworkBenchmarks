library jade_public;
import 'package:jaded/runtime.dart';
import 'package:jaded/runtime.dart' as jade;
Map<String,Function> JADE_TEMPLATES = {
'./layout.jade': ([Map locals]){///jade-begin
  if (locals == null) locals = {};
  
var buf = [];
var self = locals; 
if (self == null) self = {};
buf.add("<!DOCTYPE html><html><head><title>Fortunes</title></head><body></body></html>");;
return buf.join("");

},///jade-end
};
