package com.javanut.gl.benchmark;

import java.util.List;

import com.javanut.json.encode.JSONRenderer;
import com.javanut.pronghorn.util.Appendables;
import com.javanut.pronghorn.util.template.StringTemplateBuilder;
import com.javanut.pronghorn.util.template.StringTemplateRenderer;

public class Templates {
	
	static final byte[] ROW_FINISH = "</td></tr>\n".getBytes();
	static final byte[] ROW_MIDDLE = "</td><td>".getBytes();
	static final byte[] ROW_START = "<tr><td>".getBytes();

	static final transient StringTemplateRenderer<FortunesObject> fortuneTemplate =		
	new StringTemplateBuilder<FortunesObject>()
		   .add("<!DOCTYPE html> <html> <head><title>Fortunes</title></head> <body> <table> <tr><th>id</th><th>message</th></tr>\n")
	       .add((t,s,i)-> {
				if (i<s.list().size()) {													
					t.write(Templates.ROW_START);
					Appendables.appendValue(t, s.list().get(i).getId());
					t.write(Templates.ROW_MIDDLE);							
					Appendables.appendHTMLEntityEscaped(t, s.list().get(i).getFortune());							
					t.write(Templates.ROW_FINISH);
					return true;
				} else {
					return false;
				}
	         })		
	       .add("</table></body></html>")
	       .finish();
	
	////////////////////////////////////
	////////////////////////////////////

	static final JSONRenderer<List<ResultObject>> multiTemplate = new JSONRenderer<List<ResultObject>>()
	  .array((o,i) -> i<o.size()?o:null)
	      .startObject((o, i) ->  o.get(i))  
			.integer("id", o -> o.getId() )
			.integer("randomNumber", o -> o.getResult())
	      .endObject();
	
	////////////////////////////////////
	////////////////////////////////////
	
	static final JSONRenderer<List<ResultObject>> multiTemplateDBRest = new JSONRenderer<List<ResultObject>>()
	    	  .array((o,i) -> i<o.size()?o:null)
		          .startObject((o, i) -> o.get(i))
					.integer("id", o -> o.getId() )
					.integer("randomNumber", o -> o.getResult())
		          .endObject();
	////////////////////////////////////
	////////////////////////////////////
	
	static final JSONRenderer<ResultObject> singleTemplateDBRest = new JSONRenderer<ResultObject>()
		   	  .startObject()
				.integer("id", o -> o.getId() )
				.integer("randomNumber", o -> o.getResult())
	          .endObject();

}
