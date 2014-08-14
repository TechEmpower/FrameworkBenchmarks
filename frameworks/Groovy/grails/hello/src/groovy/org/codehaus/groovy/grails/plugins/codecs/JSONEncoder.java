/*
 * Copyright 2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.codehaus.groovy.grails.plugins.codecs;

import org.apache.commons.lang.StringUtils;
import org.codehaus.groovy.grails.support.encoding.CodecIdentifier;
import org.codehaus.groovy.grails.support.encoding.DefaultCodecIdentifier;

/**
 * Escapes characters in JSON output
 *
 * @author Lari Hotari
 * @since 2.3.4
 */
public class JSONEncoder extends AbstractCharReplacementEncoder {
    public static final CodecIdentifier JSON_CODEC_IDENTIFIER = new DefaultCodecIdentifier(
            "JSON", "Json") {
        public boolean isEquivalent(CodecIdentifier other) {
            return super.isEquivalent(other) || JavaScriptEncoder.JAVASCRIPT_CODEC_IDENTIFIER.getCodecName().equals(other.getCodecName());
        };
    };

    public JSONEncoder() {
        super(JSON_CODEC_IDENTIFIER);
    }

    /* (non-Javadoc)
     * @see org.codehaus.groovy.grails.plugins.codecs.AbstractCharReplacementEncoder#escapeCharacter(char, char)
     */
    @Override
    protected String escapeCharacter(char ch, char previousChar) {
        switch (ch) {
            case '"':
                return "\\\"";
            case '\\':
                return "\\\\";
            case '\t':
                return "\\t";
            case '\n':
                return "\\n";
            case '\r':
                return "\\r";
            case '\f':
                return "\\f";
            case '\b':
                return "\\b";
            case '\u000B': // vertical tab: http://bclary.com/2004/11/07/#a-7.8.4
                return "\\v";
            case '\u2028':
                return "\\u2028"; // Line separator
            case '\u2029':
                return "\\u2029"; // Paragraph separator
            case '/':
                // preserve special handling that exists in JSONObject.quote to improve security if JSON is embedded in HTML document
                // prevents outputting "</" gets outputted with unicode escaping for the slash
                if (previousChar == '<') {
                    return "\\u002f"; 
                }
                break;
        }
        if(ch < ' ') {
            // escape all other control characters
            return "\\u" + StringUtils.leftPad(Integer.toHexString(ch), 4, '0');
        }
        return null;
    }

    @Override
    public boolean isApplyToSafelyEncoded() {
        return true;
    }
}
