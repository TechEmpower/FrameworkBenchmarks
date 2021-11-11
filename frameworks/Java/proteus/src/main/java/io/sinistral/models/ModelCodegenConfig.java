/**
 * 
 */
package io.sinistral.models;

import java.util.List;
import java.util.Map;

import com.jsoniter.spi.CodegenConfig;
import com.jsoniter.spi.TypeLiteral;

/**
 * @author jbauer
 *
 */
public class ModelCodegenConfig implements CodegenConfig {

    @Override
    public void setup() {
       
    }

    @Override
    public TypeLiteral[] whatToCodegen() {
        return new TypeLiteral[]{
                // generic types, need to use this syntax
                new TypeLiteral<List<Integer>>() {
                },
                new TypeLiteral<Map<String, Object>>() {
                },
                // array
                TypeLiteral.create(World.class),
                // object
                TypeLiteral.create(Message.class)
        };
    }
}

 
