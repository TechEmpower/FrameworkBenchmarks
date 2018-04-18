/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package javax.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @since Common Annotations 1.0
 */
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Resource {
    public enum AuthenticationType {
        CONTAINER,
        APPLICATION
    }
    public String name() default "";

    public Class<?> type() default Object.class;
    public AuthenticationType authenticationType() default AuthenticationType.CONTAINER;
    public boolean shareable() default true;
    public String description() default "";
    public String mappedName() default "";

    public String lookup() default "";
}
