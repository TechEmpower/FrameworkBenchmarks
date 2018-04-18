/** *****************************************************************************
 * Copyright (c) 2011 - 2013 Oracle Corporation. All rights reserved.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0
 * which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     Linda DeMichiel - Java Persistence 2.1
 *
 ***************************************************************************** */
package javax.persistence;

import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Used in schema generation to specify creation of an index.
 * <p>
 * Note that it is not necessary to specify an index for a primary key,
 * as the primary key index will be created automatically.
 *
 * <p>
 * The syntax of the <code>columnList</code> element is a
 * <code>column_list</code>, as follows:
 *
 * <pre>
 *    column::= index_column [,index_column]*
 *    index_column::= column_name [ASC | DESC]
 * </pre>
 *
 * <p>
 * If <code>ASC</code> or <code>DESC</code> is not specified,
 * <code>ASC</code> (ascending order) is assumed.
 *
 * @since Java Persistence 2.1
 *
 */
@Target({})
@Retention(RUNTIME)
public @interface Index {

    /**
     * (Optional) The name of the index; defaults to a provider-generated name.
     *
     * @return String
     */
    String name() default "";

    /**
     * (Required) The names of the columns to be included in the index,
     * in order.
     *
     * @return String
     */
    String columnList();

    /**
     * (Optional) Whether the index is unique.
     *
     * @return boolean
     */
    boolean unique() default false;

}
