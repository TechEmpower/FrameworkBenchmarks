/*******************************************************************************
 * Copyright (c) 2008 - 2013 Oracle Corporation. All rights reserved.
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
 *     Linda DeMichiel - Java Persistence 2.0
 *
 ******************************************************************************/
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Specifies the primary key of an entity.
 * The field or property to which the <code>Id</code> annotation is applied 
 * should be one of the following types: any Java primitive type; 
 * any primitive wrapper type; 
 * <code>String</code>; 
 * <code>java.util.Date</code>; 
 * <code>java.sql.Date</code>; 
 * <code>java.math.BigDecimal</code>;
 * <code>java.math.BigInteger</code>.
 *
 * <p>The mapped column for the primary key of the entity is assumed 
 * to be the primary key of the primary table. If no <code>Column</code> annotation 
 * is specified, the primary key column name is assumed to be the name 
 * of the primary key property or field.
 *
 * <pre>
 *   Example:
 *
 *   &#064;Id
 *   public Long getId() { return id; }
 * </pre>
 *
 * @see Column
 * @see GeneratedValue
 *
 * @since Java Persistence 1.0
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)

public @interface Id {}
