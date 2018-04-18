/** *****************************************************************************
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
 ***************************************************************************** */
package javax.persistence;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Specifies whether an entity should be cached if caching is enabled
 * when the value of the <code>persistence.xml</code> caching element
 * is <code>ENABLE_SELECTIVE</code> or <code>DISABLE_SELECTIVE</code>.
 * The value of the <code>Cacheable</code> annotation is inherited by
 * subclasses; it can be overridden by specifying
 * <code>Cacheable</code> on a subclass.
 *
 * <p>
 * <code>Cacheable(false)</code> means that the entity and its state must
 * not be cached by the provider.
 *
 * @since Java Persistence 2.0
 */
@Target({TYPE})
@Retention(RUNTIME)
public @interface Cacheable {

    /**
     * (Optional) Whether or not the entity should be cached.
     *
     * @return boolean
     */
    boolean value() default true;

    /**
     * (Optional) 定时自动更新缓存的周期秒数，为0表示不做定时更新， 大于0表示每经过interval秒后会自动从数据库中拉取数据更新Cache
     *
     * @return int
     */
    int interval() default 0;
}
