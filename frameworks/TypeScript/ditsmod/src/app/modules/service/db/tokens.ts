import { Extension, InjectionToken } from '@ditsmod/core';

/**
 * A group of extensions intended for preparatory work for the database module.
 */
export const DB_INIT_EXTENSIONS = new InjectionToken<Extension<void>[]>('DB_INIT_EXTENSIONS');
