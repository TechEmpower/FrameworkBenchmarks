import { injectable } from '@ditsmod/core';

import { LRUCache } from 'lru-cache';
import { World } from './types.js';

@injectable()
export class CacheService {
  cache = new LRUCache<number, World>({ max: 10000 });
}
