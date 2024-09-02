import type { UpsertResult } from 'mariadb';
import type postgres from 'postgres';

export interface World {
  id: number;
  randomnumber: number;
}

export interface Fortune {
  id: number;
  message: string;
}

export class ModelService {
  fortunes: () => Promise<Fortune[]>;
  find: (id: string) => Promise<World>;
  getAllWorlds: () => Promise<World[]>;
  bulkUpdate: (worlds: World[]) => Promise<(UpsertResult | UpsertResult[]) | postgres.RowList<postgres.Row[]>>;
}
