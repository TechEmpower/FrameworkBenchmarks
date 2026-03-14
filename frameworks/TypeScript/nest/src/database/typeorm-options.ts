import { TypeOrmModuleOptions } from '@nestjs/typeorm';
import { Fortune as MongoFortune } from '../mongo/fortune.entity';
import { World as MongoWorld } from '../mongo/world.entity';
import { Fortune as SqlFortune } from '../sql/fortune.entity';
import { World as SqlWorld } from '../sql/world.entity';

const baseOptions = {
  synchronize: false,
  logging: false,
  host: 'tfb-database',
  database: 'hello_world',
} as const;

export function createTypeOrmOptions(): TypeOrmModuleOptions {
  switch (process.env.DATABASE_CONFIGURATION_PROFILE) {
    case 'mongodb':
      return {
        ...baseOptions,
        type: 'mongodb',
        port: 27017,
        entities: [MongoWorld, MongoFortune],
      };
    case 'mysql':
      return {
        ...baseOptions,
        type: 'mysql',
        port: 3306,
        username: 'benchmarkdbuser',
        password: 'benchmarkdbpass',
        entities: [SqlWorld, SqlFortune],
        extra: {
          connectionLimit: 30,
        },
      };
    case 'postgres':
    default:
      return {
        ...baseOptions,
        type: 'postgres',
        port: 5432,
        username: 'benchmarkdbuser',
        password: 'benchmarkdbpass',
        entities: [SqlWorld, SqlFortune],
        extra: {
          max: 30,
        },
      };
  }
}
