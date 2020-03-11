let config = {
  synchronize: false,
  logging: false,
  host: 'tfb-database',
  database: 'hello_world',
};

switch (process.env.DATABASE_CONFIGURATION_PROFILE) {
  case 'mongodb':
    config = {
      ...config,
      type: 'mongodb',
      port: 27017,
      entities: ['./dist/mongo/*.entity.js'],
      useUnifiedTopology: true,
    };
    break;
  case 'mysql':
    config = {
      ...config,
      type: 'mysql',
      port: 3306,
      username: 'benchmarkdbuser',
      password: 'benchmarkdbpass',
      entities: ['./dist/sql/*.entity.js'],
    };
    break;
  case 'postgres':
  default:
    config = {
      ...config,
      type: 'postgres',
      port: 5432,
      username: 'benchmarkdbuser',
      password: 'benchmarkdbpass',
      entities: ['./dist/sql/*.entity.js'],
    };
    break;
}

module.exports = config;
