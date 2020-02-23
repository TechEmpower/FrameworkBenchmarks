let config = {
  synchronize: false,
  logging: false,
  entities:
    process.env.NODE_ENV === 'production'
      ? ['./dist/**/*.entity.js']
      : ['./dist/**/*.entity.js', './src/**/*.entity.ts'],
};

switch (process.env.DATABASE_CONFIGURATION_PROFILE) {
  case 'mysql':
    config = {
      ...config,
      type: 'mysql',
      host: 'tfb-database',
      port: 3306,
      username: 'benchmarkdbuser',
      password: 'benchmarkdbpass',
      database: 'hello_world',
    };
    break;
  case 'postgres':
  default:
    config = {
      ...config,
      type: 'postgres',
      host: 'tfb-database',
      port: 5432,
      username: 'benchmarkdbuser',
      password: 'benchmarkdbpass',
      database: 'hello_world',
    };
    break;
}

module.exports = config;
