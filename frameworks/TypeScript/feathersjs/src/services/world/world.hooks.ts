import { HookContext } from '@feathersjs/feathers';
import { hooks } from 'feathers-sequelize';

const { hydrate } = hooks; 

function rawFalse(context: HookContext) {
  if (!context.params.sequelize) context.params.sequelize = {};
  Object.assign(context.params.sequelize, { raw: false });
  return context;
}

export default {
  before: {
    all: [rawFalse],
    find: [],
    get: [],
    create: [],
    update: [],
    patch: [],
    remove: []
  },

  after: {
    all: [hydrate()],
    find: [],
    get: [],
    create: [],
    update: [],
    patch: [],
    remove: []
  },

  error: {
    all: [],
    find: [],
    get: [],
    create: [],
    update: [],
    patch: [],
    remove: []
  }
};
