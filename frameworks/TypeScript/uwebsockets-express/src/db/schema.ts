import { InferModel } from 'drizzle-orm';
import { integer, pgTable, serial, varchar } from 'drizzle-orm/pg-core';

export const worlds = pgTable('world', {
  id: serial('id').primaryKey(),
  randomnumber: integer('randomnumber'),
});

export type World = InferModel<typeof worlds, 'select'>;

export const fortunes = pgTable('fortune', {
  id: serial('id').primaryKey(),
  message: varchar('message'),
});

export type Fortune = InferModel<typeof fortunes, 'select'>;
