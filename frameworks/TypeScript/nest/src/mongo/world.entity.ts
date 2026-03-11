import { Column, Entity, ObjectIdColumn } from 'typeorm';
import { ObjectId } from 'mongodb';

@Entity('world')
export class World {
  @ObjectIdColumn()
  _id?: ObjectId;

  @Column()
  id: number;

  @Column()
  randomNumber: number;
}
