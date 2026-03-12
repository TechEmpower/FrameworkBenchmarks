import { Column, Entity, ObjectIdColumn } from 'typeorm';
import { ObjectId } from 'mongodb';

@Entity('fortune')
export class Fortune {
  @ObjectIdColumn()
  _id?: ObjectId;

  @Column()
  id: number;

  @Column('text')
  message: string;
}
