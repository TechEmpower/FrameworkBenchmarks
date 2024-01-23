import { Column, Entity, ObjectIdColumn } from 'typeorm';

@Entity('world')
export class World {
  @ObjectIdColumn()
  id: number;

  @Column()
  randomNumber: number;
}
