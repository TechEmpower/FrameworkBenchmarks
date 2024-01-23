import { Column, Entity, ObjectIdColumn } from 'typeorm';

@Entity('fortune')
export class Fortune {
  @ObjectIdColumn()
  id: number;

  @Column('text')
  message: string;
}
