import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';

@Entity('world')
export class World {
  @PrimaryGeneratedColumn()
  id: number;

  @Column()
  randomnumber: number;
}
