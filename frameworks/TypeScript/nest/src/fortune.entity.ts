import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';

@Entity('fortune')
export class Fortune {
  @PrimaryGeneratedColumn()
  id: number;

  @Column('text')
  message: string;
}
