import { Entity, PrimaryGeneratedColumn, Column } from 'typeorm';

@Entity('world')
export class WorldEntity {
    @PrimaryGeneratedColumn()
    id: number;

    @Column('int')
    randomnumber: number;
}