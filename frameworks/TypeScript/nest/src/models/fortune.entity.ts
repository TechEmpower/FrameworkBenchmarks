import { Entity, PrimaryGeneratedColumn, Column } from 'typeorm';

@Entity('fortune')
export class FortuneEntity {
    @PrimaryGeneratedColumn()
    id: number;

    @Column('text')
    message: string;
}