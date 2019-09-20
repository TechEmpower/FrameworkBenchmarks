import { Injectable } from '@nestjs/common';
import { Repository } from 'typeorm';
import { WorldEntity } from '../models/world.entity';
import { FortuneEntity } from '../models/fortune.entity';
import { InjectRepository } from '@nestjs/typeorm';

@Injectable()
export class BenchService {
    constructor(
        @InjectRepository(WorldEntity)
        private worldRepository: Repository<WorldEntity>,
        @InjectRepository(FortuneEntity)
        private fortuneRepository: Repository<FortuneEntity>,
    ){}

    getOne(){

        return this.worldRepository.findOne({ where: {id: Math.floor(Math.random() * 10000) + 1}});
    }

    async getMultiple(totalQueries: string) {

        return new Promise(async (resolve, reject) => {
            const worldArr = [];
            const total = this.sanitizeQueries(totalQueries);
            for (let i = 0; i < total; i++) {
                worldArr.push(await this.getOne());
            }
            resolve(worldArr);
        });
    }

    async updateMultiple(totalQueries: string) {

        return new Promise(async (resolve, reject) => {
            const worldArr = [];
            const total = this.sanitizeQueries(totalQueries);
            for (let i = 0; i < total; i++) {
                let worldToUpdate = await this.getOne();
                worldToUpdate.randomnumber = Math.floor(Math.random() * 10000) + 1;
                worldToUpdate = await this.worldRepository.save(worldToUpdate);
                worldArr.push(worldToUpdate);
            }
            resolve(worldArr);
        });
    }

    async getFortunes(){
        return this.fortuneRepository.find().then((fortunes) => {
            const newFortune = { id: 0, message: "Additional fortune added at request time." };
            fortunes.push(newFortune);
            fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);
            return fortunes;
        });
    }

    sanitizeQueries(queries: string) {
        const isNum = parseInt(queries, null);
        if (isNum) {
            if (isNum > 500) {
                return 500;
            } else if (isNum < 1) {
                return 1;
            }
        } else {
            return 1;
        }
        return isNum;
    }
}
