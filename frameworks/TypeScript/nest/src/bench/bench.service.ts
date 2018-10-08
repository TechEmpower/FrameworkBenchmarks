import { Injectable } from '@nestjs/common';
import { Repository } from 'typeorm';
import { WorldEntity } from 'models/world.entity';
import { InjectRepository } from '@nestjs/typeorm';

@Injectable()
export class BenchService {
    constructor(
        @InjectRepository(WorldEntity)
        private worldRepository: Repository<WorldEntity>){}

    async getOne(){

        return await this.worldRepository.findOne({ where: {id: Math.floor(Math.random() * 10000) + 1}});
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
