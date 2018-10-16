import { Test, TestingModule } from '@nestjs/testing';
import { BenchController } from './bench.controller';

describe('Bench Controller', () => {
  let module: TestingModule;
  beforeAll(async () => {
    module = await Test.createTestingModule({
      controllers: [BenchController],
    }).compile();
  });
  it('should be defined', () => {
    const controller: BenchController = module.get<BenchController>(BenchController);
    expect(controller).toBeDefined();
  });
});
