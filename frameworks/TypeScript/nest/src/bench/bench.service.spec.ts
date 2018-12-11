import { Test, TestingModule } from '@nestjs/testing';
import { BenchService } from './bench.service';

describe('BenchService', () => {
  let service: BenchService;
  beforeAll(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [BenchService],
    }).compile();
    service = module.get<BenchService>(BenchService);
  });
  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
