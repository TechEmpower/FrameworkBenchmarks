import { Column, DataType, Model, Primary } from "./deps.ts";

@Model()
export class World {
  @Primary()
  id!: number;

  @Column({ type: DataType.Number })
  randomnumber!: number;
}

@Model()
export class Fortune {
  @Primary()
  id!: number;

  @Column({ type: DataType.String })
  message!: string;
}
