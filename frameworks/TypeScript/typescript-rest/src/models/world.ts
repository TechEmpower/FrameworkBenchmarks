import { Model } from "objection";

export default class World extends Model {
  static tableName: string = "world";

  readonly id: number;
  randomnumber: number;
}
