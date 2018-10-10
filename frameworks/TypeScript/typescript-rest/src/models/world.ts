import { Model } from "objection";

export default class extends Model {
  static tableName: string = "world";

  readonly id!: number;
  randomnumber!: number;
}
