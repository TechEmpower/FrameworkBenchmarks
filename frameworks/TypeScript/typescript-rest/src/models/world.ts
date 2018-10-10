import { Model } from "objection";

<<<<<<< HEAD
export default class World extends Model {
  static tableName: string = "world";

  readonly id: number;
  randomnumber: number;
=======
export default class extends Model {
  static tableName: string = "world";

  readonly id!: number;
  randomnumber!: number;
>>>>>>> Wraps up initial server configuration
}
