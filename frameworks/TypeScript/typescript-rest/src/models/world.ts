import { Model } from "objection";

<<<<<<< HEAD
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
=======
export default class World extends Model {
  static tableName: string = "world";

  readonly id: number;
  randomnumber: number;
>>>>>>> Finishes framework implementation, minus fortunes
}
