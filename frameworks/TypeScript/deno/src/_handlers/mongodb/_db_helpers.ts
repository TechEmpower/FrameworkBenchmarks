import { MongoClient, Bson } from "../../depends.ts";

const mongoUrl = "mongodb://tfb-database:27017";
const dbName = "hello_world";

export const client = new MongoClient();
await client.connect(mongoUrl);
export let World = client.database(dbName).collection('world');
export let Fortune = client.database(dbName).collection('fortune');

export const randomNumber = (): number => Math.floor(Math.random() * 10000) + 1;

export const fillArray = async<T=any>(v: T, l: number) => {
  let o = [];
  for (let i = 0; i < l; i += 1) o.push(v);
  return o;
}

export interface FortuneData {
  id: number,
  message: string
}
const _fortunes_head = [
  "<!DOCTYPE html>",
  "<html>",
  "<head><title>Fortunes</title></head>",
  "<body>",
  "<table>",
  "<tr>",
  "<th>id</th>",
  "<th>message</th>",
  "</tr>",
].join("");
const _fortunes_end = ["</table>", "</body>", "</html>"].join("");
const _fortunes_com = ["<tr>","</tr>","<td>","</td>"];
export const generateFortunes = (input: FortuneData) => {
  
}