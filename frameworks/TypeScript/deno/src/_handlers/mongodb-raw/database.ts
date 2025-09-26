import { MongoClient } from "../../depends.ts";

const mongoUrl = "mongodb://tfb-database:27017";
const dbName = "hello_world";

export const client: MongoClient = new MongoClient();
await client.connect(mongoUrl);
export let World = client.database(dbName).collection("world");
export let Fortune = client.database(dbName).collection("fortune");

export const randomNumber = (): number => Math.floor(Math.random() * 10000) + 1;

export const fillArrayWithFn = async<T = any>(v: Function, l: number) => {
  let o = [];
  for (let i = 0; i < l; i += 1) o.push(await v());
  return o;
};

export interface FortuneData {
  id: number;
  message: string;
  _id?: unknown;
}
export const htmlEncodeByRegExp = (str: string) => {
  let s = "";
  if (str.length == 0) return "";
  s = str.replaceAll(/&/g, "&amp;");
  s = s.replaceAll(/</g, "&lt;");
  s = s.replaceAll(/>/g, "&gt;");
  // s = s.replaceAll(/ /g, "&nbsp;");
  s = s.replaceAll(/\'/g, "'");
  s = s.replaceAll(/\"/g, '"');
  return s;
};
export const _fortunes_head = [
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
export const _fortunes_end = ["</table>", "</body>", "</html>"].join("");
export const _fortunes_com = ["<tr>", "</tr>", "<td>", "</td>"];
export const generateFortunes = (input: FortuneData[]) => {
  let f = input
    .map((v) =>
      [
        _fortunes_com[0],
        _fortunes_com[2],
        v.id.toString(),
        _fortunes_com[3],
        _fortunes_com[2],
        htmlEncodeByRegExp(v.message),
        _fortunes_com[3],
        _fortunes_com[1],
      ].join("")
    )
    .join("");

  return _fortunes_head + f + _fortunes_end;
};

export const randomWorld = async () => {
  let world = (await World.findOne({
    id: randomNumber(),
  })) as FortuneData;
  world._id = undefined;
  return world;
};

export const getAllFortunes = async () => {
  return await Fortune.find().toArray();
};

export const updateQuery = async () => {
  const one = (await World.findOne({
    id: randomNumber(),
  })) as any;
  one.randomNumber = randomNumber();
  await World.updateOne(
    {
      id: one.id,
    },
    one
  );
  return {
    id: one.id,
    randomNumber: one.randomNumber,
  };
};

export const additionalFortune = {
  id: 0,
  message: "Additional fortune added at request time.",
};

export const resolveQueryNumber = (s: string) => {
  let r: number;
  if (/^\d+$/.test(s)) {
    r = Number(s);
    if (r > 500) r = 500;
    if (r < 1) r = 1;
  } else {
    r = 1;
  }
  return r;
};
