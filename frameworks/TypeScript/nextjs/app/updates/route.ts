import { db, findWorld, upsertWorlds, World } from "@/lib/db"
import { NextRequest } from "next/server"

export async function GET(request: NextRequest) {
  const queriesParam = request.nextUrl.searchParams.get("queries")
  const queriesCount = Math.min(Math.max(Number(queriesParam) || 1, 1), 500)

  const ids = new Set<number>()
  while (ids.size < queriesCount) {
    ids.add(1 + Math.floor(Math.random() * 10000))
  }

  const promises = new Array<Promise<World | undefined>>()
  for (const id of ids) {
    promises.push(findWorld(id))
  }

  const results = await Promise.all(promises) as World[]
  for (const result of results) {
    result.randomNumber = 1 + Math.floor(Math.random() * 10000)
  }

  await upsertWorlds(results)

  return Response.json(results)
}
