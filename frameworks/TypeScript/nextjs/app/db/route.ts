import { findWorld } from "@/lib/db"

export async function GET() {
  const id = 1 + Math.floor(Math.random() * 10000)
  return Response.json(await findWorld(id))
}
