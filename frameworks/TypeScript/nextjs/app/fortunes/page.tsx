import { db } from "@/lib/db"

// Prevent database queries during build phase.
export const dynamic = "force-dynamic"

export default async function Page() {
  const fortunes = await db.selectFrom("Fortune").selectAll().execute()
  fortunes.push({ id: 0, message: "Additional fortune added at request time." })
  fortunes.sort((a, b) => a.message.localeCompare(b.message))

  return <>
    <title>Fortunes</title>

    <table>
      <thead>
        <tr>
          <th>id</th>
          <th>message</th>
        </tr>
      </thead>
      <tbody>
        {fortunes.map(fortune =>
          <tr key={fortune.id}>
            <td>{fortune.id}</td>
            <td>{fortune.message}</td>
          </tr>
        )}
      </tbody>
    </table>
  </>
}
