import { NextRequest, NextResponse } from "next/server"

export function middleware(request: NextRequest) {
  const response = NextResponse.next()
  response.headers.set("Server", "Next.js")
  return response
}
