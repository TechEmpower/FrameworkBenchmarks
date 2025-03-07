import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  output: "standalone",

  async headers() {
    return [
      {
        source: "/(.*?)",
        headers: [
          { key: "Server", value: "Next.js" },
        ],
      },
    ]
  },
};

export default nextConfig;
