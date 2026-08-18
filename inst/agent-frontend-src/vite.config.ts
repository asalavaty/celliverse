import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// The agent is served by the Plumber API, which mounts the built assets at "/"
// and exposes the backend under "/api". We therefore use a relative base so the
// bundle works no matter what host/port the R server binds to.
//
// During `npm run dev`, Vite serves on :5173 and proxies /api to the R server on
// :8000 so you get hot-reload against a live backend.
export default defineConfig({
  plugins: [react()],
  base: "./",
  build: {
    // `npm run build:pkg` writes straight into inst/react-app (the shipped assets).
    outDir: "dist",
    emptyOutDir: true,
    chunkSizeWarningLimit: 1500,
  },
  server: {
    port: 5173,
    proxy: {
      "/api": {
        target: "http://127.0.0.1:8000",
        changeOrigin: true,
        // SSE needs buffering off; Vite's http-proxy handles EventSource fine.
        ws: false,
      },
    },
  },
});
