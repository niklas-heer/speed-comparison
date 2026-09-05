import { defineConfig } from "astro/config";
export default defineConfig({
  output: "static",
  trailingSlash: "always",
  devToolbar: { enabled: false },
});
