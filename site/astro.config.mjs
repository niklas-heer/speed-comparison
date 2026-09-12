import { defineConfig } from "astro/config";
import mdx from "@astrojs/mdx";
export default defineConfig({
  integrations: [mdx()],
  output: "static",
  trailingSlash: "always",
  devToolbar: { enabled: false },
});
