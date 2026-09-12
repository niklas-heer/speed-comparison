// One catalog keeps article pages, the journal and related reading in sync.
const modules = import.meta.glob("../content/journal/*.{md,mdx}", {
  eager: true,
});
export const posts = Object.entries(modules)
  .map(([path, post]: [string, any]) => ({
    ...post,
    slug: path
      .split("/")!
      .pop()!
      .replace(/\.mdx?$/, ""),
  }))
  .sort((a, b) => b.frontmatter.date.localeCompare(a.frontmatter.date));
