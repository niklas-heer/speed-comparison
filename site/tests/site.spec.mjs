import { test, expect } from "@playwright/test";
test("filter, sort, inspect provenance and download original evidence", async ({
  page,
}) => {
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  await page.goto("/");
  await expect(page.locator("h1")).toContainText("One calculation.");
  await expect(page.locator("#result-rows tr:visible")).toHaveCount(75);
  await page.getByRole("searchbox").fill("go@1.25.5");
  await expect(page.locator("#result-rows tr:visible")).toHaveCount(1);
  await page.getByRole("searchbox").fill("this-does-not-exist");
  await expect(page.locator("#empty-results")).toBeVisible();
  await page.getByRole("searchbox").fill("");
  await page.locator("#simd").selectOption("true");
  for (const value of await page
    .locator("#result-rows tr:visible")
    .evaluateAll((rows) => rows.map((r) => r.dataset.simd)))
    expect(value).toBe("true");
  await page.locator("#simd").selectOption("");
  await page.locator("#sort").selectOption("slow");
  await expect(page.locator("#result-rows tr:visible").first()).toContainText(
    "Octave",
  );
  await page.getByRole("searchbox").fill("go@1.25.5");
  await page.getByRole("link", { name: "Inspect Go, go", exact: true }).click();
  await expect(page.locator("h1")).toContainText("Go");
  await expect(page.locator("#samples")).toContainText("1.165925425");
  await page.getByText("go@1.25.5", { exact: true }).click();
  await expect(
    page
      .getByText("/nix/store/zzvsjgylnphvhms3lgr2qlwdxmc68z66-go-1.25.5", {
        exact: false,
      })
      .first(),
  ).toBeVisible();
  await expect(page.locator("#environment")).toContainText("Not recorded");
  await expect(page.locator("#environment")).toContainText("6.18.38-talos");
  const response = await page.request.get(
    "/history/2026-09-05T193245/raw/go.json",
  );
  expect(response.ok()).toBe(true);
  expect((await response.json()).Version).toBe("1.25.5");
  expect(errors).toEqual([]);
});
test("archive, journal and narrow layouts remain usable", async ({
  page,
}, testInfo) => {
  for (const route of [
    "/",
    "/results/2026-09-05T193245/go/",
    "/runs/2022-10-15T164557/",
    "/journal/a-more-inspectable-benchmark/",
    "/methodology/",
  ]) {
    await page.goto(route);
    await expect(page.locator("h1")).toBeVisible();
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
  }
  await page.goto("/");
  await page.screenshot({
    path: `test-results/home-${testInfo.project.name}.png`,
    fullPage: false,
  });
  await page.goto("/results/2026-09-05T193245/go/");
  await page.screenshot({
    path: `test-results/detail-${testInfo.project.name}.png`,
    fullPage: false,
  });
});
