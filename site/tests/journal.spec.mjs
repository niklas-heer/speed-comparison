import { test, expect } from "@playwright/test";

test("readers can follow the pipeline, pause it and use the keyboard", async ({
  page,
}) => {
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  await page.goto("/journal/how-code-becomes-a-benchmark/");
  const figure = page.locator("[data-pipeline]");
  await expect(figure.locator("[data-panel]:visible")).toHaveCount(1);
  await figure.getByRole("button", { name: /03 Measure/ }).focus();
  await page.keyboard.press("Enter");
  await expect(figure.locator("[data-panel]:visible")).toContainText(
    "Reuse the build. Make new observations.",
  );
  await figure.getByRole("button", { name: "Play the flow" }).click();
  await expect(
    figure.getByRole("button", { name: "Pause the flow" }),
  ).toBeVisible();
  await expect(figure.locator("[data-panel]:visible")).toContainText(
    "Build the environment",
    { timeout: 6000 },
  );
  await figure.getByRole("button", { name: "Pause the flow" }).click();
  await expect(figure).not.toHaveClass(/is-playing/);
  await figure.getByRole("button", { name: /06 Read/ }).click();
  await expect(figure.locator("[data-panel]:visible")).toContainText(
    "does not start another benchmark",
  );
  expect(errors).toEqual([]);
});

test("workload controls distinguish the two protocols without inventing timings", async ({
  page,
}) => {
  await page.goto("/journal/why-a-billion-terms/");
  const lab = page.locator("[data-measurement]");
  await expect(lab.locator("[data-total]")).toHaveText("4 billion");
  await lab.getByLabel("Protocol", { exact: true }).selectOption("baseline");
  await expect(lab.locator("[data-total]")).toHaveText("6 billion");
  await expect(lab.locator("[data-protocol-panel]:visible li")).toHaveCount(6);
  await lab.getByLabel("Terms in each execution").selectOption("100000000");
  await expect(lab.locator("[data-total]")).toHaveText("600 million");
  await lab.getByLabel("Protocol", { exact: true }).selectOption("current");
  await expect(lab.locator("[data-total]")).toHaveText("400 million");
  await lab.getByLabel("Terms in each execution").selectOption("10000");
  await expect(lab.locator("[data-total]")).toHaveText("40,000");
  await expect(
    lab.locator("[data-protocol-panel]:visible .measured"),
  ).toHaveCount(3);
});

test("reduced motion offers manual steps and stops playback if the preference changes", async ({
  page,
}) => {
  await page.goto("/journal/how-code-becomes-a-benchmark/");
  await page.getByRole("button", { name: "Play the flow" }).click();
  await page.emulateMedia({ reducedMotion: "reduce" });
  const figure = page.locator("[data-pipeline]");
  await expect(figure).not.toHaveClass(/is-playing/);
  await figure.getByRole("button", { name: "Next stage" }).click();
  await expect(figure.locator("[data-panel]:visible")).toContainText(
    "Build the environment",
  );
});

test("journal links, article navigation and both themes work at each viewport", async ({
  page,
}, testInfo) => {
  for (const theme of ["dark", "light"]) {
    await page.goto("/journal/");
    if (theme === "light")
      await page.getByRole("button", { name: "Switch to light theme" }).click();
    await expect(page.locator(".story-card")).toHaveCount(3);
    await page.screenshot({
      path: `test-results/journal-${theme}-${testInfo.project.name}.png`,
      fullPage: true,
    });
    const links = await page
      .locator(".story-card")
      .evaluateAll((cards) => cards.map((card) => card.getAttribute("href")));
    for (const link of links) {
      await page.goto(link);
      await expect(page.locator("h1")).toBeVisible();
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= innerWidth,
        ),
      ).toBe(true);
      const anchors = await page
        .locator(".story-toc a")
        .evaluateAll((links) => links.map((link) => link.hash));
      expect(anchors.length).toBeGreaterThan(3);
      for (const anchor of anchors)
        await expect(page.locator(anchor)).toHaveCount(1);
      await page.locator(".explainer").first().scrollIntoViewIfNeeded();
      await page.screenshot({
        path: `test-results/${link.split("/")[2]}-${theme}-${testInfo.project.name}.png`,
      });
    }
  }
});

test("articles retain explanations and evidence when JavaScript is disabled", async ({
  browser,
  baseURL,
}) => {
  const context = await browser.newContext({
    javaScriptEnabled: false,
    baseURL,
  });
  const page = await context.newPage();
  await page.goto("/journal/how-code-becomes-a-benchmark/");
  await expect(page.locator("[data-panel]:visible")).toHaveCount(6);
  await expect(
    page.getByRole("button", { name: "Play the flow" }),
  ).toBeHidden();
  await page.goto("/journal/why-a-billion-terms/");
  await expect(page.locator("[data-protocol-panel]:visible")).toHaveCount(2);
  await page.goto("/journal/a-more-inspectable-benchmark/");
  await expect(page.locator(".evidence-sample")).toHaveCount(3);
  const response = await page.request.get(
    await page
      .getByRole("link", { name: /Open the raw JSON/ })
      .getAttribute("href"),
  );
  expect((await response.json()).TimesPerRun).toHaveLength(3);
  await context.close();
});
