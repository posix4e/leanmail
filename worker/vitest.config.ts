import { cloudflareTest, readD1Migrations } from "@cloudflare/vitest-plugin";
import { defineConfig } from "vitest/config";

export default defineConfig(async () => {
  const migrations = await readD1Migrations(new URL("./migrations", import.meta.url).pathname);
  return {
    plugins: [
      cloudflareTest({
        wrangler: { configPath: "./wrangler.jsonc" },
        miniflare: {
          bindings: {
            TEST_MIGRATIONS: migrations,
            INTERNAL_SECRET: "test-internal-secret",
            SESSION_SECRET: "test-session-secret",
            // the local-development settings the tests expect, so they never depend on worker/.dev.vars
            DEV_ACCOUNT: "dev",
            MAIL_FROM: "no-reply@localhost.test",
            PUBLIC_ORIGIN: "http://127.0.0.1:8787",
          },
        },
      }),
    ],
    test: { setupFiles: ["./test/setup.ts"] },
  };
});
