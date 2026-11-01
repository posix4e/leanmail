import type { D1Migration } from "cloudflare:test";
import type { Env as WorkerEnv } from "../src/env";

// MAIL is left out: Container's default generic is Cloudflare.Env, and naming MailContainer here
// makes the type refer to itself. Tests that need the full Env cast `env as unknown as WorkerEnv`.
declare global {
  namespace Cloudflare {
    interface Env extends Omit<WorkerEnv, "MAIL"> {
      TEST_MIGRATIONS: D1Migration[];
    }
  }
}
