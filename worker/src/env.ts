import type { ContainerEnv, MailContainer } from "./container";

/** Every binding, var and secret the Worker reads. */
export interface Env extends ContainerEnv {
  MAIL: DurableObjectNamespace<MailContainer>;
  DB: D1Database;
  RAW: R2Bucket;
  EMAIL: SendEmail;
  /** Local development only (worker/.dev.vars): the account unauthenticated localhost requests reach. */
  DEV_ACCOUNT?: string;
}
