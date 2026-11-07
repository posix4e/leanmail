/** The one piece of nodejs_compat used here (src/gmailsync.ts): it decodes 30 MB without a copy per byte. */
declare module "node:buffer" {
  export const Buffer: { from(text: string, encoding: "base64url"): Uint8Array };
}
