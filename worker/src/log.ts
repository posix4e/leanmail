/** One JSON line per event. Callers pass counts, ids and classes only: no addresses, no subjects. */
export function log(ev: string, fields: Record<string, unknown> = {}): void {
  console.log(JSON.stringify({ ev, ...fields }));
}

export function errText(e: unknown): string {
  return (e instanceof Error ? `${e.name}: ${e.message}` : String(e)).slice(0, 200);
}
