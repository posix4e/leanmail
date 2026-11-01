/*
LeanMail C shim: the three things Lean's standard library cannot do for us.
  1. SQLite statements (the bundled amalgamation, FTS5 on).
  2. HTTPS requests through libcurl (Lean has TCP but no TLS).
  3. Digests and randomness through libcrypto (SHA-256, HMAC, random bytes).
Every function is a thin, total wrapper: errors come back as IO errors with SQLite's or curl's own
message, never as a crash. Pointers are held in Lean external objects with finalizers.
Licensed under Apache-2.0; see LICENSE at the repository root.
*/
#include <lean/lean.h>
#include <sqlite3.h>
#include <curl/curl.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/rand.h>
#include <openssl/core_names.h>
#include <openssl/params.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

#if defined(__SIZEOF_POINTER__) && __SIZEOF_POINTER__ == 4
#error "LeanMail's shim supports 64-bit platforms only."
#endif

#define LM_API __attribute__((visibility("default")))

/* ---------- helpers ---------- */

static lean_obj_res lm_io_error(const char *msg) {
  return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg ? msg : "unknown error")));
}

static lean_obj_res lm_io_error_prefixed(const char *prefix, const char *msg) {
  size_t n = strlen(prefix) + 2 + (msg ? strlen(msg) : 0) + 1;
  char *buf = (char *)malloc(n);
  if (!buf) return lm_io_error(prefix);
  snprintf(buf, n, "%s: %s", prefix, msg ? msg : "");
  lean_obj_res r = lm_io_error(buf);
  free(buf);
  return r;
}

/* ---------- SQLite: connections ---------- */

static void lm_db_finalize(void *p) {
  if (p) sqlite3_close_v2((sqlite3 *)p);
}
static void lm_noop_foreach(void *p, b_lean_obj_arg a) { (void)p; (void)a; }

static lean_external_class *lm_db_class = NULL;
static lean_external_class *lm_stmt_class = NULL;

static void lm_stmt_finalize(void *p) {
  if (p) sqlite3_finalize((sqlite3_stmt *)p);
}

static void lm_init_classes(void) {
  if (!lm_db_class) lm_db_class = lean_register_external_class(lm_db_finalize, lm_noop_foreach);
  if (!lm_stmt_class) lm_stmt_class = lean_register_external_class(lm_stmt_finalize, lm_noop_foreach);
}

static inline sqlite3 *lm_db(b_lean_obj_arg o) { return (sqlite3 *)lean_get_external_data(o); }
static inline sqlite3_stmt *lm_stmt(b_lean_obj_arg o) { return (sqlite3_stmt *)lean_get_external_data(o); }

/* open : String → IO Db */
LM_API lean_obj_res lm_sqlite_open(b_lean_obj_arg path, lean_obj_arg w) {
  (void)w;
  lm_init_classes();
  sqlite3 *db = NULL;
  int rc = sqlite3_open_v2(lean_string_cstr(path), &db,
                           SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_FULLMUTEX, NULL);
  if (rc != SQLITE_OK) {
    lean_obj_res e = lm_io_error_prefixed("sqlite open", db ? sqlite3_errmsg(db) : sqlite3_errstr(rc));
    if (db) sqlite3_close_v2(db);
    return e;
  }
  sqlite3_busy_timeout(db, 5000);
  return lean_io_result_mk_ok(lean_alloc_external(lm_db_class, db));
}

/* exec : Db → String → IO Unit  (one or more statements, no results) */
LM_API lean_obj_res lm_sqlite_exec(b_lean_obj_arg dbo, b_lean_obj_arg sql, lean_obj_arg w) {
  (void)w;
  char *err = NULL;
  int rc = sqlite3_exec(lm_db(dbo), lean_string_cstr(sql), NULL, NULL, &err);
  if (rc != SQLITE_OK) {
    lean_obj_res e = lm_io_error_prefixed("sqlite exec", err ? err : sqlite3_errstr(rc));
    if (err) sqlite3_free(err);
    return e;
  }
  return lean_io_result_mk_ok(lean_box(0));
}

/* prepare : Db → String → IO Stmt */
LM_API lean_obj_res lm_sqlite_prepare(b_lean_obj_arg dbo, b_lean_obj_arg sql, lean_obj_arg w) {
  (void)w;
  lm_init_classes();
  sqlite3_stmt *st = NULL;
  int rc = sqlite3_prepare_v2(lm_db(dbo), lean_string_cstr(sql), -1, &st, NULL);
  if (rc != SQLITE_OK) return lm_io_error_prefixed("sqlite prepare", sqlite3_errmsg(lm_db(dbo)));
  if (!st) return lm_io_error("sqlite prepare: empty statement");
  return lean_io_result_mk_ok(lean_alloc_external(lm_stmt_class, st));
}

static lean_obj_res lm_bind_result(sqlite3_stmt *st, int rc, const char *what) {
  if (rc != SQLITE_OK) return lm_io_error_prefixed(what, sqlite3_errmsg(sqlite3_db_handle(st)));
  return lean_io_result_mk_ok(lean_box(0));
}

/* bindNull : Stmt → UInt32 → IO Unit */
LM_API lean_obj_res lm_sqlite_bind_null(b_lean_obj_arg sto, uint32_t idx, lean_obj_arg w) {
  (void)w;
  return lm_bind_result(lm_stmt(sto), sqlite3_bind_null(lm_stmt(sto), (int)idx), "sqlite bind null");
}
/* bindInt : Stmt → UInt32 → Int64 → IO Unit */
LM_API lean_obj_res lm_sqlite_bind_int(b_lean_obj_arg sto, uint32_t idx, uint64_t v, lean_obj_arg w) {
  (void)w;
  return lm_bind_result(lm_stmt(sto), sqlite3_bind_int64(lm_stmt(sto), (int)idx, (sqlite3_int64)v), "sqlite bind int");
}
/* bindReal : Stmt → UInt32 → Float → IO Unit */
LM_API lean_obj_res lm_sqlite_bind_real(b_lean_obj_arg sto, uint32_t idx, double v, lean_obj_arg w) {
  (void)w;
  return lm_bind_result(lm_stmt(sto), sqlite3_bind_double(lm_stmt(sto), (int)idx, v), "sqlite bind real");
}
/* bindText : Stmt → UInt32 → String → IO Unit */
LM_API lean_obj_res lm_sqlite_bind_text(b_lean_obj_arg sto, uint32_t idx, b_lean_obj_arg s, lean_obj_arg w) {
  (void)w;
  size_t n = lean_string_size(s) - 1;
  int rc = sqlite3_bind_text64(lm_stmt(sto), (int)idx, lean_string_cstr(s), (sqlite3_uint64)n, SQLITE_TRANSIENT, SQLITE_UTF8);
  return lm_bind_result(lm_stmt(sto), rc, "sqlite bind text");
}
/* bindBlob : Stmt → UInt32 → ByteArray → IO Unit */
LM_API lean_obj_res lm_sqlite_bind_blob(b_lean_obj_arg sto, uint32_t idx, b_lean_obj_arg b, lean_obj_arg w) {
  (void)w;
  size_t n = lean_sarray_size(b);
  const void *p = n ? (const void *)lean_sarray_cptr(b) : (const void *)"";
  int rc = sqlite3_bind_blob64(lm_stmt(sto), (int)idx, p, (sqlite3_uint64)n, SQLITE_TRANSIENT);
  return lm_bind_result(lm_stmt(sto), rc, "sqlite bind blob");
}

/* step : Stmt → IO Bool   (true = a row is available, false = done) */
LM_API lean_obj_res lm_sqlite_step(b_lean_obj_arg sto, lean_obj_arg w) {
  (void)w;
  int rc = sqlite3_step(lm_stmt(sto));
  if (rc == SQLITE_ROW) return lean_io_result_mk_ok(lean_box(1));
  if (rc == SQLITE_DONE) return lean_io_result_mk_ok(lean_box(0));
  return lm_io_error_prefixed("sqlite step", sqlite3_errmsg(sqlite3_db_handle(lm_stmt(sto))));
}

/* reset : Stmt → IO Unit  (also clears bindings) */
LM_API lean_obj_res lm_sqlite_reset(b_lean_obj_arg sto, lean_obj_arg w) {
  (void)w;
  sqlite3_reset(lm_stmt(sto));
  sqlite3_clear_bindings(lm_stmt(sto));
  return lean_io_result_mk_ok(lean_box(0));
}

/* columnCount : Stmt → UInt32 */
LM_API uint32_t lm_sqlite_column_count(b_lean_obj_arg sto) {
  return (uint32_t)sqlite3_column_count(lm_stmt(sto));
}
/* columnType : Stmt → UInt32 → UInt32   (1 int, 2 real, 3 text, 4 blob, 5 null) */
LM_API uint32_t lm_sqlite_column_type(b_lean_obj_arg sto, uint32_t i) {
  return (uint32_t)sqlite3_column_type(lm_stmt(sto), (int)i);
}
/* columnName : Stmt → UInt32 → String */
LM_API lean_obj_res lm_sqlite_column_name(b_lean_obj_arg sto, uint32_t i) {
  const char *n = sqlite3_column_name(lm_stmt(sto), (int)i);
  return lean_mk_string(n ? n : "");
}
/* columnInt : Stmt → UInt32 → Int64 */
LM_API uint64_t lm_sqlite_column_int(b_lean_obj_arg sto, uint32_t i) {
  return (uint64_t)sqlite3_column_int64(lm_stmt(sto), (int)i);
}
/* columnReal : Stmt → UInt32 → Float */
LM_API double lm_sqlite_column_real(b_lean_obj_arg sto, uint32_t i) {
  return sqlite3_column_double(lm_stmt(sto), (int)i);
}
/* columnText : Stmt → UInt32 → String */
LM_API lean_obj_res lm_sqlite_column_text(b_lean_obj_arg sto, uint32_t i) {
  const unsigned char *t = sqlite3_column_text(lm_stmt(sto), (int)i);
  int n = sqlite3_column_bytes(lm_stmt(sto), (int)i);
  if (!t) return lean_mk_string("");
  return lean_mk_string_from_bytes((const char *)t, (size_t)n);
}
/* columnBlob : Stmt → UInt32 → ByteArray */
LM_API lean_obj_res lm_sqlite_column_blob(b_lean_obj_arg sto, uint32_t i) {
  const void *p = sqlite3_column_blob(lm_stmt(sto), (int)i);
  int n = sqlite3_column_bytes(lm_stmt(sto), (int)i);
  lean_obj_res arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
  if (n > 0 && p) memcpy(lean_sarray_cptr(arr), p, (size_t)n);
  return arr;
}

/* changes : Db → IO Int64 ; lastInsertRowid : Db → IO Int64 */
LM_API lean_obj_res lm_sqlite_changes(b_lean_obj_arg dbo, lean_obj_arg w) {
  (void)w;
  return lean_io_result_mk_ok(lean_box_uint64((uint64_t)sqlite3_changes64(lm_db(dbo))));
}
LM_API lean_obj_res lm_sqlite_last_insert_rowid(b_lean_obj_arg dbo, lean_obj_arg w) {
  (void)w;
  return lean_io_result_mk_ok(lean_box_uint64((uint64_t)sqlite3_last_insert_rowid(lm_db(dbo))));
}
/* version : Unit → String */
LM_API lean_obj_res lm_sqlite_version(lean_obj_arg u) {
  (void)u;
  return lean_mk_string(sqlite3_libversion());
}

/* ---------- libcurl ---------- */

/* Response caps: a trusted endpoint (the Worker, R2, Gmail, Cloudflare) may answer up to 64 MiB
   (a raw message is at most ~35 MB as Gmail's base64); a "safe" request (an untrusted URL: a
   List-Unsubscribe target, a non-local AI endpoint) 16 MiB. Header lines: 200 either way. */
#define LM_HTTP_MAX_BODY_TRUSTED ((size_t)64 * 1024 * 1024)
#define LM_HTTP_MAX_BODY_SAFE ((size_t)16 * 1024 * 1024)
#define LM_HTTP_MAX_HEADERS 200
/* flags bit 0: the "safe" profile (https only, no redirects, no proxy, no private addresses). */
#define LM_HTTP_SAFE 1u

typedef struct { unsigned char *data; size_t len, cap, max; int over; } lm_buf;

static size_t lm_buf_write(void *ptr, size_t size, size_t nmemb, void *ud) {
  lm_buf *b = (lm_buf *)ud;
  size_t n = size * nmemb;
  if (b->len + n > b->max) { b->over = 1; return 0; }
  if (b->len + n > b->cap) {
    size_t cap = b->cap ? b->cap : 4096;
    while (cap < b->len + n) cap *= 2;
    unsigned char *d = (unsigned char *)realloc(b->data, cap);
    if (!d) return 0;
    b->data = d;
    b->cap = cap;
  }
  memcpy(b->data + b->len, ptr, n);
  b->len += n;
  return n;
}

typedef struct { const unsigned char *data; size_t len, pos; } lm_src;

static size_t lm_src_read(char *ptr, size_t size, size_t nmemb, void *ud) {
  lm_src *s = (lm_src *)ud;
  size_t n = size * nmemb;
  size_t left = s->len - s->pos;
  if (n > left) n = left;
  memcpy(ptr, s->data + s->pos, n);
  s->pos += n;
  return n;
}

/* Collect "Name: value" header lines (without CRLF) into a Lean Array String. */
typedef struct { lean_object *arr; size_t count; int over; } lm_hdrs;

static size_t lm_hdr_write(char *ptr, size_t size, size_t nmemb, void *ud) {
  lm_hdrs *h = (lm_hdrs *)ud;
  size_t n = size * nmemb;
  /* skip status lines and the blank line */
  if (n >= 5 && strncmp(ptr, "HTTP/", 5) == 0) return n;
  size_t end = n;
  while (end > 0 && (ptr[end - 1] == '\r' || ptr[end - 1] == '\n')) end--;
  if (end == 0) return n;
  if (++h->count > LM_HTTP_MAX_HEADERS) { h->over = 1; return 0; }
  h->arr = lean_array_push(h->arr, lean_mk_string_from_bytes(ptr, end));
  return n;
}

/* Loopback, private, link-local, CGNAT, multicast, reserved and unspecified IPv4. */
static int lm_v4_private(uint32_t a) {
  uint8_t b0 = (uint8_t)(a >> 24), b1 = (uint8_t)(a >> 16), b2 = (uint8_t)(a >> 8);
  if (b0 == 0 || b0 == 10 || b0 == 127) return 1;
  if (b0 == 100 && (b1 & 0xC0) == 64) return 1;          /* 100.64.0.0/10 CGNAT */
  if (b0 == 169 && b1 == 254) return 1;                    /* link-local */
  if (b0 == 172 && (b1 & 0xF0) == 16) return 1;            /* 172.16.0.0/12 */
  if (b0 == 192 && b1 == 168) return 1;
  if (b0 == 192 && b1 == 0 && b2 == 0) return 1;           /* 192.0.0.0/24 */
  if (b0 == 198 && (b1 & 0xFE) == 18) return 1;            /* 198.18.0.0/15 benchmarking */
  if (b0 >= 224) return 1;                                 /* multicast, reserved, broadcast */
  return 0;
}

static int lm_addr_private(const struct sockaddr *sa) {
  if (sa->sa_family == AF_INET) {
    const struct sockaddr_in *v4 = (const struct sockaddr_in *)sa;
    return lm_v4_private(ntohl(v4->sin_addr.s_addr));
  }
  if (sa->sa_family == AF_INET6) {
    const unsigned char *x = ((const struct sockaddr_in6 *)sa)->sin6_addr.s6_addr;
    static const unsigned char zero10[10] = {0};
    int lead_zero = memcmp(x, zero10, 10) == 0;
    /* ::ffff:a.b.c.d (mapped) and ::a.b.c.d (compatible): the IPv4 rules */
    if (lead_zero && ((x[10] == 0xff && x[11] == 0xff) || (x[10] == 0 && x[11] == 0))) {
      uint32_t a = ((uint32_t)x[12] << 24) | ((uint32_t)x[13] << 16) | ((uint32_t)x[14] << 8) | x[15];
      if (x[10] == 0 && x[11] == 0 && a <= 1) return 1;   /* :: and ::1 */
      return lm_v4_private(a);
    }
    /* 64:ff9b::/96 NAT64: the embedded IPv4 */
    if (x[0] == 0x00 && x[1] == 0x64 && x[2] == 0xff && x[3] == 0x9b) {
      static const unsigned char z8[8] = {0};
      if (memcmp(x + 4, z8, 8) == 0) {
        uint32_t a = ((uint32_t)x[12] << 24) | ((uint32_t)x[13] << 16) | ((uint32_t)x[14] << 8) | x[15];
        return lm_v4_private(a);
      }
    }
    if ((x[0] & 0xFE) == 0xFC) return 1;                   /* fc00::/7 unique local */
    if (x[0] == 0xFE && (x[1] & 0xC0) == 0x80) return 1;   /* fe80::/10 link-local */
    if (x[0] == 0xFE && (x[1] & 0xC0) == 0xC0) return 1;   /* fec0::/10 site-local */
    if (x[0] == 0xFF) return 1;                            /* multicast */
    if (x[0] == 0x20 && x[1] == 0x01 && x[2] == 0x0d && x[3] == 0xb8) return 1; /* documentation */
    return 0;
  }
  return 1; /* anything else (unix sockets, …) is never a public address */
}

typedef struct { int blocked; } lm_sock_ctx;

/* The safe profile's socket opener: refuse to connect to a non-public address, whatever name
   resolved to it (so DNS rebinding cannot reach the container's own network). */
static curl_socket_t lm_open_socket(void *clientp, curlsocktype purpose, struct curl_sockaddr *addr) {
  lm_sock_ctx *x = (lm_sock_ctx *)clientp;
  if (purpose != CURLSOCKTYPE_IPCXN || lm_addr_private(&addr->addr)) {
    x->blocked = 1;
    return CURL_SOCKET_BAD;
  }
  return socket(addr->family, addr->socktype, addr->protocol);
}

static int lm_curl_ready = 0;

static void lm_curl_init_once(void) {
  if (!lm_curl_ready) {
    curl_global_init(CURL_GLOBAL_DEFAULT);
    lm_curl_ready = 1;
  }
}

/* init : IO Unit — curl_global_init, once, at startup (before any thread makes a request). */
LM_API lean_obj_res lm_http_init(lean_obj_arg w) {
  (void)w;
  lm_curl_init_once();
  return lean_io_result_mk_ok(lean_box(0));
}

/*
request : (method url : String) → (headers : Array String) → (body : ByteArray) → (timeoutMs : UInt32)
        → (flags : UInt32) → IO RawResponse
RawResponse is a Lean structure declared as (headers : Array String) (body : ByteArray) (status : UInt32):
two object fields then one 4-byte scalar.
*/
LM_API lean_obj_res lm_http_request(b_lean_obj_arg method, b_lean_obj_arg url, b_lean_obj_arg headers,
                                    b_lean_obj_arg body, uint32_t timeout_ms, uint32_t flags, lean_obj_arg w) {
  (void)w;
  /* the CLI never calls lm_http_init: the first request does it */
  lm_curl_init_once();
  int safe = (flags & LM_HTTP_SAFE) != 0;
  CURL *c = curl_easy_init();
  if (!c) return lm_io_error("curl init failed");

  lm_buf out = {0, 0, 0, safe ? LM_HTTP_MAX_BODY_SAFE : LM_HTTP_MAX_BODY_TRUSTED, 0};
  lm_hdrs hdrs = { lean_mk_empty_array(), 0, 0 };
  lm_sock_ctx sock = { 0 };
  lm_src src = { (const unsigned char *)lean_sarray_cptr(body), lean_sarray_size(body), 0 };
  struct curl_slist *list = NULL;
  size_t nh = lean_array_size(headers);
  for (size_t i = 0; i < nh; i++) {
    list = curl_slist_append(list, lean_string_cstr(lean_array_get_core(headers, i)));
  }
  /* curl adds Expect: 100-continue for larger bodies; we never want the round trip. */
  list = curl_slist_append(list, "Expect:");

  const char *m = lean_string_cstr(method);
  curl_easy_setopt(c, CURLOPT_URL, lean_string_cstr(url));
  curl_easy_setopt(c, CURLOPT_CUSTOMREQUEST, m);
  curl_easy_setopt(c, CURLOPT_HTTPHEADER, list);
  curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, lm_buf_write);
  curl_easy_setopt(c, CURLOPT_WRITEDATA, &out);
  curl_easy_setopt(c, CURLOPT_HEADERFUNCTION, lm_hdr_write);
  curl_easy_setopt(c, CURLOPT_HEADERDATA, &hdrs);
  curl_easy_setopt(c, CURLOPT_TIMEOUT_MS, (long)timeout_ms);
  curl_easy_setopt(c, CURLOPT_CONNECTTIMEOUT_MS, (long)(timeout_ms < 10000 ? timeout_ms : 10000));
  curl_easy_setopt(c, CURLOPT_NOSIGNAL, 1L);
  curl_easy_setopt(c, CURLOPT_FOLLOWLOCATION, 0L);
  curl_easy_setopt(c, CURLOPT_ACCEPT_ENCODING, "");
  curl_easy_setopt(c, CURLOPT_USERAGENT, "leanmail/0.1");
  curl_easy_setopt(c, CURLOPT_MAXFILESIZE_LARGE, (curl_off_t)out.max);
  if (safe) {
    curl_easy_setopt(c, CURLOPT_PROTOCOLS_STR, "https");
    curl_easy_setopt(c, CURLOPT_REDIR_PROTOCOLS_STR, "https");
    /* no proxy from the environment: the address checked must be the one connected to */
    curl_easy_setopt(c, CURLOPT_PROXY, "");
    curl_easy_setopt(c, CURLOPT_OPENSOCKETFUNCTION, lm_open_socket);
    curl_easy_setopt(c, CURLOPT_OPENSOCKETDATA, &sock);
  } else {
    curl_easy_setopt(c, CURLOPT_PROTOCOLS_STR, "http,https");
  }
  if (strcmp(m, "HEAD") == 0) curl_easy_setopt(c, CURLOPT_NOBODY, 1L);
  if (src.len > 0 || strcmp(m, "POST") == 0 || strcmp(m, "PUT") == 0 || strcmp(m, "PATCH") == 0) {
    curl_easy_setopt(c, CURLOPT_UPLOAD, 1L);
    curl_easy_setopt(c, CURLOPT_READFUNCTION, lm_src_read);
    curl_easy_setopt(c, CURLOPT_READDATA, &src);
    curl_easy_setopt(c, CURLOPT_INFILESIZE_LARGE, (curl_off_t)src.len);
    /* UPLOAD implies PUT; CUSTOMREQUEST above keeps the verb we were given. */
  }

  CURLcode rc = curl_easy_perform(c);
  lean_obj_res result;
  if (rc != CURLE_OK) {
    const char *why = sock.blocked ? "blocked: not a public address"
                    : out.over || rc == CURLE_FILESIZE_EXCEEDED ? "response too large"
                    : hdrs.over ? "too many response headers"
                    : curl_easy_strerror(rc);
    result = lm_io_error_prefixed("http", why);
    lean_dec(hdrs.arr);
  } else {
    long status = 0;
    curl_easy_getinfo(c, CURLINFO_RESPONSE_CODE, &status);
    lean_obj_res bytes = lean_alloc_sarray(1, out.len, out.len);
    if (out.len) memcpy(lean_sarray_cptr(bytes), out.data, out.len);
    lean_obj_res resp = lean_alloc_ctor(0, 2, sizeof(uint32_t));
    lean_ctor_set(resp, 0, hdrs.arr);
    lean_ctor_set(resp, 1, bytes);
    lean_ctor_set_uint32(resp, 2 * sizeof(void *), (uint32_t)status);
    result = lean_io_result_mk_ok(resp);
  }
  free(out.data);
  curl_slist_free_all(list);
  curl_easy_cleanup(c);
  return result;
}

/* ---------- libcrypto ---------- */

static lean_obj_res lm_bytes(const unsigned char *p, size_t n) {
  lean_obj_res arr = lean_alloc_sarray(1, n, n);
  if (n) memcpy(lean_sarray_cptr(arr), p, n);
  return arr;
}

/* sha256 : ByteArray → ByteArray */
LM_API lean_obj_res lm_sha256(b_lean_obj_arg data) {
  unsigned char out[32];
  unsigned int n = 0;
  EVP_Digest(lean_sarray_cptr(data), lean_sarray_size(data), out, &n, EVP_sha256(), NULL);
  return lm_bytes(out, 32);
}

/* hmacSha256 : (key msg : ByteArray) → ByteArray */
LM_API lean_obj_res lm_hmac_sha256(b_lean_obj_arg key, b_lean_obj_arg msg) {
  unsigned char out[32];
  unsigned int n = 0;
  HMAC(EVP_sha256(), lean_sarray_cptr(key), (int)lean_sarray_size(key),
       lean_sarray_cptr(msg), lean_sarray_size(msg), out, &n);
  return lm_bytes(out, 32);
}

/* randomBytes : USize → IO ByteArray */
LM_API lean_obj_res lm_random_bytes(size_t n, lean_obj_arg w) {
  (void)w;
  lean_obj_res arr = lean_alloc_sarray(1, n, n);
  if (n && RAND_bytes(lean_sarray_cptr(arr), (int)n) != 1) {
    lean_dec(arr);
    return lm_io_error("RAND_bytes failed");
  }
  return lean_io_result_mk_ok(arr);
}

/* p256Verify : (x y msg derSig : ByteArray) → Bool
   ECDSA P-256 with SHA-256 over msg, the public key the affine point (x, y) (32 bytes each), the
   signature DER-encoded as WebAuthn sends it. False on any malformed input. */
LM_API uint8_t lm_p256_verify(b_lean_obj_arg x, b_lean_obj_arg y, b_lean_obj_arg msg, b_lean_obj_arg sig) {
  if (lean_sarray_size(x) != 32 || lean_sarray_size(y) != 32) return 0;
  size_t siglen = lean_sarray_size(sig);
  if (siglen < 8 || siglen > 80) return 0;
  unsigned char point[65];
  point[0] = 0x04;
  memcpy(point + 1, lean_sarray_cptr(x), 32);
  memcpy(point + 33, lean_sarray_cptr(y), 32);
  OSSL_PARAM params[3];
  params[0] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, (char *)"prime256v1", 0);
  params[1] = OSSL_PARAM_construct_octet_string(OSSL_PKEY_PARAM_PUB_KEY, point, sizeof point);
  params[2] = OSSL_PARAM_construct_end();
  uint8_t ok = 0;
  EVP_PKEY *pkey = NULL;
  EVP_MD_CTX *md = NULL;
  EVP_PKEY_CTX *kctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
  if (!kctx || EVP_PKEY_fromdata_init(kctx) != 1 ||
      EVP_PKEY_fromdata(kctx, &pkey, EVP_PKEY_PUBLIC_KEY, params) != 1) goto done;
  md = EVP_MD_CTX_new();
  if (!md || EVP_DigestVerifyInit(md, NULL, EVP_sha256(), NULL, pkey) != 1) goto done;
  ok = EVP_DigestVerify(md, lean_sarray_cptr(sig), siglen, lean_sarray_cptr(msg), lean_sarray_size(msg)) == 1;
done:
  EVP_MD_CTX_free(md);
  EVP_PKEY_free(pkey);
  EVP_PKEY_CTX_free(kctx);
  return ok;
}
