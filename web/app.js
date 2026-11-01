// LeanMail: progressive enhancement only. Every page works without this file.
// Search as you type, action buttons over PATCH /api/msg/:id, j/k/enter in lists,
// the sign-in code step, the admin tables, local times, and the passkey (WebAuthn) glue.
"use strict";
(function () {
  var H = { "X-Requested-With": "LeanMail", "Accept": "application/json" };

  function $(sel, root) { return (root || document).querySelector(sel); }
  function $all(sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); }
  function el(tag, attrs, kids) {
    var n = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      if (k === "text") n.textContent = attrs[k]; else n.setAttribute(k, attrs[k]);
    });
    (kids || []).forEach(function (k) { if (k) n.appendChild(typeof k === "string" ? document.createTextNode(k) : k); });
    return n;
  }
  function toast(msg) {
    var t = el("div", { "class": "toast", role: "status", text: msg });
    document.body.appendChild(t);
    setTimeout(function () { t.remove(); }, 2500);
  }
  function api(method, url, body) {
    var opts = { method: method, headers: Object.assign({}, H), credentials: "same-origin" };
    if (body !== undefined) { opts.headers["Content-Type"] = "application/json"; opts.body = JSON.stringify(body); }
    return fetch(url, opts).then(function (r) {
      return r.text().then(function (t) {
        var j = null; try { j = t ? JSON.parse(t) : null; } catch (e) { j = null; }
        if (!r.ok) { var err = new Error((j && j.error) || ("HTTP " + r.status)); err.status = r.status; throw err; }
        return j;
      });
    });
  }

  // local times
  function fmt(ts, kind) {
    var d = new Date(ts), now = new Date();
    if (kind === "full") return d.toLocaleString(undefined, { weekday: "short", year: "numeric", month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
    if (d.toDateString() === now.toDateString()) return d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" });
    var o = { month: "short", day: "numeric" };
    if (d.getFullYear() !== now.getFullYear()) o.year = "numeric";
    return d.toLocaleDateString(undefined, o);
  }
  function localTimes(root) {
    $all("time[data-ts]", root).forEach(function (t) {
      var ts = Number(t.getAttribute("data-ts"));
      if (ts) { t.textContent = fmt(ts, t.getAttribute("data-fmt")); t.title = new Date(ts).toString(); }
    });
  }

  // search as you type
  function liveSearch() {
    var q = $("#q"), live = $("#live");
    if (!q || !live) return;
    var timer = null, seq = 0;
    function hide() { live.hidden = true; live.textContent = ""; }
    q.addEventListener("input", function () {
      clearTimeout(timer);
      var v = q.value.trim();
      if (v.length < 2) { hide(); return; }
      timer = setTimeout(function () {
        var mine = ++seq;
        api("GET", "/api/search?limit=8&q=" + encodeURIComponent(v)).then(function (j) {
          if (mine !== seq) return;
          live.textContent = "";
          var items = (j && j.items) || [];
          if (!items.length) { live.appendChild(el("p", { "class": "hint", text: "No matches yet. Press Enter to search." })); }
          else {
            live.appendChild(el("ul", {}, items.map(function (m) {
              var from = (m.from && (m.from.name || m.from.address)) || "(unknown sender)";
              return el("li", {}, [el("a", { href: "/m/" + encodeURIComponent(m.id) }, [
                el("strong", { text: from }), " ", document.createTextNode(m.subject || "(no subject)"),
                el("span", { "class": "snip", text: m.snippet || "" })])]);
            })));
          }
          live.hidden = false;
        }).catch(hide);
      }, 250);
    });
    q.addEventListener("keydown", function (e) { if (e.key === "Escape") { hide(); q.blur(); } });
    document.addEventListener("click", function (e) { if (!live.contains(e.target) && e.target !== q) hide(); });
  }

  // action buttons
  var LEAVES = ["done", "snooze", "snooze_1d", "snooze_3d", "snooze_week", "junk"];
  function actions() {
    $all("form.actions").forEach(function (f) {
      f.addEventListener("submit", function (e) {
        var b = e.submitter;
        if (!b || !b.value) return;
        e.preventDefault();
        var id = f.getAttribute("data-id"), action = b.value;
        $all("button", f).forEach(function (x) { x.disabled = true; });
        api("PATCH", "/api/msg/" + encodeURIComponent(id), { action: action }).then(function () {
          if (LEAVES.indexOf(action) >= 0) location.href = "/inbox"; else location.reload();
        }).catch(function () {
          // fall back to the plain form post
          $all("button", f).forEach(function (x) { x.disabled = false; });
          f.appendChild(el("input", { type: "hidden", name: "action", value: action }));
          HTMLFormElement.prototype.submit.call(f);
        });
      });
    });
  }

  // j/k/enter over list rows, "/" to search
  function keys() {
    var rows = $all(".row");
    var i = -1;
    function sel(n) {
      if (!rows.length) return;
      if (i >= 0) rows[i].classList.remove("sel");
      i = Math.max(0, Math.min(rows.length - 1, n));
      rows[i].classList.add("sel");
      rows[i].scrollIntoView({ block: "nearest" });
    }
    document.addEventListener("keydown", function (e) {
      var t = e.target, tag = t && t.tagName;
      if (e.metaKey || e.ctrlKey || e.altKey || tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || (t && t.isContentEditable)) return;
      if (e.key === "/") { var q = $("#q"); if (q) { e.preventDefault(); q.focus(); } return; }
      if (!rows.length) return;
      if (e.key === "j") { e.preventDefault(); sel(i + 1); }
      else if (e.key === "k") { e.preventDefault(); sel(i - 1); }
      else if (e.key === "Enter" && i >= 0) { e.preventDefault(); var a = $("a", rows[i]); if (a) location.href = a.href; }
    });
  }

  // sign-in: send the email in place, then show the code step
  function signin() {
    var f = $("#email-form"), step = $("#code-step");
    if (!f || !step) return;
    f.addEventListener("submit", function (e) {
      e.preventDefault();
      var email = $("#email", f).value.trim(), action = f.getAttribute("action");
      var btn = $("button", f); btn.disabled = true;
      var req = action === "/signin"
        ? fetch(action, { method: "POST", credentials: "same-origin", headers: { "Content-Type": "application/x-www-form-urlencoded", "X-Requested-With": "LeanMail" }, body: "email=" + encodeURIComponent(email) })
        : fetch(action, { method: "POST", credentials: "same-origin", headers: { "Content-Type": "application/json", "X-Requested-With": "LeanMail" }, body: JSON.stringify({ email: email }) });
      req.then(function (r) {
        if (r.status >= 500) throw new Error("send failed");
        f.hidden = true; step.hidden = false;
        $("#code-email").value = email;
        var c = $("#code"); if (c) c.focus();
      }).catch(function () { btn.disabled = false; toast("Could not send the mail. Try again."); });
    });
    var code = $("#code");
    if (code) code.addEventListener("input", function () {
      if (/^\d{6}$/.test(code.value)) { var cf = $("#code-form"); if (cf) HTMLFormElement.prototype.submit.call(cf); }
    });
  }

  // admin tables, loaded from the operator through the Worker
  function admin() {
    var box = $("#admin");
    if (!box) return;
    var kind = box.getAttribute("data-admin"), src = box.getAttribute("data-src");
    function show(msg) { box.textContent = ""; box.appendChild(el("p", { "class": "hint", text: msg })); }
    function when(ts) { return ts ? new Date(ts).toLocaleString() : ""; }
    function load() {
      api("GET", src + (kind === "signups" ? "?status=all" : "")).then(function (j) {
        var items = (j && j.items) || [];
        box.textContent = "";
        if (!items.length) { show(kind === "signups" ? "No sign-ups." : "No accounts."); return; }
        var head = kind === "signups" ? ["Email", "Note", "Asked", "Decision", ""] : ["Email", "Status", "Domains", "Created", ""];
        var tb = el("tbody");
        items.forEach(function (it) {
          var cells, buttons = el("td");
          if (kind === "signups") {
            cells = [it.email, it.note || "", when(it.created_at), it.decision || "pending"];
            if (!it.decision) {
              [["approve", "Approve"], ["reject", "Reject"]].forEach(function (p) {
                var b = el("button", { type: "button", text: p[1] });
                b.addEventListener("click", function () {
                  b.disabled = true;
                  api("POST", "/admin/signups/" + encodeURIComponent(it.id) + "/" + p[0]).then(load).catch(function (e) { b.disabled = false; toast(e.message); });
                });
                buttons.appendChild(b);
              });
            }
          } else {
            var dom = el("input", { type: "text", value: (it.domains || []).join(", "), "aria-label": "Domains", "class": "mono" });
            var save = el("button", { type: "button", text: "Save domains" });
            save.addEventListener("click", function () {
              var ds = dom.value.split(/[\s,]+/).filter(Boolean);
              api("PUT", "/admin/accounts/" + encodeURIComponent(it.id) + "/domains", { domains: ds }).then(function () { toast("Saved."); load(); })
                .catch(function (e) { toast(e.message); });
            });
            cells = [it.email + (it.is_admin ? " (admin)" : ""), it.status, null, when(it.created_at)];
            cells[2] = el("div", {}, [dom, save]);
            if (it.status === "active") {
              var dis = el("button", { type: "button", "class": "danger", text: "Disable" });
              dis.addEventListener("click", function () {
                if (!confirm("Disable " + it.email + "?")) return;
                api("POST", "/admin/accounts/" + encodeURIComponent(it.id) + "/disable").then(load).catch(function (e) { toast(e.message); });
              });
              buttons.appendChild(dis);
            }
          }
          tb.appendChild(el("tr", {}, cells.map(function (c) { return el("td", {}, [c]); }).concat([buttons])));
        });
        box.appendChild(el("div", { "class": "admin-wrap" }, [el("table", { "class": "admin" }, [
          el("thead", {}, [el("tr", {}, head.map(function (h) { return el("th", { text: h }); }))]), tb])]));
      }).catch(function (e) {
        show(e.status === 404 ? "The operator's admin API is not reachable from here (no operator routing in front of this server)."
          : e.status === 403 ? "This account is not an operator administrator." : "Could not load: " + e.message);
      });
    }
    load();
  }

  function misc() {
    $all(".token-once input").forEach(function (i) { i.addEventListener("focus", function () { i.select(); }); });
  }

  document.addEventListener("DOMContentLoaded", function () {
    localTimes(document);
    liveSearch();
    actions();
    keys();
    signin();
    admin();
    misc();
  });
})();

// Compose: autosave the draft after 10 s without typing (PUT /api/drafts/:id with the revision
// it edits), Cmd/Ctrl+Enter sends. The form works without this; the script only keeps the hidden
// revision and client id in step with what the server holds.
(function () {
  "use strict";
  var H = { "X-Requested-With": "LeanMail", "Accept": "application/json", "Content-Type": "application/json" };
  function rand() {
    var a = new Uint8Array(12); (window.crypto || {}).getRandomValues ? crypto.getRandomValues(a) : a.forEach(function (_, i) { a[i] = Math.random() * 256; });
    return Array.prototype.map.call(a, function (b) { return ("0" + b.toString(16)).slice(-2); }).join("");
  }
  function field(f, name) { var e = f.elements[name]; return e ? e.value : ""; }
  function compose() {
    var f = document.getElementById("compose");
    if (!f) return;
    var state = document.getElementById("draft-state");
    var timer = null, saving = null, dirty = false, stopped = false, submitting = false;
    function say(t) { if (state) state.textContent = t; }
    function save() {
      if (stopped || !dirty) return Promise.resolve();
      var id = field(f, "draft_id");
      if (!id) return Promise.resolve();
      dirty = false;
      var rev = Number(field(f, "revision")) || 0;
      var body = {
        mutation_id: rand(), revision: rev, from: field(f, "from"), to: field(f, "to"), cc: field(f, "cc"),
        bcc: field(f, "bcc"), subject: field(f, "subject"), text: field(f, "body"),
        reply_of: field(f, "reply_of") || null, forward_of: field(f, "forward_of") || null,
        in_reply_to: field(f, "in_reply_to") || null, references: field(f, "references")
      };
      say("Saving…");
      saving = fetch("/api/drafts/" + encodeURIComponent(id), {
        method: "PUT", credentials: "same-origin", headers: Object.assign({ "If-Match": String(rev) }, H), body: JSON.stringify(body)
      }).then(function (r) {
        return r.json().catch(function () { return null; }).then(function (j) {
          if (r.status === 409) { stopped = true; say("This draft was changed elsewhere; autosave is off."); return; }
          if (!r.ok || !j) { dirty = true; say("Not saved"); return; }
          f.elements.revision.value = String(j.revision);
          if (j.send_id) f.elements.client_id.value = j.send_id;
          say("Draft saved " + new Date().toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" }));
        });
      }).catch(function () { dirty = true; say("Offline: not saved"); }).then(function () { saving = null; });
      return saving;
    }
    f.addEventListener("input", function (e) {
      if (e.target && e.target.type === "file") return;
      dirty = true;
      clearTimeout(timer);
      timer = setTimeout(save, 10000);
    });
    f.addEventListener("submit", function (e) {
      clearTimeout(timer);
      stopped = true;
      // an autosave in flight moves the revision: post after it lands
      if (saving && !submitting) {
        e.preventDefault();
        submitting = true;
        var b = e.submitter;
        saving.then(function () { if (b && f.requestSubmit) f.requestSubmit(b); else HTMLFormElement.prototype.submit.call(f); });
      }
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Enter" && (e.metaKey || e.ctrlKey) && f.contains(e.target)) {
        e.preventDefault();
        var b = document.getElementById("send-btn");
        if (f.requestSubmit) f.requestSubmit(b); else if (b) b.click();
      }
    });
    window.addEventListener("beforeunload", function () { if (dirty && !stopped) save(); });
  }
  document.addEventListener("DOMContentLoaded", compose);
})();

// M7: forms that ask first (Apply at Cloudflare), and the calendar's RSVP without a reload
(function () {
  "use strict";
  document.addEventListener("submit", function (e) {
    var f = e.target;
    if (!(f instanceof HTMLFormElement)) return;
    var q = f.getAttribute("data-confirm");
    if (q && !window.confirm(q)) e.preventDefault();
  }, true);
  document.addEventListener("DOMContentLoaded", function () {
    Array.prototype.slice.call(document.querySelectorAll("form.rsvp")).forEach(function (f) {
      f.addEventListener("submit", function (e) {
        var b = e.submitter;
        var m = /^\/calendar\/([^/]+)\/rsvp$/.exec(f.getAttribute("action") || "");
        if (!b || !m || !window.fetch) return;
        e.preventDefault();
        fetch("/api/events/" + encodeURIComponent(m[1]), {
          method: "PATCH", credentials: "same-origin",
          headers: { "Content-Type": "application/json", "X-Requested-With": "LeanMail" },
          body: JSON.stringify({ partstat: b.value })
        }).then(function (r) {
          if (!r.ok) throw new Error("rsvp " + r.status);
          Array.prototype.slice.call(f.querySelectorAll("button")).forEach(function (x) {
            var on = x === b;
            x.classList.toggle("on", on);
            x.setAttribute("aria-pressed", on ? "true" : "false");
          });
        }).catch(function () { HTMLFormElement.prototype.submit.call(f); });
      });
    });
  });
})();

// M6b: the optional AI. "Summarize thread" on a message without a reload, and "Suggest" on the
// compose page, which fills the body with a suggestion (never saved by itself). Both only when
// the server's AI is on: the button is rendered by the page, the compose one after GET /api/ai.
(function () {
  "use strict";
  var H = { "X-Requested-With": "LeanMail", "Accept": "application/json", "Content-Type": "application/json" };
  function post(url, body) {
    return fetch(url, { method: "POST", credentials: "same-origin", headers: H, body: JSON.stringify(body || {}) })
      .then(function (r) {
        return r.json().catch(function () { return null; }).then(function (j) {
          if (!r.ok) throw new Error((j && j.error) || ("HTTP " + r.status));
          return j;
        });
      });
  }
  function note(msg) {
    var t = document.createElement("div"); t.className = "toast"; t.setAttribute("role", "status"); t.textContent = msg;
    document.body.appendChild(t); setTimeout(function () { t.remove(); }, 2500);
  }
  function summaries() {
    Array.prototype.slice.call(document.querySelectorAll("form.ai-summarize")).forEach(function (f) {
      f.addEventListener("submit", function (e) {
        var b = f.querySelector("button");
        var t = b && b.getAttribute("data-thread");
        var box = document.querySelector("#ai-summary .ai-summary-text");
        if (!t || !box || !window.fetch) return;
        e.preventDefault();
        var refresh = f.elements.refresh && f.elements.refresh.value === "1";
        var was = b.textContent;
        b.disabled = true;
        b.textContent = "Summarizing…";
        post("/api/threads/" + encodeURIComponent(t) + "/summary" + (refresh ? "?refresh=1" : "")).then(function (j) {
          box.textContent = "";
          var p = document.createElement("p"); p.textContent = j.summary; box.appendChild(p);
          var h = document.createElement("p"); h.className = "hint"; h.textContent = "Summary by " + j.model; box.appendChild(h);
          b.textContent = "Summarize again";
          if (!f.elements.refresh) {
            var i = document.createElement("input"); i.type = "hidden"; i.name = "refresh"; i.value = "1"; f.appendChild(i);
          }
        }).catch(function (err) {
          b.textContent = was;
          note(err.message === "privileged" ? "This thread is privileged." : "The summary did not come back. Try again.");
        }).then(function () { b.disabled = false; });
      });
    });
  }
  function suggest() {
    var f = document.getElementById("compose");
    var body = f && f.elements.body;
    if (!body || !window.fetch) return;
    fetch("/api/ai", { credentials: "same-origin", headers: { "X-Requested-With": "LeanMail", "Accept": "application/json" } })
      .then(function (r) {
        if (!r.ok) return;
        var row = document.createElement("div"); row.className = "ai-suggest row-buttons";
        var intent = document.createElement("input");
        intent.type = "text"; intent.maxLength = 2000; intent.placeholder = "What should it say? (optional)";
        intent.setAttribute("aria-label", "What the suggestion should say");
        var b = document.createElement("button"); b.type = "button"; b.textContent = "Suggest";
        row.appendChild(intent); row.appendChild(b);
        body.parentNode.insertBefore(row, body.nextSibling);
        b.addEventListener("click", function () {
          var mid = (f.elements.reply_of && f.elements.reply_of.value) || (f.elements.forward_of && f.elements.forward_of.value) || null;
          b.disabled = true; b.textContent = "Thinking…";
          post("/api/ai/draft", { message_id: mid, intent: intent.value }).then(function (j) {
            var rest = body.value;
            body.value = j.text + (rest ? "\n\n" + rest : "");
            body.dispatchEvent(new Event("input", { bubbles: true }));
            body.focus();
          }).catch(function (err) {
            note(err.message === "privileged" ? "This thread is privileged; nothing was sent to the AI." : "No suggestion came back.");
          }).then(function () { b.disabled = false; b.textContent = "Suggest"; });
        });
      }).catch(function () {});
  }
  document.addEventListener("DOMContentLoaded", function () { summaries(); suggest(); });
})();

// Passkeys (lean/Server/Auth/Passkey.lean): "Sign in with a passkey" on /signin, which asks the
// Worker for a challenge (POST /auth/passkey/challenge) and posts a discoverable assertion to
// /auth/passkey/login; and "Add a passkey" in Settings over /api/passkeys. Binary fields travel
// as base64url. Both stay hidden where the browser has no WebAuthn.
(function () {
  function $(sel) { return document.querySelector(sel); }
  function supported() {
    return !!(window.PublicKeyCredential && navigator.credentials && navigator.credentials.create && window.TextEncoder);
  }
  function b64u(buf) {
    var s = "", b = new Uint8Array(buf);
    for (var i = 0; i < b.length; i++) s += String.fromCharCode(b[i]);
    return btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  }
  function unb64u(s) {
    s = s.replace(/-/g, "+").replace(/_/g, "/");
    while (s.length % 4) s += "=";
    var bin = atob(s), out = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
  }
  function toast(msg) {
    var t = document.createElement("div");
    t.className = "toast"; t.setAttribute("role", "status"); t.textContent = msg;
    document.body.appendChild(t);
    setTimeout(function () { t.remove(); }, 3500);
  }
  function post(url, body) {
    return fetch(url, {
      method: "POST", credentials: "same-origin", body: JSON.stringify(body || {}),
      headers: { "Content-Type": "application/json", "Accept": "application/json", "X-Requested-With": "LeanMail" }
    }).then(function (r) {
      return r.json().catch(function () { return null; }).then(function (j) {
        if (!r.ok) throw new Error((j && j.error) || ("HTTP " + r.status));
        return j;
      });
    });
  }

  function signin() {
    var box = $("#passkey-signin"), btn = $("#passkey-signin-btn");
    if (!box || !btn || !supported()) return;
    box.hidden = false;
    btn.addEventListener("click", function () {
      btn.disabled = true;
      post("/auth/passkey/challenge").then(function (o) {
        return navigator.credentials.get({ publicKey: {
          challenge: new TextEncoder().encode(o.challenge), rpId: o.rpId, allowCredentials: [],
          userVerification: o.userVerification || "preferred", timeout: o.timeout } });
      }).then(function (cred) {
        if (!cred) throw new Error("cancelled");
        var r = cred.response;
        return post("/auth/passkey/login", { id: cred.id, rawId: b64u(cred.rawId), type: cred.type, response: {
          clientDataJSON: b64u(r.clientDataJSON), authenticatorData: b64u(r.authenticatorData),
          signature: b64u(r.signature), userHandle: r.userHandle ? b64u(r.userHandle) : "" } });
      }).then(function () { location.href = "/inbox"; }).catch(function (e) {
        btn.disabled = false;
        if (e && e.name === "NotAllowedError") return;
        toast("That passkey did not sign you in. Get a link by mail instead.");
      });
    });
  }

  function settings() {
    var box = $("#passkey-add"), btn = $("#passkey-add-btn");
    if (!box || !btn || !supported()) return;
    box.hidden = false;
    var nojs = $("#passkey-nojs");
    if (nojs) nojs.hidden = true;
    btn.addEventListener("click", function () {
      btn.disabled = true;
      post("/api/passkeys/options").then(function (o) {
        return navigator.credentials.create({ publicKey: {
          challenge: unb64u(o.challenge), rp: o.rp,
          user: { id: unb64u(o.user.id), name: o.user.name, displayName: o.user.displayName },
          pubKeyCredParams: o.pubKeyCredParams, timeout: o.timeout, attestation: o.attestation,
          authenticatorSelection: o.authenticatorSelection,
          excludeCredentials: (o.excludeCredentials || []).map(function (c) { return { type: c.type, id: unb64u(c.id) }; }) } });
      }).then(function (cred) {
        var r = cred.response, name = $("#passkey-name");
        return post("/api/passkeys", { id: cred.id, rawId: b64u(cred.rawId), type: cred.type,
          response: { clientDataJSON: b64u(r.clientDataJSON), attestationObject: b64u(r.attestationObject) },
          transports: r.getTransports ? r.getTransports() : [], name: name ? name.value : "" });
      }).then(function () { location.href = "/settings?saved=passkey-added#passkeys"; }).catch(function (e) {
        btn.disabled = false;
        if (e && e.name === "NotAllowedError") return;
        toast(e && e.name === "InvalidStateError" ? "This device already holds a passkey for this account."
          : "The passkey was not added: " + ((e && e.message) || "error") + ".");
      });
    });
  }

  document.addEventListener("DOMContentLoaded", function () { signin(); settings(); });
})();
