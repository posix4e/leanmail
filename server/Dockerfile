# LeanMail container image: the Lean server behind Litestream.
# Stage 1 builds leanmail-server and leanmail with the pinned Lean toolchain; stage 2 fetches
# Litestream; stage 3 is the slim runtime.
#
# Build from the repo root (`make docker`). The Lean toolchain and Litestream tarballs are read
# from server/.cache/, which `make toolchain-cache` fills (large downloads keep dropping inside the
# emulated build); without them each stage downloads its own with a resume loop.

FROM --platform=linux/amd64 debian:bookworm AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl git ca-certificates build-essential libcurl4-openssl-dev libssl-dev zstd \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src/lean
# `server/.cach[e]` is a wildcard so the COPY still succeeds when the cache directory is absent.
COPY lean/lean-toolchain server/.cach[e] /tmp/toolchain/
RUN set -eu; TC="$(cat /tmp/toolchain/lean-toolchain)"; VER="${TC#leanprover/lean4:v}"; \
    TAR="/tmp/toolchain/lean-${VER}-linux.tar.zst"; \
    if ! zstd -tq "$TAR" 2>/dev/null; then \
      echo "toolchain cache missing or incomplete; downloading lean ${VER}"; \
      i=0; until zstd -tq "$TAR" 2>/dev/null; do \
        i=$((i+1)); [ "$i" -le 50 ] || exit 1; \
        curl -fL -C - --speed-time 30 --speed-limit 1024 -sS -o "$TAR" \
          "https://releases.lean-lang.org/lean4/v${VER}/lean-${VER}-linux.tar.zst" || sleep 3; \
      done; \
    fi; \
    mkdir -p /opt/lean && tar --zstd -xf "$TAR" --strip-components=1 -C /opt/lean \
    && rm -rf /tmp/toolchain \
    && /opt/lean/bin/lean --version | grep -q "version ${VER}," && /opt/lean/bin/lean --version
# Lean's bundled clang/lld links against the toolchain's own glibc and does not search the distro's
# library directory, so `-lcurl` (lakefile `shimLinkArgs`) is not found without LIBRARY_PATH.
ENV PATH=/opt/lean/bin:$PATH LIBRARY_PATH=/usr/lib/x86_64-linux-gnu
COPY lean/ ./
# Under qemu amd64 emulation a forked lake child sometimes hangs forever (0 CPU) before exec. A
# rerun resumes from what is already built, so bound each attempt and retry; a real compile error
# just fails fast on every attempt and stops the build.
RUN i=0; until timeout 600 lake build leanmail-server leanmail; do \
      i=$((i+1)); [ "$i" -lt 5 ] || exit 1; echo "lake build failed or hung; retrying ($i)"; \
    done
RUN cd .lake/build/bin && strip leanmail-server leanmail && ls -la && ./leanmail version

FROM --platform=linux/amd64 debian:bookworm-slim AS litestream
# Keep in step with LITESTREAM_VERSION in the Makefile, which caches the tarball in server/.cache/.
ARG LITESTREAM_VERSION=0.5.17
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY server/litestream.yml server/.cache/litestream-${LITESTREAM_VERSION}-linux-x86_64.tar.g[z] /tmp/litestream/
RUN set -eu; TAR="/tmp/litestream/litestream-${LITESTREAM_VERSION}-linux-x86_64.tar.gz"; \
    i=0; until gzip -tq "$TAR" 2>/dev/null; do \
      i=$((i+1)); [ "$i" -le 50 ] || exit 1; \
      curl -fL -C - --speed-time 30 --speed-limit 1024 -sS -o "$TAR" \
        "https://github.com/benbjohnson/litestream/releases/download/v${LITESTREAM_VERSION}/litestream-${LITESTREAM_VERSION}-linux-x86_64.tar.gz" \
        || sleep 3; \
    done; \
    tar -xzf "$TAR" -C /usr/local/bin litestream && rm -rf /tmp/litestream \
    && /usr/local/bin/litestream version

FROM --platform=linux/amd64 debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4 libssl3 ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY --from=litestream /usr/local/bin/litestream /usr/local/bin/litestream
COPY --from=build /src/lean/.lake/build/bin/leanmail-server /usr/local/bin/leanmail-server
COPY --from=build /src/lean/.lake/build/bin/leanmail /usr/local/bin/leanmail
COPY server/litestream.yml /etc/litestream.yml
COPY server/entrypoint.sh /entrypoint.sh
COPY web/ /opt/leanmail/web/
RUN chmod +x /entrypoint.sh && mkdir -p /data
ENV DATA_DIR=/data PORT=8080
ENV WEB_DIR=/opt/leanmail/web
EXPOSE 8080
ENTRYPOINT ["/entrypoint.sh"]
