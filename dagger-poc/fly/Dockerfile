# Fly.io remote build machine for speed-comparison
# This image has everything needed to run Dagger builds with Nix/Devbox
#
# Uses Debian with Nix installed - simpler than nixos/nix for Docker-in-Docker
#
# Usage: This is deployed as a Fly Machine and started on-demand for builds

FROM debian:bookworm-slim

# Install base dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    curl \
    git \
    ca-certificates \
    gnupg \
    xz-utils \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Install Docker
RUN install -m 0755 -d /etc/apt/keyrings && \
    curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg && \
    chmod a+r /etc/apt/keyrings/docker.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian bookworm stable" > /etc/apt/sources.list.d/docker.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends docker-ce docker-ce-cli containerd.io docker-buildx-plugin && \
    rm -rf /var/lib/apt/lists/*

# Create nixbld group and users required for Nix
RUN groupadd -r nixbld && \
    for i in $(seq 1 10); do \
    useradd -r -g nixbld -G nixbld -d /var/empty -s /sbin/nologin nixbld$i; \
    done

# Install Nix in single-user mode
RUN curl -L https://nixos.org/nix/install | sh -s -- --no-daemon

# Add Nix to PATH
ENV PATH="/root/.nix-profile/bin:/nix/var/nix/profiles/default/bin:${PATH}"

# Configure Nix for flakes and binary cache
RUN mkdir -p /etc/nix && \
    echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf && \
    echo 'substituters = https://cache.nixos.org/' >> /etc/nix/nix.conf && \
    echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf

# Install Devbox
RUN curl -fsSL https://get.jetpack.io/devbox | bash -s -- -f

# Install Dagger CLI
RUN curl -fsSL https://dl.dagger.io/dagger/install.sh | sh && \
    mv ./bin/dagger /usr/local/bin/dagger

# Install uv (Python package manager)
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:${PATH}"

# Working directory
WORKDIR /workspace

# Startup script
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
