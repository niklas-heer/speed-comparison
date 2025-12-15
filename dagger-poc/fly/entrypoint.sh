#!/bin/bash
set -e

# Start Docker daemon in background
echo "Starting Docker daemon..."
dockerd &

# Wait for Docker to be ready
echo "Waiting for Docker daemon..."
for i in {1..30}; do
    if docker info >/dev/null 2>&1; then
        echo "Docker is ready!"
        break
    fi
    sleep 1
done

# Keep container running
echo "Builder ready. Waiting for commands..."
tail -f /dev/null
