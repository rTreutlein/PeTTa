FROM swipl:latest

# Install system build tools for your Python/native deps
RUN apt-get update \
 && apt-get install -y --no-install-recommends gcc \
 && rm -rf /var/lib/apt/lists/*

