# syncron
A simple tool that synchronizes two folders real-time.

## Installation
1. `git clone https://github.com/TOSPIO/syncron.git`
2. `cd syncron`
3. `cabal update`
3. `cabal install`

## Usage
Usage: syncron SRCDIR DSTDIR [-n|--no-startup-sync] [--exclude EXCLUDED_DIRS]
  Synchronize SRCDIR to DSTDIR

Available options:
  -h,--help                Show this help text
  -n,--no-startup-sync     Do not sync on startup. Only sync on changes
  --exclude EXCLUDED_DIRS  The paths to exclude from being watched
