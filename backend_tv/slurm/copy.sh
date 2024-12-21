#!/bin/bash -xe

REMOTE='uqklam5@rangpur.compute.eait.uq.edu.au'

cd "$(git rev-parse --show-toplevel)"
# ssh "$REMOTE" mkdir -p alive2-slurm
rsync -a -P -z --exclude=.git --exclude=build --exclude='*.tar.zst' --exclude='*.tar.gz' --exclude logs --exclude logs-aslp \
  \
  . "$REMOTE:alive2-slurm"

