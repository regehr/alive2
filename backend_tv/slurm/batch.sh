#!/bin/bash
#SBATCH --partition=largecpu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#XSBATCH --mem-per-cpu=200   # memory (MB)
#XSBATCH --time=0-2:00       # time (D-HH:MM)
#XSBATCH -o slurm.%N.%j.out  # STDOUT
#XSBATCH -e slurm.%N.%j.err  # STDERR

set -xe -o pipefail

# flake=$(realpath "$(dirname $0)"/../..)
flake="/home/Staff/uqklam5/alive2-slurm"

t=$(mktemp -d alive2.XXXX -p /tmp)
echo $t
[[ -n "$t" ]]
cd $t

nohup nix shell github:katrinafyi/pac-nix#aslp --command aslp-server &
server_pid=$!

(set +x; while ! nc -z localhost 8000; do sleep 1; done)

nix shell "$flake" --command backend-tv "$flake"/fptosi.ll

kill $!
wait
echo $t
