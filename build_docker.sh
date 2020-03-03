#!/bin/sh -ex

# docker system prune -a

tar cz Dockerfile build_deps.sh clone_and_test.sh CMakeLists.txt docs include lib patches runtime scripts test tools utils unittests | docker build -t alive2ce -
container=$(/usr/bin/docker run -d alive2ce true)
docker export $container | docker import - alive2ce_squashed
docker build -t alive2ce_final - < Dockerfile.metadata
docker tag alive2ce_final regehr/alive2ce

# docker push regehr/alive2ce
