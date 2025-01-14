#!/bin/bash

cd "$(dirname "$0")"
if ! [[ -d build ]]; then echo "$0 should be run after build.sh" >&2; exit 1; fi

VARNISH_DIR=$(mktemp -d -t aslp-server-varnish.XXXX)
echo "VARNISH_DIR=$VARNISH_DIR"
mkdir -p "$VARNISH_DIR"

trap "trap - SIGTERM && rm -rfv $VARNISH_DIR && kill -- -$$" SIGINT SIGTERM EXIT

cd build

# write varnish cache configuration file
cat <<EOF >$VARNISH_DIR/default.vcl
vcl 4.0;

backend default {
  .host = "127.0.0.1";
  .port = "8100";
}

sub vcl_recv {
  # aslp bridge checks / to see if the server is up, make this very fast.
  if (req.url == "/") {
    return(synth(200));
  }
  if (req.url == "/favicon.ico") {
    return(synth(404));
  }
}

sub vcl_backend_response {
  set beresp.ttl = 24h;
}
EOF

aslp/bin/aslp-server --port 8100 &
pid=$!
sleep 1
if ! kill -0 "$pid"; then
  echo 'aslp-server exited suspiciously soon... quitting in case of initialisation error.' >&2
  exit 1
fi

echo
./varnish/bin/varnishd -F -a localhost:8000 -n $VARNISH_DIR -f $VARNISH_DIR/default.vcl -s memory=malloc,4G


: <<'END_COMMENT'
admin console:
build/varnish/bin/varnishadm -n $VARNISH_DIR

logs:
build/varnish/bin/varnishlog -n $VARNISH_DIR -b

table of statistics:
build/varnish/bin/varnishstat -n $VARNISH_DIR

list of fetched URLS:
build/varnish/bin/varnishtop -I ReqURL:opcode -n $VARNISH_DIR
END_COMMENT
