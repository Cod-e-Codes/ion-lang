#!/usr/bin/env bash
# Generate a large .ion file and compile the emitted C with -Wall -Wextra -Werror.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COMPILER="${COMPILER:-$ROOT/target/release/ion-compiler}"
CC="${CC:-gcc}"
STRUCT_COUNT="${STRUCT_COUNT:-40}"
SPAWN_COUNT="${SPAWN_COUNT:-20}"

OUT_ION="$SCRIPT_DIR/test_large_codegen_smoke.ion"
OUT_C="${OUT_ION%.ion}.c"

cat >"$OUT_ION" <<'HEADER'
enum Option<T> {
    Some(T);
    None;
}

HEADER

i=0
while [ "$i" -lt "$STRUCT_COUNT" ]; do
    cat >>"$OUT_ION" <<EOF
struct Payload$i {
    id: int;
    tag: int;
    data: Vec<int>;
}

EOF
    i=$((i + 1))
done

cat >>"$OUT_ION" <<'MID'

fn build_payload(id: int) -> Payload0 {
    let mut data: Vec<int> = Vec::new();
    Vec::push(&mut data, id);
    return Payload0 { id: id, tag: id * 2, data: data };
}

fn main() -> int {
    let (job_tx, job_rx): (Sender<int>, Receiver<int>) = channel<int>();
    let (done_tx, done_rx): (Sender<int>, Receiver<int>) = channel<int>();

    spawn {
        let mut rx: Receiver<int> = job_rx;
        let mut sum: int = 0;
        let mut n: int = 0;
        while n < SPAWN_PLACEHOLDER {
            sum = sum + recv(&mut rx);
            n = n + 1;
        }
        send(&done_tx, sum);
    };

MID

# Replace placeholder with actual spawn count
sed -i.bak "s/SPAWN_PLACEHOLDER/$SPAWN_COUNT/" "$OUT_ION" && rm -f "$OUT_ION.bak"

i=0
while [ "$i" -lt "$SPAWN_COUNT" ]; do
    echo "    send(&job_tx, $i);" >>"$OUT_ION"
    i=$((i + 1))
done

cat >>"$OUT_ION" <<'FOOTER'

    let mut items: Vec<Payload0> = Vec::new();
    let built: Payload0 = build_payload(7);
    Vec::push(&mut items, built);

    let mut done_rx_mut: Receiver<int> = done_rx;
    let total: int = recv(&mut done_rx_mut);
    if total != EXPECTED_SUM_PLACEHOLDER {
        return 1;
    }
    return 0;
}
FOOTER

expected_sum=$((SPAWN_COUNT * (SPAWN_COUNT - 1) / 2))
sed -i.bak "s/EXPECTED_SUM_PLACEHOLDER/$expected_sum/" "$OUT_ION" && rm -f "$OUT_ION.bak"

echo "Generated $OUT_ION ($STRUCT_COUNT structs, $SPAWN_COUNT spawn sends)"

"$COMPILER" "$OUT_ION" >/dev/null

CFLAGS="${CFLAGS:--Wall -Wextra -Werror}"
RUNTIME_CFLAGS="$CFLAGS"
case "$(uname -s 2>/dev/null)" in
    MINGW*|MSYS*|CYGWIN*) RUNTIME_CFLAGS="${CFLAGS} -Wno-error=unknown-pragmas" ;;
esac
RUNTIME_OBJ="${RUNTIME_OBJ:-$SCRIPT_DIR/.large_codegen_runtime.o}"

"$CC" $RUNTIME_CFLAGS -c "$ROOT/runtime/ion_runtime.c" -I"$ROOT" -I"$ROOT/runtime" -o "$RUNTIME_OBJ"
"$CC" $CFLAGS -c "$OUT_C" -I"$ROOT" -I"$ROOT/runtime" -o "$SCRIPT_DIR/test_large_codegen_smoke.o"

echo "PASS: large codegen smoke (compile only)"
