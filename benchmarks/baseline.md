# VM Benchmark Baseline

Baseline command:

```sh
./scripts/bench-vm.sh
```

Captured on local development machine in release mode.

| file | compile_ms | exec_ms | profile_ms | instr | regs | frames | heap | calls | imports | result |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| closure_counter.monkey | 0.886 | 42.494 | 134.091 | 1,000,018 | 3 | 2 | 6 | 100,001 | 0 | 15000150000 |
| for_in_list.monkey | 0.254 | 13.519 | 34.244 | 240,009 | 1 | 1 | 30,002 | 0 | 0 | 449985000 |
| function_loop.monkey | 0.079 | 35.282 | 423.390 | 4,065,008 | 4 | 2 | 5,002 | 5,000 | 0 | 99500000 |
| function_recursion.monkey | 0.164 | 55.556 | 113.902 | 628,008 | 5 | 52 | 102,003 | 102,000 | 0 | 2550000 |
| import_stdlib.monkey | 0.523 | 3.292 | 2.808 | 16,428 | 4 | 2 | 2,064 | 28 | 3 | 1999011 |
| json_parse.monkey | 0.073 | 3.541 | 5.713 | 25,009 | 3 | 1 | 7,004 | 1,000 | 1 | 19000 |
| while_sum.monkey | 0.006 | 57.997 | 686.917 | 8,000,004 | 1 | 1 | 1 | 0 | 0 | 1999999000000 |
| total | 1.984 | 211.682 | 1,401.065 | 13,974,484 | | | 146,082 | 208,029 | 4 | |

`compile_ms` measures root module VM compilation after parsing. `exec_ms` is normal VM execution without profiling. `profile_ms` is a separate profiled execution used to collect instruction, heap, call, and import counters; it is intentionally much slower and should not be used as the primary performance number.

Elapsed time is machine-dependent. Instruction counts, register/frame peaks, heap counts, calls, imports, and results should remain stable unless VM code generation or runtime semantics change intentionally.
