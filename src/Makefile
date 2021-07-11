all: debug

# Warn on everything, halt early, all the checks, debug symbols in build
debug_args = -E -O0 --halt-at-warn --print-errors-warnings-when-generating-interface --no-infer-det  --target-debug --warn-insts-with-functors-without-type --warn-unused-imports --warn-interface-imports-in-parents --warn-missing-opt-files --warn-missing-trans-opt-files  --warn-non-contiguous-foreign-procs --warn-duplicate-calls --warn-implicit-stream-calls  --warn-obvious-non-tail-recursion  --warn-unknown-format-calls --warn-dead-procs --warn-dead-preds --warn-suspicious-recursion --warn-suspicious-foreign-procs --warn-suspicious-foreign-code  --warn-inconsistent-pred-order --warn-inconsistent-pred-order-foreign-procs --inform-ite-instead-of-switch --inform-incomplete-switch --inform-suboptimal-packing --cflag -g 

debug: mlatu.m 
		mmc --make mlatu --use-grade-subdirs $(debug_args)

# All the optimizations, we don't care about time
release_args = --no-verbose-make --rebuild -O6 --intermodule-optimization --constraint-propagation --control-granularity --no-ansi-c --intermod-unused-args --trace-optimized --profile-optimized --no-delay-death --no-fully-strict --infer-all

release: clean mlatu.m
		mmc --make mlatu --use-grade-subdirs $(release_args)

install: release
		cp $(realpath ./mlatu) ~/.local/bin

clean: 
		rm -f ./mlatu *.err *.mh 
		rm -r Mercury