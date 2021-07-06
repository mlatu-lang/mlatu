all: debug

# Basic options
both_args = -m mlatu -s none.gc

debug_warns = --warn-insts-with-functors-without-type --warn-unused-imports --warn-interface-imports-in-parents --warn-missing-opt-files --warn-missing-trans-opt-files --warn-inconsistent-pred-order --warn-inconsistent-pred-order-foreign-procs --warn-non-contiguous-foreign-procs --warn-non-stratification --warn-duplicate-calls --warn-implicit-stream-calls --warn-suspicious-recursion --warn-obvious-non-tail-recursion --warn-dead-procs --warn-dead-preds --warn-unknown-format-calls --warn-suspicious-foreign-procs --warn-suspicious-foreign-code 

# Warn on everything, halt early, all the checks
debug_args = -E --halt-at-warn --print-errors-warnings-when-generating-interface  --inform-ite-instead-of-switch --inform-incomplete-switch --inform-suboptimal-packing --no-infer-det $(debug_warns)

# All the optimizations
release_args = -O6 --intermod-opt --loop-invariants --constraint-propagation --optimize-unused-args --unneeded-code --optimize-higher-order --type-specialization --optimize-constant-propagation --introduce-accumulators --optimize-constructor-last-call --optimize-dead-procs --excess-assign --optimize-duplicate-calls --delay-constructs --optimize-saved-vars --deforestation --analyse-exceptions --analyse-trail-usage --analyse-mm-tabling --optimize-dups --optimize-proc-dups --optimize-reassign --use-macro-for-redo-fail --inline-alloc

debug: mlatu.m 
		mmc $(both_args) $(debug_args)

release: mlatu.m
		mmc $(both_args) $(release_args)

install: release
		cp ./mlatu ~/.local/bin

clean: 
		rm -f ./mlatu *.err *.mh 
		rm -r Mercury