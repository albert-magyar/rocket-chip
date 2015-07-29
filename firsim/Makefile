#=======================================================================
# Makefile for Verilog simulation w/ VCS
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
# This makefile will build a rtl simulator and run various tests to
# verify proper functionality.
#

default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
mem_gen = $(base_dir)/firsim/vlsi_mem_gen
sim_dir = .
output_dir = $(sim_dir)/output

BACKEND ?= rocketchip.RocketChipBackendLoFIRRTL
CONFIG ?= DefaultVLSIConfig
TB ?= rocketTestHarness
FIR ?= 1

include $(base_dir)/Makefrag
include $(sim_dir)/Makefrag
-include $(generated_dir)/Makefrag-tests.$(CONFIG)
include $(base_dir)/vsim/Makefrag-sim

all: $(simv)
debug: $(simv_debug)


fir:
	cd $(base_dir) && mkdir -p $(generated_dir) && $(SBT) "project rocketchip" "elaborate $(MODEL) --backend $(BACKEND) --targetDir $(generated_dir) --W0W --configDump --noInlineMem --configInstance rocketchip.$(CONFIG)";

fir-v: $(fir)
	firrtl -i $(generated_dir)/$(MODEL).$(CONFIG).fir -o $(generated_dir)/$(MODEL).$(CONFIG).v -X verilog
	cd $(generated_dir) && \
	if [ -a $(MODEL).$(CONFIG).conf ]; then \
	  $(mem_gen) $(generated_dir)/$(MODEL).$(CONFIG).conf >> $(generated_dir)/$(MODEL).$(CONFIG).v; \
	fi


clean:
	rm -rf $(junk) simv* csrc *.key DVE* *.h *.a *.daidir $(generated_dir)

.PHONY: default all debug clean
