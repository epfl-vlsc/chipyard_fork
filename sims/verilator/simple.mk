#########################################################################################
# verilator makefile
#########################################################################################
ifeq ($(shell which verilator),)
$(error Did not find Verilator in PATH. Make sure all requirements are installed)
endif

#########################################################################################
# general path variables
#########################################################################################
base_dir=$(abspath ../..)
sim_dir=$(abspath .)

#########################################################################################
# include shared variables
#########################################################################################
include $(base_dir)/variables.mk

#########################################################################################
# name of simulator (used to generate *.f arguments file)
#########################################################################################
sim_name = verilator

#########################################################################################
# verilator simulator types and rules
#########################################################################################
sim_prefix = simulator
sim = $(sim_dir)/$(sim_prefix)-$(MODEL_PACKAGE)-$(CONFIG)
sim_debug = $(sim_dir)/$(sim_prefix)-$(MODEL_PACKAGE)-$(CONFIG)-debug
sim_source_tarball = $(sim_dir)/$(CONFIG)_sources.tar.gz

# include $(base_dir)/sims/common-sim-flags.mk

# If verilator seed unspecified, verilator uses srand as random seed
ifdef RANDOM_SEED
SEED_FLAG=+verilator+seed+I$(RANDOM_SEED)
else
SEED_FLAG=
endif

.PHONY: default debug source_tar

default: $(sim)
debug: $(sim_debug)

#########################################################################################
# simulaton requirements
#########################################################################################
SIM_FILE_REQS += \
	SimpleTestDriver.v

# copy files and add -FI for *.h files in *.f
$(sim_files): $(SIM_FILE_REQS) $(ALL_MODS_FILELIST) | $(GEN_COLLATERAL_DIR)
	cp -f $(SIM_FILE_REQS) $(GEN_COLLATERAL_DIR)
	$(foreach file,\
		$(SIM_FILE_REQS),\
		$(if $(filter %.h,$(file)),\
			echo "-FI $(addprefix $(GEN_COLLATERAL_DIR)/, $(notdir $(file)))" >> $@;,\
			echo "$(addprefix $(GEN_COLLATERAL_DIR)/, $(notdir $(file)))" >> $@;))

#########################################################################################
# import other necessary rules and variables
#########################################################################################
include $(base_dir)/common.mk

#########################################################################################
# verilator-specific user-interface variables and commands
#########################################################################################
HELP_COMPILATION_VARIABLES += \
"   VERILATOR_PROFILE      = 'none' if no verilator profiling (default)" \
"                            'all' if full verilator runtime profiling" \
"                            'threads' if runtime thread profiling only" \
"   VERILATOR_THREADS      = how many threads the simulator will use (default 1)" \
"   USE_FST                = set to '1' to build Verilator simulator to emit FST instead of VCD."

HELP_SIMULATION_VARIABLES += \
"   USE_FST                = set to '1' to run Verilator simulator emitting FST instead of VCD."

#########################################################################################
# verilator/cxx binary and flags
#########################################################################################
CPP_HARNESS = $(sim_dir)/SimpleSim.cpp

VERILATOR := verilator --cc --exe

#----------------------------------------------------------------------------------------
# user configs
#----------------------------------------------------------------------------------------
VERILATOR_PROFILE ?= none
RUNTIME_PROFILING_CFLAGS := $(if $(filter $(VERILATOR_PROFILE),all),-g -pg,)
RUNTIME_PROFILING_VFLAGS := $(if $(filter $(VERILATOR_PROFILE),all),\
                              --prof-threads --prof-cfuncs,\
                              $(if $(filter $(VERILATOR_PROFILE),threads),\
								--prof-threads,))

VERILATOR_THREADS ?= 1
RUNTIME_THREADS := --threads $(VERILATOR_THREADS) --threads-dpi all

USE_FST ?= 0
TRACING_OPTS := $(if $(filter $(USE_FST),0),\
	                  --trace,--trace-fst --trace-threads 1)
# TODO: consider renaming +vcdfile in TestDriver.v to +waveformfile (or similar)
get_waveform_flag = +vcdfile=$(1).$(if $(filter $(USE_FST),0),vcd,fst)

#----------------------------------------------------------------------------------------
# verilation configuration/optimization
#----------------------------------------------------------------------------------------
# we initially had --noassert for performance, but several modules use
# assertions, including dramsim, so we enable --assert by default
VERILATOR_OPT_FLAGS ?= \
	-O3 \
	--x-assign 0

TB = Main

VERILATOR_PREPROC_DEFINES = \
	-DRESET_DELAY=10 \
	-DPRINTF_COND=$(TB).printf_cond \
	-DSTOP_COND=!$(TB).reset \
	-DMODEL=$(MODEL) \
	-URANDOMIZE_MEM_INIT \
	-URANDOMIZE_REG_INIT \
	-URANDOMIZE_GARBAGE_ASSIGN \
	-URANDOMIZE_INVALID_ASSIGN


VERILATOR_SUPPRESS_FLAGS = \
	-Wno-WIDTH

VERILATOR_NONCC_OPTS = \
	$(VERILATOR_OPT_FLAGS) \
	$(VERILATOR_PREPROC_DEFINES) \
	$(VERILATOR_SUPPRESS_FLAGS) \
	--top-module $(TB) \
	-f $(sim_common_files) \
	$(CPP_HARNESS)

#----------------------------------------------------------------------------------------
# gcc configuration/optimization
#----------------------------------------------------------------------------------------
VERILATOR_CXXFLAGS =

VERILATOR_LDFLAGS = $(SIM_LDFLAGS)

#----------------------------------------------------------------------------------------
# full verilator+gcc opts
#----------------------------------------------------------------------------------------
VERILATOR_OPTS = $(VERILATOR_NONCC_OPTS)

#########################################################################################
# verilator build paths and file names
#########################################################################################
model_dir = $(build_dir)/$(long_name)
model_dir_debug = $(build_dir)/$(long_name).debug

model_header = $(model_dir)/V$(TB).h
model_header_debug = $(model_dir_debug)/V$(TB).h

model_mk = $(model_dir)/V$(TB).mk
model_mk_debug = $(model_dir_debug)/V$(TB).mk

#########################################################################################
# build makefile fragment that builds the verilator sim rules
#########################################################################################
$(model_mk): $(sim_common_files) $(EXTRA_SIM_REQS)
	rm -rf $(model_dir)
	mkdir -p $(model_dir)
	$(VERILATOR) $(VERILATOR_OPTS) $(EXTRA_SIM_SOURCES) -o $(sim) -Mdir $(model_dir)
	touch $@

$(model_mk_debug): $(sim_common_files) $(EXTRA_SIM_REQS)
	rm -rf $(model_dir_debug)
	mkdir -p $(model_dir_debug)
	$(VERILATOR) $(VERILATOR_OPTS) +define+DEBUG $(EXTRA_SIM_SOURCES) -o $(sim_debug) $(TRACING_OPTS) -Mdir $(model_dir_debug)
	touch $@

#########################################################################################
# invoke make to make verilator sim rules
#########################################################################################
$(sim): $(model_mk) $(dramsim_lib)
	$(MAKE) VM_PARALLEL_BUILDS=1 -C $(model_dir) -f V$(TB).mk

$(sim_debug): $(model_mk_debug) $(dramsim_lib)
	$(MAKE) VM_PARALLEL_BUILDS=1 -C $(model_dir_debug) -f V$(TB).mk

$(sim_source_tarball): $(sim_common_files)
	tar -T $(sim_common_files)  -czf $@


source_tar: $(sim_source_tarball)

#########################################################################################
# create a verilator vpd rule
#########################################################################################
.PRECIOUS: $(output_dir)/%.vpd %.vcd
$(output_dir)/%.vpd: $(output_dir)/% $(sim_debug)
	rm -f $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	(set -o pipefail && $(NUMA_PREFIX) $(sim_debug) $(PERMISSIVE_ON) $(SIM_FLAGS) $(EXTRA_SIM_FLAGS) $(SEED_FLAG) $(VERBOSE_FLAGS) -v$@.vcd $(PERMISSIVE_OFF) $< </dev/null 2> >(spike-dasm > $<.out) | tee $<.log)

#########################################################################################
# general cleanup rules
#########################################################################################
.PHONY: clean clean-sim clean-sim-debug
clean:
	rm -rf $(CLASSPATH_CACHE) $(gen_dir) $(sim_prefix)-*

clean-sim:
	rm -rf $(model_dir) $(sim)

clean-sim-debug:
	rm -rf $(model_dir_debug) $(sim_debug)
