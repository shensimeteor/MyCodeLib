# ------------------------------------------------------------------------------
#                              MODIFIABLE SECTION
#
# ------------------------------------------------------------------------------
# project, compiler and library configuration
PROJECT = tests
PROJECT_ROOT = /disk1/home/shensi/MyCodes/Fortran/utility/tests

FC = ifort
FFLAGS = -O0 -g -check all


# ------------------------------------------------------------------------------
#                             DON'T TOUCH SECTION
#
# ------------------------------------------------------------------------------
# objects, targets and libraries
VPATH = \
    $(PROJECT_ROOT)/ \
    $(PROJECT_ROOT)/.codemate/processed_codes

OBJECTS = utility.o ArgsManager.o ArgsManager.o test_filepath.o

EXE_TARGETS = test_filepath

# DO NOT MODIFY THEM, SET THEM WHEN INVOKING CODEMATE SCAN
DEPEND_MACROS =

all: start $(EXE_TARGETS) end

utility.o:
ArgsManager.o: utility.o
ArgsManager.o: utility.o
test_filepath.o: utility.o ArgsManager.o

.SECONDEXPANSION:

OBJECTS_test_filepath = ArgsManager.o utility.o test_filepath.o

INCLUDES =
LIBRARIES =

# ------------------------------------------------------------------------------
# implicit building rules
define fortran_implicit_rules
%.o: %.$(1)
	@echo " Creating dependency $$@"
	@echo $$(seperator)
	@TEMPLATE_PATTERN='.*\.t\.$(1)'; \
	 if [[ $$$$(dirname $$<) == '.' && '$$<' =~ $$$$TEMPLATE_PATTERN ]]; then \
	     SRC=$(PROJECT_ROOT)/.codemate/processed_codes/$$<; \
	 else \
	     SRC=$$<; \
	 fi; \
	 $(FC) -c $$$$SRC $(OPTIONS) $(DEPEND_MACROS) $(FFLAGS) $(INCLUDES)
%.t.F90: %.$(1)
	@echo " Processing template $$@"
	@echo $$(seperator)
	@codemate -silence scan $$< 2> $(PROJECT_ROOT)/.codemate/processed_codes/$$@
endef
fortran_suffices := f90 F90
$(foreach suffix, $(fortran_suffices), \
    $(eval $(call fortran_implicit_rules,$(suffix))))

# ------------------------------------------------------------------------------
# target building rules
define target_build_rules
$(1): $$$$(OBJECTS_$(1))
	@echo " Creating target '$(1)'"
		@if [ "$(OPTIONS)" != "" ]; then \
			echo " Configurations:"; \
			echo "   $(OPTIONS)"; \
		fi
	@$$(FC) -o $(1) $$(OBJECTS_$(1)) \
		$(OPTIONS) $$(FFLAGS) $(INCLUDES) $(LIBRARIES) && \
		echo " ---> $(1) is created."
	@echo $$(seperator)
endef
$(foreach target, $(EXE_TARGETS), \
    $(eval $(call target_build_rules,$(target))))

# ------------------------------------------------------------------------------
# auxiliary rules
seperator = "-------------------------------------------------------------"

.PHONY: clean start end
clean:
	@echo $(seperator)
	@if ls *.o > /dev/null 2>&1; then rm *.o; fi
	@if ls *.mod > /dev/null 2>&1; then rm *.mod; fi
	@if ls *.i90 > /dev/null 2>&1; then rm *.i90; fi
	@if ls *.cmm > /dev/null 2>&1; then rm *.cmm; fi
	@for target in $(EXE_TARGETS); do \
		if ls $$target > /dev/null 2>&1; then \
			rm $$target; \
		fi; \
	done
	@echo " Project has been cleaned."
	@echo $(seperator)

start:
	@echo $(seperator)
	@if test "$(PROJECT)" = ""; then \
		echo " Project powered by CodeMate!"; \
	else \
		echo " Project: >>> $(PROJECT) <<<"; \
	fi
	@echo $(seperator)

end:
	@echo " Finished"
	@echo $(seperator)
