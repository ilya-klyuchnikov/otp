#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1997-2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%

include $(ERL_TOP)/make/target.mk

EBIN = ../../ebin
BIN  = ../../bin

include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk

VSN = $(SNMP_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/snmp-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ESCRIPT_BIN  = $(ESCRIPT_SRC:%.src=$(BIN)/%)
ERL_FILES    = $(MODULES:%=%.erl)
EBIN_FILES   = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) 
TARGET_FILES = $(EBIN_FILES) $(ESCRIPT_BIN)

GENERATED_PARSER = $(PARSER_MODULE:%=%.erl)

PARSER_TARGET = $(PARSER_MODULE).$(EMULATOR)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/snmp/ebin

ifeq ($(SNMP_WARNING_AS_ERROR),)
  ERL_COMPILE_FLAGS += -Werror
else
  ifeq ($(SNMP_WARNING_AS_ERROR),true)
    ERL_COMPILE_FLAGS += -Werror
  endif
endif

ifeq ($(WARN_UNUSED_VARS),)
  ERL_COMPILE_FLAGS += +warn_unused_vars
else
  ifeq ($(WARN_UNUSED_VARS),true)
    ERL_COMPILE_FLAGS += +warn_unused_vars
  endif
endif

ERL_COMPILE_FLAGS += -I../../include \
                     -Dversion=\"$(VSN)$(PRE_VSN)\" \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}' \
                     -I$(ERL_TOP)/lib/stdlib \
                     +nowarn_missing_spec_documented

YRL_FLAGS = -o .

ifeq ($(ERL_DETERMINISTIC),yes)
	YRL_FLAGS += +deterministic
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(filter-out opt, $(TYPES)):
	@${MAKE} TYPE=$@ opt

opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) $(GENERATED_PARSER)
	rm -f core *~

docs:

info:
	@echo "PARSER_SRC:       $(PARSER_SRC)"
	@echo "PARSER_MODULE:    $(PARSER_MODULE)"
	@echo ""
	@echo "GENERATED_PARSER: $(GENERATED_PARSER)"
	@echo "PARSER_TARGET:    $(PARSER_TARGET)"
	@echo ""
	@echo "MODULES:      $(MODULES)"
	@echo ""
	@echo "TARGET_FILES: $(TARGET_FILES)"
	@echo ""
	@echo "EBIN: $(EBIN)"
	@echo ""
	@echo "ESCRIPT_SRC:      $(ESCRIPT_SRC)"
	@echo "ESCRIPT_BIN:      $(ESCRIPT_BIN)"
	@echo ""
	@echo ""


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

parser: $(PARSER_TARGET)

$(GENERATED_PARSER): $(PARSER_SRC)


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/src/compiler"
	$(INSTALL_DATA) $(ESCRIPT_SRC) $(PARSER_SRC) $(ERL_FILES) $(INTERNAL_HRL_FILES) "$(RELSYSDIR)/src/compiler"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(EBIN_FILES) "$(RELSYSDIR)/ebin"
	$(INSTALL_DIR) "$(RELSYSDIR)/bin"
	$(INSTALL_SCRIPT) $(ESCRIPT_BIN) "$(RELSYSDIR)/bin"

release_docs_spec:

include depend.mk

