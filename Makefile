# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT
#
GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
BINDIR                 ?= $(PREFIX)/bin
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/markdown-ada
INSTALL_EXEC_DIR       ?= $(DESTDIR)$(BINDIR)
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/markdown-ada

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR) --exec-subdir=$(INSTALL_EXEC_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/markdown.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/markdown_driver.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/markdown.gpr -XHARDWARE_PLATFORM=x86_64
	# gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/markdown_driver.gpr --mode=usage -XHARDWARE_PLATFORM=x86_64

clean:
	gprclean -q -P gnat/markdown.gpr
	gprclean -q -P gnat/markdown_driver.gpr

