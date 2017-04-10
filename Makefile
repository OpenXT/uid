#
# Copyright (c) 2011 Citrix Systems, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

include common.make

PACKAGES = str,uuid,stdext,log,bigarray,camomile,json,jsonrpc,http,dbus,tscommon,xenmmap,xenbus,xenstore
OCAMLC   = ocamlfind ocamlc -linkpkg -package $(PACKAGES)
OCAMLOPT = ocamlfind ocamlopt -linkpkg -package $(PACKAGES)

OCAMLOPTFLAGS += -thread

INTF = dbus_signals.cmi httpserver.cmi
uid_OBJS = utils ui_config dBus_conv dbus_signals httpserver server dbus_interface file_handler dBus_handler main

ALL_OCAML_OBJS = $(uid_OBJS)

PROGRAMS = uid
OCAML_PROGRAM = uid

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

install:
	install -d $(DESTDIR)/usr/bin
	install uid $(DESTDIR)/usr/bin

include Makefile.rules
