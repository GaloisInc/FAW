#############################################################################
# DIRECTORIES TO USE
#############################################################################
ROOT_DIR:=../
BIN_DIR:=$(ROOT_DIR)/bin
BUILD_DIR:=$(ROOT_DIR)/build/
INCLUDE_DIRS:=. \
$(ROOT_DIR)/include 

LIBRARY_DIRS:=. \
../lib/ 


#_LD_LIB_PATHS=$(ROOT_DIR)/lib/opendds/gnu/:$(ROOT_DIR)/lib/gstreamer/gnu:$(LD_LIBRARY_PATH)
#LD_LIB_PATHS=$(patsubst ' ','',$(_LD_LIB_PATHS))

SOURCE_DIRS:=. \
$(ROOT_DIR)/src/

#############################################################################
#  SRC/OBJS/LIBS LIST
#############################################################################
C_SRCS:=\
des.c\
df_read.c\
dfparse.c\
extract.c\
graphic.c\
image.c\
j_io.c\
j_string.c\
label.c\
md5c.c\
menu.c\
rbd_jpeg.c\
symbol.c\
text.c\
tre.c\
unblock.c\
write_fs.c\
write_text.c
CPP_SRCS:= 
STATIC_LIBS:=
DEBUG_LIBS:= 
RELEASE_LIBS:= 
TARGET:=extract


#############################################################################
# BASELINE FLAG (PREPROCESSOR, COMPILE, LINK)
#############################################################################
DEFINES= -DUSE_MD5
CC:= g++
CPP:= g++

#DLB Changes
#CC:= afl-gcc
#CC:= afl-g++
#CPP:= afl-g++
#DLB Changes

CFLAGS:= -Wall -fPIC -g -Wno-unknown-pragmas -Wno-format-security
CPPFLAGS:= -Wall -fPIC -g -Wno-unknown-pragmas -Wno-format-security
LDFLAGS:= 
INC_DIRS=$(addprefix -I,$(INCLUDE_DIRS))
LIB_DIRS=$(addprefix -L,$(LIBRARY_DIRS))
VPATH:=$(shell echo $(SOURCE_DIRS) | sed "s/ \+/:/g")


#############################################################################
# DEBUG / RELEASE 
#############################################################################
_OBJS:= $(C_SRCS:.c=.o) $(CPP_SRCS:.cpp=.o)
DBG_CFLAGS= $(CFLAGS) -O0
DBG_CPPFLAGS= $(CPPFLAGS) -O0
DBG_DEFINES= $(DEFINES) -DDEBUG
DBG_BUILD_DIR= $(BUILD_DIR)/debug/gnu/
DBG_OBJS= $(patsubst %.o, $(DBG_BUILD_DIR)/%.o, $(notdir $(_OBJS)))
DBG_DYN_LIBS = $(addprefix -l,$(DYNAMIC_LIBS)) 
DBG_STATIC_LIBS = $(addsuffix .a, $(addprefix -l:lib,$(STATIC_LIBS) $(DEBUG_LIBS)))
DBG_TARGET= $(TARGET)_d

REL_CFLAGS= $(CFLAGS) -O2
REL_CPPFLAGS= $(CPPFLAGS) -O2
REL_DEFINES= $(DEFINES) -DNDEBUG
REL_BUILD_DIR= $(BUILD_DIR)/release/gnu/
REL_OBJS= $(patsubst %.o, $(REL_BUILD_DIR)/%.o, $(notdir $(_OBJS)))
REL_DYN_LIBS = $(addprefix -l,$(DYNAMIC_LIBS)) 
REL_STATIC_LIBS = $(addsuffix .a, $(addprefix -l:lib,$(STATIC_LIBS) $(RELEASE_LIBS)))
REL_TARGET= $(TARGET)

#############################################################################
# RULES
#############################################################################
# DEBUG RULES
$(DBG_TARGET): $(DBG_OBJS) | make_bindir
	@echo "BUILDING DEBUG TARGET = $(DBG_TARGET)"
	$(CC) $(LDFLAGS) $(DBG_OBJS) $(LIB_DIRS) $(DBG_STATIC_LIBS) -Wl,-Bdynamic $(DBG_DYN_LIBS) -o $(BIN_DIR)/$@

$(DBG_BUILD_DIR)/%.o: %.c $(DEPENDS) | make_dbg_builddir
	$(CC) $(DBG_CFLAGS) $(DBG_DEFINES) $(INC_DIRS) $< -c -o $@

$(DBG_BUILD_DIR)/%.o: %.cpp $(DEPENDS) | make_dbg_builddir
	@#@echo "COMPILING DEBUG OBJECT = $@"
	$(CPP) $(DBG_CPPFLAGS) $(DBG_DEFINES) $(INC_DIRS) $< -c -o $@

# RELEASE RULES
$(REL_TARGET): $(REL_OBJS)
	@echo "BUILDING RELEASE TARGET = $(REL_TARGET)"
	$(CC) $(LDFLAGS) $(REL_OBJS) $(LIB_DIRS) $(REL_STATIC_LIBS) -Wl,-Bdynamic $(REL_DYN_LIBS) -o $(BIN_DIR)/$@

$(REL_BUILD_DIR)/%.o: %.c $(DEPENDS) | make_rel_builddir
	@#@echo "COMPILING RELEASE OBJECT = $@"
	$(CC) $(REL_CFLAGS) $(REL_DEFINES) $(INC_DIRS) $< -c -o $@

$(REL_BUILD_DIR)/%.o: %.cpp $(DEPENDS) | make_rel_builddir
	@#@echo "COMPILING RELEASE OBJECT = $@"
	$(CPP) $(REL_CPPFLAGS) $(REL_DEFINES) $(INC_DIRS) $< -c -o $@

# COMMON RULES
clean: clean_debug clean_release
clean_debug:
	@echo "CLEANING DEBUG TARGET = $(DBG_TARGET)"
	rm -rf $(DBG_BUILD_DIR)/*.o $(LIB_DIR)/$(DBG_TARGET)
clean_release:
	@echo "CLEANING RELEASE TARGET = $(REL_TARGET)"
	rm -rf $(REL_BUILD_DIR)/*.o $(LIB_DIR)/$(REL_TARGET)
make_dbg_builddir:
	mkdir -p $(DBG_BUILD_DIR)
make_rel_builddir:
	mkdir -p $(REL_BUILD_DIR)
make_bindir:
	mkdir -p $(BIN_DIR)

#############################################################################
# TARGETS
#############################################################################
.PHONY: all clean
debug: $(DBG_TARGET)
	@echo "*******       DEBUG BUILD FINISHED    *******\n"
release: $(REL_TARGET)
	@echo "*******       RELEASE BUILD FINISHED    *******\n"
all: debug release 

#############################################################################
# VPATH for each file type
#############################################################################
#vpath %.h $(subst $(space),':',$(strip $INCLUDE_DIRS))
#vpath %.c $(subst $(space),':',$(strip $SOURCE_DIRS))
#vpath %.cpp $(subst $(space),':',$(strip $SOURCE_DIRS))
#vpath %.cpp $(ROOT_DIR)/test/src/dds_api_test/:$(ROOT_DIR)/test/src/dds_api_test/msg
#vpath %.o $(DBG_BUILD_DIR)/:$(REL_BUILD_DIR)/












