# newLISP makefile for Android NDK
# SysV IPC with 'semaphore', readline and utf8 support have been disabled
# the ANDROID compile flag controls code changes in some of the source files
# -DSUPPORT_UTF8 can be added if required, then also add nl-utf8.c

LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE := newlisp
LOCAL_CFLAGS := -Wall -Wno-long-long -Wno-strict-aliasing -O2 -c -DLINUX -DANDROID -DSUPPORT_UTF8
LOCAL_LDLIBS := -lm -ldl
LOCAL_SRC_FILES := newlisp.c nl-symbol.c nl-math.c nl-list.c nl-liststr.c nl-string.c \
nl-sock.c nl-import.c nl-xml-json.c nl-web.c nl-matrix.c nl-debug.c pcre.c nl-filesys.c nl-utf8.c
include $(BUILD_EXECUTABLE)

