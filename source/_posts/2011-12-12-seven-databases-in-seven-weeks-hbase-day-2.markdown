---
layout: post
title: "Seven Databases in Seven Weeks HBase Day 2"
date: 2011-12-12 23:46
comments: true
categories: [Books]
tags: [7databases7weeks, databases, hbase]
series: "Seven Databases in Seven Weeks"
---

### Setting up HBase

just download and unpack the distrib (0.9.4 now).
Important: ulimit -n 10240 (otherwise HBase was hanging on my
machine).

### LZO

http://wiki.apache.org/hadoop/UsingLzoCompression

# old site, but information almost correct
https://github.com/omalley/hadoop-gpl-compression/wiki/FAQ

Homebrew: defaults to 64 bits version, so we're good
sudo brew install lzo --use-gcc

# then download hadoop plugin (as it is the one we need to update):
# https://github.com/omalley/hadoop-gpl-compression
# this plugin is no longer maintained ...

# let's try this one
https://github.com/toddlipcon/hadoop-lzo

patch build.xml
use special invocation to start: use gcc-4.2 instead of llvm; set
CFLAGS to find both Java include files and lzo; ...
env
JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/
C_INCLUDE_PATH=/System/Library/Frameworks/JavaVM.framework/Headers:/usr/local/Cellar/lzo/2.06/include/
LIBRARY_PATH=/usr/local/Cellar/lzo/2.06/lib/ CFAGS='-arch x86_64'
CC=/usr/bin/gcc-4.2  ant clean compile-native test tar

Must copy build directory to right hbase directory (hbase scripts
override any environment properties)
build -> hbase/build; build/*.jar -> hbase/lib

