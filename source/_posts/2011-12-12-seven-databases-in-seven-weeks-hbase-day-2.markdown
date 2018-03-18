---
layout: post
title: "Seven Databases in Seven Weeks HBase Day 2"
date: 2011-12-12 23:46
comments: true
categories: [Books]
tags: [7databases7weeks, databases, hbase]
series: "Seven Databases in Seven Weeks"
---
And on the second day with HBase, we load it with [Wikipedia](http://www.wikipedia.org/). Actually I had to do it twice to get it to work: on my first attempt the process kind of froze at about 200,000 articles.

<!--more-->

After some digging (and finding this very helpful [page](http://ofps.oreilly.com/titles/9781449396107/installation.html) from [HBase: The Definitive Guide](http://ofps.oreilly.com/titles/9781449396107/)), I tried again with a different setting for the limit on open files:

```
ulimit -n 10240
```

With that, HBase was able to keep churning along (the limit is per session, so HBase had to be restarted). I started the import process in the morning, and when I finally stopped it it had passed 10,000,000 pages (not all of them actual articles). Parsing links was equally successful.

### Consistency

Unlike Riak, which offers [eventual consistency](http://en.wikipedia.org/wiki/Eventual_consistency), HBase ensures row level consistency. This means that each row has only one value, and a write to the row is either entirely successful, or not performed at all (so an update will never be applied partially).

This idea that each row is atomic is a simple yet effective mental model; I feel I should be able to use this model to design reliable solutions on HBase. To make them fast as well is a different matter entirely: I'd first need more experience with the concept of column families and their various options.

### Logging

HBase uses [Write-Ahead Logging](http://en.wikipedia.org/wiki/Write-ahead_logging), exactly like PostgreSQL and many other databases (Riak too) and file systems. This is a low level mechanism designed to help with consistency: first a description of the updates is written into a log file (and flushed); then the update is performed. If there's a problem during the update, it is always possible to compare the write-ahead log and execute again whatever updates are missing or partial.

### Regions and servers

I must say I am still a bit unclear on this topic: I have a standalone instance of HBase, so naturally there is no distribution involved.

HBase first keep the data sorted by key, and distributes contiguous chunks of data to each region (growing the number of regions if needed).


### HBase and names

In a typical relational database, just as in a normal programming language, the name you give to things (tables, columns or variables) is a programmer oriented feature that has no impact on performance.

The idea that you should use short variable names for 'performance reason' is either a joke or a beginner's mistake.

Except in HBase, where the length of names can impact storage performance. See the [HBase book, Try to minimize row and column sizes](http://hbase.apache.org/book.html#rowkey.design).

Exercises
---------

### Compression in HBase

I could not really find any article on the pros and cons of compression in either HBase or Hadoop. I guess the pros and cons here are the same as any other use of compression: trading IO for CPU. Smaller (because compressed) data can be saved to and read from the disk faster, but at the cost of higher CPU usage.

### Bloom filters

Bloom filters are describe on the always helpful [Wikipedia](http://en.wikipedia.org/wiki/Bloom_filter). Such a filter is a tool to determine quickly if a piece of information in not in a specific storage, with a configurable probability for false positive.

Say you have a key value distributed data store. For each store, you maintain a Bloom filter of the keys.

Assuming you are looking for a key, you can use the Bloom filters to quickly determine where to look further.

If a Bloom filter for a store states the key is not present, you know you can ignore the store. If it says the key is present, it could be wrong, so you have to look. How often it returns yes when it should say no is a trade-off between the size of the filter and the probability of error.

With HBase being distributed by default, knowing where to look for a key or a key, column pair can increase performance.

### Column family options for compression

There use to be `RECORD` and `BLOCK` options, but they appear deprecated. What is left is to specify the compression algorithm for either regular compression, or compacting compression (which happens when HBase reorganize the store). The compacting compression setting can use the same values (i.e. algorithm names) as the compression setting. In the shell, the option is `COMPRESSION_COMPACT`.

The available algorithms are `NONE` (no encryption), `GZ`, `LZO` and `SNAPPY` (which is probably better still than LZO).

### Column family compression design consideration

I could not find any definitive answer to this, but I would guess that:

 * already compressed data (such as JPEG) should be in an uncompressed column family
 * rarely used by very large data could use a slower but more efficient algorithm such as GZ
 * small but very often used families should not be compressed

### Installing LZO

To install LZO compression is not exactly trivial, especially on Mac OS X.

The first step is to install the library; I did it with [Homebrew](http://mxcl.github.com/homebrew/). It installs 64 bits versions by default; the only thing to remember is that by default on Mac OS X 10.7, the default compiler is [LLVM](http://llvm.org), but often [GCC](http://gcc.gnu.org/) is better.

```
sudo brew install lzo --use-gcc
```

and LZO will end up under `/usr/local/Cellar/lzo/2.06/`

Next step is to build the hadoop LZO plugin. The basic information is available on the Hadoop [wiki](http://wiki.apache.org/hadoop/UsingLzoCompression), but the main repository it refers to is obsolete. There is another, maintained [repository](https://github.com/toddlipcon/hadoop-lzo) on Github.

```
git clone https://github.com/toddlipcon/hadoop-lzo
```

#### Mac OS X

Building on Linux should work right away, but Mac OS X (especially 10.7) is slightly different in frustrating way. The `ld` command is not GNU, but BSD, so it does not understand the same options.

To get the library to compile, you need to edit the `build.xml` file and clear the `LDFLAGS` (by default the value is `-Wl,--no-as-needed`, it needs to be empty).

{% include_code hadoop-lzo.patch lang:text 7d7w/hbase/hadoop-lzo.patch %}

From inside the repository, it can be applied with

```
patch -p1 < hadoop-lzo.patch
```

Once this is done, the `ant` invocation documented in the Wiki should almost work. Two things need to be changed: first is the use of `GCC` instead of `LLVM` (by setting the `CC` variable); second is the strange name of the `include` directory for Java. The build script expects it under `$JAVA_HOME/include`, but of course in Mac OS X it had to be somewhere else (`/System/Library/Frameworks/JavaVM.framework/Headers`, if you need to know), so I added it directly to the include path `C_INCLUDE_PATH`:

```
env JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/ \
C_INCLUDE_PATH=/System/Library/Frameworks/JavaVM.framework/Headers:/usr/local/Cellar/lzo/2.06/include/ \
LIBRARY_PATH=/usr/local/Cellar/lzo/2.06/lib/ CFAGS='-arch x86_64' \
CC=/usr/bin/gcc-4.2  ant clean compile-native test tar
```

Normally, you should now have a `build` directory with the jar and native libraries.

The final step is to deploy this in HBase. HBase expect everything to be under the `$HBASE_HOME/lib`. The instructions from the wiki give the right commands (I just added the creation of the `$HBASE_HOME/lib/native` directory, which does not exist by default):

```
cp build/hadoop-lzo-0.4.15/hadoop-lzo-0.4.15.jar $HBASE_HOME/lib/
mkdir -p $HBASE_HOME/lib/native
tar -cBf - -C build/hadoop-lzo-0.4.15/lib/native/ . | tar -xBvf - -C $HBASE_HOME/lib/native
```

Now you can test whether the new library is enabled: run the command:

```
$ ./bin/hbase org.apache.hadoop.hbase.util.CompressionTest /tmp/data.lzo lzo
```

and it should output:

```
11/12/14 09:13:21 INFO lzo.GPLNativeCodeLoader: Loaded native gpl library
11/12/14 09:13:21 INFO lzo.LzoCodec: Successfully loaded & initialized native-lzo library [hadoop-lzo rev c7d54fffe5a853c437ee23413ba71fc6af23c91d]
11/12/14 09:13:21 INFO compress.CodecPool: Got brand-new compressor
SUCCESS
```

And that's it. The most frustrating part is that HBase will appear to hang when you try to enable a table that uses LZO compression if anything went wrong (and forgot to test as above). The logs will reveal that `hadoop-native` cannot be found. This means that the native libraries cannot be loaded. So make sure that you have all the files below:

```
$HBASE_HOME/lib/native/Mac_OS_X-x86_64-64/libgplcompression.0.dylib
$HBASE_HOME/lib/native/Mac_OS_X-x86_64-64/libgplcompression.a
$HBASE_HOME/lib/native/Mac_OS_X-x86_64-64/libgplcompression.dylib
$HBASE_HOME/lib/native/Mac_OS_X-x86_64-64/libgplcompression.la
```

After that, restart the server, and you can use LZO compression instead of GZ.

And this completes Day 2. Next and final day is about deploying HBase to the cloud. This might take more than just a day as I need some time to figure out how to use [AWS EC2](http://aws.amazon.com/ec2/) and which options to choose, but hopefully I'll be able to deploy Riak there as well.
