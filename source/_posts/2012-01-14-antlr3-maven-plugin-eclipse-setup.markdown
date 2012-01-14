---
layout: post
title: "ANTLR3 Maven Plugin - Eclipse setup"
date: 2012-01-14 11:25
comments: true
categories: ["How-To"]
tags: [howto, eclipse, maven, antlr3]
---
Setting up Eclipse and Maven is getting easier, but some cases
require a bit more search and work. As I was experimenting with the
[ANTLR](http://www.antlr.org/)
[Maven plugin](http://antlr.org/antlr3-maven-plugin/index.html), I
found the default behaviour to be pretty much useless: Eclipse knew
nothing about the grammar files or the generated classes, so the rest
would not compile; even after adding the relevant source folders I
still had to run explicit Maven commands after modifying the grammar
files and refresh the workspace...

I eventually found a better way, which I document here.

<!-- more -->

There is an
[antlr3-maven-archetype](http://www.antlr.org/wiki/display/ANTLR3/Building+ANTLR+Projects+with+Maven),
which I started from. However, for the purpose of clarity, I will
start from scratch here.

### Installing m2e

The Maven plugin for Eclipse is called m2e (m2eclipse is an obsolete
version), and is available in the default Eclipse
Marketplace. However, the current version (1.0 at the time of writing)
does not handle the life cycle of some common Maven plugins very
well. In particular, it does not know where to put the generation of
classes from grammar files into the Eclipse life cycle.

The 1.1 milestone does it much better, so I suggest to install it. The
location is
[http://download.eclipse.org/technology/m2e/milestones/1.1](http://download.eclipse.org/technology/m2e/milestones/1.1),
which can be used for the "Install New Software" function.

### Creating a project with ANTLR

Create a new Maven Project, and skip the archetype selection (i.e. use
simple project). As I said above, I could use the ANTLR v3 archetype,
but chose not to.

#### Optional: set the target option

By default Maven uses compiler source and target version 1.5. On Mac
OS X Lion, there is no JDK 1.5 (only 1.6), so I always update pom.xml
to set the `source` and `target` configuration options to something
meaningful:

{% codeblock lang:xml %}
<build>
  <plugins>
    <plugin>
      <groupId>org.apache.maven.plugins</groupId>
      <artifactId>maven-compiler-plugin</artifactId>
      <version>2.3.2</version>
      <configuration>
        <source>1.6</source>
        <target>1.6</target>
      </configuration>
    </plugin>
  </plugins>
</build>
{% endcodeblock %}

#### Add ANTLR plugin

I create a property for the ANTLR version, as I will need for both the
ANTLR plugin and the jar:

{% codeblock lang:xml %}
 <properties>
   <antlr.version>3.4</antlr.version>
 </properties>
{% endcodeblock %}

Then I add the plugin declaration

{% codeblock lang:xml %}
<plugin>
  <groupId>org.antlr</groupId>
  <artifactId>antlr3-maven-plugin</artifactId>
  <version>${antlr.version}</version>
  <executions>
    <execution>
  	  <goals>
        <goal>antlr</goal>
      </goals>
    </execution>
  </executions>
</plugin>
{% endcodeblock %}

Finally I add the dependency to the ANTLR runtime:

{% codeblock lang:xml %}
<dependencies>
  <dependency>
    <groupId>org.antlr</groupId>
  	<artifactId>antlr-runtime</artifactId>
  	<version>${antlr.version}</version>
  </dependency>
</dependencies>
{% endcodeblock %}

At this stage, Eclipse is upset because the lifecycle configuration
`org.antlr:antlr3-maven-plugin:3.4:antlr` is not covered. But as we're
using m2e 1.1, we can look for the appropriate connector in the m2e
Marketplace. There should be only one: antlr by Sonatype, which should
be installed.

#### Packaging the ANTLR runtime with the code

This is something that the original ANTLR v3 Maven archetype suggests:
to include the ANTLR runtime into the generated jar.

Using the
[Maven Assembly Plugin](http://maven.apache.org/plugins/maven-assembly-plugin/),
it is possible to declare what goes into the generated jar. As it is
self-contained, it is also possible to declare a main class (not done
below as I did not have a main class yet):

{% codeblock lang:xml %}
<plugin>
  <artifactId>maven-assembly-plugin</artifactId>
  <configuration>
    <descriptorRefs>
      <descriptorRef>jar-with-dependencies</descriptorRef>
    </descriptorRefs>
  </configuration>
  <executions>
    <execution>
	  <id>make-assembly</id>
      <phase>package</phase>
      <goals>
        <goal>attached</goal>
	  </goals>
    </execution>
  </executions>
</plugin>
{% endcodeblock %}

#### Tuning the Eclipse project

Now, the ANTLR plugin can process code under
`src/main/antlr3`, so we can create this folder, and add it as source
folder in the Eclipse project properties. Creating or updating a
grammar file in Eclipse will also create or update 

The ANTLR connector also added the `target/generated-sources/antlr3`
directory as another source folder, but it will disappear when
executing the Maven/Update Project Configuration action, so it is best
to add it manually. You can then change the properties for this folder
to check 'Locked' (to avoid accidental edition) and 'Derived' (to hide
the content from the "Open Resource" command).

Note that the plugin is unable to follow the `@header` directive
properly (that is, it will copy the directory structure of the grammar
file, instead of following the directory structure implied by the
`@header` directive), so the grammar files must use the same directory
structure as the Java package intended for the generated classes. In
other words, if you want your generated classes to have the package
`org.something`, you both need to put the grammar files under
`src/main/antlr3/org/something`, and use the `@header package`
directive to set the package of the generated classes.

It is also unable to handle grammar files directly under
`src/main/antlr3`. If you try, it will generate this error: "error(7):
cannot find or open file: null/NestedNameList.g" when running the
`process-sources` goal. Running this goal is also the only way to get
the error message if something is wrong with the grammar file (unless
you install an ANTLR Eclipse plugin, which I didn't try).

Small gotcha: I found that with the current version of plugins,
connectors and so on, Eclipse does not detect changes to generated
classes directly: it is always one change behind, especially when
there are errors.

If you made a mistake in the grammar file that
causes the generated classes not to compile anymore, you would have to
change the grammar file twice for the error markers to go away; the
first time, Eclipse will correctly report that the errors in the
classes are gone, but the project error markers will stay; the second
change (even if you changed nothing, just add a character, delete it,
and save), and the error markers will finally disappear.

This is more annoying than really a serious problem, and in any case
the files are always properly generated, so if there is no error, all files
are kept up-to-date.

##### Automating the above steps

If you include the `build-helper-maven-plugin` plugin in your
`pom.xml`, then it is possible to automatically add the relevant
source folders to Eclipse:

{% codeblock lang:xml %}
<plugin>
  <groupId>org.codehaus.mojo</groupId>
  <artifactId>build-helper-maven-plugin</artifactId>
  <version>1.7</version>
  <executions>
    <execution>
      <id>add-antlr-source</id>
      <phase>generate-sources</phase>
      <goals>
        <goal>add-source</goal>
      </goals>
      <configuration>
        <sources>
          <source>src/main/antlr3</source>
          <source>target/generated-sources/antlr3</source>
        </sources>
      </configuration>
    </execution>
  </executions>
</plugin>
{% endcodeblock %}

To use it, another connector is necessary, but it is found directly in
the m2e Marketplace.

Once in the `pom.xml`, just importing the project into Eclipse will
create the relevant source folders automatically. However the 'Locked'
and 'Derived' flags on the `target/generated-sources/antlr3` folder
are stored in the workspace `.metadata`, so these flags have to be set
manually for each workspace.

### The easier way

If all the above seems tedious, it is because it is. The
`antlr3-maven-archetype` will generate much of it, but not for
instance the additional source folders.

I have the kind of laziness that causes me to spend hours trying to
save a few minutes later on, so I created my own archetype, a trivial
little thing whose only purpose is to get the basic setup in place
quickly.

It does not really do much, and perhaps should best seen as a
template, which is why the best use is to
[download](https://github.com/fdumontmd/antlr3-simple-archetype) it,
adjust it to your own need, then install it locally.

Hope this helps.
