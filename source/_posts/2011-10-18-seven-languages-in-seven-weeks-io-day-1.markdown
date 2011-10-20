---
layout: post
title: "Seven Languages in Seven Weeks Io Day 1"
date: 2011-10-18 21:12
comments: true
categories: [Books]
tags: [7languages7weeks, io]
series: "Seven Languages in Seven Weeks"
---
The language for this week is [Io](http://iolanguage.com/), a prototype-based language. The language embraces minimalism, down to the documentation (which is really terse).
<!--more-->
JavaScript is another prototype-based language (although admittedly not minimalist); Bruce Tate explains his choice of Io over JavaScript by observing that JavaScript is already well known. That might be the case, but I wonder whether the prototype nature of JavaScript is well known as well.

Anyway, this week is about Io. As stated above, it is a prototype-based language (meaning there is no difference between class and object), with a simple and regular message passing syntax and semantic. 

Exercises
---------

Today's exercises explore various basic aspects of the language.

### Io is typed

Trying to evaluate
{% codeblock Testing typing lang:io %}
Io> 1 + "one"
{% endcodeblock %}
generates an error:
```
Exception: argument 0 to method '+' must be a Number, not a 'Sequence'
---------
message '+' in 'Command Line' on line 1
```
The error message in this case is fairly clear and verbose, fortunately.

### False values

0 and the empty strings are both true, while `nil` is false:
{% codeblock Testing false lang:io %}
Io> 0 and true
==> true
Io> "" and true
==> true
Io> nil and true
==> false
{% endcodeblock %}

### Slots in an object

The method [`slotNames`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/slotNames) gives the list of slots:
{% codeblock Listing slots lang:io %}
Object slotNames
{% endcodeblock %}
returns
```
==> list(serializedSlotsWithNames, isNil, serialized, relativeDoFile, prependProto, pause, <, futureSend, contextWithSlot, return, @, currentCoro, break, isIdenticalTo, ancestorWithSlot, getSlot, setSlotWithType, method, evalArgAndReturnNil, lazySlot, resend, isTrue, isKindOf, asSimpleString, while, setProtos, shallowCopy, init, removeProto, proto, stopStatus, clone, actorRun, serializedSlots, setSlot, removeAllSlots, handleActorException, become, apropos, hasSlot, -, doFile, doString, uniqueId, setIsActivatable, print, <=, launchFile, doRelativeFile, thisLocalContext, type, write, isLaunchScript, ifNonNil, >, thisContext, removeSlot, block, writeln, perform, doMessage, @@, switch, evalArg, list, deprecatedWarning, for, ?, ifError, try, asString, asyncSend, coroDo, do, performWithArgList, yield, argIsActivationRecord, slotNames, hasLocalSlot, wait, message, argIsCall, isActivatable, println, !=, foreachSlot, not, inlineMethod, .., coroDoLater, loop, ancestors, raiseIfError, newSlot, and, appendProto, cloneWithoutInit, slotSummary, continue, setProto, super, hasProto, ifNonNilEval, justSerialized, if, ==, or, protos, >=, returnIfNonNil, , uniqueHexId, removeAllProtos, slotValues, coroFor, coroWith, actorProcessQueue, thisMessage, ifNil, memorySize, returnIfError, hasDirtySlot, slotDescriptionMap, updateSlot, compare, ownsSlots, evalArgAndReturnSelf, getLocalSlot, in, markClean, isError, ifNilEval)
```

### Difference between =, := and ::=

The difference is explained in the [Io Guide](http://www.iolanguage.com/scm/io/docs/IoGuide.html), in the [Assignments](http://www.iolanguage.com/scm/io/docs/IoGuide.html#Syntax-Assignment) section, in terms of equivalent methods.

Each operator is mapped to a method:

  * `=` is mapped to [`updateSlot`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/updateSlot)
  * `:=` is mapped to [`setSlot`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/setSlot)
  * `::=` is mapped to [`newSlot`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/newSlot)

The difference between `updateSlot` and `setSlot` is simple: `setSlot` creates and set the value of a slot, whereas `updateSlot` only updates the value of an _existing_ slot.

`newSlot` is the same as `setSlot`, but it creates a setter method:
{% codeblock Assignment operators lang:io %}
Io> Vehicle := Object clone
==>  Vehicle_0x7fc3dbeb7110:
  type             = "Vehicle"

Io> Vehicle description := "a fast car"
==> a fast car
Io> Vehicle slotNames
==> list(description, type)
Io> Ferrari := Vehicle clone
==>  Ferrari_0x7fc3dbe4f2e0:
  type             = "Ferrari"

Io> Ferrari colour ::= "red"
==> red
Io> Ferrari slotNames
==> list(setColour, type, colour)
{% endcodeblock %}

### Execute the code in a slot given its name

The `getSlot` method returns the value for the named slot. Apparently if the value is a method, this calls the method as well, if there are no arguments (otherwise the method object is returned):
{% codeblock Executing method by name lang:io %}
Io> Object getSlot("isNil")
==> false
Io> nil getSlot("isNil")
==> true
Io> Object getSlot("compare")
==> Object_compare()
{% endcodeblock %}

Wrapping up day 1
-----------------

So Io has a minimal and consistent syntax. The objects can be easily introspected, and the operators are mapped to methods which can be redefined. 