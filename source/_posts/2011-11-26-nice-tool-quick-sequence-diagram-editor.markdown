---
layout: post
title: "Nice tool: Quick Sequence Diagram Editor"
date: 2011-11-26 12:23
comments: true
categories: [Software]
tags: [kudos, sdedit]
---
There are tools that provide a simple, elegant solution to some of my recurrent problems. Today I'd like to introduce you to the [Quick Sequence Diagram Editor](http://sdedit.sourceforge.net/index.html) (sdedit for short).
<!--more-->
First, to get that out of the way: if a mouse makes you generally more productive than a keyboard, if you need to look to find the next letter to press on, then you probably won't want to use sdedit. But then again you probably don't have any business producing UML Sequence diagrams anyway.

This is because sdedit's main interface is not a mouse, but the keyboard: instead of dragging visual elements into place, like most diagram editors, sdedit let you type a description of the diagram, and automatically generates the diagram as you type.

Purpose
-------

As the name says, sdedit allows you to generate Sequence Diagrams. It is not meant as full UML designer toolkit. And perhaps even in this focused area, it probably lacks features, but for my needs I found it perfectly adequate.

In general, when I need such a diagram, it is to represent the flow of information between systems in an integration scenario (and usually these systems will be various web applications). The intended audience is the technical staff of my clients, which are not programmers (they are IT managers, system or database administrators, operations, ...).

I need to communicate the sequence of calls, the parameters and return values, as well as the occasional asynchronous messages. If I ever had to add some subtle qualifications to such a diagram, I would rather do it in English, rather than use a graphical notation that few people know and most will misunderstand anyway.

Usage
-----

sdedit is written in Java, and packages are available for all main platforms (and a generic binary is available for the other platforms). It is very small (2 MB) which, in these days of multi-dvds IDEs, is so refreshing (and to think that some people used to joke that Emacs was so resource hungry it needed 8 MB of memory...).

When it is first launched, it greets the user with an uncluttered screen:
{% img https://lh3.googleusercontent.com/-wm3qYBXjKZ0/TtHZDVgYi3I/AAAAAAAAB4o/ji2R6edXCj0/s800/Screen%252520Shot%2525202011-11-27%252520at%25252015.30.04.png 'sdedit initial screen' %}

To start creating the diagram, you first need to declare a few participants, using a [simple language](http://sdedit.sourceforge.net/enter_text/index.html). sdedit uses recognizes specific keywords that can be used to customize the appearance of the diagram:

{% img https://lh5.googleusercontent.com/-7gCWxCHdNl0/TtHaqn_FzMI/AAAAAAAAB5A/GUS3tPAwPIo/s800/Screen%252520Shot%2525202011-11-27%252520at%25252015.35.36.png 'sedit participants' %}

Messages are created by stating who initiates them, and who receives them, with a familiar object oriented syntax:

{% img https://lh4.googleusercontent.com/-Xfn_SgHyQXw/TtHctQyR_DI/AAAAAAAAB5M/jt3dKtzMeoc/s800/Screen%252520Shot%2525202011-11-27%252520at%25252015.45.58.png 'sdedit message calls' %}

Asynchronous messages are also possible; grouping calls into a named sequence (and nesting such sequences) is also supported.

Getting help
------------

The [website](http://sdedit.sourceforge.net/index.html) has a complete documentation, but I found that the integrated help page, especially its set of examples is far more useful:

{% img https://lh5.googleusercontent.com/-ms8UYivtoOM/TtHdweYCUlI/AAAAAAAAB5Y/IJpHfomujU8/s800/Screen%252520Shot%2525202011-11-27%252520at%25252015.50.28.png 'sdedit examples' %}

That, and the fact that diagrams are redrawn as you type, makes it easy to experiment and figure out what works.

Try it...
---------

I found that sdedit is useful to clarify my ideas. All too often, a UML designer is just a designer for a diagram that is already fully formed in your mind. But with sdedit simple input syntax and instant feedback, I can experiment and see immediately which design works (or appears to work).

I'm not sure other parts of UML are suitable for such an exploratory design tool, but thanks to sdedit, at least a Sequence Diagram becomes a bicycle for the mind.
