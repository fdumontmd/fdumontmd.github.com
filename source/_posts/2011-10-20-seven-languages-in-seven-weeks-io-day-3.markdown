---
layout: post
title: "Seven Languages in Seven Weeks Io Day 3"
date: 2011-10-20 15:16
comments: true
categories: [Books]
tags: [7languages7weeks, io]
series: "Seven Languages in Seven Weeks"
---
Third and final day with Io (as far as the book is concerned). This time Io metaprogramming abilities are used to bend the language into strange shapes.
<!--more-->
Of course, metaprogramming bends the mind just as much at the language, and this last day is quite a ride.

Everything is up for redefinition, and new syntactic structure can be added as well. Operators were covered in [Day 2](/blog/2011/10/19/seven-languages-in-seven-weeks-io-day-2). Today curly braces and square brackets are covered as well. The neat thing is that the method used for the iterpretation is looked up using the same logic as other methods. So it is possible to define a square bracket syntax to access list items simply with:

{% codeblock lang:io %}
List squareBrackets := method(i, at(i))
{% endcodeblock %}

With the definition above, it becomes possible to use the familiar bracket syntax:
{% codeblock lang:io %}
Io> list(1,2,3)[1]
==> 2
{% endcodeblock %}

Updates are not possible (as far as I can tell), however, so for that usage it is less expressive than the `[]` and `[]=` methods in Ruby.

The content inside the braces or brackets must be a comma separated list, each element acting as an argument for the `curlyBrackets` or `squareBrackets` methods (no link as I could not find any documentation for either method).

A moderately annoying problem with operator extensions is that they are not available in the file in which they are defined.

The last topic covered was concurrency. Io implements an actor model, like all the cool kids ([Erlang](http://www.erlang.org/), [Scala](http://www.scala-lang.org/), ...). The book doesn't go into details. And the available documentation is sparse as well. But from what I could gather, the model is cooperative concurrency, and an asynchronous message is a simple extension of the standard one, with one caveat: the `call sender` information is lost when asynchronous messages are used. Or more precisely, `call sender` does not return the original initiator of the call, but the piece of logic in the target coroutine that handles the dispatching of messages. So to return a answer, the sender must be passed as argument.

For instance:
{% codeblock Actors lang:io %}
Agent := Object clone
Agent msg := "message"
agent1 := Agent clone
agent1 msg = "agent1 pinged"
agent2 := Agent clone
agent2 msg = "agent2 ponged"
agent1 ping := method(sender, message, cutoff,
	if(cutoff > 0,
		wait(Random value(1, 5) floor)
		"Received message: " print
		message println
		sender @@pong(self, msg, cutoff - 1)
		yield,
		yield))
agent2 pong := method(sender, message, cutoff,
	if(cutoff > 0,
		wait(Random value(1, 5) floor)
		"Received message: " print
		message println
		sender @@ping(self, msg, cutoff - 1)
		yield,
		yield))
agent1 @@ping(agent2, agent2 msg, 5)
Coroutine currentCoroutine pause
{% endcodeblock %}

The code above will spawn two agents, and they will exchange messages (5 times here).

Exercises
---------
Today's exercises are only on metaprogramming, essentially syntax extension.

### Indenting XML output

To indent properly, the Builder must keep track of the nesting depth. This is done with a slot, and a few utility methods. The `depth` slot is the nesting depth, it is changed with `nest` (increase) and `unnest` (decrease), which should bracket the code that processes children. Finally, `indent` emits the required amount of blank space.

{% include_code lang:io 7l7w/io/builder_indent.io %}

produces the following

```
<ul>
  <li>
    Io
  </li>
  <li>
    Lua
  </li>
  <li>
    JavaScript
  </li>
</ul>
```

### Bracket syntax for list

This is not difficult: the implementation just creates an empty list, then [`append`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/append) each arguments to the list, before returning it:
{% codeblock Bracket syntax for list lang:io %}
squareBrackets := method(
     l := List clone
     call message arguments foreach(arg,
          l append(arg)
     )
     l
)
{% endcodeblock %}

The result is:
{% codeblock Building a list lang:io %}
Io> [1,2,3]
==> list(1, 2, 3)
{% endcodeblock %}

### Attribute syntax for XML Builder

The last exercise is a bit tricky. The code as presented in the code mixes parsing and output. The problem now is that the first argument could be the attribute list, rather than a child element. The solution is to stop printing the result as we parse it, and instead to build a string representation of the XML.

{% include_code lang:io 7l7w/io/builder_attrib.io %}

Once loaded, it can interpret the following (using `print` to display the generated text):

{% codeblock lang:io %}
Builder ul({"author": "Tate"}, li("Io"), li("Lua"), li("JavaScript")) print
{% endcodeblock %}

as

```
<ul author="Tate">
  <li>
    Io
  </li>
  <li>
    Lua
  </li>
  <li>
    JavaScript
  </li>
</ul>
```

Wrapping day 3 and Io
---------------------

that [Io](http://www.iolanguage.com/) is interesting, but in the sense and to the extent that [Brainfuck](http://en.wikipedia.org/wiki/Brainfuck) is interesting. And I don't necessarily mean that in a bad way.

The terseness and uniformity of syntax achieves quite a great deal; the actor model is modern and hip, although the cooperative concurrency isn't.

This is a language that requires commitment; it isn't clear it deserves so much.
