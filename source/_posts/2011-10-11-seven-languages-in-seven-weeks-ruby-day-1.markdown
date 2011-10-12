---
layout: post
title: "Seven Languages in Seven Weeks Ruby Day 1"
date: 2011-10-11 13:21
comments: true
categories: [Books]
tags: [7languages7weeks, ruby]
---
The first day of the first language ([Ruby](http://www.ruby-lang.org/)) in [Seven Languages in Seven Weeks](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks), is fun and refreshing (especially after a long day with Java).
<!--more-->
This is of course mostly due to the nature of Ruby, a language that is unapologetically designed to improve the programmer's experience.

Basic types are introduced, as well as various looping and branching mechanisms.

The exercises were painless, and the required code very short.

Print the string "Hello, world"
-------------------------------
That should be easy enough. The simplest solution is to use `puts`:
{% codeblock Hello, World lang:ruby %}
puts "Hello, world"
{% endcodeblock %}

The difference between `puts` and `print` is that the former adds a new line, while the latter does not. So, a less natural way would be:
{% codeblock Hello, World done wrong lang:ruby %}
print "Hello, world/n"
{% endcodeblock %}

Finally, `p` is used to inspect its argument, so it is not usable in this context: as the argument is a string, `p` would print it enclosed with double quotes.

In “Hello, Ruby,” find the index of the word “Ruby.”
----------------------------------------------------
The [index](http://www.ruby-doc.org/core-1.9.2/String.html#method-i-index) method on the [String](http://www.ruby-doc.org/core-1.9.2/String.html) class is just what we need:
{% codeblock Hello, Ruby lang:ruby %}
"Hello, Ruby".index("Ruby")
{% endcodeblock %}

The `index` method is actually more flexible, and regular expressions can be used as well.

Print your name ten times
-------------------------
The most natural way is to use the [times](http://www.ruby-doc.org/core-1.9.2/Integer.html#method-i-times) from the [Integer](http://www.ruby-doc.org/core-1.9.2/Integer.html) class:
{% codeblock Hello, many times lang:ruby %}
10.times { puts "Frederic" }
{% endcodeblock %}

This produces just what is needed. Alternatives will be looked at in the next exercise.

Print the string “This is sentence number i,” with i changing from 1 to 10
--------------------------------------------------------------------------
Once again, I could use the `times` method:
{% codeblock Counting sentences lang:ruby %}
10.times { | i | puts "This is sentence number #{i+1}" }
{% endcodeblock %}

The problem this time is that the range is from 0 to 9, so I have to add 1 to the variable before printing it.

[Ranges](http://www.ruby-doc.org/core-1.9.2/Range.html) can be used as well:
{% codeblock Counting over range lang:ruby %}
(1..10).each { | i | puts "This is sentence number #{i}" }
{% endcodeblock %}

Alternatively, I could build an enumerator using the [upto](http://www.ruby-doc.org/core-1.9.2/Integer.html#method-i-upto) method:
{% codeblock Counting over enumerator lang:ruby %}
1.upto(10) { | i | puts "This is sentence number #{i}" }
{% endcodeblock %}

In these last two the index variable ranges over the correct values.

I could then go over more basic looping constructs, like `while`, but they really do not bring much here.

Bonus problem: Guessing game
----------------------------
Here, a basic looping construct like `while` feels natural (at least to me). The code is fairly simple, there is no error checking on input, but hey, it's just day 1.

{% include_code Guessing Game lang:ruby guess.rb %}

And this wraps up day 1.
