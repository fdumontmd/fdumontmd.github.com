---
layout: post
title: "Seven Languages in Seven Weeks Ruby Day 1"
date: 
comments: true
categories: [Study, Books]
tags: [ruby]
---

<!--more-->
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

{% codeblock Hello, Ruby lang:ruby %}
"Hello, Ruby".index("Ruby")
{% endcodeblock %}

Print your name ten times
-------------------------
{% codeblock Hello, many times lang:ruby %}
10.times { puts "Frederic" }
{% endcodeblock %}

Print the string “This is sentence number i,” with i changing from 1 to 10
--------------------------------------------------------------------------
{% codeblock Counting sentences lang:ruby %}
10.times { | i | puts "This is sentence number #{i+1}" }
{% endcodeblock %}

Run a Ruby program from a file.

Bonus problem: Guessing game
----------------------------

{% include_code Guessing Game lang:ruby guess.rb %}
