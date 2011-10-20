---
layout: post
title: "Seven Languages in Seven Weeks Ruby Day 2"
date: 2011-10-12 15:05
comments: true
categories: [Books]
tags: [7langages7weeks, ruby]
series: "Seven Languages in Seven Weeks"
---
Day 2 is about defining things, functions, classes, as well as a second look at basic containers.
<!--more-->
Blocks are covered as well. It is hard to overstate their importance in making a language flexible and expressive. In a language like Ruby with regular evaluation (i.e. not lazy), blocks (with an unobtrusive syntax) are essential to implement embedded DSL (alternatives are laziness, as in Haskell, or more selective mechanisms to control evaluation, such as lisp's macros, or Io's method call meta-API).

But that's a topic for another book (which I really look forward to reading).

Back to this one.

Print the contents of an array of sixteen numbers, 4 numbers at a time
----------------------------------------------------------------------
First, without `each_slice`:
{% codeblock Slicing, the hard way lang:ruby %}
def print_slices(arr, slice=4)
  acc = []
  arr.each do |a|
    acc.push(a)
    if acc.length == slice
      p acc
      acc = []
    end
  end
  p acc if acc.length > 0
end
{% endcodeblock %}

The implementation is simple enough, the values are accumulated until there's enough. I'm using the `p` printing command because neither `puts` nor `print` do exactly what I need: `puts` prints each element on a different line; `print` does not add a newline.

Then, the easy (and natural way):
{% codeblock Slicing the righ way lang:ruby %}
def print_each_slice(arr, slice=4)
  arr.each_slice(slice) { |a| p a }
end
{% endcodeblock %}

Better initializer method for Tree
----------------------------------
This one is not hard either. I'm using two class methods, one as a factory method (`self.build`), the other as the real processor (`self.h_to_children`):
{% include_code Tree builder lang:ruby 7l7w/ruby/tree.rb %}

First `self.h_to_children`: its purpose it to turn a list of key, value pairs (each key being a String, and each value a Hash or nil) into a list of Trees. It uses the [`collect`](http://www.ruby-doc.org/core-1.9.2/Enumerable.html#method-i-collect) method to transform each key, value pair into a Tree (recursively converting the value on the way).

`self.build` takes the list of Trees; if there's only one, it is our root. Otherwise, it creates a new root, using the list of Trees as children.

{% codeblock Building a Tree lang:ruby %}
test_hash = {'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}},
    'uncle' => {'child 3' => {}, 'child 4' => {} } } }

Tree::build(test_hash)
{% endcodeblock %}
produces
```
#<Tree:0x007fb9599119b8 @children=[#<Tree:0x007fb9599142d0 @children=[#<Tree:0x007fb959914488 @children=[], @name="child 1">, #<Tree:0x007fb9599142f8 @children=[], @name="child 2">], @name="dad">, #<Tree:0x007fb959911a08 @children=[#<Tree:0x007fb959914190 @children=[], @name="child 3">, #<Tree:0x007fb959911a58 @children=[], @name="child 4">], @name="uncle">], @name="grandpa"> 
```
while
{% codeblock Building a Tree part 2 lang:ruby %}
Tree::build(test_hash['grandpa'])
{% endcodeblock %}
produces
```
#<Tree:0x007ff891895300 @children=[#<Tree:0x007ff891895698 @children=[#<Tree:0x007ff891895828 @children=[], @name="child 1">, #<Tree:0x007ff8918956c0 @children=[], @name="child 2">], @name="dad">, #<Tree:0x007ff891895350 @children=[#<Tree:0x007ff8918954b8 @children=[], @name="child 3">, #<Tree:0x007ff8918953c8 @children=[], @name="child 4">], @name="uncle">], @name="root"> 
```

Simple grep
-----------
As stated in the book, it is very simple to implement a basic `grep` function in Ruby. Here's one that relies on the magic variable [`ARGF`](http://www.ruby-doc.org/core-1.9.2/ARGF.html) (oddly enough, it is documented as a constant of the [`Object`](http://www.ruby-doc.org/core-1.9.2/Object.html) class):
{% include_code Simple grep lang:ruby 7l7w/ruby/grep.rb %}

```
$ ./grep.rb grep grep.rb
```
produces
```
3: def grep(t, f)
7: grep(ARGV.shift, ARGF.read)
```

It is not very satisfactory, however. Grepping over several files does not distinguish between them, and the line number becomes meaningless:
```
$ ./grep.rb def *.rb
```
produces
```
3: def grep(t, f)
13: def grep(header,text, file)
17: def make_header(filename, required)
36: def play()
50: def print_slices(arr, slice=4)
64: def print_each_slice(arr, slice=4)
72:   def initialize(name, children=[])
77:   def self.build(hash)
86:   def visit_all(&block)
91:   def visit(&block)
96:     def self.h_to_children(hash = {})
```

Certainly there must be a better way. The code below is an attempt at that. Some of the complexity comes from my attempt to mimic the original `grep` behaviour when dealing with one or many files. With only one file, it does not print the file name. With many, it does.

Also, I revert to reading from `ARGF.read` when there is no filename, that is, when I should read from `stdin` (which `ARGF.read` does when `ARGV` is empty).

Finally, if there are file names to iterate over, I need to check for errors, print a statement, and continue.

The new code has a function to make a header from the file name if needed (that is, when there are more than one file to process).
 
{% include_code Slightly better grep lang:ruby 7l7w/ruby/grep2.rb %}

```
$ ./grep2.rb grep grep.rb
```
produces the same output as the previous version:
```
3: def grep(t, f)
7: grep(ARGV.shift, ARGF.read)
```
whereas
```
$ ./grep2.rb def *.rb not_there
```
is more informative and more reliable (the first version would just crash on non readable file names):
```
grep.rb - 3: def grep(t, f)
grep2.rb - 3: def grep(header,text, file)
grep2.rb - 7: def make_header(filename, required)
guess.rb - 1: def play()
slices.rb - 1: def print_slices(arr, slice=4)
slices.rb - 15: def print_each_slice(arr, slice=4)
tree.rb - 4:   def initialize(name, children=[])
tree.rb - 9:   def self.build(hash)
tree.rb - 18:   def visit_all(&block)
tree.rb - 23:   def visit(&block)
tree.rb - 28:     def self.h_to_children(hash = {})
Error reading file not_there
```

And that wraps it up for day 2.
