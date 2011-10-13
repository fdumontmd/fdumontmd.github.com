---
layout: post
title: "Seven Languages in Seven Weeks Ruby Day 3"
date: 2011-10-13 15:30
comments: true
categories: [Book]
tags: [7langages7weeks, ruby]
series: "Seven Languages in Seven Weeks"
---
Third and final day on Ruby. This time, metaprogramming techniques are covered.
<!--more-->

[Metaprogramming](http://en.wikipedia.org/wiki/Metaprogramming) allows a program to write programs, or more interestingly, to modify itself. The structure of the running program is made available to introspection API, and can be updated or extended.

Ruby as a really powerful set of tools for metaprogramming, but a good understanding of Ruby's metamodel and some of its darker corners is required to fully benefit from them.

But first let's finish the homework (day 3 has only a short one).

Improved Acts as CSV module
---------------------------

{% include_code Acts as CSV module lang:ruby 7l7w/ruby/acts_as_csv_module.rb %}

The code is fairly straightforward. The new class `CsvRow` does most of the job. It uses `method_missing` to access the relevant column. There is no error checking, so please don't make mistakes...

The codes behaves as intended:
```
csv = RubyCsv.new
csv.each {|row| puts row.one}
```
does print
```
lions
```

As an alternative, the code below creates the methods during the initialization of the instance. There could (should?) be an easier way, but I could not find one. The new methods are added to the singleton class, so each instance has its own set.

{% codeblock Using define_method rather than method_missing lang:ruby %}
class CsvRow
  def initialize(h, r)
    @headers = h
    @row = r
    singleton = class << self; self; end
    h.each do |field|
      singleton.send(:define_method, field.to_sym) { r[h.index(field)] }
    end
  end
end
{% endcodeblock %}

The code uses the `send` method because `define_method` must be used from within the class (it is a private method), but when I open the class I change the scope and loose access to the original parameters `h` and `r`.

With such modification, the codes still executes as required.

Wrapping up day 3
-----------------

This chapter was short, certainly, but it gives a tantalizing overview of metaprogramming.

However, these techniques bring to light the fact that Ruby does not have definitions, only code that defines things, and that the evaluation order of this code matters. This becomes clearer when trying to modifies classes as they are being define.

Consider the following fragment. The `Path` class does nothing really important, but it could for instance wrap methods with a proxy. For this it needs to know the methods that are defined on the target class.

`Target1` and `Target2` both define the same methods (through the use of `attr_accessor`), but `Target1` includes `Patch` first, then define the attribute, while `Target2` includes `Patch` last.

{% include_code Evaluation order matters lang:ruby 7l7w/ruby/meta.rb %}

When executed, the code produces
```
In Target1: false
In Target2: true
```

So in Ruby, it is fair to say that there are no declarations, only instructions, all of them executed in order (some of these instructions create functions, classes, or blocks to be executed later).

Indeed, the following program fails to execute:
{% include_code Evaluation order really matters... lang:ruby 7l7w/ruby/eval.rb %}

while the equivalent Perl one succeeds:
{% include_code ... or not depending on the language lang:perl 7l7w/ruby/eval.pl %}

This is because Perl processes the definitions first, then executes the instructions in order.

Ruby's execution mode is similar to Common Lisp's. Actually, Common Lisp makes is even more complex by virtue of being a compiled language with various phases (eval, compile and load), allowing (and sometimes requiring) selective evaluation of various parts of the code. Hopefully Ruby metaprogramming will not be that complex.

Still, despite the potential for obfuscation, metaprogramming (combined with Ruby's low ceremony syntax) supports the creation of elegant DSL and simplifies program architectures. It is a way to centralizes complexity, and drain it from the rest of the code.

About Ruby
----------
I really like Ruby. Even as the bastard child of Perl and Smalltalk that it is, it has a level of consistency and cohesion that well thought. Each of its shortcoming (Ruby can be rather slow, and as noted above metaprogramming can become very complex) is a reasonable trade off, and it can be argued that the advantages these trades off bought more than compensate for the shortcomings.

More importantly, the Ruby ecosystem is bristling with interesting tools and ideas, and it really is fascinating to explore.

Despite its different origin, Ruby is a Lisp for the 21st century.