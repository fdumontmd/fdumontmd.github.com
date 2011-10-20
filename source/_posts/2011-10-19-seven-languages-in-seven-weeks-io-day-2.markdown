---
layout: post
title: "Seven Languages in Seven Weeks Io Day 2"
date: 2011-10-19 23:48
comments: true
categories: [Books]
tags: [7languages7weeks, io]
series: "Seven Languages in Seven Weeks"
---
Day 2 covers method definition and it's potential. Io manages to express quite a few interesting things in its minimal syntax.
<!--more-->

The more interesting features of methods are the [`Message`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Message) meta information (the ability of a method invocation to introspect both the caller, callee, and the parameters), and the selective parameter evaluation, which supports the creation of new control operations (such as fancy loop or conditionals).

Exercises
---------

In line with today's topic, exercises are about defining and executing methods.

### Fibonacci sequence

I change the exercise a bit: I defined the fibonacci sequence for Number, and it uses the number it is called on as the argument. So I compute `40 fib` rather than `fib(40)`. For this reason I have to use `self` when I want to refer to the original argument explicitly.

The recursive (and slow) method translated directly from the definition:
{% codeblock Fibonacci sequence, recursive lang:io %}
Number fib := method(if (self < 2, 1, ((self - 1) fib + (self - 2) fib)))
{% endcodeblock %}

For the iterative method, I'm using a intermediate function with accumulators to build the result (I have easier time thinking in functional than imperative terms):
{% codeblock Fibonacci sequence, iterative functional lang:io %}
Number fibrec := method(a, b, n, if(n <= 1, b, fibrec(b, a+b, n-1)))
Number fib := method(fibrec(1, 1, self))
{% endcodeblock %}

Finally, an iterative imperative method, with explicit looping (the iteration starts at 2 because the `for` method iterates up to and including the upper bound):
{% codeblock Fibonacci sequence, iterative imperative lang:io %}
Number fib := method(
	a := 1
	b := 1
	for(i, 2, self,
		c := b
		b := a + b
		a := c
	)
	b
)
{% endcodeblock %}

### Change `/` to return 0 when divided by 0

First I save the original definition of `/`, then I update the operator slot with the new definition:
{% codeblock Redefining division lang:io %}
Number oldDiv := Number getSlot("/")
Number updateSlot("/", method(d, if(d == 0, 0,  oldDiv(d))))
{% endcodeblock %}

### Add up all the numbers in a two dimensional array

Assuming the array is implemented as a [`List`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List) of list, the following invocation will sum the numbers:
{% codeblock Summing over nested lists lang:io %}
arr := list(list(1,2,3), list(4,5,6), list(7,8,9))
arr reduce(xs, x, xs + (x reduce(+, 0)), 0)
{% endcodeblock %}

The initial value is supplied explicitly; otherwise [`reduce`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/reduce) would use the first value, which is not a number but a list.

Alternatively (and much shorter):
{% codeblock Summing over nested lists, the easy way lang:io %}
arr := list(list(1,2,3), list(4,5,6), list(7,8,9))
arr flatten reduce(+)
{% endcodeblock %}

### Define `myAverage`

Using the `reduce` method, it is easy to compute the sum of a list. The [`size`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/size) method can then be used to compute the average:
{% codeblock Reimplementing average lang:io %}
List myAverage := method(
	self reduce(+) / self size
)
{% endcodeblock %}
If the list is empty, the `reduce` method returns `nil`, so we get an exception (as `nil` does not respond to the `/` method). But this is consistent with the existing [`average`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/average) method.

#### Throwing Exception

Technically, this solution already raises an [`Exception`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Exception) when one of the elements is not a [`Number`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Number), but here is how I would implement explicit type checking:

{% codeblock Reimplementing average lang:io %}
OperatorTable addOperator("+?", 3)
Number +? := method(num,
	if(num hasProto(Number),
		self + num,
		Exception raise("Not a number")))

List myAverage := method(
	self reduce(+?) / self size
)

list(1,2,3, "hello") myAverage
{% endcodeblock %}

I am using a new operator, `+?`, with the same priority as `+`, and explicitly check the prototype with [`hasProto`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/hasProto). 

### Two-dimensional list prototype

First I clone a specialization of [`List`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List) as the implementation to the two dimensional array.

The initialization simply creates then grows the internal lists to the appropriate size, and stores the original parameters as slots:
{% codeblock Two dimensional arrays, initialization lang:io %}
Dim2 := List clone
dim2 := method(x, y, 
	d := Dim2 clone setSize(x) map(x, list() setSize(y))
	d newSlot("x", x)
	d newSlot("y", y)
	d
)
{% endcodeblock %}

The accessor methods can use the dimension slots to check for out of bound access:
{% codeblock Two dimensional array, accessor methods lang:io %}
Dim2 checkBounds := method(xg, yg, if(x <= xg or y <= yg or xg < 0 or yg < 0, Exception raise("Indexes out of bound")))
Dim2 get := method(x, y, checkBounds(x, y); at(x) at(y)) 
Dim2 set := method(x, y, v, checkBounds(x, y); at(x) atPut(y, v); d)
{% endcodeblock %}

The method `checkBounds` guarantees an exception is raised if the position parameters are not within bounds. The method `get` simply invokes [`at`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/at) twice to get at the data; `set` first locate the right sub list with `at`, then update the correct value with [`atPut`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/List/atPut), and finally returns the updated array.

### Two-dimensional list transpose method

With `Dim2` defined as above, the `transpose` method is trivial:
{% codeblock Two-dimensional list transpose method lang:io %}
Dim2 transpose := method(
	d := dim2(y, x)
	for(i, 0, x-1,
		for(j, 0, y-1, d set(j, i, get(i, j))
	))
	d
)
{% endcodeblock %}

Just initialize a new array, swapping the dimensions, then iterate over both dimensions, swapping the parameters for the `get` and the `set` methods.

### Matrix Input/Output

First the method [`asString`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/asString) can be used to get the string representation of an object, including the two-dimensional list.

However, the default `asString` returns the same representation as for regular nested lists. In order to read the object from the string, `asString` has to be overridden to emit something specific.

{% codeblock Two-dimensional array asString lang:io %}
Dim2 asString := method(
	buf := "twodim(" asMutable
	buf appendSeq(x asString)
	buf appendSeq(",")
	buf appendSeq(y asString)
	foreach(l, buf appendSeq(","); buf appendSeq(l asString))
	buf appendSeq(")")
	buf)
{% endcodeblock %}

With this defined, a two dimensional list has a unique representation:
{% codeblock asString example lang:io %}
Io> dim2(3,4)
==> twodim(3,4,list(nil, nil, nil, nil),list(nil, nil, nil, nil),list(nil, nil, nil, nil)){% endcodeblock %}

With this in place, I can define a `twodim` function that creates an instance of `Dim2` and fills it with the passed data:
{% codeblock twodim method lang:io %}
twodim := method(x, y,
	d := dim2(x, y)
	for(i, 0, x-1, d atPut(i, call sender doMessage(call message argAt(i+2)))
	))
{% endcodeblock %}

The `twodim` method is not defined on `Dim2`, but globally, so that the content of a string representing a `Dim2` instance can be parsed in any context.

With this in place, the object can be serialized and unserialized, using the [`doString`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/doString) method (which evaluates the string in the target context):

{% codeblock Testing serialization lang:io %}
Io> d := dim2(3,4)
==> twodim(3,4,list(nil, nil, nil, nil),list(nil, nil, nil, nil),list(nil, nil, nil, nil))
Io> for(i, 0, 2, for(j, 0, 3, d set(i, j, i+j)))
==> twodim(3,4,list(0, 1, 2, 3),list(1, 2, 3, 4),list(2, 3, 4, 5))
Io> doString(d asString)
==> twodim(3,4,list(0, 1, 2, 3),list(1, 2, 3, 4),list(2, 3, 4, 5))
{% endcodeblock %}

So the last step is to store the string representation in a file, and read from it:
{% codeblock Writing to a file lang:io %}
d := twodim(3,4,list(0, 1, 2, 3),list(1, 2, 3, 4),list(2, 3, 4, 5))
f := File with("foo.txt")
f remove
f openForUpdating
f write(d asString)
f close
{% endcodeblock %}

Unsurprisingly, the content of the file is
{% include_code Serialized Two-dimensional list instance lang:io 7l7w/io/foo.txt %}

Reading is just as simple:
{% codeblock Reading from a file lang:io %}
f := File with("foo.txt")
f openForReading
d1 := doString(f readLine) 
f close
d1
d == d1
{% endcodeblock %}

The original `Dim2` instance is equal to the unserialized one. I did not really expected that (I didn't write any comparison code for the new object), but Io provided a sensible implementation anyway.

Note: theres is a [`serialized`](http://www.iolanguage.com/scm/io/docs/reference/index.html#/Core/Core/Object/serialized) method, but it's output representation in the case of `Dim2` is the same as the one for `List`. There should be a way to override `serialized` as well, but it's exact semantic is not clear to me.

### Guess a Number Game

Given the above, the last exercise a walk in the park. Getting the number from the standard input was a bit harder to figure out. For some reason, on Mac OS X, reading from the standard input also displays `nil`; I guess it is a bug, although not a very serious one.

{% include_code Guess a Number Game lang:io 7l7w/io/guess.io %}

Wrapping up
-----------

Well, that was quite a day. The exercises did not cover the more advanced use of method (such as implicit argument evaluation), but otherwise gave the opportunity to define useful behaviours, and play with important classes from the standard library.