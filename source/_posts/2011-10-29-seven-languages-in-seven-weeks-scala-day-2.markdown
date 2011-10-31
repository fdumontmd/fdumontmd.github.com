---
layout: post
title: "Seven Languages in Seven Weeks Scala Day 2"
date: 2011-10-29 10:33
comments: true
categories: [Books]
tags: [7languages7weeks, scala]
series: "Seven Languages in Seven Weeks"
---
Second day is dedicated to containers, mostly. Lists, Sets and Maps, some of their most useful methods, and the use of code blocks (anonymous functions)
<!--more-->
The various containers are nothing new or special for Java programmers, but the anonymous functions are an effective way to greatly increase the power of existing iteration methods by having them accept arbitrary logic to process each element. This is nothing new or special for functional programmers, of course.

While this is possible in Java (where anonymous classes often play this role), the result is less fluid than in functional languages, so it comes less naturally.

Exercises
---------
### Using foldLeft to compute sum of string sizes.

Not as challenging an exercise as [yesterday's](/blog/2011/10/28/seven-languages-in-seven-weeks-scala-day-1/). Still, it shows how light and easy to use Scala's anonymous functions are.

A first version with the `foldLeft` method:
{% codeblock foldLeft lang:scala %}
scala> val list = List("one", "two", "three")
list: List[java.lang.String] = List(one, two, three)

scala> val sum = list.foldLeft(0) {(sum, s) => sum + s.size }
sum: Int = 11
{% endcodeblock %}

A second version with the `/:` operator. The anonymous function is of course strictly identical.

{% codeblock foldLeft operator lang:scala %}
scala> val list = List("one", "two", "three")
list: List[java.lang.String] = List(one, two, three)

scala> val sum = (0 /: list) {(sum, s) => sum + s.size }
sum: Int = 11
{% endcodeblock %}

### Censor trait

Looking for a Scala documentation of `String` is somewhat frustrating, because there's none. But that in turns means that Scala just uses Java's [String](http://download.oracle.com/javase/1.4.2/docs/api/java/lang/String.html).

Java String comes with a method that seems to do just what is needed here: [`replaceAll`](http://download.oracle.com/javase/1.4.2/docs/api/java/lang/String.html#replaceAll%28java.lang.String,%20java.lang.String%29).

{% codeblock replaceAll lang:scala %}
scala> "one two three".replaceAll("two", "TWO")
res1: java.lang.String = one TWO three
{% endcodeblock %}

{% codeblock replaceAll, word boundaries lang:scala %}
scala> "she sells shells".replaceAll("\\bshe\\b", "the lady") 
res2: java.lang.String = the lady sells shells
{% endcodeblock %}

{% codeblock replaceAll, case insensitive lang:scala %}
scala> "She sells shells".replaceAll("(?i)\\bshe\\b", "the lady") 
res3: java.lang.String = the lady sells shells
{% endcodeblock %}

As seen in the last two examples, the first argument is actually a regular expression. For a while, I toyed with the idea of using Scala's [`Regex.replaceAllIn`](http://www.scala-lang.org/api/current/scala/util/matching/Regex.html), so I could check whether the match was capitalized or all upper case and insert the replacement word with identical case, as Emacs does. But this is a whole lot more work, and generic code can only handle a few cases (all lower case, all upper case and capitalized) satisfactorily.

The first version iterates over the pairs in the censored words map, and for each one replaces each basic forms of the censored word by the same form of its replacement (the forms being capitalized, lower case and upper case).

{% include_code Censor, version 1 lang:scala 7l7w/scala/censor.scala %}

I like the way Scala allows me to write extremely short code for utility methods (like `c`, `l`, ...).

One problem with this version is that there's a mutable variable. Using `foldLeft`, the mutable variable is no longer needed:

{% include_code Censor, version 2 lang:scala 7l7w/scala/censor_fold.scala %}

With the code above, the world is now safe from the threat of rude language:

{% include_code Censor test code lang:scala 7l7w/scala/censor_test.scala %}

produces:

```
Original text:
Phil Wenneck: God damn it!
Alan Garner: Gosh darn it!
Phil Wenneck: Shit!
Alan Garner: Shoot!
Censored text:
Phil Wenneck: God damn it!
Alan Garner: Gosh beans it!
Phil Wenneck: Shit!
Alan Garner: Pucky!
```

#### Loading from file

To load censored words from a file, I first need to define a format. To keep things simple, each pair is on one line, separated by one or more spaces.

The [`Source`](http://www.scala-lang.org/api/current/index.html#scala.io.Source) object contains a useful `fromFile` method (unfortunately, not documented directly. You have to dig it from the source file). Then it is possible to `foldLeft` the lines to populate the replacement map.

The rest of the code is identical.

{% include_code Censor, loading from a file lang:scala 7l7w/scala/censor_load.scala %}

outputs:

```
$ scala censor_load.scala 

Original text:
Phil Wenneck: God damn it!
Alan Garner: Gosh darn it!
Phil Wenneck: Shit!
Alan Garner: Shoot!

Censored text:
Phil Wenneck: God damn it!
Alan Garner: Gosh bean it!
Phil Wenneck: Shit!
Alan Garner: Pucky!

Improved censored text:
Phil Wenneck: God d--n it!
Alan Garner: Gosh bean it!
Phil Wenneck: S--t!
Alan Garner: Pucky!
```

{% include_code censor.txt lang:text 7l7w/scala/censor.txt %}
{% include_code censor2.txt lang:text 7l7w/scala/censor2.txt %}

Wrapping up Day 2
-----------------

Scala's syntax is clearly much shorter than Java's, and fairly expressive as well. The code flows, is more concise, and feels natural (assuming that you think functional code feels natural, as I do).

Moreover, looking at the [online documentation](www.scala-lang.org/api/current/index.html), I can see that there's more depth to Scala's type system than can be covered in such a book. This is another area I look forward to investigating further.
