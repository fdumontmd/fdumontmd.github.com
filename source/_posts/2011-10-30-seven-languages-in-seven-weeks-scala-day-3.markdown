---
layout: post
title: "Seven Languages in Seven Weeks Scala Day 3"
date: 2011-10-30 22:18
comments: true
categories: [Books]
tags: [7languages7weeks, scala]
series: "Seven Languages in Seven Weeks"
---
On the last day with Scala, the book introduces the XML support and concurrency.
<!--more-->

Scala and XML
-------------

XML is a rather unfortunate part of modern computing, one that is tedious with most languages.

Scala chose to solve this by making XML part of the language. This means that the following fragment (from Burak's draft [scala xml book](http://burak.emir.googlepages.com/scalaxbook.docbk.html), linked to from [A Tour of Scala: XML Processing](http://www.scala-lang.org/node/131)):

{% codeblock Scala and XML lang:scala %}
val labPhoneBook = 
    <phonebook>
      <descr>
        This is the <b>phonebook</b> of the 
        <a href="http://acme.org">ACME</a> corporation.
      </descr>
      <entry>
        <name>Burak Emir</name> 
        <phone where="work">+41 21 693 68 67</phone>
      </entry>
    </phonebook>

println(labPhoneBook)
{% endcodeblock %}

stores an XML fragment, not a string, in `labPhoneBook`.

Braces can be used to insert Scala code directly into the XML:

{% codeblock Scala: dynamic XML lang:scala %}
scala> val name = "Fred"
name: java.lang.String = Fred

scala> val fragment = <blog>
     | <post>
     | <author>{name}</author>
     | </post>
     | </blog>
fragment: scala.xml.Elem = 
<blog>
<post>
<author>Fred</author>
</post>
</blog>
{% endcodeblock %}

The interactive Scala shell recognizes XML fragments, so it knew the expression was not complete until `</blog>`. Very, very nice.

Braces can also be used to extract information from the XML, as seen in the book. This, combined with the `\` XPath projection operator (which, of course, returns a data structure that can be iterated over with the usual methods), provides a very pleasant way to parse XML.

There is far more to Scala's XML support (see the link above). I don't usually do much XML processing (most of my handling of XML is performed through dedicated libraries that abstract XML away entirely), but if I had to, I'd certainly would give a Scala a try.

Scala Concurrency
-----------------



Exercises
---------

### Displaying links

To collect the links, I thought about using HTML or XML parsing, but could not find a parser robust enough to handle the pages. In the end, I just used a regular expression.

The code is fairly short:

 * first import the relevant package and class
 * define the regular expression
 * slightly reorganize `PageLoader` to list all the links found through the regular expression; the links are added to a `Set` to ensure unicity, then iterated over

{% codeblock Displaying links lang:scala %}
import scala.util.matching.Regex

val linkPattern = new Regex("""<a +href=\"([^\"]+)\"[^>]*>""", "link")

object PageLoader {
  def getPageSize(url : String) = {
    val text = Source.fromURL(url).mkString
    ((Set(): Set[String]) /: linkPattern.findAllIn(text).matchData) { 
      (s, md) => s + md.group("link") } foreach { 
        link => println(url + " => " + link) }
    text.length
  }
}
{% endcodeblock %}

The code to add the links to a Set is not the simplest I could think of, but it is the simplest that worked. The problem I have is that the collections methods other return iterators, but I cannot simply build a new collection from an interator. I guess there must be a simpler way, but right now it is eluding me.

Full code:
{% include_code sizer.scala, full code lang:scala 7l7w/scala/sizer.scala %}

### Following links

This exercise build on the previous one. Now that we have the links, we should try to follow them and add their size.

First, the links should be somewhat normalized. A basic idea would be to make sure each contains a host; those that do not would be prefixed with the main url (the logic is not fool proof: checking the errors, I found a few javascript fragments that were interpreted as a link.)

Then there is a second, more serious issue: some links take forever to load. So I rewrote the fetching logic to add a timeout. Because `try` blocks are actually expression in Scala, I just return an empty string as the content of pages that cannot be read.

Finally, at this stage I found it hard to propagate the concurrency method (sequential or parallel) down to the `PageLoader.getPageSize` method, I just implemented the two methods in separate versions:

##### Sequential version

This version takes over 10 minutes to run on my machine.

{% include_code sizer_links.scala lang:scala 7l7w/scala/sizer_links_seq.scala %}

Output:
{% include_code lang:text 7l7w/scala/seq_output.txt %}

#### Parallel version

This version takes about 100 seconds (less than a sixth of the sequential version). Obviously, the code is more complex, but clearly much faster as well.

{% include_code sizer_links.scala lang:scala 7l7w/scala/sizer_links_par.scala %}

Output:
{% include_code lang:text 7l7w/scala/par_output.txt %}

Wrapping up Day 3 and Scala
---------------------------

I really feel I would enjoy working full time with Scala. I disagree with the author statement that Scala has an academic syntax. It does not take that much time to get used to, and it feels quite lighter than Java's (of course, coming from Ruby would provide an entirely different perspective).

As I was doing the exercises, I found a few dark corners, most notably about collections and iterators. 

{% codeblock lang:scala %}
Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import scala.util.matching.Regex
import scala.util.matching.Regex

scala> 

scala> val re = new Regex("a")
re: scala.util.matching.Regex = a

scala> 

scala> val str = "abracadabra"
str: java.lang.String = abracadabra

scala> 

scala> val matches = re.findAllIn(str).matchData
matches: java.lang.Object with Iterator[scala.util.matching.Regex.Match] = non-empty iterator

scala> 

scala> matches foreach {md => println(md)}
a
a
a
a
a

scala> println(matches.size)
0
{% endcodeblock %}

The code just finds regular expression matches in a string, prints each of them, them prints how many it found (at least that's what I expected it would do).

But apparently, the `foreach` method consumes the data, and `size` returns 0.

Scala tries to tell me something with its warning about a `non-empty iterator`. Unfortunately, the same code run non interactively does not display any warning.

Obviously, it is not safe to keep references to some intermediate data structures (like iterators). I hope that as I learn more about the language, I will also learn to recognize such situations.