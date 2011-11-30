---
layout: post
title: "Seven Languages in Seven Weeks Wrap Up"
date: 2011-11-26 12:24
comments: true
categories: [Books]
tags: [7languages7weeks]
series: "Seven Languages in Seven Weeks"
---
Wow, seven weeks already. And seven languages later, time to see where I can go from here.
<!--more-->
This was a rather pleasant journey. I felt that some exercises with Io and Scala were more frustrating than the rest, but overall it was not too taxing. 

It brought interesting benefits: I got a better look at emerging trends in software, but now it is up to me to develop this first look into something useable.

General comments
----------------

The exercises level was rather uneven. I guess that is inevitable, given that some of the languages were introduced as stepping stones for others. Still, from a CSV parser (even one using meta-programming) to a backtracking maze solver, there's a fairly large range.

The idea of just giving enough information to get started, but not so much that the book is the only source of information needed to solve the exercises, was very good. A language is not just a syntax, a semantic, and a library. It is most of all a community, a set of websites, forums, mailing lists, ... It was smart to push the reader towards that.

And yes, clearly, each language was barely covered, but some of the third day exercises were hinting at the more advanced features.

All in all, this was a good book. It is useful to be exposed to different languages, if only to expand one's approach to problem solving. The features of tomorrow's mainstream languages are being explored today in smaller languages; being at least aware of this evolution is required to grow as a professional software engineer.

But the author spends way too much time complaining about syntax (or claiming that a particular syntax is a weakness). I find the accusation that Scala's syntax is too academic particularly pointless. Rightfully, he adds that this is subjective, but what on earth is an academic syntax? A syntax is either easy on the fingers or not (the former being better), and either adapted to the semantic or not. It can support the features, or make them ugly. It should be concise in what it promotes, and verbose in what it discourages. 

As far as I could tell, each language had a syntax that matches its needs and those of its users.

Conclusion: I recommend the book. Even knowing some of the languages I still learned a few things, and for those languages I knew, I had a chance for a second look and a new appreciation of what they offer.

From here to a bit further
--------------------------

It would be unfortunate if this book was the last step I took in these various languages. So here I recap what I think of each language, and where I'd like to go with them.

### Ruby

I like Ruby. It's fun, open, does not make judgment about what is proper software engineering (who cares if you like monkey patching...). The syntax binds all the features well, and the ecosystem is really interesting.

It might not have the performance, and no concurrency model worth mentioning, but as a new glue or script language, or perhaps for the Rails platform (although my current needs are not met by RoR), it shines.

I had read the [Programming Ruby](http://pragprog.com/book/ruby/programming-ruby) (the pickaxe book), and a few books on Rails, and I liked what I saw.

I'll probably follow up with [Metaprogramming Ruby](http://pragprog.com/book/ppmetr/metaprogramming-ruby). 

And as I'm doing a lot of web development, I will try to look at Ruby driven [Selenium](http://seleniumhq.org/).

### Io

Well, I won't spend much more time with Io. I really don't see any advantages. Yes, it's compact, the interpreter is small, so it's ideal for embedded languages. Which I don't need.

As I said earlier, there is a prototype based, functional Lisp inspired language that is worth learning. It is supported by several major software companies or organisations, has users in the billions (or will soon): Javascript.

### Prolog

I had learned Prolog at university. I had enjoyed it at the time, and doing the exercises reminded me of that fun.

Prolog is, by and large, about exploring search trees, so the techniques it teaches are usable in other languages. The maze solver in Haskell is essentially a tree search and would be implemented the same way in Prolog.

I'm glad that a recent trip back to Europe allowed me to retrieve my two Prolog books: [The Art of Prolog](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=8327) and [The Craft of Prolog](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=8336). Both are large, so it might take a while.

### Scala

I didn't know Scala but had heard about it and was very curious. It is indeed fairly expressive, more concise than Java, and the concurrency model much better.

The type system is somewhat strange: much better than Java, but more verbose and less flexible than Haskell, and sometimes with bizarre results: the concatenation of a `List` of `Int` and a `List` of `String` is a `List` of `Any`...

In any case, Scala seems to be gaining momentum, so I'll try and learn more about it in the near future. There are plenty of books, at least one free ([Programming Scala](http://programmingscala.com/), from O'Reilly), and one by the language creator ([Programming in Scala](http://www.artima.com/shop/programming_in_scala)).

### Erlang

Erlang was another language I had checked before, yet this book showed me something new. An important development of Erlang is OTP (Open Telecom Platform), but until very recently, there were few resources to learn about it.

The whole concept of supervisors is just the beginning; as far as I can tell, OTP offers a large range of tools to monitor and keep processes alive on a network. Where this really becomes interesting, is that Scala is adopting some ideas ([Akka](http://akka.io/) is inspired by Erlang/OTP), so some knowledge of one can be reused.

There is an entry level book, [Programming in Erlang](http://pragprog.com/book/jaerlang/programming-erlang), by Joe Armstrong (Erlang's father), and a couple of more advanced books, specifically on OTP: [Erlang and OTP in Action](http://www.manning.com/logan/) and [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do). I'm going through Programming in Erlang, then the in Action one.

### Clojure

Clojure is a Lisp for the JVM. That in itself should make it interesting, but somehow it does not do it for me. Maybe it is that Clojure is not as good Lisp it could be because of the JVM limitations. Or maybe I'm just not into Lisp anymore (I used to like Perl too, but wonder why today).

But with my professional investment in the JVM, I will probably investigate Clojure a bit further. Just not urgently.

It might be interesting to see if [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html) can easily be ported to Clojure.

### Haskell

Haskell is an interesting language. But it is extremely demanding, and there are few resources to come to grip with it. I knew enough about Haskell to go through this book easily, but whenever I try to go further, I find that difficulties accumulate quickly.

It appears that Haskell's features are all somewhat complicated in their own way:

#### Types

Haskell type system is perhaps the most sophisticated of all widely used type systems. Basic Haskell type system is already very rich, but [GHC](http://www.haskell.org/ghc/) adds extensions that brings the expressivity of the type system to new heights.

I would assume that Haskell strength must be in this type system; there are interesting libraries and applications that are built around clever use of types. But there are few resources on using types for design (or designing with types), except for very academic papers with no obvious practical uses (I probably lack imagination)

GHC's extensions are similarly explored in other academic papers, but the sum of these extensions is hard to make sense of (and it sometimes feels like the authors of GHC are also trying to make sense of this patchwork and find a more consistent set of features).

This is in serious needs of a good book, but I can't think of any that fits the bill.

#### Laziness

Haskell's laziness goes much further than Clojure's. I did not mention it in [Haskell Day 3](/blog/2011/11/19/seven-languages-in-seven-weeks-haskell-day-3/)'s Maze solution, but laziness makes this implementation very effective: only the first solution is computed. While the implementation is strictly equivalent to nested list comprehensions, only the part required for the first output is actually run.

Laziness (combine with the List Monad) makes the algorithm very short: a mere 14 lines. But it makes it also very obscure. I know, intellectually, that laziness will run just a much as needed, but I don't understand it as well as, say, I understand the time and space cost of an algorithm in C.

When digging a bit deeper in Haskell, newcomers learn to be wary of laziness: it introduces space leaks that can be hard to track. Solutions exist, but are spread in academic papers that require extensive understanding of the whole background theories (why do academic papers always refer to other, more obscure academic papers, and never to a single "Idiot Guide to..."?)

Of all the features of Haskell, I would think that laziness is the harder to master. There are few generally available resources. [Purely Functional Data Structures](http://www.cambridge.org/gb/knowledge/isbn/item1161740/?site_locale=en_GB) explores laziness and provides a theoretical framework to compute the time cost of lazy algorithms, but does so assuming a partial laziness built on top of SML.

[Algorithms: A Functional Programming Approach](http://www.iro.umontreal.ca/~lapalme/Algorithms-functional.html) is about Haskell, but appears to be out of print (glad I got a copy when I first got interested in Haskell).

Both books are in my todo stack (which by now requires all my skills in rock climbing to handle).

#### Syntax

Haskell's syntax is another of its strengths: it is very compact, and the type system abstracts away many details. So it's no surprise that many people (or maybe just me) have a problem with it.

I learned OCaml at university (those were good days). My first few assignments, I had to implement my own list like data type, because I could not figure out how to use the default list in pattern matching. The language was so weird that it did not even occurred to me that a language whose standard list type was unusable would not leave the lab it grew in. Then eventually the syntax settled in my brain, and I started to wonder why it had been so hard before.

Haskell is similar (they are both somewhat descendant of ML), but far more abstract (OCaml does not support either type classes or user operators). So just looking at an expression, it is not easy to assign it a type (there might be many) or a single meaning (there might be many as well).

Another thing that I find hard to keep under control is the creation of anonymous functions. Between higher order functions that produce functions, and the do notation (which produces functions as well), it is hard to track what is going on, even it apparently simple expressions.

This is not to say that Haskell's syntax does not "work". It does, clearly, but the expressivity it grants is difficult to master.

#### The compiler

The compiler is another peace of the puzzle. All compilers are fantastically complex beasts; the underlying theories are just as fascinating as they are overwhelming. But in general, at least with regular imperative (or perhaps just with strict languages), my intuitions about what is costly, what is a possible leak, ... just based on the code, are usually correct. With Haskell, there are many areas where I'm not sure what the compiler will do. Functional languages enthusiasts used to say that any inefficiency in their favourite language was just waiting for a sufficiently advanced compiler, but now I have a feeling that understanding what the compiler is doing is just as hard as figuring out program wide memory allocation in C...

And it matters. Strictness analysis, for instance, is used by GHC (and, I suppose, other Haskell compilers) to figure out which expressions could be strict (i.e. not lazy) without changing the overall semantic. This changes the time and space cost of some expressions, and makes performance difficult to predict.

#### The meta-language

Haskell is its own meta-language. What is a core feature in most languages is just a library in Haskell. This means that there is little uniformity even in fundamental part of the languages. There are [8 ways to report errors](http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors) (perhaps more today). And at least that many monad transformer libraries.

Having to combine these various libraries (and more importantly, the libraries that use them) is difficult, and adds accidental complexity. What makes it more frustrating is that Haskell type system is otherwise very effective at making it easy to create code parts that can be combined in various ways (while checking that the combination always make some sense).

It would be good to standardise some of these, and maybe the [Haskell Platform](http://hackage.haskell.org/platform/) will achieve this, although it would be nice if it was keeping up with at least GHC (at the time of writing, version 7.0.3 is in the Platform, but 7.2.2 is already out, with important changes).

#### and so on...

I would like to like Haskell. But if any language can be accused of being too academic, it is Haskell. Sometimes it feels like the main purpose of the language is to make it possible to prove, through the type system, that a specific, inefficient sort algorithm is indeed a sort algorithm.

I'm really curious about organisations that use Haskell for commercial purposes. What kind of features do they use the most? Is laziness really a good thing, or it is flushed away from the code and replaced by explicit on demand evaluation? Given a chance, I'd like to work in such an organisation, as I feel Haskell requires a different approach to large scale problem solving than the one I'm used to.

### Other topics

From the same publisher ([The Pragmatic Bookshelf](http://pragprog.com/)) as this book,
[Seven Databases in Seven Weeks](http://pragprog.com/book/rwdata/seven-databases-in-seven-weeks) is looking very interesting. I'm also professionally bound to SQL, so I haven't really been paying attention to the whole NoSQL movement. Yet there are definitively interesting things going on, and the variety of alternative database models makes such a book a welcome introduction.

As I've been doing way too much sequential processing in the past, I am looking to expand my horizon and learn about large scale concurrency (Erlang, Scala, ... help here), and asynchronous, event based processing. [Event Processing in Action](http://www.manning.com/etzion/) appears to be a decent way to get started.

Finally, and while it is completely unrelated to whatever this book was about, I haven't studied algorithms since I left university. I really feel I should get back to that topic, and [The Algorithm Design Manual](http://www.algorist.com/) is just what I need.