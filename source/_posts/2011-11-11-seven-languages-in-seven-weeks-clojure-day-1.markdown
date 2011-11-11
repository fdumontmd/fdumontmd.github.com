---
layout: post
title: "Seven Languages in Seven Weeks Clojure Day 1"
date: 2011-11-11 08:02
comments: true
categories: [Books]
tags: [7languages7weeks, clojure]
series: "Seven Languages in Seven Weeks"
---
Sixth week, sixth language, this time [Clojure](http://clojure.org/), the latest attempt to make [Lisp](http://en.wikipedia.org/wiki/Lisp_(programming_language\)) popular.
<!--more-->
Clojure has the usual features of Lisp: code as data, macros, lists among many other containers (no, Lisp is not just lists), and so on. It also brings other features, such as pattern matching, and treating some containers as functions for specific purposes (both features that [Arc](http://arclanguage.org/), the language that [Paul Graham](http://www.paulgraham.com/) invented, seems also to have). Pattern matching is certainly a welcome feature; I have to see more Clojure code to figure out whether I like the data as function one (I'm sure it allows very neat idioms).

Clojure is also supports lazy evaluation, like Haskell (which is the last language for this book).

Finally, Clojure runs on both the JVM and the CLR, which allows it to go wherever either platform goes (which means pretty much everywhere), and to reuse these platforms' extensive libraries.

The first day covers little, compared to other languages. Various containers are introduced, along with some useful functions operating on them; pattern matching is described (it is similar to Erlang's and other functional languages); finally we learn how to define functions, and use them in higher-order functions like [`apply`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/apply).

Exercises
---------

Comparatively to previous languages' first day, today is very short and simple.

### Define function big

Nothing special here, the function [`count`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/count) works on all collections, including strings.

{% codeblock Define function big lang:clojure %}
user=> (defn big  
             "Is string st longer than n characters?"
             [st n]
             (> (count st) n))
#'user/big
{% endcodeblock %}

Testing:

{% codeblock Test function big lang:clojure %}
user=> (big "hello" 5)
false
user=> (big "hello" 4)
true
{% endcodeblock %}

### Define function collection-type

Using only functions and concepts seen so far, a first implementation using map. First I use the repl to get the class of these containers:

{% codeblock class of containers lang:clojure %}
user=> (class ())  
clojure.lang.PersistentList$EmptyList
user=> (class '(1))
clojure.lang.PersistentList
user=> (class []) 
clojure.lang.PersistentVector
user=> (class {})
clojure.lang.PersistentArrayMap
{% endcodeblock %}

So the empty list has a different class from a non-empty list.

Now I can map these classes to the proper symbol:
{% codeblock mapping classes to symbols lang:clojure %}
(def col-classes {(class '(1)) :list, (class '()) :list, (class []) :vector, (class {}) :map})
{% endcodeblock %}

With this, getting the right answer is as simple as:
{% codeblock lang:clojure%}
user=> (col-classes (class '()))
:list
user=> (col-classes (class '(1)))
:list
user=> (col-classes (class [1 2 3]))
:vector
user=> (col-classes (class {:one "one"}))
:map
{% endcodeblock %}

So the function can be written as:

{% codeblock Define function collection-type lang:clojure %}
user=> (defn collection-type                                                                                   
             "Return either :list, :vector or :map, depending of the type of col."                      
             [col]                                                                                      
             ({(class '(1)) :list, (class '()) :list, (class []) :vector, (class {}) :map} (class col)))
#'user/collection-type
{% endcodeblock %}

Testing it on literal values:

{% codeblock Test function collection-type lang:clojure %}
user=> (collection-type ())
:list
user=> (collection-type '())
:list
user=> (collection-type '(1 2 3))
:list
user=> (collection-type [1 2 3]) 
:vector
user=> (collection-type {:one "one"})
:map
{% endcodeblock %}

Interesting, perhaps, but there must be a better way. Using type predicates ([`list?`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/list?), [`map?`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/map?) and [`vector?`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/vector?)), and the already seen [`if`](http://clojure.org/special_forms#Special%20Forms--(if%20test%20then%20else?)):

{% codeblock collection-type, predicate edition lang:erlang %}
user=> (defn collection-type                                                                                   
             "Return either :list, :vector or :map, depending of the type of col."                      
             [col]                                                                                      
             (if (list? col) :list
                 (if (map? col) :map
                     (if (vector? col) :vector))))
#'user/collection-type
{% endcodeblock %}

Testing output is not repeated, as it is identical to the one above.

These nested `if`'s are ugly. Fortunately, Lisp has a kind of generalized `switch`, called [`cond`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/cond). So the definition above is equivalent to

{% codeblock collection-type, cond predicate edition lang:erlang %}
user=> (defn collection-type                                                                                   
             "Return either :list, :vector or :map, depending of the type of col."                      
             [col]                                                                                      
             (cond (list? col) :list
                   (map? col) :map
                   (vector? col) :vector))
#'user/collection-type
{% endcodeblock %}

Now this is clean and elegant.

Once again, the test returns the expected results so they are not reproduced.

One thing to note: the [`cons`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/cons) function does not return a list, but a sequence (this is unlike `cons` in all the other Lisps):

{% codeblock cons is not a team player lang:clojure %}
user=> (cons 1 '(2 3))
(1 2 3)
user=> (class (cons 1 '(2 3)))
clojure.lang.Cons
user=> (list? (cons 1 '(2 3)))
false
user=> (seq? (cons 1 '(2 3)))
true
{% endcodeblock %}

Fortunately, [`assoc`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/assoc) is better behaved:

{% codeblock assoc lang:clojure %}
user=> (collection-type (assoc  {:two 2} :one 1))
:map
{% endcodeblock %}

I like the idea of a collection's type not changing when I add element, so the behaviour of `cons` is something I will have to watch for (or find the better way that must exist).

And this is all for Day 1.