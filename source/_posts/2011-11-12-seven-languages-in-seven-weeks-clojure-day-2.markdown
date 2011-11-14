---
layout: post
title: "Seven Languages in Seven Weeks Clojure Day 2"
date: 2011-11-12 19:25
comments: true
categories: [Books]
tags: [7languages7weeks, clojure]
series: "Seven Languages in Seven Weeks"
---
Today Clojure's coverage is getting beefier with tail recursion (soso), lazy sequences and ... macros!
<!--more-->
The support for tail recursion is disappointing. But given that the JVM implementers never actually understood the value of tail recursion (or perhaps they overestimated its cost), it would have been surprising if a language running on top of the JVM had a solution. Scala is similarly limited.

Lazy sequences are cool, but not spectacularly so. It is a more restricted concept than laziness in Haskell. Still, lazy lists form a large part of my use of Haskell's laziness (perhaps the only part I can use), so I do not expect to feel limited in any way with Clojure.

Finally, macros. Writing macros separates men from boys. Writing macros that write macros separates gods from men. Macros are good. Abuse of macros can cause headaches, but the good kind (it's just your brain swelling).

Exercises
---------

### Implementation of common macros in Clojure

That one is very easy. The web site has a dedicated [page](http://clojure.org/macros) with the common macros. The source code is available for each. [`defmacro`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/defmacro) is actually a macro, and its source code is, well, let's just say I'm happy someone else wrote it. And yes, it is a macro that writes macros.

To expand the code of macros, I wrote this small function to recursively expand macro definitions:
{% codeblock rec-expand function lang:clojure %}
(defn rec-expand [lst]                                            
          (if (seq? lst) (map rec-expand (macroexpand lst)) lst))
{% endcodeblock %}

Without it, it is difficult to see deeply into the code:
```
user=> (macroexpand '(defn collection-type                                                                                   
             "Return either :list, :vector or :map, depending of the type of col."                      
             [col]                                                                                      
             (cond (list? col) :list
                   (map? col) :map
                   (vector? col) :vector)))
(def collection-type (.withMeta (clojure.core/fn collection-type ([col] (cond (list? col) :list (map? col) :map (vector? col) :vector))) (.meta (var collection-type))))
```

[`macroexpand`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/macroexpand) did not expand the [`cond`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/cond) macros.

With `rec-expand`:
```
user=> (rec-expand '(defn collection-type                                                                                   
             "Return either :list, :vector or :map, depending of the type of col."                      
             [col]                                                                                      
             (cond (list? col) :list
                   (map? col) :map
                   (vector? col) :vector))) 
(def collection-type (. (fn* collection-type ([col] (if (list? col) :list (if (map? col) :map (if (vector? col) :vector ()))))) withMeta (. (var collection-type) meta)))
```

### Implementation of a lazy sequence

For this exercise, I use [`lazy-seq`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/lazy-seq), which evaluates its body on demand (and remembers the value).

One first example is a simple reimplementation of `(iterate inc n)`:

{% codeblock upfrom lang:clojure %}
(defn upfrom [n] 
    "[n, n+1, n+2, ...."
    (lazy-seq (cons n (upfrom (inc n)))))
{% endcodeblock %}

Testing it:
```
user=> (take 10 (upfrom 0))
(0 1 2 3 4 5 6 7 8 9)
```

A bit more interesting, perhaps, is a Fibonacci sequence not using `map`:

{% codeblock Fibonacci sequence lang:clojure %}
(defn fibs [a b]
  "Fibonacci sequence starting with a, b, ..."
  (lazy-seq (cons a (fibs b (+ a b)))))
(def fib (fibs 1 1))
{% endcodeblock %}

Computing the 20 first Fibonacci numbers:
```
user=> (take 20 fib)
(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)
```

### Unless with else condition

For this exercise, I use two features that were not covered in the book

 * variable lists of arguments support
 * backquote notation

Clojure support for variable lists of arguments is nice: there can be a different body for each list, and the last one can a placeholder variable for "all the remaining arguments". The concept of arguments (and variable lists) applies to everything that takes arguments: functions, macros, ... Here I use it to differentiate between basic `(unless test body)` and `(unless test body else)`.

Backquote notation is what makes macro useable. Rather than using `list` to build the form, I use the backquote. Then expressions prefixed with tilde are replaced by their value. Lisps had an identical concept (but comma was used instead of tilde).

This makes macros shorter and easier to read.

{% codeblock extended unless lang:clojure %}
(defmacro unless 
  ([test body] `(if (not ~test) ~body)) 
  ([test body alt] `(if (not ~test) ~body ~alt)))
{% endcodeblock %}

Testing it:
```
user=> (unless false (println "Danger!"))
Danger!
nil
user=> (unless true (println "Danger!") (println "No danger...")) 
No danger...
nil
```

### Using defrecord and defprotocol

Ok, I'm not very inspired by this exercise. I was thinking of doing the classic shape class hierarchy, but finally settled for an employee protocol:
{% codeblock Employee protocol lang:clojure %}
(defprotocol Employee
  (work [e h])
  (get-raise [e a]))
{% endcodeblock %}

The first implementation is a manager, which is modeled according to things I've heard about other companies, not mine, where managers are hard-working and have to be pulled from their desk to be fed:
{% codeblock Manager implementation lang:clojure %}
(defrecord Manager [hobby]         
  Employee
  (work [_ h]
    (println (str "Working " (/ h 2) " hours then " (/ h 2) " hours of " hobby)))
  (get-raise [_ a]
    (println (str "You are too modest. You deserve " (* 1.1 a)))))
{% endcodeblock %}

Testing the manager:
```
user=> (def m (Manager. "golf"))
#'user/m
user=> (work m 8)
Working 4 hours then 4 hours of golf
nil
user=> (get-raise m 1000)
You are too modest. You deserve 1100.0
nil
```

Of course we need people to actually do something:
{% codeblock Worker implemenation lang:clojure %}
(defrecord Worker []         
  Employee
  (work [_ h]
    (println (str "Working " h " hours then " (/ h 2) " hours overtime")))
  (get-raise [_ a]
    (println (str "There is an economic crisis. Be happy with " (* 0.1 a)))))
{% endcodeblock %}

Testing the worker:
```
user=> (def w (Worker.))
#'user/w
user=> (work w 8)
Working 8 hours then 4 hours overtime
nil
user=> (get-raise w 100)
There is an economic crisis. Be happy with 10.0
nil
```

Ok, nothing fancy. But looking at the doc I see it is possible to extend basic Java classes, so I think there's far more depth to this construct. Otherwise, there's always `defmacro` to play with.

And this completes Day 2.