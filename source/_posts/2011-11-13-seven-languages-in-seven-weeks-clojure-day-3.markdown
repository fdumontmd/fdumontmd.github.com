---
layout: post
title: "Seven Languages in Seven Weeks Clojure Day 3"
date: 2011-11-13 20:30
comments: true
categories: [Books]
tags: [7languages7weeks, clojure]
---
The final day with Clojure covers its support for concurrent programming. It has a bestiary of concepts that are somewhat similar yet distinct from other modern languages.
<!--more-->
First of all Clojure adopts the [Software Transactional Memory](http://en.wikipedia.org/wiki/Software_transactional_memory), an approach mutable state and concurrency that is similar to databases. The idea is not really new, but it became more popular with Hashell's implementation.

There are also atoms, which are variables which guarantee atomic updates. The atomicity is a property of the atom, rather than of the functions that are used to update it, which is really nice.

Futures are also present. They represent values whose value is being computed in a dedicated thread. Trying to read from a [`future`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/future) could block if the value has not been computed yet (yes, Java has the same concept).

Finally, agents are ... somewhat perplexing. They look like agents in other languages (Io, Scala, Erlang, ...), except that they do not have any special behaviour attached. Instead, any function sent to them is queued and executed sequentially in a dedicated thread. At least, that's how I understand currently, but when I tried to use them for the Barber shop simulation, I ran into some problems, so there is more to this story.

Exercises
---------

### Find the implemenation of a blocking queue

The description of [`fill-queue`](http://richhickey.github.com/clojure-contrib/seq-utils-api.html#clojure.contrib.seq-utils/fill-queue) is a bit confusing, but it looks like a blocking lazy queue.

### Accounts simulation

The code is very straightforward. An account is a `ref`, whose value is the balance. I add a validator function to each to ensure that the balance is not negative (`check-balance` is passed to [`set-validator!`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/set-validator!) in `make-account`).

The `credit` and `debit` functions just update the balance with the [`alter`](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/alter) function. The transaction is not provided at this level just yet.

The `balance` function is just there for clarity.

The various `bank-` functions just operate on a vector of accounts, so they take the `bank` (a `vector`), and an `acc_num` (account number).

The `bank-transfer` is the only function using a transaction: it moves an amount between two accounts. To test the transaction failure mode, I credit the recipient account before I debit the other.

Finally, `bank-balances` prints the balance of all accounts.

{% codeblock Accounts simulation lang:clojure %}
(defn check-balance [b]
	"Check that the balance of account is not negative"
	(<= 0 b))

(defn make-account []
	"Create a new account"
	(let [r (ref 0)]
		(set-validator! r check-balance)
		r))	

(defn credit [account, amount]
	"Add amount to account's balance"
	(alter account + amount))

(defn debit [account, amount]
	"Debit amount from account's balance"
	(alter account - amount))
	
(defn balance [account]
	"Return balance of account"
	@account)

(defn make-bank [n]
	"Create a bank of n accounts"
	(vec (repeatedly n make-account)))
	
(defn bank-credit [bank, acc_num, amount]
	"Add amount to acc_num's balance"
	(credit (nth bank acc_num) amount))

(defn bank-debit [bank, acc_num, amount]
	"Debit amount from acc_num's balance"
	(debit (nth bank acc_num) amount))

(defn bank-balance [bank, acc_num]
	"Return the balance of acc_num"
	(balance (nth bank acc_num)))

(defn bank-transfer [bank, acc_num1, acc_num2, amount]
	"Transfer amount from acc_num1 to acc_num2 in bank"
	(dosync 
		(bank-credit bank acc_num2 amount)
		(bank-debit bank acc_num1 amount)))

(defn bank-balances [bank]
	"Show the balance of all accounts"
	(dotimes [i (count bank)]
		(println (str "Account " i ": " (bank-balance bank i)))))
{% endcodeblock %}

The test code:

{% codeblock Test code lang:clojure %}
;; create the bank
(def bank (make-bank 3))

;; put some money in
(dosync (bank-credit bank 0 100))

;; show the balances
(bank-balances bank)

;; a first transfer
(dosync (bank-transfer bank 0 1 75))

;; show the balances
(bank-balances bank)

;; not enough fund, so it will fail
(dosync (bank-transfer bank 0 2 75))

;; and indeed, the balances have not changed
(bank-balances bank)
{% endcodeblock %}

When input into the repl, the test code gives the following output:

```
user=> (def bank (make-bank 3))
#'user/bank
user=> (dosync (bank-credit bank 0 100))
100
user=> (bank-balances bank)
Account 0: 100
Account 1: 0
Account 2: 0
nil
user=> (dosync (bank-transfer bank 0 1 75))
25
user=> (bank-balances bank)
Account 0: 25
Account 1: 75
Account 2: 0
nil
user=> (dosync (bank-transfer bank 0 2 75))
java.lang.IllegalStateException: Invalid reference state (NO_SOURCE_FILE:0)
user=> (bank-balances bank)
Account 0: 25
Account 1: 75
Account 2: 0
nil
```

So the balance is protected from being negative, and the transaction is rolled back. Despite the fact that the amount was supposed to be credited to account number 2 first, the balance of that account has not changed as the transaction failed.

### Barber shop simulation

This problem was mostly a matter of figuring out the right model. The concept of agent seems to have some hidden complexity (or lock) which prevented my initial solution from working.

The idea is that the `barber` is an agent; it is updated by a function representing getting an haircut (`get-haircut`). The value of the agent is the number of client served.

The `waiting-room` is an atom. It's value is the number of free chairs.

The `get-haircut` function is sent to the `barber`. When it starts, it frees (increase) the `waiting-room` count of free chairs. Then it waits for 20 milliseconds, and finally updates the `barber` count of clients.

Finally, the `try-to-sit` function checks the number of free chairs: if none, the customer just leaves. Otherwise, the customer sits in a chair (signaling it's intent to get a haircut by sending `get-haircut` to the `barber`), and the count of free chairs is decreased. I tried to print a statement when a customer leaves because there is no chair, but there were too many such messages so I commented it out.

{% codeblock Barber problem, part 1 lang:clojure %}
(def barber (agent 0))

(def waiting-room (atom 3))

(defn get-haircut [count]
	(swap! waiting-room inc)
	(println "Start haircut")
	(Thread/sleep 20)
	(inc count))

(defn try-to-sit [count]
	(if (== count 0)
		(do
			;;(println "No free chairs. The customer leaves")
			count)
		(do
			(println "Sit in waiting room")
			(send barber get-haircut)
			(dec count))))
{% endcodeblock %}

When a new customer walks in, it will try to sit in a free chair. This is done in the `new-customer` function.

The `shop-loop` is a tail recursive loop that initiates a `new-customer` at random intervals. It runs until after at least `max` milliseconds.

{% codeblock Barber problem, part 2 lang:clojure %}
(defn new-customer []
  (swap! waiting-room try-to-sit))

(defn shop-loop [max]
	(let [orig (System/currentTimeMillis)]
		(loop []
			(if (< (- (System/currentTimeMillis) orig) max)
				(let [next (+ 10 (rand 20))]
					(Thread/sleep next)
					(new-customer)
					(recur))
				max))))
{% endcodeblock %}

Finally we get to the main function. First `init` is defined to help reset the values of atoms and agents. Then `start` will indeed reset all values, then run `shop-loop` in a `future`, wait the required amount of time, and finally get the current value of `barber` (which might not have fully finished). My first version tried to run `shop-loop` on a dedicated agent, but that was for some reason blocking on the `barber` agent, so I used a `future` instead.

{% codeblock Barber problem, part 3 lang:clojure %}
(defn init [_ v] v)

(defn start [max]
	(println "starting")
	(send barber init 0)
	(swap! waiting-room init 3)
	(future (shop-loop max))
	(Thread/sleep max)
	@barber)
{% endcodeblock %}
	
With this implementation, the barber performed 458 hair cuts (500 was the theoretical maximum).

Wrapping up Clojure
-------------------

There is much more to Clojure that what has been covered so far. The object model is far more flexible than Java's. Metadata can be added to various entities, which I understand could be used to improve interactive usage (documentation strings are already a kinf of metadata).

I always found Lisp languages to be very elegant and expressive; Clojure definitively is a Lisp, and I really wanted to like the language. Yet for some reason I found it somewhat disappointing. Perhaps it is the limitations the designers put (for good reason) in the language. Perhaps it is the fact that the language runs on the JVM, an environment that I instinctively link to all things corporate, safe and boring, so I don't really believe it could be exciting.

I'm not really sure, perhaps it is just that I'm feeling tired and I have a cold, and Clojure really is awesome. So I'll have another look at that language, and I hope I can give it the attention it seems to deserve.