---
layout: post
title: "Seven Languages in Seven Weeks - About Io"
date: 2011-10-21 09:14
comments: true
categories: [Books]
tags: [7languages7weeks, io]
series: "Seven Languages in Seven Weeks"
short_title: "About Io"
---
[Yesterday](/blog/2011/10/19/seven-languages-in-seven-weeks-io-day-3) in wrapping up I dismissed the language in a very terse manner; I thought I should expand on this.
<!--more-->
Io intends to be a number of things:

 * an embedded language
 * a concurrent language with actor model
 * a prototype based language

The problem is that each of these is well covered by more popular alternatives. [Lua](http://www.lua.org/) is designed as an embedded language and widely used as such. [Erlang](http://www.erlang.org/) is the typical actor based concurrent language, and once again, widely used as such (interestingly, on the [Io FAQ page](http://iolanguage.com/about/faq/), there is a reference to a benchmark highlighting the performance of coroutine based servers. But the coroutine based server is actually about an Erlang server, [Yaws](http://yaws.hyber.org/)). And new languages, such as [Scala](http://www.scala-lang.org/), also implements the actor model.

As for prototype based language, I am not sure there is an intrinsic value in such a design choice (which is why I hesitate to call it a feature). But JavaScript is also a prototype based language, and once again, it is very widely used.

Does that mean there is no place for Io? I wouldn't say so; but the main problem I have with the language is that I just can't figure where such a language would shine.

A [thread](http://lambda-the-ultimate.org/node/2596) on [Lambda the Ultimate](http://lambda-the-ultimate.org/) introduces interesting arguments both for and against the language. The pro links in particular are intriguing. Various new behaviours can be implemented on the language. But then the question becomes: would such behaviours be in any way useful for solving real problems, rather than as an intellectual exercise?