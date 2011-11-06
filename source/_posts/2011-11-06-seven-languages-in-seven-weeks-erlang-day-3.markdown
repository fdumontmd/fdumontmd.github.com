---
layout: post
title: "Seven Languages in Seven Weeks Erlang Day 3"
date: 2011-11-06 10:17
comments: true
categories: [Books]
tags: [7languages7weeks, erlang]
series: "Seven Languages in Seven Weeks"
published: false
---
One last day with Erlang; it was about time we get to perhaps its most significant aspect: its concurrency model, and recipe for error recovery, embodied in its motto, "Let it crash."
<!--more-->
Erlang makes it very cheap and easy to spawn processes (in this context, internal processes. External processes are called nodes), and have the various processes communicate by sending messages to each others. Extending the communication to the network follows the same pattern, and is just as easy.

Processes can also monitor each others, in various ways, so that a crashed process can be restarted. As it is both easy and natural to do, Erlang discourages complex error recovery methods, and instead proposes that processes be allowed to crash, and be restarted. While it is possible to abuse this principle, it can lead to much simpler code and general logic; the lack of mutable data in this regard is another advantage, as there is less risk to leave anything in an inconsistent state (it is still possible, as there are updatable stores).

The book rightfully does not claim to be exhaustive in its coverage, but the exercises are rich and complex enough to offer a glimpse of what is possible in Erlang.

Other points, such as the ability to update code while running, are not covered, but participate in the set of features that make Erlang so suited for robust applications.

Exercises
---------

### An OTP service that will restart a process if it dies

That sounds like the [`supervisor`]() module TODO add reference. 

### Documentation for building a simple OTP server

Perhaps [`gen_server`]() module TODO add reference. Or a link to a tutorial.

### Monitor the `translate_service`

Just to explore the API introduced today, I went a bit beyond the exercise, and made the `doctor` able to monitor any kind of process, and attach it to a supplied atom.

First, a utility function `start` creates a new `doctor` process, then sends it a message to spawn the monitored process.

The new `loop` function takes both the function to be spawned and monitored, and the atom to attach it to. Both parameters are kept in the loop as argument.

{% include_code Improved doctor Module lang:erlang 7l7w/erlang/doctor.erl %}

The `translate_service` is slightly modified to die of shame on untranslatable words.

{% include_code Modified translate_service Module lang:erlang 7l7w/erlang/translate_service.erl %}

With these defined, it is possible to run two monitored services and attach them to different atoms:

{% codeblock Testing the Doctor lang:erlang %}
1> DocRev = doctor:start(fun roulette:loop/0, revolver).
Creating and monitoring process, attaching to atom revolver.
<0.33.0>
2> DocTrans = doctor:start(fun translate_service:loop/0, translator).
Creating and monitoring process, attaching to atom translator.
<0.36.0>
3> revolver ! 1.
1
4> revolver ! 3.
3
5> revolver ! 1.
1
6> translate_service:translate(translator, "casa").
click.
bang.
The process revolver <0.34.0> died with reason {roulette,die,at,{10,31,0}}. Restarting
.Creating and monitoring process, attaching to atom revolver.
"house"
7> translate_service:translate(translator, "que").
The process translator <0.37.0> died with reason {"que",not_understood,
                                                  received_at,
                                                  {10,31,0}}."I do not understand"
 Restarting
.Creating and monitoring process, attaching to atom translator.
8> translate_service:translate(translator, "casa").
"house"
{% endcodeblock %}

Both services are restarted properly when dying.

### Self monitoring Doctor

For the Doctor to successfully monitor itself, it should also be able to restart (or restart monitoring) whatever process it was monitoring when it died.

For this, I create a new `d_monitor` function that spawns a monitor loop. The atom argument is used (or abused) to indicate whether we are starting an doctor monitoring loop, or the target function monitoring loop. In the former case, `d_monitor` is called again; in the latter the target function is spawned. 

When `d_monitor` is called from `start`, the former case is triggered; when called from `loop`, the latter case is.

This means that for every call to `start`, there is two "nested" `loop`s running, the outside one monitoring the inside one, and the inside one in charge of the target function.

To test the code, I gave the inside `loop` a non zero parameter which is decreased every time it has to restart the monitored function. When it reaches zero, the monitoring stops, and has to be restarted as well.

Finally, instead of spawning directly the target function, I use the `check_for` function to first try to locate a process given the atom; only if this fails would a new process be spawned. The function is not very robust (in particular, if two monitors are trying to restart the process at the same time, one might fail rather than reattaching, because of the order of concurrent evaluation).

{% include_code Module Doctor_2 lang:erlang 7l7w/erlang/doctor_2.erl %}

To test the new code, I have written it so that the inner `loop` will die after 3 restarts of the monitored process:

{% codeblock Self monitoring Doctor lang:erlang %}
1> doctor_2:start(fun roulette:loop/0, revolver).
Creating and monitoring process.
Creating and monitoring process, attaching to atom revolver.
<0.33.0>
2> revolver ! 3.
bang.
3
The process revolver <0.35.0> died with reason {roulette,die,at,{11,6,30}}. Restarting
.Creating and monitoring process, attaching to atom revolver.
3> revolver ! 3.
bang.
3
The process revolver <0.38.0> died with reason {roulette,die,at,{11,6,32}}. Restarting
.Creating and monitoring process, attaching to atom revolver.
4> revolver ! 3.
bang.
3
The process revolver <0.40.0> died with reason {roulette,die,at,{11,6,34}}. Restarting
.The process {out,revolver} <0.34.0> died with reason {running,out_of,time,
                                                      {11,6,34}}. Restarting
.Creating and monitoring process.
Creating and monitoring process, attaching to atom revolver.
5> revolver ! 1.
click.
1
{% endcodeblock %}

As seen at the third call to `revolver ! 3`, after restarting the process, the monitor dies, then is restarted and attaches to the already running `revolver` process.

### Monitoring the monitor. And vice-versa

### Logging messages to a file

### Network aware translate_service