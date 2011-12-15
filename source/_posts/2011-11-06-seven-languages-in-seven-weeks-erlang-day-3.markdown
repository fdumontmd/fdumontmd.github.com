---
layout: post
title: "Seven Languages in Seven Weeks Erlang Day 3"
date: 2011-11-06 10:17
comments: true
categories: [Books]
tags: [7languages7weeks, erlang]
series: "Seven Languages in Seven Weeks"
---
One last day with Erlang; it was about time we get to perhaps its most significant aspect: its concurrency model, and recipe for error recovery, embodied in its motto, "Let it crash."
<!--more-->
Erlang makes it very cheap and easy to spawn processes (in this context, internal processes. External processes are called nodes), and have the various processes communicate by sending messages to each others. Extending the communication to the network follows the same pattern, and is just as easy.

Processes can also monitor each others, in various ways, so that a crashed process can be restarted. As it is both easy and natural to do, Erlang discourages complex error recovery methods, and instead proposes that processes be allowed to crash, and be restarted. While it is possible to abuse this principle, it can lead to much simpler code and general logic; the lack of mutable data in this regard is another advantage, as there is less risk to leave anything in an inconsistent state (it is still possible, as there are updatable stores).

The book rightfully does not claim to be exhaustive in its coverage, but the exercises are rich and complex enough to offer a glimpse of what is possible in Erlang.

Other capabilities, such as the ability to update code while running, are not covered, but participate in the set of features that make Erlang so suited for robust applications.

Exercises
---------

### An OTP service that will restart a process if it dies

That sounds like the [`supervisor`](http://www.erlang.org/doc/man/supervisor.html) module's job description. It is not trivial, however: supervised processes must have a descriptor that explains how to start, stop and restart them.

There is a fairly detailed description of the setup in [Learn you some Erlang for great good](http://learnyousomeerlang.com/), but it requires a good understanding of everything that comes before. A much shorter introduction is found [here](http://www.hccp.org/supervisors.html).

For instance, using `supervisor` for keeping the `translate_service`
up and running (assuming it dies on unknown words):

{% include_code Supervised Translation Service lang:erlang 7l7w/erlang/translate_service_sup.erl %}

I register the spawned process to the atom `translator`; this is because I could not find a way to retrieve the process id of the translate loop.

Starting the supervised process is now simple:

{% codeblock Using the Supervised Translator lang:erlang %}
1> c(translate_service_sup).
{ok,translate_service_sup}
2> translate_service_sup:start_service().
start_service
Starting...
{ok,<0.38.0>}
3> translate_service_sup:translate("casa").
"house"
4> translate_service_sup:translate("que"). 
Starting...
"I do not understand"
5> translate_service_sup:translate("casa").
"house"
6> translate_service_sup:translate("que"). 
Starting...
"I do not understand"
7> translate_service_sup:translate("que"). 
** exception exit: shutdown
{% endcodeblock %}

As seen above, the translator chokes on an unknown word, but is restarted immediately. However, if the process dies too often (here, more than once per minute), the supervisor kills it definitively.

### Documentation for building a simple OTP server

That question a bit open ended. If we are talking about a simple TCP server, then the [`gen_tcp`](http://www.erlang.org/doc/man/gen_tcp.html) module is enough to get a server, as shown in [this post](http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/).

To go further, and add supervisors and other OTP goodies, there is a [tutorial](http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles) on 
[trapexit.org](http://www.trapexit.org/), but it uses code generating tools which are not part of the standard Erlang distribution.

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

For this exercise, I will go back to the simpler, original `doctor` module (the one that only keeps the shooter alive).

I will use three atoms, `doctor`, `monitor` and `revolver` (for `roulette:loop`), to register the processes. The `doctor:loop` will be able to restart both `monitor` and `shooter`; `monitor:loop` will be able to restart `doctor`; `roulette` will have no such capacity.

To keep things easy, I will also make sure the processes can easily be killed.

First, I've move the function to look for a process by name to its own module; I also make sure that each module has a `start` function that registers a spawned process, and triggers the initialization process. This function is now the argument to the `look_for` function, so it no longer has to do the spawning and registering.

{% include_code Look for process lang:erlang 7l7w/erlang/mon_doc/look.erl %}

The `doctor` module is similar to the original. The differences are that it respond to the `die` message, also it has a `start` message that checks for the `monitor` process. It uses the `monitor:start` function, as explained above, so that if the `monitor` has to be started, it will also be properly initialized.

{% include_code Doctor lang:erlang 7l7w/erlang/mon_doc/doctor.erl %}

The `monitor` module is simpler: it only needs to check for the `doctor` process, running the `doctor:start` function if needed.

{% include_code Monitor lang:erlang 7l7w/erlang/mon_doc/monitor.erl %}

Finally, the slightly modified `roulette` module (it now has a `roulette:start` function):

{% include_code Roulette lang:erlang 7l7w/erlang/mon_doc/roulette.erl %}

The basic doctoring still works:

{% codeblock Testing the Monitored Doctor lang:erlang %}
1> doctor:start().
Looking for monitor...
start
Creating and monitoring process, attaching to atom monitor.
Looking for revolver...
Looking for doctor...
Creating and monitoring process, attaching to atom revolver.
reattaching to running process doctor.
2> revolver ! 1.
1
click.
3> revolver ! 3.  
3
bang.
The process <0.36.0> died with reason {roulette,die,at,{21,33,1}}.
Looking for monitor...
reattaching to running process monitor.
Looking for revolver...
Creating and monitoring process, attaching to atom revolver.
4> revolver ! 1.
1
click.
{% endcodeblock %}

The `doctor` can be killed; `monitor` will restart it:

{% codeblock Testing the Monitored Doctor 2 lang:erlang %}
5> doctor ! die.
die
Aaargh...
The process <0.33.0> died with reason {doctor,died,at,{21,33,47}}.
Looking for doctor...
Creating and monitoring process, attaching to atom doctor.
Looking for monitor...
reattaching to running process monitor.
Looking for revolver...
Creating and monitoring process, attaching to atom revolver.
** exception error: {doctor,died,at,{21,33,47}}
6> revolver ! 3.
3
bang.
The process <0.43.0> died with reason {roulette,die,at,{21,33,56}}.
Looking for monitor...
reattaching to running process monitor.
Looking for revolver...
Creating and monitoring process, attaching to atom revolver.
7> revolver ! 1.
click.
1
{% endcodeblock %}

Finally, `monitor` can also be killed; it is restarted by `doctor`:

{% codeblock Testing the Monitored Doctor 3 lang:erlang %}
8> monitor ! die.
die
Aaargh...
The process <0.35.0> died with reason {monitor,died,at,{21,34,44}}.
Looking for monitor...
Creating and monitoring process, attaching to atom monitor.
Looking for revolver...
Looking for doctor...
reattaching to running process revolver.
reattaching to running process doctor.
9> doctor ! die.
Aaargh...
die
The process <0.42.0> died with reason {doctor,died,at,{21,34,47}}.
Looking for doctor...
Creating and monitoring process, attaching to atom doctor.
Looking for monitor...
reattaching to running process monitor.
Looking for revolver...
Creating and monitoring process, attaching to atom revolver.
10> revolver ! 3.
bang.
3
The process <0.52.0> died with reason {roulette,die,at,{21,34,51}}.
Looking for monitor...
reattaching to running process monitor.
Looking for revolver...
Creating and monitoring process, attaching to atom revolver.
11> revolver ! 1.
click.
1
{% endcodeblock %}

### Logging messages to a file

For this I will just derive the code from the one on this [post](http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/) (which I mentioned above).

Rather than echoing the received messages, they are written to disk:

{% include_code Logging messages lang:erlang 7l7w/erlang/logger.erl %}

This works nicely because the [`file:write`](http://www.erlang.org/doc/man/file.html) function expects bytes rather than a string.

### Network aware `translate_service`

Once again I use the simple TCP server code. For this exercise, I have extended the translation service so that it can learn new words in a given session: using the `put word:meaning` command, the server will learn a new word. This is done by keeping a dictionary (a list of tuples) as argument for the `recv_loop` function.

I convert from binary to list because they are slightly easier to work with.

{% include_code Network aware translate_service lang:erlang 7l7w/erlang/trans/translate_service.erl %}

As seen in the interaction below, the translator knows its basic words, and can learn new ones:

```
$ telnet localhost 9000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
casa
house
blanca
white
que
I do not understand que
put que:what
Noted
que
what
^]
telnet> Connection closed.
```

Wrapping up Day 3 and Erlang
----------------------------

Erlang was a fun and interesting language to play with. Looking at the documentation and existing tutorials (especially the intermediate to advanced ones), I had a feeling that, if Java can be said to be Enterprise, Erlang is Industrial. It does look like a control panel in a power plant: rich, complex and yet as simple as possible for what it is meant to achieve, and likely to kill you if you approach it with buzzwords in mind.

This is another language I will be eager to get back to.
