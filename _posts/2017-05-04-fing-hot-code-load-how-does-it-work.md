---
layout: post
title: F'ing hot code loading, how does it work?
permalink: "/fing-hot-code-load-how-does-it-work/"
published: true
---

### Hot code loading in Erlang, how does that work?

You probably know by now that one of the killer features of Erlang (besides the concurrency thing) is the ability to change code without downtime. This is something
that OTP already gives us for free and provides great business value (time is money), this is an attempt to uncover what's really going on being the scenes when
you perform a hot code load.

So let's start with the simplest possible thing which is a process that offers a KV-like interface with just two methods: `get/1` and `set/2`, no OTP is used,
only plain old `!` and `receive`:  

{% highlight erlang %}
-module(simple_proc).

-export([start/1,
         set/2,
         get/1]).

-record(state, {
    value }).

start(Args) ->
    spawn_link(fun() ->
                State = init(Args),
                loop(State)
               end).

set(Pid, Value) ->
    Pid ! {set, Value},
    ok.

get(Pid) ->
    Pid ! {get, self()},
    receive
        Reply -> Reply
        after 5000 -> {error, timeout}
    end.

init(Args) ->
    Value = proplists:get_value(value, Args, 42),
    #state{value = Value}.

loop(State) ->
    receive
        Msg ->
            NewState = handle_msg(Msg, State),
            loop(NewState)
    end.

handle_msg({set, Value}, State) ->
    State#state{value = Value};
handle_msg({get, From},
           #state{value = Value} = State) ->
    From ! {value, Value},
    State.
{% endhighlight %}

Let's try it out (in OTP20):

{% highlight shell %}
Erlang/OTP 20 [DEVELOPMENT] [erts-9.0] [source-77c6f46a] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)hipe
1> c(simple_proc).
{ok,simple_proc}
2> Pid = simple_proc:start([{value, 42}]).
<0.67.0>
3> simple_proc:get(Pid).
{value,42}
4> simple_proc:set(Pid, 43).
ok
5> simple_proc:get(Pid).
{value,43}
{% endhighlight %}

Pretty simple, but actually what would be nicer is if our `get/1` operation actually returned `{ok, {value, 42}}` instead of just `{value, 42}`,
so to have that, we'll just change the tuple being returned in `handle_msg/2`:

{% highlight erlang %}
44c44
<     From ! {value, Value},
---
>     From ! {ok, {value, Value}},
{% endhighlight %}

Since we don't want to stop our process we'll just hot code load that one module and be done with it, right? Just run the `c/1` which compiles and loads our
module with the change we made and call the `get/1` method to make sure that everything is working as expected.

{% highlight shell %}
6> c(simple_proc)
7> simple_proc:get(Pid).
{value,42}
{% endhighlight %}

What happened? The term returned is exactly the same, our changes didn't have any effect! Fortunately we have this other shell command which explicitly loads
a module: `l/1` let's try that and see if it's any better:

{% highlight shell %}
8> l(simple_proc)
** exception exit: killed
9> simple_proc:get(Pid).
{error,timeout}
{% endhighlight %}

Well that's just wonderful, now the process died and our `get/1` operation is timing out. FML.

### Why?

Let's try and get a high level view of how the VM supports code loading without downtime. Inside the bowels of the beast, for each module there are two code pointers:
current (ie. the one that is running right now) and old which starts out pointing to null. When we did that `c/1` command earlier the VM switched these pointers, old pointer became the currently running code and current became the new code, however (and this is the important bit) *our process was left pointing to the old code*, we can see this
be using the [`erlang:check_process_code/2`](http://erlang.org/doc/man/erlang.html#check_process_code-2) method, it tells us if a given pid is running old code or not.

{% highlight shell %}
6> c(simple_proc)
7> simple_proc:get(Pid).
{value,42}
8> erlang:check_process_code(Pid, simple_proc).
true
{% endhighlight %}

Something needs to happen inside the process for it to know that it should switch to the current version of the code module instead of using the old one, that
thing is a fully qualified function call (that's a call of the Module:Function format). The best place for it is in our `loop/1` function:

{% highlight erlang %}
6a7,8
> -export([loop/1]).
>
35c37
<             loop(NewState)
---
>             ?MODULE:loop(NewState)
{% endhighlight %}

Notice how we need to export `loop/1` even though we're making the call from inside the same module.

But still, going back a bit, why did the process crash when we ran `l/1`? Digging a bit in the OTP source you'll find it's implementation in
`lib/stdlib/src/c.erl`, `c/1` does the same thing except it compiles the file from disk first:

{% highlight erlang %}
l(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).
{% endhighlight %}

So it first does a code purge and only then does it reload the beam file from disk, there are two kinds of purge: brutal and soft.

Brutal (as the name implies) searches for any process that is still running old code for the module and kills it, soft does the same thing without the killing so you know what will happen if you go brutal, you can do a soft purge by calling [`code:soft_purge/1`](http://erlang.org/doc/man/code.html#soft_purge-1). Brutal purge is basically freeing the structures associated with the old code so that's why it's important to ensure that there are no running processes still pointing to it.
In our example, the first time we ran `c/1` the code purge did nothing (because old pointer was null), right after that new code was loaded, this means the old pointer 
is now pointing to the old code. The second time we ran `l/1`, the first thing this does is a brutal purge, now things are different because there is one process that
is still pointing to the old code and that's why it dies. Let's fool around a bit now that we know what methods are being called:

{% highlight shell %}
1> Pid = simple_proc:start([{value, 42}]).
<0.62.0>
2> code:purge(simple_proc).
false
3> code:soft_purge(simple_proc).
true
4> erlang:check_process_code(Pid, simple_proc).
false
5> code:load_file(simple_proc).
{module,simple_proc}
6> erlang:check_process_code(Pid, simple_proc).
true
% `true` means there are processes running old code
7> code:soft_purge(simple_proc).
false
% `false` here means the the module cannot be purged
% without killing processes
8> code:purge(simple_proc).
** exception exit: killed
{% endhighlight %}

### Conclusion

Our example is a bit contrived, most of the time you'll want to be using OTP's gen_server, supervisor and whatnot, these already take care of the needed qualified function
calls and also offer the `code_change/3` method that acts as checkpoint when upgrading (or downgrading) from one version of the code to the next.
Still the purge thing needs to be taken into account, if your gen_server is getting blocked somewhere in it's execution path (say in a TCP accept) it might still get
killed during a brutal purge simply because it did not went through the main loop (and the qualified function call).

<!-- 
OTP20-rc
- l(simple_proc) first does a code:purge and then code:load_file
- code:purge
- code:load_file
    * erlang:load_module/2
    * erlang:prepare_loading/2
    * erlang:finish_loading/1
    * finish_loading_1/1 @ beam_bif_load.c 
    * erts_finish_loading/4 @ beam_load.c
        * beam_make_current_old/3 @ beam_bif_load.c
            * delete_code/1 @ beam_bif_load.c
                 modp->old = modp->curr;
                 erts_module_instance_init(&modp->curr) @ module.c;
                    modi->code_hdr = 0;
        *  inst_p->code_hdr = stp->hdr;
           inst_p->code_length = size;
     
### Notes

lib/stdlib/src/c.erl, l/1 

l(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).
-->

