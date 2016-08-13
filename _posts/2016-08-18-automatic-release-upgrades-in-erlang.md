---
layout: post
title: Automatic release upgrades in Erlang
permalink: /automatic-release-upgrades-in-erlang/
published: true
---

### TL;DR

Automate your Erlang release upgrades using rebar3 all the way to the gen_server state conversion.

### Summary

In this article i'll demonstrate the use of a [rebar3 plugin](https://github.com/lrascao/rebar3_appup_plugin) for handling `.appup` files that contain the instructions necessary to upgrade from one release to another. It supports the following features:

* Automatic generation of the `.appup` file containing instructions necessary to upgrade and downgrade from one release version to the other. [[1]](#application-upgrade-generation)
* Validation of any `.appup.src` files that might be present, this is a script that can contain Erlang code. It is evaluated and it's results are written to an `.appup` file that is then moved to the target dir. [[2]](#using-an-appup-src)
* Automatic code injection for `gen_server` state record conversion between versions. [[3]](#state-conversion-through-code-injection)
* Automatically generated module dependencies. [[4]](#module-dependencies)

## <a name="application-upgrade-generation"></a> Application upgrade generation

This is achieved by first generating two releases, the one that is live right now and the one that we want to upgrade to, we then invoke the plugin and have it generate a special `.appup` file that OTP knows how to process in order to generate a release upgrade file (`relup`). This file contains low level Erlang VM instructions that will transition the Erlang application from one version to another without any downtime.

Using rebar3 the most simple flow necessary to generate the `.appup` files is:

   * Checkout the original version (the one that is running in production right now)
   * `rebar3 release` in order to generate that release
   * Checkout the version you wish to upgrade to
   * `rebar3 release` again in order to generate the destination version release
   * We now have both version releases (source and destination), run the generate command: `rebar3 appup generate` (since there are only two versions there is no ambiguity), an `.appup` file is generated containing the necessary instructions to upgrade/downgrade between versions.
   * Now that we are in the possession of the `.appup` we can ask rebar3 to generate the relup: `rebar3 relup`
   * Finally pack the resulting release into a tarball that is ready to be deployed to production: `rebar3 tar`

Using the example [relapp1 release](https://github.com/lrascao/relapp1) (this app is also used for the plugin's regression tests):

{% highlight shell %}
    $ git clone https://github.com/lrascao/relapp1.git relapp1
    # checkout the original version that it's running
    # somewhere live
    $ git checkout 1.0.11
    $ rebar3 release
    # checkout the version with the hotfix we need to deploy
    $ git checkout 1.0.12
    $ rebar3 release
    # generate the .appup that contains the necessary
    # instructions for the upgrade from 1.0.11 to 1.0.12
    # and the downgrade from 1.0.12 to 1.0.11
    $ rebar3 appup generate
    $ ===> Generated appup ("1.0.11" <-> "1.0.12") for relapp in
    $ "_build/default/lib/relapp/ebin/relapp.appup"
    # generate the relup which essentially takes the appup
    # instructions and translates them to lower level
    # VM instructions
    $ rebar3 relup
    # finally generate a tarball with the 1.0.12 upgrade that
    # is ready for deployment
    $ rebar3 tar
{% endhighlight %}

### The application upgrade format

In the previous example the appup that is generated looks like this:

{% highlight erlang %}
%% appup generated for relapp by rebar3_appup_plugin ("2016/08/18 16:00:18")
{"1.0.12",
    [{"1.0.11",
        [{update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]}]}],
    [{"1.0.11",
        [{update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]}]}
]}.
{% endhighlight %}

The [appup doc](http://erlang.org/doc/man/appup.html) tells us that the acceptable format for an `.appup` is:

{% highlight erlang %}
{Vsn,
  [{UpFromVsn, UpgradeInstructions}, ...],
  [{DownToVsn, DowngradeInstructions}, ...]}.
{% endhighlight %}

So in this case we see that to upgrade from version `1.0.11` to `1.0.12` we need to update the `relapp_srv2` module which is actually a `gen_server` process. The same update needs to occur to downgrade from `1.0.12` to `1.0.11`.

### Deploying the upgrade

Deploying the new version is also straightforward, it involves (after ssh'ing into the live machine):

   * Creating a new folder beneath `releases for the version you're upgrading to
   * Copying the new tarball to that folder without the version in it's filename (ie. relapp-1.0.12.tar.gz becomes relapp.tar.gz)
   * Issue the upgrade/downgrade command

In the case of the example release upgrading from 1.0.11 to 1.0.12:

{% highlight shell %}
    $ cd <live_release_dir>
    $ mkdir releases/1.0.12
    $ cp relapp-1.0.12.tar.gz releases/1.0.12/relapp.tar.gz
    $ bin/relapp upgrade 1.0.12
{% endhighlight %}

### Soft vs Brutal purge

In the `update` and `load_module` `appup` directives there is a parameter that specifies the type of purge that the Erlang VM will apply when loading code. There are two phases when a purge is aplplied: pre and post purge and two types of purge: soft and brutal, according to the [doc](http://erlang.org/doc/man/appup.html):

```
PrePurge
  Defaults to brutal_purge. It controls what action to take with
  processes executing old code before loading the new module
  version.
  If the value is brutal_purge, the processes are killed.
  If the value is soft_purge, release_handler:install_release/1
  returns {error,{old_processes,Mod}}.
```

```
PostPurge
  Defaults to brutal_purge. It controls what action to take with
  processes that are executing old code when the new module
  version has been loaded. If the value is brutal_purge, the code
  is purged when the release is made permanent and the processes
  are killed. If the value is soft_purge, the release handler
  purges the old code when no remaining processes execute the
  code.
```

A good example of this behaviour in practice is in [a test app commit](https://github.com/lrascao/relapp1/commit/563c4ac642bf912949ca00db5553da57a7303c5c), in this example `relapp_m1` (a simple library module) sends an anonymous function to the `relapp_srv2` `gen_server` that will store in it's state. If you look into it you will see something like this:
{% highlight erlang %}
{state,0,<<"name">>,<<"description">>,
       #Fun<relapp_m1.0.72101784>}
{% endhighlight %}

Now what happens if we load a new version of `relapp_m1`? This is where the purge option comes into play, a `brutal_purge` will kill the gen server process, that might not be the ideal method since the supervisor's restart intensity could be reached, perhaps in this case a `soft_purge` should be chosen instead and then manually restart the gen server or supervisor tree. As a general rule it is best not to send functions across processes if this is to be avoided.

## <a name="using-an-appup-src"></a> Using an .appup.src

You can generate the `.appup` file every time you pack a release upgrade with the `rebar3 appup generate` call. However when there is the need to add custom data or instructions to the `.appup` it's useful to have it in source control alongside your `.app.src` file. The `.appup.src` file can contain Erlang code and it's result should be a [valid appup Erlang term](http://erlang.org/doc/man/appup.html).

The plugin will search for any files ending in `.appup.src`, evaluate them and The plugin will look for any files ending in `.appup.src`, evaluate them and have their results written to an `.appup` file that will then be used to generate the relup. The last term for the expression in this file should a properly formatted `.appup` term.
Here is a simple example of this in practice in the [relapp test release](https://github.com/lrascao/relapp1/commit/bd301cbc7a93e7187da074557fa7d3c3faf14b63):

{% highlight erlang %}
Version = "1.0.17",
{ok, V} = relapp_m1:test("1.0.16"),
UpFrom = DownTo = V,
{Version,
    [{UpFrom,
        [{load_module,relapp_m1,brutal_purge,brutal_purge,
                              [relapp_srv2]},
                 {update,relapp_srv,
                         {advanced,[]},
                         brutal_purge,brutal_purge,
                         [relapp_m1]},
                 {update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]}]}],
    [{DownTo,
        [{update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]},
                 {update,relapp_srv,
                         {advanced,[]},
                         brutal_purge,brutal_purge,
                         [relapp_m1]},
                 {load_module,relapp_m1,brutal_purge,brutal_purge,
                              [relapp_srv2]}]}
]}.
{% endhighlight %}

Note that we are able to invoke methods in our codebase (ie. the `relapp_m1` module).

## <a name="state-conversion-through-code-injection"></a> State conversion through code injection

Use of records to contain a `gen_server`'s state is widespread in Erlang. Due to the fact that records are in fact just tuples a problem arises when performing relups between release versions that involve state changes. There must be a way of converting between to two records. Several solutions are available to overcome this:

### Manual conversion

The most simple way is just taking the old record that arrives through `code_change` and by making several `erlang:setelement/2` calls, convert the old tuple to the new one. You are in fact just converting one tuple into another. This is a bit of a hackish solution, but it works.

### Record conversion using exprecs

Ulf Wiger's excellent project [parse_trans](https://github.com/uwiger/parse_trans) contains a parse_transform called [exprecs](https://github.com/uwiger/parse_trans/blob/master/doc/exprecs.md) that generates methods to manipulate your exported records, one interesting method is `'#convert'` which (as the name implies) converts one record to another. It needs the following to work:

  * Requesting the parse_transform to process your `gen_server` file.
    * `-compile({parse_transform, exprecs}).`
  * Declaring a record with the same definition as your previous version `gen_server` state, so assuming your gen_server's state record is called `state` you would need to declare a `state__VSN` record where VSN is the old `gen_server`'s version as specified through the `-vsn` directive (obtained through the `Module:module_info(attributes)` call).
  * Inform exprecs that we want to generate methods for the `state` record
    * `-export_records([state]).`
  * Invoking the expecs convert method in `code_change` callback to migrate from the old version of the record to the new one.
    * `{NewState, _} = Module:'#convert-'(VSN, OldState).`

Here is a simple example snippet with the relevant entries:

{% highlight erlang %}
-module(m).
%% this is the current gen_server version, the one we want to
%% upgrade from is 1.0.0
-vsn("1.0.1").

%% ask exprecs to apply the parse transform to this file
-compile({parse_transform, exprecs}).

%% below is the previous state record definition with a new
%% name containing it's version, it only has one field.
%% Do note that in the 1.0.0 version the record is still
%% named 'state', the renaming to 'state__1.0.0' is just
%% so that exprecs is able to distinguish between versions
-record('state__1.0.0', {
    id = 0 :: non_neg_integer(),
  }).

%% this is our new state record definition
%% as you can see, we've added a new name field to it
-record(state, {
    id = 0 :: non_neg_integer(),
    name = <<>> :: binary()
  }).

%% ask the exprecs parse transform to generate it's methods
%% for our state record on compilation
-export_records([state]).

%% pattern match on a requested code_change from module
%% version 1.0.0 to 1.0.1
code_change("1.0.0", OldState, Extra) ->
    lager:debug("code change from 1.0.0 version has been "
                "requested"),
    lager:debug("   extra: ~p", [Extra]),
    {NewState, _} = m:'#convert-'("1.0.0", OldState),
    {ok, NewState};
{% endhighlight %}

`exprecs` will look at the `OldState` tuple to determine the record name, append the version suffix and convert from the old record structure to the new one while maintaining old values.

### Plugin code injection

This plugin offers the choice of injecting the necessary code in `gen_server:code_change/3` that takes care of record migration from the old version to the new. The plugin will be called into action when the new version tarball is being generated (hence the `tar` provider hook in `rebar.config`), it will:

  * Scan the `.appup` file for `gen_server`'s' that need upgrade
  * Check for the existence of the `state_record` directive. This informs the plugin of two things: the user wants it to perform the code injection and the name of the record that represents the state. An example can be found [here](https://github.com/lrascao/relapp1/commit/0489c78af736f462f388e03953f84277bfa984ed#diff-319563e2e8eca2f29eab68597c7feba8R13).
  * Extract `gen_server` information from the source and destination releases
      - Abstract code
      - State record definition abstract code
      - State record fields structure
  * It will then inject code into the `gen_server`'s destination beam file:
      - The old state record abstract code, so that it know about the previous version record definition.
      - A method that is in charge of performing the conversion of the state record between the two versions.
      - An invocation of the above method at the beginning of `gen_server:code_change/3`. This call will be the very first instruction of `code_change`, so from the user's point of view the state record is already converted when the code change method is called, the old state is kept in the `Extra` variable as the tuple `{old_state, OldState}`.

##### Conversion method

The Erlang code that performs the conversion can be found in [`priv/templates/convert.tpl`](https://github.com/lrascao/rebar3_appup_plugin/blob/develop/priv/templates/convert.tpl), based on a proplist of named record fields for the old and new versions it uses `erlang:setelement/3` and `erlang:element/2` to populate a new version record, new record fields are filled out with record defaults, updated fields are copied, renaming fields is not supported.

##### gen_server:code_change hook injection

In [`priv/templates/convert_call.tpl`](https://github.com/lrascao/rebar3_appup_plugin/blob/develop/priv/templates/convert_call.tpl) you can also see the call that is injected into the beginning of the `code_change` method, it simply is a call to the conversion method making sure that the original variable names are kept and setting them to their new values.

## <a name="module-dependencies"></a> Module dependencies

OTP allows you to define a list of module dependencies that each upgraded module is dependent on, as described in [the upgrade instructions doc](http://erlang.org/doc/man/appup.html#Release%20Upgrade%20Instructions):

```
    Defaults to [] and defines other modules that Mod is
    dependent on. In the relup file, instructions for suspending
    processes using Mod come before instructions for suspending
    processes using modules in DepMods when upgrading, and
    conversely when downgrading. In case of circular dependencies,
    the order of the instructions in the appup file is kept.
```

The plugin generates these dependencies list for you by using the `xref` tool, for each generated module in the `.appup` the plugin performs an `xref:analyze/2` that yields a list of modules that it makes calls to, this list is then intersected with the modules being upgraded and added to the relevant entries in the `.appup`.
