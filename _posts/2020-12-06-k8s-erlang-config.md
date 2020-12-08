---
layout: post
title: K8s Erlang configuration
permalink: "/k8s-erlang-configuration/"
published: true
---

Kubernetes offers us a cool way of running Erlang services in the cloud, however there are some hurdles to neet to be overcome. This will hopefully be #1 in a series of posts where i'll be going through some aspects of running the Erlang VM
in k8s.

* [Getting started](#getting-started)
* [Application configuration](#application-configuration)
* [Application secrets](#application-secrets)
* [Erlang VM configuration](#erlang-vm-configuration)
* [Putting it all together](#putting-it-all-together)

### [Getting started](#getting-started)

Configuration in Erlang is hard, you have to know quite a bit of Erlang syntax to write it and it's easy to get confused with all the lists and the tuples. That's why [`basho`](https://github.com/basho) developers (of Riak fame) came up with
[`cuttlefish`](https://github.com/Kyorai/cuttlefish), a tool that deals with all the complexity of the Erlang configuration by exposing to the end user a simple `sysctl.conf` syntax style file.
An Erlang developer does the hard work of exposing this simple interface to the non-Erlang user, instead of both having to share the complexity, I've written a [rebar3 plugin](https://github.com/lrascao/rebar3_scuttler)
that proposes to ease this workflow with a (hopefully) clean interface.

Before jumping into too much detail of how to achieve this, let me show you the end result. The following is a partial kubernetes deployment yaml file for a [simple Erlang web service](https://github.com/lrascao/simple_web_server) listening for requests
on port `8585` (full version at the [repo](https://github.com/lrascao/simple_web_server/blob/develop/k8s/simple-web-service/deployment.yaml)).

{% highlight yaml %}

apiVersion: apps/v1
kind: Deployment
spec:
  selector:
    matchLabels:
      app: simple-web-service
  template:
    metadata:
      labels:
        app: simple-web-service
    spec:
      containers:
        - name: simple-web-service
          image: simple-web-server:latest
          ports:
            - containerPort: 8585
          resources:
            requests:
              cpu: "500m"
              memory: "250Mi"
            limits:
              cpu: "2000m"
              memory: "1Gi"
          volumeMounts:
            - name: vol-redis-conf
              mountPath: /srv/service/etc/conf.d/redis.conf
              subPath: redis.conf
            - name: vol-redis-password-conf
              mountPath: /srv/service/etc/conf.d/redis-password.conf
              subPath: redis-password.conf
            - name: vol-erlang-vm
              mountPath: /srv/service/etc/conf.d/erlang-vm-k8s.conf
              subPath: erlang-vm-k8s.conf
            - name: vol-erlang-vm-downwardapi
              mountPath: /srv/service/etc/conf.d/erlang-vm-total-schedulers
              subPath: erlang-vm-total-schedulers
            - name: vol-erlang-vm-downwardapi
              mountPath: /srv/service/etc/conf.d/erlang-vm-total-memory
              subPath: erlang-vm-total-memory
      volumes:
        - name: vol-redis-conf
          configMap:
            name: conf.simple-web-server.redis
        - name: vol-redis-password-conf
          secret:
            secretName: secret-redis-password
        - name: vol-erlang-vm
          configMap:
            name: conf.simple-web-server.erlang-vm
        - name: vol-erlang-vm-downwardapi
          downwardAPI:
            items:
              - path: erlang-vm-total-schedulers
                resourceFieldRef:
                  containerName: simple-web-service
                  resource: limits.cpu
                  divisor: 1
              - path: erlang-vm-total-memory
                resourceFieldRef:
                  containerName: simple-web-service
                  resource: limits.memory
                  divisor: 1Mi
{% endhighlight %}

Looking past the rest of the k8s fields in the yaml, notice how in `volumeMounts` several files are being mounted onto a `/srv/service/etc/conf.d/` directory. This is configuration that is being
injected into the container's storage that is to be consumed by the Erlang application.

### [Application configuration](#application-configuration)

Our simple web server uses `redis` as the caching layer of data that is being fetched from dynamodb. We need to inform the Erlang application of it's location so it is able to connect to it.
We start off by declaring a binary data `configMap` that contains a host and a port ([source](https://github.com/lrascao/simple_web_server/blob/develop/k8s/simple-web-service/deployment.yaml#L36)).

The redis host and port are mentioned in the [redis deployment yaml name field](https://github.com/lrascao/simple_web_server/blob/develop/k8s/redis/deployment.yaml#L54).

{% highlight yaml %}
apiVersion: v1
kind: ConfigMap
metadata:
  name: conf.simple-web-server.redis
data:
  redis.conf: |
    redis.host = redis
    redis.port = 6379
{% endhighlight %}

Next up is creating a volume that is based off of this `configMap`

{% highlight yaml %}
      volumes:
        - name: vol-redis-conf
          configMap:
            name: conf.simple-web-server.redis
{% endhighlight %}

Next up is making this configMap available as a volume mount so it becomes available to the Erlang application running inside the container.
{% highlight yaml %}
      volumeMounts:
        - name: vol-redis-conf
          mountPath: /srv/service/etc/conf.d/redis.conf
          subPath: redis.conf
{% endhighlight %}

This `.conf` file is automatically picked up and will override any other existing `redis.host` and `redis.port` definitions. With the correct endpoint made available
the application will have no problem connecting to the redis server.

### [Application secrets](#application-secrets)

Another typical pattern is secret management, this is also offered by k8s. We normally want these secrets to be hidden from everyone but made available in cleartext to the Erlang application.
We'll use a redis password as a use case for this, let's go through the steps to achieve it.

First thing is enabling client password authentication in redis itself, to do this we'll change a bit our [redis deployment](https://github.com/lrascao/simple_web_server/blob/develop/k8s/redis/deployment.yaml).

We'll add our own custom `redis.conf` and enable the `requirepass` directive:
{% highlight conf %}
# Require clients to issue AUTH <PASSWORD> before processing any other
# commands.  This might be useful in environments in which you do not trust
# others with access to the host running redis-server.
#
# This should stay commented out for backward compatibility and because most
# people do not need auth (e.g. they run their own servers).
# 
# Warning: since Redis is pretty fast an outside user can try up to
# 150k passwords per second against a good box. This means that you should
# use a very strong password otherwise it will be very easy to break.
#
requirepass Pa$$w0rd
{% endhighlight %}

Use `kubectl` builtin kustomize feature to build out a `configMap` out of the full `redis.conf` file ([source](https://github.com/lrascao/simple_web_server/tree/develop/k8s/redis)).

{% highlight yaml %}
configMapGenerator:
  - name: redis-conf
    files:
      - redis.conf

generatorOptions:
  disableNameSuffixHash: true
{% endhighlight %}

Applying it is pretty simple as well (`-k` will look for `kustomization.yaml` files in the [specified directory](https://github.com/lrascao/simple_web_server/tree/develop/k8s/redis) and apply them):
{% highlight shell %}
$ kubectl apply -k .
{% endhighlight %}

This `configMap` that contains the `redis.conf` now needs to be made available to `redis` in some container directory, we'll just apply the same formula from the previous application configuration section:

Create the volume:

{% highlight yaml %}
      volumes:
        - name: redis-conf
          configMap:
            name: redis-conf
{% endhighlight %}

Mount it:

{% highlight yaml %}
    volumeMounts:
      - name: redis-conf
        mountPath: /etc/redis.conf
        subPath: redis.conf
{% endhighlight %}

Finally tell redis what directory to read it from ([source](https://github.com/lrascao/simple_web_server/blob/develop/k8s/redis/deployment.yaml#L42)):

{% highlight yaml %}
    command:
      - redis-server
      - /etc/redis.conf
{% endhighlight %}

Now that our redis is set to use a client password we need to configure our application to supply this password upon connection, this is where k8s secrets come in, we'll start off by creating a `.conf` file with our redis
secret password in it. (If you version control this file you'll probably want to keep it encrypted, i like [transcrypt](https://github.com/elasticdog/transcrypt) for this).

{% highlight conf %}
redis.password = Pa$$w0rd
{% endhighlight %}

Let's leverage kustomize again and build out a k8s secret out of the password file ([source](https://github.com/lrascao/simple_web_server/blob/develop/k8s/simple-web-service/kustomization.yaml)):

{% highlight yaml %}
secretGenerator:
  - name: secret-redis-password
    files:
      - redis-password.conf

generatorOptions:
  disableNameSuffixHash: true
{% endhighlight %}

Now rinse and repeat the same `volume`, `volumeMount` procedure from above:

{% highlight yaml %}
      volumes:
        - name: vol-redis-password-conf
          secret:
            secretName: secret-redis-password
{% endhighlight %}

{% highlight yaml %}
      volumeMounts:
        - name: vol-redis-password-conf
          mountPath: /srv/service/etc/conf.d/redis-password.conf
          subPath: redis-password.conf
{% endhighlight %}

At this point you'll end up with two files in your `/srv/service/etc/conf.d` container directory that contain the necessary configuration for a successful connection to a password protected redis.

### [Erlang VM configuration](#erlang-vm-configuration)

Another important bit is the configuration of the Erlang VM itself, note the `resources` entry in the deployment yaml. [`adoptingerlang.org`](https://adoptingerlang.org/docs/production/kubernetes/) does a much
better job of explaining how container resources interact with the Erlang VM so i won't go into that here. Suffice to say that we're requesting k8s for a set amount of CPU/memory while at the same time setting upper boundaries for these same resources:

{% highlight yaml %}
          resources:
            requests:
              cpu: "500m"
              memory: "250Mi"
            limits:
              cpu: "2000m"
              memory: "1Gi"
{% endhighlight %}

Next we're mounting two volumes in the `simple-web-service` pod, one built out of a [`configMap`](https://kubernetes.io/docs/concepts/storage/volumes/#configmap) and the other using the
[`downward API`](https://kubernetes.io/docs/tasks/inject-data-application/downward-api-volume-expose-pod-information/#the-downward-api):
We're mounting these two files under `/srv/service/etc/conf.d/`, all files under this directory will get included and considered as configuration the same as the previous `redis` case, it's just
that it's for the Erlang VM itself this time.

{% highlight yaml %}
          volumeMounts:
            - name: vol-erlang-vm
              mountPath: /srv/service/etc/conf.d/erlang-vm-k8s.conf
              subPath: erlang-vm-k8s.conf
            - name: vol-erlang-vm-downwardapi
              mountPath: /srv/service/etc/conf.d/erlang-vm-total-schedulers
              subPath: erlang-vm-total-schedulers
            - name: vol-erlang-vm-downwardapi
              mountPath: /srv/service/etc/conf.d/erlang-vm-total-memory
              subPath: erlang-vm-total-memory
      volumes:
        - name: vol-erlang-vm
          configMap:
            name: conf.simple-web-server.erlang-vm
        - name: vol-erlang-vm-downwardapi
          downwardAPI:
            items:
              - path: erlang-vm-total-schedulers
                resourceFieldRef:
                  containerName: simple-web-service
                  resource: limits.cpu
                  divisor: 1
              - path: erlang-vm-total-memory
                resourceFieldRef:
                  containerName: simple-web-service
                  resource: limits.memory
                  divisor: 1Mi
{% endhighlight %}

Let's take a look into the `downwardAPI` first, this k8s feature allows to access pod/container fields, in this case we're obtaining the CPU limit defined in the `simple-web-service` container.

This value will get dumped into a `erlang-vm-total-schedulers` file. The `volumeMount` field then requests k8s to mount the file at `/srv/service/etc/conf.d/erlang-vm-total-schedulers`.
Same thing applies to `erlang-vm-total-memory`.

Moving on to the  `configMap`, here we're declaring a `.conf` file that contains these two relevant Erlang VM parameters to k8s.

{% highlight yaml %}
apiVersion: v1
kind: ConfigMap
metadata:
  name: conf.simple-web-server.erlang-vm
data:
  erlang-vm-k8s.conf: |
    ## all parameters are available at
    #   https://github.com/lrascao/rebar3_scuttler/blob/develop/priv/erlang.vm.args.schema
    ##  
    ## Sets scheduler busy wait threshold. Defaults to medium.
    ## The threshold determines how long schedulers are to busy wait when
    ## running out of work before going to sleep.
    ## Note: This flag can be removed or changed at any time without prior notice.
    ##
    ## Acceptable values:
    ##   - one of: none, very_short, short, medium, long, very_long
    erlang.sbwt = none

    ## Sets the number of scheduler threads to create and scheduler threads to set online.
    ## The maximum for both values is 1024.
    ## If the Erlang runtime system is able to determine the number of logical processors
    ## configured and logical processors available,
    ## Schedulers defaults to logical processors configured, and SchedulersOnline defaults to
    ## logical processors available; otherwise the default values are 1.
    ## If the emulator detects that it is subject to a CPU quota, the default value for
    ## SchedulersOnline will be limited accordingly.
    ## Schedulers can be omitted if :SchedulerOnline is not and conversely.
    ## The number of schedulers online can be changed at runtime through
    ## erlang:system_flag(schedulers_online, SchedulersOnline).
    ## If Schedulers or SchedulersOnline is specified as a negative number, the value is
    ## subtracted from the default number of logical processors configured or logical
    ## processors available, respectively.
    ## Specifying value 0 for Schedulers or SchedulersOnline resets the number of scheduler
    ## threads or scheduler threads online, respectively, to its default value.
    ##
    ## Acceptable values:
    ##   - an integer
    erlang.schedulers.total = $(<erlang-vm-total-schedulers)
{% endhighlight %}


* `erlang.sbwt` is set to one of several possible values, `none` in this case.
* `erlang.schedulers.total` is assigned to the result of a value substitution,
   in this case we're reading the output file of the `downwardAPI` section detailed above, `erlang-vm-total-schedulers` located in the same directory as `erlang-vm-k8s.conf`.

### [Putting it all together](#putting-it-all-together)

So it's now time for you, Erlang developer, to make the previous interaction possible in your application. Now let's make use of the [rebar3_scuttler](https://github.com/lrascao/rebar3_scuttler) plugin, the steps nededed do make this happen are better explained in the [project's README](https://github.com/lrascao/rebar3_scuttler/blob/develop/README.md),
here i'll just highlight some of the relevant details related to the `simple_web_server` project.

Below is a `rebar.config` snippet where we're informing the plugin of where to read the non-Erlang configuration from, if no file is present at the start, one is generated out of the defaults you declared in the `.schema` file(s).

{% highlight erlang %}
{scuttler, [
    % this is the human readable .conf file that the users of your application
    % will understand and edit in order to change configuration parameters,
    % it's location is relative to the root dir of the release
    % (ie. alongside bin, releases, etc)
    {conf_file, "etc/simple_web_server.conf"},
    ...
{% endhighlight %}

Next up is another snippet from `rebar.config` instructing the plugin to do two things:

1. generate an Erlang VM args file to `releases/{release_version}/vm.generated.args` based on the parameters declared in the `.conf` file(s)
  (`etc/simple_web_server.conf` in this example).
2. search all `.schema` files in `priv/schemas` and copy them over to generated release at `releases/{release_version}/schema`, on release start
  generate a `"releases/{release_version}/config/generated/user_defined.config"` file based on settings declared at the `.conf` file(s)

{% highlight erlang %}
{schemas, [
           {vm_args, "releases/{release_version}/vm.generated.args"},
           {"priv/schemas", "releases/{release_version}/schema",
            "releases/{release_version}/config/generated/user_defined.config"}
    ]},
{% endhighlight %}

Here is the cuttlefish schema that maps the `.conf` parameters to the Erlang-specific `sys.config` ones for the configuration of the `eredis` application:

{% highlight erlang %}
%% @doc Redis host
{mapping, "redis.host", "eredis.host", [
    {datatype, string},
    {default, "{{redis_host}}"}
]}.

%% @doc Redis port
{mapping, "redis.port", "eredis.port", [
    {datatype, integer},
    {default, {{redis_port}}}
]}.

%% @doc Redis password
{mapping, "redis.password", "eredis.password", [
    {datatype, string}
]}.
{% endhighlight %}

The final two things that are missing are [adding the pre start hook](https://github.com/lrascao/rebar3_scuttler/blob/develop/README.md#adding-the-pre-start-hook) and [including the generated files](https://github.com/lrascao/rebar3_scuttler/blob/develop/README.md#adding-the-pre-start-hook).
After this is in place here's the flow of what will happen the next time you start the `simple_web_server` release:

* The pre start hook will invoke the cuttlefish tool with the apropriate parameters
* The cuttlefish tool will consult the `.schema` files and the `.conf` file, any extra `.conf` files found in the same directory `conf.d` will also get included
* Two files will be generated (`vm.generated.args` and `user_defined.config`), these will both be included by `vm.args` and `sys.config` respectively thus setting or overriding the parameters.

<!-- 
Notes
-->

