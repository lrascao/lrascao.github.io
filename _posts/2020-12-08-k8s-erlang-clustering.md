---
layout: post
title: K8s Erlang Clustering
permalink: "/k8s-erlang-clustering/"
published: true
---

This is #2 in my series of the Erlang VM in k8s, this one is about node clustering.

* [Motivation](#motivation)
* [Erlang VM clustering](#erlang-vm-clustering)
* [Clustering in k8s](#clustering-in-k8s)
* [Headless service](#headless-service)
* [DNS Erlang clustering](#dns-erlang-clustering)

### TL;DR

Want to cluster Erlang VMs in Kubernetes? Here's what you need to do:
* Create a k8s headless service
* Have your `subdomain` deployment field match the headless service name
* Add the `endpoint_pod_names` option to CoreDNS's Kubernetes plugin
* Write some Erlang code to perform DNS discovery and cluster using that

### [Motivation](#motivation)

The usual guides around Erlang/Elixir clustering involve setting up a [StatefulSet](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/) and that is fine. This guide will instead detail how to achieve the same
result with a regular deployment, a [headless service](https://kubernetes.io/docs/concepts/services-networking/service/#headless-services), an obscure CoreDNS k8s plugin option and some Erlang DNS discovery.

### [Erlang VM clustering](#erlang-vm-clustering)

When starting up a distributed Erlang node (eg. with the `-name <name>` option) you're instructing the VM to obtain the fully qualified domain name (ie. FQDN) of the host and assume that as the node name, for clustering purposes this
means it will only accept requests from other nodes that are able to reach the host through that name, here is a simple example that illustrates this:

{% highlight shell %}
$ erl -name n1 -setcookie cookie
Erlang/OTP 21 [erts-10.3] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [hipe]

Eshell V10.3  (abort with ^G)
(n1@imacpro.home)1>
{% endhighlight %}

Notice that `imacpro.home` is the same as the output of the `hostname -f`, this is the FQDN of this host.
Let's try running a new node on another shell and cluster the two. We'll do this and refer to `n1` not by it's FQDN but by the `localhost` address, it should still work right? After all we know that both nodes are running on the
same machine...

{% highlight shell %}
$ erl -name n2 -setcookie cookie
Erlang/OTP 21 [erts-10.3] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [hipe]

Eshell V10.3  (abort with ^G)
(n2@imacpro.home)1> net_adm:ping('n1@127.0.0.1').
pang
{% endhighlight %}

It failed! Ok, let's try it out with `localhost` instead of `127.0.0.1`:

{% highlight shell %}
(n2@imacpro.home)2> net_adm:ping('n1@localhost').
=ERROR REPORT==== 22-Nov-2020::21:45:38.925875 ===
** System running to use fully qualified hostnames **
** Hostname localhost is illegal **

pang
(n2@imacpro.home)3>
{% endhighlight %}

Now it's failing with even more error output, let's try it out with the FQDN then:

{% highlight shell %}
(n2@imacpro.home)3> net_adm:ping('n1@imacpro.home').
pong
(n2@imacpro.home)4>
{% endhighlight %}

Finally it was able to connect to `n1` and cluster with it.
What our little experiment is telling us is that the FQDN that the node assumes and the one that other nodes use to cluster with it must match.
A practical check for this is trying out `hostname -f` and ensuring that using this name you are able to reach it from whatever other nodes that you're looking to cluster.

Let's now try and take this knowledge to Kubernetes and see how it applies, we'll again use the [simple web server Erlang app](https://github.com/lrascao/simple_web_server) as our testbed.
Build the Docker image and deploy it in a pod, while you're at it note one relevant field in [deployment.yaml](https://github.com/lrascao/simple_web_server/blob/develop/k8s/simple-web-service/deployment.yaml#L129) that is the
[subdomain](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-hostname-and-subdomain-fields) field, we'll get to why it's relevant shortly. Let's attach to the container and find its FQDN:

{% highlight shell %}
$ kubectl exec -it simple-web-service-68b97dc4bf-qxwnl  -- /bin/sh
/srv/service # hostname -f
simple-web-service-68b97dc4bf-qxwnl.simple-web-service-headless.default.svc.cluster.local
{% endhighlight %}

The name that this host knows itself by is `<pod-name>.<subdomain>.<namespace>.svc.<zone>`, up until now we're good. When we start a distributed Erlang this is the name it will assume, from the previous lesson we know that this must also be the name
that the other nodes must use when clustering. You're probably wondering why is the `subdomain` portion of the FQDN is set to `simple-web-service-headless`, this is relevant but we'll get to why it is so in a bit.

### [Clustering in k8s](#clustering-in-k8s)

We don't really need a `StatefulSet` to cluster Erlang VMs together, all they need is a way to find each other. A common discovery pattern is using a headless service coupled with DNS, what you'll need to do to achieve this in a nutshell:
* Create a headless service that groups all the pods in that deployment
* When the VM starts up in each container, perform a DNS lookup on the service name, find all the other hostnames behind the service
* Cluster with all of the hosts

#### [Headless service](#headless-service)

First step is creating the headless service, it's name will be `simple-web-service-headless` and it will all select all pods with the `app`:`simple-web-service` label
{% highlight yaml %}
apiVersion: v1
kind: Service
metadata:
  name: simple-web-service-headless
spec:
  type: ClusterIP
  clusterIP: None
  selector:
    app: simple-web-service
  ports:
    - name: discovery
      protocol: TCP
      port: 8585
      targetPort: 8585
{% endhighlight %}

Let's find it's DNS name after deploying it, the doc tells us that [SRV records are created for headless services](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#srv-records) in the following format:
{% highlight yaml %}
_<port-name>._<port-protocol>.<service-name>.<namespace>.svc.<zone>
{% endhighlight %}

so let's check that, first find out the FQDN of the headless service

{% highlight shell %}
$ kubectl exec dnsutils-68bd8dc878-cr725 -- host _discovery._tcp.simple-web-service-headless
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local has address 172.17.0.10
{% endhighlight %}

Now that we know this let's fetch it's SRV records:

{% highlight shell %}
$ kubectl exec dnsutils-68bd8dc878-cr725 -- dig -t srv _discovery._tcp.simple-web-service-headless.default.svc.cluster.local

; <<>> DiG 9.11.6-P1 <<>> -t srv _discovery._tcp.simple-web-service-headless.default.svc.cluster.local
;; global options: +cmd
;; Got answer:
;; WARNING: .local is reserved for Multicast DNS
;; You are currently testing what happens when an mDNS query is leaked to DNS
;; ->>HEADER opcode: QUERY, status: NOERROR, id: 38682
;; flags: qr aa rd; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 3
;; WARNING: recursion requested but not available

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 4096
; COOKIE: 55b1fe2e509b573a (echoed)
;; QUESTION SECTION:
;_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. IN SRV

;; ANSWER SECTION:
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. 30 IN SRV 0 50 8585 172-17-0-10.simple-web-service-headless.default.svc.cluster.local.

;; ADDITIONAL SECTION:
172-17-0-10.simple-web-service-headless.default.svc.cluster.local. 30 IN A 172.17.0.10

;; Query time: 0 msec
;; SERVER: 10.96.0.10#53(10.96.0.10)
;; WHEN: Tue Nov 24 11:39:21 UTC 2020
;; MSG SIZE  rcvd: 532
{% endhighlight %}

By scaling up the number of replicas in the deployment we should get back more ip addresses:

{% highlight shell %}
$ kubectl scale deployment simple-web-service --replicas 2
$ kubectl exec dnsutils-68bd8dc878-cr725 -- nslookup _discovery._tcp.simple-web-service-headless.default.svc.cluster.local

; <<>> DiG 9.11.6-P1 <<>> -t srv _discovery._tcp.simple-web-service-headless.default.svc.cluster.local
;; global options: +cmd
;; Got answer:
;; WARNING: .local is reserved for Multicast DNS
;; You are currently testing what happens when an mDNS query is leaked to DNS
;; ->>HEADER opcode: QUERY, status: NOERROR, id: 38682
;; flags: qr aa rd; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 3
;; WARNING: recursion requested but not available

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 4096
; COOKIE: 55b1fe2e509b573a (echoed)
;; QUESTION SECTION:
;_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. IN SRV

;; ANSWER SECTION:
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. 30 IN SRV 0 50 8585 172-17-0-10.simple-web-service-headless.default.svc.cluster.local.
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. 30 IN SRV 0 50 8585 172-17-0-11.simple-web-service-headless.default.svc.cluster.local.

;; ADDITIONAL SECTION:
172-17-0-11.simple-web-service-headless.default.svc.cluster.local. 30 IN A 172.17.0.11
172-17-0-10.simple-web-service-headless.default.svc.cluster.local. 30 IN A 172.17.0.10

;; Query time: 0 msec
;; SERVER: 10.96.0.10#53(10.96.0.10)
;; WHEN: Tue Nov 24 11:39:21 UTC 2020
;; MSG SIZE  rcvd: 532
{% endhighlight %}

Hm.. we're getting back two records, that's expected as we have two pods behind the service. What's not convenient is
the DNS record format being returned: `172-17-0-10.simple-web-service-headless.default.svc.cluster.local.`. As explained previously we need this to match the name
that the node knows itself by, in this case we'd need `simple-web-service-68b97dc4bf-qxwnl.simple-web-service.default.svc.cluster.local`.
The Erlang VM will deny the clustering request if we use this ip address format hostname.

It's time to dig into [CoreDNS](https://coredns.io/) and more specifically its [Kubernetes plugin](https://github.com/coredns/coredns/blob/master/plugin/kubernetes/README.md):

#### [Kubernetes and CoreDNS](#kubernetes-coredns)

From Kubernetes version 1.13 CoreDNS is the default DNS server. It embeds a plugin to be used in the k8s environment and there's a plugin option that is relevant to our interests:

```
endpoint_pod_names: uses the pod name of the pod targeted by the endpoint as the endpoint name in A records, e.g., endpoint-name.my-service.namespace.svc.cluster.local. in A 1.2.3.4
By default, the endpoint-name name selection is as follows: Use the hostname of the endpoint, or if hostname is not set, use the dashed form of the endpoint IP address
(e.g., 1-2-3-4.my-service.namespace.svc.cluster.local.) If this directive is included, then name selection for endpoints changes as follows:
Use the hostname of the endpoint, or if hostname is not set, use the pod name of the pod targeted by the endpoint. If there is no pod targeted by the endpoint, use the dashed IP address form.
```

This looks like what we need, by setting this option we should be getting back a DNS record with a pod name prefix instead of an ip address, let's get to it

{% highlight shell %}
# open up the CoreDNS configuration
$ kubectl edit configmaps coredns --namespace kube-system
# find the kubernetes plugin configuration section and add endpoint_pod_names, eg:
#       kubernetes cluster.local in-addr.arpa ip6.arpa {
#           pods insecure
#           fallthrough in-addr.arpa ip6.arpa
#           ttl 30
#           endpoint_pod_names
#       }
# restart the coredns pods
$ kubectl rollout restart --namespace kube-system deployment/coredns
{% endhighlight %}

And retry the SRV lookup again 

{% highlight shell %}
$ kubectl exec dnsutils-68bd8dc878-cr725 -- nslookup _discovery._tcp.simple-web-service-headless.default.svc.cluster.local

; <<>> DiG 9.11.6-P1 <<>> -t srv _discovery._tcp.simple-web-service-headless.default.svc.cluster.local
;; global options: +cmd
;; Got answer:
;; WARNING: .local is reserved for Multicast DNS
;; You are currently testing what happens when an mDNS query is leaked to DNS
;; ->>HEADER opcode: QUERY, status: NOERROR, id: 1943
;; flags: qr aa rd; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 3
;; WARNING: recursion requested but not available

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 4096
; COOKIE: 890564f2ead30e14 (echoed)
;; QUESTION SECTION:
;simple-web-service-headless.default.svc.cluster.local. IN SRV

;; ANSWER SECTION:
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. 29 IN SRV 0 50 8585 simple-web-service-68b97dc4bf-qxwnl.simple-web-service-headless.default.svc.cluster.local.
_discovery._tcp.simple-web-service-headless.default.svc.cluster.local. 29 IN SRV 0 50 8585 simple-web-service-68b97dc4bf-7zltb.simple-web-service-headless.default.svc.cluster.local.

;; ADDITIONAL SECTION:
simple-web-service-68b97dc4bf-qxwnl.simple-web-service-headless.default.svc.cluster.local. 29 IN A 172.17.0.10
simple-web-service-68b97dc4bf-7zltb.simple-web-service-headless.default.svc.cluster.local. 29 IN A 172.17.0.11

;; Query time: 0 msec
;; SERVER: 10.96.0.10#53(10.96.0.10)
;; WHEN: Tue Nov 24 11:42:24 UTC 2020
;; MSG SIZE  rcvd: 628
{% endhighlight %}

<!-- 
__
-->

Alright, now we're cooking, both names now match, we have everything ready to go ahead with the clustering. Now it should become clear why we decided on that specific `subdomain` deployment field, it
needs to match the headless service name so the entire hostname matches on both sides.
Summing up, the pod hostname that the own node sees is:

{% highlight erlang %}
<pod-name>.<subdomain>.<namespace>.svc.<zone>
{% endhighlight %}

The SRV DNS record that resolves externally (with the `endpoint_pod_names` option applied) is:

{% highlight erlang %}
<pod-name>.<service-name>.<namespace>.svc.<zone>
{% endhighlight %}

### [DNS Erlang clustering](#dns-erlang-clustering)

From now it's pretty straightforward, the [following snippet](https://github.com/lrascao/simple_web_server/blob/develop/src/simple_web_server_cluster.erl#L98) finds all the hosts that are backing up the headless
service that we've created for the purpose of discovery

{% highlight erlang %}
-spec k8s_reverse_lookup(SrvRecord :: string()) -> {ok, [string()]}.
k8s_reverse_lookup(SrvRecord) ->
    % 1. get the FQDN of the headless service
    {ok, #hostent{h_name = SrvRecordFQDN}} = inet:gethostbyname(SrvRecord),
    % 2. ask for SRV records backing up the headless service, this will gives us back, all pod FQDN
    %    names under the headless service subdomain
    {ok, #dns_rec{anlist = Answers}} = inet_res:resolve(SrvRecordFQDN, in, srv),
    % 3. go through each of these records and extract the host
    Hosts =
        lists:map(fun(#dns_rr{type = srv,
                              class = in,
                              data = {_Priority, _Weight, _Port, Host}}) ->

                        Host
                  end,
                  Answers),
    {ok, Hosts}.
{% endhighlight %}

Final thing is simply [clustering up with the list of hosts](https://github.com/lrascao/simple_web_server/blob/develop/src/simple_web_server_cluster.erl#L50) and you're done!

<!-- 
Notes
-->

