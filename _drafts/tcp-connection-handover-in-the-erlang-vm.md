---
layout: page
title: TCP connection handover in the Erlang VM
permalink: "/tcp-handover-erlang-vm/"
published: true
---




### TL;DR

Enable Erlang inter-node communication in a firewalled environment without additional configuration other than opening up the port that EPMD occupies.

## Current state

Erlang nodes, on startup, request a port from the OS (tipically in the range from 1024-65535). 
They then proceed to register themselves with EPMD by supplying the node name (specified with -name on startup) and the system allocated port.
When a node, say node1@host1, wants to connect with another one, node2@host2, it first connects to EPMD on host2 (tipically on port 4369), requests the registered port for name node2 and connects to this port directly.
This implies that host1 must be able to connect to any port on host2 since it doesn't really know which one the OS allocated for node2.
This restriction is cumbersome in firewalled environments where sysadmins usually aren't very receptive to opening up all communication ports. Sometimes this is not possible at all due to security constraints (banks, telecoms, ...)

## Proposed Solution

### Summary

A TCP connection handover mechanism will be detailed in this article which removes the firewall restriction, nodes will still be able to connect to each other but all connections are established through the EPMD port, so the only port that will need to be open between the nodes is this one.

### High level description
Suppose node1@host1 wants to connect to node2@host2, the mechanism works this way:
- node1 opens a connection to host2:epmd_port (we'll refer to it as connection c1), sends handover request command to it
- EPMD receives the handover request, looks up node2 in it's tables, finds that it registered itself previously with port2
- EPMD sends a handover request command to node2 (through it's heartbeat connection), node2 creates a unique Unix domain socket and sends back it's name to EPMD
- EPMD receives the Unix domain socket name, connects to it and sends the socket descriptor associated with c1 through it, from this point on EPMD no longer cares about connection c1 since it's effectively handed over to node2, so it simply closes it.
- node2 receives c1 through the Unix domain socket, from this point on c1 is an inter-node TCP connection between node1 and node2, node2 sends back a reply with a success status through c1
- both node1 and node2 use connection c1 as the inter-node connection in exactly the same manner as up until now

### The nitty-gritty

All code is available on github on a OTP fork (https://github.com/lrascao/otp/tree/feature/tcp_connection_handover). A pull request to the OTP is also pending, i'm not expecting it to be merged as it is  a radical change from the current architecture, this is meant mainly as an learning exercise of the Erlang VM. The main blocks for the feature are:
- The passfd driver
- EPMD handover command
- Node aliases in the Erlang VM

### Passfd Driver




