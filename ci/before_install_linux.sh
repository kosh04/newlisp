#!/bin/sh
## Enable ipv6 loopback only for testing
# sudo /sbin/sysctl -w net.ipv6.conf.all.disable_ipv6=0
# sudo /sbin/sysctl -w net.ipv6.conf.default.disable_ipv6=0
sudo /sbin/sysctl -w net.ipv6.conf.lo.disable_ipv6=0
ip addr
