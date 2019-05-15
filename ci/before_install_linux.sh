#!/bin/sh
sudo /sbin/sysctl -a | grep -F ipv6.conf | grep disable
sudo /sbin/sysctl -w net.ipv6.conf.all.disable_ipv6=0
sudo /sbin/sysctl -w net.ipv6.conf.default.disable_ipv6=0
sudo /sbin/sysctl -w net.ipv6.conf.lo.disable_ipv6=0
ip addr
# Note: IPv6 is not available by default. workaround this:
# https://github.com/travis-ci/travis-ci/issues/8891#issuecomment-353403729
echo '{"ipv6":true, "fixed-cidr-v6":"2001:db8:1::/64"}' | sudo tee /etc/docker/daemon.json
sudo service docker restart
ip addr
