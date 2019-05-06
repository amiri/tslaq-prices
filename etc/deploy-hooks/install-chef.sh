#!/bin/bash

DISTRIBUTION=`cat /etc/lsb-release | grep "DISTRIB_CODENAME" | cut -d"=" -f2`
CHANNEL="stable"

DEBIAN_FRONTEND=noninteractive apt-get -y update

DEBIAN_FRONTEND=noninteractive apt-get -y --force-yes -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" dist-upgrade

DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" apt-transport-https

wget -qO - https://packages.chef.io/chef.asc | sudo apt-key add -

echo "deb https://packages.chef.io/repos/apt/$CHANNEL $DISTRIBUTION main" > /etc/apt/sources.list.d/chef-${CHANNEL}.list

DEBIAN_FRONTEND=noninteractive apt-get -y update

DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" chef
