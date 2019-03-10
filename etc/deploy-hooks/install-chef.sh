#!/bin/bash

DEBIAN_FRONTEND=noninteractive apt-get -y update
DEBIAN_FRONTEND=noninteractive apt-get -y --force-yes -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" dist-upgrade
DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" chef
