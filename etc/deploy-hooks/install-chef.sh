#!/bin/bash

apt-get -y update
apt-get -y -o DPkg::Options::=--force-confdef dist-upgrade
apt-get install -y -o DPkg::Options::=--force-confdef chef
