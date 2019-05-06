#!/bin/bash

cd /tmp/deployments/tslaq-prices/etc/chef
chef-client -z -c client.rb -j default-chef.json
# rm -rf /tmp/deployments/*
