#!/bin/bash

cd /tmp/deployments/tslaq-prices/etc/chef
chef-client --chef-license accept -l info -z -c client.rb -j default-chef.json
rm -rf /tmp/deployments/*
