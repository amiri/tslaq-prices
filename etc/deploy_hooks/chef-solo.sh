#!/bin/bash

cd /tmp/deployments/tslaq-prices
chef-solo -c etc/chef/solo.rb -j etc/chef/default-chef.json
