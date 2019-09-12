#
# Cookbook:: tslaq-prices-basic
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

apt_package 'tzdata'

group 'tslaq' do
  gid    2016
end

user 'tslaq' do
  comment 'TSLAQ User'
  uid 2016
  gid 2016
  home '/home/tslaq'
  shell '/bin/bash'
end

directory '/var/local/tslaq-prices/' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  action :create
end

directory '/var/local/tslaq-prices/bin' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  action :create
end

binary = `find /tmp/deployments/tslaq-prices/.stack-work/install -wholename "*bin/tslaq-prices-exe"`
binary1 = binary.chomp

Chef::Log.info("Binary: #{binary1}")

remote_file '/var/local/tslaq-prices/bin/tslaq-prices' do
  source "file://#{binary1}"
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  action :create
end

cron_d 'download-prices' do
  action :delete
end

cron_d 'tslaq-prices' do
  action :create
  minute '0'
  hour '*/3'
  weekday '1,2,3,4,5'
  user 'tslaq'
  mailto 'amiribarksdale@gmail.com'
  command '/var/local/tslaq-prices/bin/tslaq-prices'
end
