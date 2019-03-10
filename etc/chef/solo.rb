current_dir = Dir.pwd
file_cache_path "#{current_dir}/etc/chef-cache/"
cookbook_path "#{current_dir}/etc/chef/cookbooks/"
role_path "#{current_dir}/etc/chef/roles"
data_bag_path "#{current_dir}/etc/chef/data_bags"
environment_path "#{current_dir}/etc/chef/environments"
ssl_verify_mode :verify_peer
log_level :info
