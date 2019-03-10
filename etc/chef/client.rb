current_dir = Dir.pwd
file_cache_path "#{current_dir}/chef-cache/"
cookbook_path "#{current_dir}/cookbooks/"
role_path "#{current_dir}/roles"
data_bag_path "#{current_dir}/data_bags"
environment_path "#{current_dir}/environments"
ssl_verify_mode :verify_peer
log_level :info
