#!/bin/bash

cd /home/ubuntu
sudo apt-get update
sudo apt-get -y install ruby wget
wget https://aws-codedeploy-us-east-1.s3.amazonaws.com/latest/install
chmod +x ./install
sudo ./install auto
