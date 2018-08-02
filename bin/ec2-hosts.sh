#!/bin/bash

# Example Output:
# Name   PrivateIpAddress  LaunchTime                InstanceId           State       InstanceType
# web01  10.10.1.11        2017-07-29T08:51:32.000Z  i-02fd145fecb87ccb3  running     t2.small
# web02  10.10.1.12        2017-07-29T09:19:02.000Z  i-094f827affc84a7a2  running     m4.large

(
  echo -e "Name\tPrivateIpAddress\tLaunchTime\tInstanceId\tState\tInstanceType"
  AWS_DEFAULT_REGION=ap-northeast-1 aws ec2 describe-instances | jq -r '.Reservations | .[].Instances[] | [(.Tags[] | select(.Key == "Name").Value), .PrivateIpAddress // "-", .LaunchTime, .InstanceId, .State.Name, .InstanceType] | @tsv'
) | sort | column -t
