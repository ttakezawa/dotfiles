#!/bin/bash

# Example Output:
# web01  2017-07-29T08:51:32.000Z  i-02fd145fecb87ccb3  10.10.1.11  running
# web02  2017-07-29T09:19:02.000Z  i-094f827affc84a7a2  10.10.1.12  running

AWS_DEFAULT_REGION=ap-northeast-1 aws ec2 describe-instances | jq -r '.Reservations | .[].Instances[] | [(.Tags[] | select(.Key == "Name").Value), .LaunchTime, .InstanceId, .PrivateIpAddress, .State.Name, .InstanceType] | @tsv' | sort | column -t
