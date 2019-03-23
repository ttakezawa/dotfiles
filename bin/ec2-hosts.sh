#!/bin/bash

# Example Output:
# Name   PrivateIpAddress  LaunchTime                InstanceId           State       InstanceType
# web01  10.10.1.11        2017-07-29T08:51:32.000Z  i-02fd145fecb87ccb3  running     t2.small
# web02  10.10.1.12        2017-07-29T09:19:02.000Z  i-094f827affc84a7a2  running     m4.large

function mysed() {
  sed=$(type -P gsed sed 2>/dev/null | head -n1)
  $sed "$@"
}

(
  echo -e "Name\tPrivateIpAddress\tLaunchTime\tInstanceId\tState\tInstanceType"
  AWS_DEFAULT_REGION=ap-northeast-1 aws ec2 describe-instances | jq -r '.Reservations | .[].Instances[] | [if has("Tags") then (.Tags[] | select(.Key == "Name").Value) else "_No_Name_" end, .PrivateIpAddress // "-", .LaunchTime, .InstanceId, .State.Name, .InstanceType] | @tsv'
) | awk '{print $1, $3, $0}' \
  | mysed -r 's/\s+/ /g'     \
  | LC_ALL=C sort            \
  | cut -d' ' -f3-           \
  | column -t                \
  | GREP_COLORS="mt=05;32" grep --color=always -E 'pending|$'                                     \
  | GREP_COLORS="mt=05;31" grep --color=always -E 'rebooting|stopping|shutting-down|terminated|$' \
  | GREP_COLORS="mt=00;31" grep --color=always -E 'stopped|$'                                     \
  | awk '{ if ("'$(date --iso-8601=seconds -d '-24HOURS')'" < $3 && $3 != "LaunchTime" ) { print gensub(/(20[-0-9T:.]*Z)/, "\033[32m\\1\033[0m", 1) } else { print $0 } }'
