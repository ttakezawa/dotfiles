#!/bin/bash

# ANSI カラー
GREEN="\033[32m"
YELLOW="\033[33m"
RED="\033[31m"
CYAN="\033[36m"
RESET="\033[0m"

# キャッシュ・ロックファイル
CACHE_FILE="$HOME/.cache/ccstatusline-usage.txt"
LOCK_FILE="$HOME/.cache/ccstatusline-usage.lock"

# キャッシュが180秒未満なら再利用して即終了
if [[ -f "$CACHE_FILE" ]]; then
  AGE=$(($(date +%s) - $(stat -f '%m' "$CACHE_FILE")))
  [[ $AGE -lt 180 ]] && cat "$CACHE_FILE" && exit 0
fi

# ロックファイルによるレート制限: 30秒以内は API を呼ばない
if [[ -f "$LOCK_FILE" ]]; then
  LOCK_AGE=$(($(date +%s) - $(stat -f '%m' "$LOCK_FILE")))
  if [[ $LOCK_AGE -lt 30 ]]; then
    [[ -f "$CACHE_FILE" ]] && cat "$CACHE_FILE" && exit 0
    echo "[Timeout]" && exit 1
  fi
fi
touch "$LOCK_FILE"

# Keychain からトークン取得
creds=$(security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null)
token=$(echo "$creds" | grep -o '"accessToken":[^,]*' | sed 's/.*:"\([^"]*\)".*/\1/')
if [[ -z "$token" ]]; then
  [[ -f "$CACHE_FILE" ]] && cat "$CACHE_FILE" && exit 0
  echo -e "${RED}Token❌${RESET}"
  exit 1
fi

# usage API（タイムアウト5秒）
response=$(curl -s --max-time 5 \
  -H "Authorization: Bearer $token" \
  -H "anthropic-beta: oauth-2025-04-20" \
  https://api.anthropic.com/api/oauth/usage 2>/dev/null)

if [[ -z "$response" ]]; then
  [[ -f "$CACHE_FILE" ]] && cat "$CACHE_FILE" && exit 0
  echo "[API Error]"
  exit 1
fi

five=$(echo "$response" | jq -r '.five_hour.utilization // empty' 2>/dev/null)
seven=$(echo "$response" | jq -r '.seven_day.utilization // empty' 2>/dev/null)

# パース失敗時は古いキャッシュにフォールバック
if [[ -z "$five" || -z "$seven" ]]; then
  [[ -f "$CACHE_FILE" ]] && cat "$CACHE_FILE" && exit 0
  echo "[Parse Error]"
  exit 1
fi

five=$(echo "$five" | awk '{printf "%d", $1}')
seven=$(echo "$seven" | awk '{printf "%d", $1}')

# リセット時刻を取得
reset5_raw=$(echo "$response" | jq -r '.five_hour.resets_at // empty')
reset7_raw=$(echo "$response" | jq -r '.seven_day.resets_at // empty')

# ISO8601 → 読みやすい形式に変換
format_reset() {
  local raw="$1"
  [ -z "$raw" ] && return
  if command -v gdate &>/dev/null; then
    gdate -d "$raw" "+%m/%d %H:%M" 2>/dev/null
  else
    python3 -c "
from datetime import datetime, timezone, timedelta
import sys
dt = datetime.fromisoformat('$raw')
local = dt.astimezone()
print(local.strftime('%m/%d %H:%M'))
" 2>/dev/null
  fi
}

reset5=$(format_reset "$reset5_raw")
reset7=$(format_reset "$reset7_raw")

# バーを作る
make_bar() {
  local pct=$1
  local full=$((pct / 10))
  local empty=$((10 - full))
  [ "$full" -gt 0 ] && printf "%0.s■" $(seq 1 $full)
  [ "$empty" -gt 0 ] && printf "%0.s□" $(seq 1 $empty)
}

bar5=$(make_bar $five)
bar7=$(make_bar $seven)

# 色を決める（80%以上:赤、50%以上:黄、それ以外:緑）
color_for() {
  local pct=$1
  if [ "$pct" -ge 80 ]; then echo -e "$RED"
  elif [ "$pct" -ge 50 ]; then echo -e "$YELLOW"
  else echo -e "$GREEN"
  fi
}

c5=$(color_for $five)
c7=$(color_for $seven)

# 表示 & キャッシュ保存
r5_info=""
r7_info=""
[ -n "$reset5" ] && r5_info=" ${CYAN}${reset5}${RESET}"
[ -n "$reset7" ] && r7_info=" ${CYAN}${reset7}${RESET}"

echo -e "5h ${c5}${bar5} ${five}%${RESET}${r5_info} | 7d ${c7}${bar7} ${seven}%${RESET}${r7_info}" | tee "$CACHE_FILE"
