#!/usr/bin/env bash
# Claude Code statusLine — lualine-style powerline segments, dracula theme.
# Segments: model > git branch/status > pwd > cost/duration > context.
# Needs a Nerd Font for  (powerline) and  (branch) glyphs.

input=$(cat)

ESC=$'\033'
SEP=$''
THINSEP=$'\xee\x82\xb1'  # U+E0B1, between same-colored segments
GITICON=$'\xee\x82\xa0'  # U+E0A0 branch glyph

# lualine dracula theme palette (lua/lualine/themes/dracula.lua):
# a = purple bg / black fg bold, b = lightgray bg, c = gray bg.
# xterm-256 indices, NOT truecolor: Claude Code quantizes 38;2 escapes to the
# 256 palette with per-channel round-up, washing colors out (#bd93f9 became
# #d7afff). These are the nearest cube colors, chosen by hand.
BLACK=236     # #303030 (~#282a36)
GRAY=238      # #444444 (~#44475a)
LIGHTGRAY=60  # #5f5f87 (~#5f6a8e)
WHITE=255     # #eeeeee (~#f8f8f2)
CYAN=117      # #87d7ff (~#8be9fd)
GREEN=84      # #5fff87 (~#50fa7b)
ORANGE=215    # #ffaf5f (~#ffb86c)
PURPLE=141    # #af87ff (~#bd93f9)
RED=203       # #ff5f5f (~#ff5555)
YELLOW=228    # #ffff87 (~#f1fa8c)

fgc() { printf '%s[38;5;%sm' "$ESC" "$1"; }

# jq emits "-" for missing values: a literal tab-separated record survives
# `read` (empty fields would collapse, since tab is IFS whitespace).
IFS=$'\t' read -r cwd model cost dur_ms used_pct in_tok out_tok win_size <<EOF
$(echo "$input" | jq -r '[
  (.cwd // .workspace.current_dir // "-"),
  (.model.display_name // "-"),
  (.cost.total_cost_usd // "-"),
  (.cost.total_duration_ms // "-"),
  (.context_window.used_percentage // "-"),
  (.context_window.total_input_tokens // "-"),
  (.context_window.total_output_tokens // "-"),
  (.context_window.context_window_size // "-")
] | @tsv')
EOF
for v in cwd model cost dur_ms used_pct in_tok out_tok win_size; do
  [ "${!v}" = "-" ] && eval "$v=''"
done

segs_bg=()
segs_body=()
add_seg() { segs_bg+=("$1"); segs_body+=("$2"); }

# model (lualine_a: purple, bold dark text); vim mode is left entirely
# to the TUI's built-in indicator — statusline updates are debounced at
# 300ms, so reflecting the mode here always lags
if [ -n "$model" ]; then
  add_seg "$PURPLE" "$(fgc "$BLACK")${ESC}[1m${model}${ESC}[22m"
fi

# git branch + dirty / ahead-behind (lualine_b)
if [ -n "$cwd" ]; then
  branch=$(git -C "$cwd" -c gc.auto=0 symbolic-ref --short HEAD 2>/dev/null \
        || git -C "$cwd" -c gc.auto=0 rev-parse --short HEAD 2>/dev/null)
  if [ -n "$branch" ]; then
    body="$(fgc "$WHITE")${GITICON} ${branch}"
    dirty=$(git -C "$cwd" -c gc.auto=0 status --porcelain 2>/dev/null | grep -c .)
    [ "$dirty" -gt 0 ] && body+=" $(fgc "$ORANGE")*${dirty}"
    counts=$(git -C "$cwd" -c gc.auto=0 rev-list --left-right --count '@{upstream}...HEAD' 2>/dev/null)
    if [ -n "$counts" ]; then
      behind=${counts%%[^0-9]*}
      ahead=${counts##*[^0-9]}
      [ "$ahead" -gt 0 ] && body+=" $(fgc "$CYAN")⇡${ahead}"
      [ "$behind" -gt 0 ] && body+=" $(fgc "$CYAN")⇣${behind}"
    fi
    add_seg "$LIGHTGRAY" "$body"
  fi
fi

# pwd, ~-shortened (lualine_c)
if [ -n "$cwd" ]; then
  short_cwd=$cwd
  [ "${cwd#"$HOME"}" != "$cwd" ] && short_cwd="~${cwd#"$HOME"}"
  add_seg "$GRAY" "$(fgc "$WHITE")${short_cwd}"
fi

# session cost + duration (lualine_y)
cost_body=""
if [ -n "$cost" ]; then
  cost_body=$(printf '$%.2f' "$cost")
fi
if [ -n "$dur_ms" ]; then
  s=$(printf '%.0f' "$dur_ms")
  s=$((s / 1000))
  if [ "$s" -lt 60 ]; then dur="${s}s"
  elif [ "$s" -lt 3600 ]; then dur="$((s / 60))m"
  else dur="$((s / 3600))h$(((s % 3600) / 60))m"
  fi
  cost_body+="${cost_body:+ · }${dur}"
fi
if [ -n "$cost_body" ]; then
  add_seg "$LIGHTGRAY" "$(fgc "$WHITE")${cost_body}"
fi

# context (lualine_z): green/yellow/red as the window fills
if [ -n "$used_pct" ]; then
  pct=$(printf '%.0f' "$used_pct")
  if [ "$pct" -lt 50 ]; then ctx_bg=$GREEN
  elif [ "$pct" -lt 75 ]; then ctx_bg=$YELLOW
  else ctx_bg=$RED
  fi
  tok=""
  if [ -n "$in_tok" ] && [ -n "$win_size" ]; then
    used_tok=$((in_tok + ${out_tok:-0}))
    tok=" $(((used_tok + 500) / 1000))k/$((win_size / 1000))k"
  fi
  add_seg "$ctx_bg" "$(fgc "$BLACK")${ESC}[1mctx ${pct}%${tok}${ESC}[22m"
fi

# render: each segment paints its bg; the powerline arrow blends prev bg into
# next, falling back to the thin separator between same-colored neighbors
out=""
prev_bg=""
for i in "${!segs_bg[@]}"; do
  bg=${segs_bg[i]}
  if [ "$bg" = "$prev_bg" ]; then
    out+="${ESC}[48;5;${bg}m$(fgc "$WHITE")${THINSEP}"
  elif [ -n "$prev_bg" ]; then
    out+="$(fgc "$prev_bg")${ESC}[48;5;${bg}m${SEP}"
  fi
  out+="${ESC}[48;5;${bg}m ${segs_body[i]} "
  prev_bg=$bg
done
[ -n "$prev_bg" ] && out+="${ESC}[0m$(fgc "$prev_bg")${SEP}${ESC}[0m"
printf '%s\n' "$out"
