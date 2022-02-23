#!/usr/bin/env python3

import os
from typing import List, Tuple
import sqlite3
from shlex import quote
import subprocess
import sys
import re
import discum

cdb = os.getenv("CARDS_CDB", None)
if cdb is None:
    print("CARDS_CDB")
    sys.exit(1)

zh_cdb = os.getenv("CARDS_CDB_ZH", None)
if zh_cdb is None:
    print("CARDS_CDB_ZH")
    sys.exit(1)

discord_token = os.getenv("DISCORD_TOKEN", None)
if discord_token is None:
    print("DISCORD_TOKEN")
    sys.exit(1)


active_servers = os.getenv("ACTIVE_SERVERS", "")
if not active_servers:
    print("ACTIVE_SERVERS")
    sys.exit(1)
active_servers = set(active_servers.split(","))

cur = sqlite3.connect(cdb).cursor()
r = list(cur.execute("select id, name from texts"))
names = list(set([x[1] for x in r]))
input_str = "\n".join(names)
name_maps = {x[0]: x[1] for x in r}

zh_cur = sqlite3.connect(zh_cdb).cursor()
zh_names = list(
    set([str(x[0]) + ":" + x[1] for x in zh_cur.execute("select id, name from texts")])
)
zh_input_str = "\n".join(zh_names)

bot = discum.Client(
    token=discord_token,
    log=False,
)

my_guilds = bot.getGuilds().json()
print(my_guilds)
servs = []
for serv in active_servers:
    for guild in my_guilds:
        if serv == guild["id"]:
            print(f'active on \x1b[31;1m{guild["name"]}\x1b[0m')
            servs.append(serv)
            break
if not servs:
    print("active_servers empty")
    sys.exit(1)
active_servers = set(servs)


def filter_card_name(name):
    p = subprocess.Popen(
        f"fzf -f {quote(name)}",
        shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    r = p.communicate(input_str.encode())
    output = r[0].decode()
    return output.splitlines()


def filter_card_name_zh(name) -> List[List[str]]:
    p = subprocess.Popen(
        f"fzf -d':' -f {quote(name)} --with-nth 2..",
        shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    r = p.communicate(zh_input_str.encode())
    output = r[0].decode()
    return [x.split(":") for x in output.splitlines()]


@bot.gateway.command
def helloworld(resp):
    if resp.event.ready_supplemental:  # ready_supplemental is sent after ready
        user = bot.gateway.session.user
        print("Logged in as {}#{}".format(user["username"], user["discriminator"]))
    if resp.event.message:
        m = resp.parsed.auto()
        guild_id = (
            m["guild_id"] if "guild_id" in m else None
        )  # because DMs are technically channels too
        if guild_id not in active_servers:
            return
        if m["author"]["id"] != bot.gateway.session.user["id"]:
            return
        channel_id = m["channel_id"]
        content = m["content"]
        if not re.match(r"^.{.*}$", content):
            return
        process(content, channel_id)


def find_en_name(id: str):
    try:
        return name_maps.get(int(id), None)
    except ValueError:
        return None


TOP_N = 7


def process(content, channel_id):
    match content[:2]:
        # fuzzy search
        case "f{":
            msg = None
            inp = content[2:-1]
            if inp[:3] == "zh ":
                # handle chinese name
                results = filter_card_name_zh(inp[3:])
                new_results = []
                orig_num = len(results)
                for result in results[:TOP_N]:
                    en_name = find_en_name(result[0])
                    if en_name is None:
                        en_name = result[0]
                    new_results.append((en_name, result[1]))
                results = new_results
                match orig_num:
                    case 0:
                        pass
                    case 1:
                        msg = f"> **{results[0][1]}** [{results[0][0]}]"
                    case n:
                        content = "\n".join(
                            [f"> **{x[1]}** {x[0]}" for x in results[:TOP_N]]
                        )
                        if n > TOP_N:
                            content += "\n..."
                        msg = content
            else:
                results = filter_card_name(inp)
                match len(results):
                    case 0:
                        msg = f"[{inp}]"
                    case 1:
                        msg = f"> [{results[0]}]"
                    case n:
                        content = "\n".join(["> " + x for x in results[:TOP_N]])
                        if n > TOP_N:
                            content += "\n..."
                        msg = content
            if msg is not None:
                bot.sendMessage(channel_id, msg)
        case _:
            pass


if __name__ == "__main__":
    bot.gateway.run(auto_reconnect=True)
