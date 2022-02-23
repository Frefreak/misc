#!/usr/bin/env python3

import os
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
names = list(set([x[0] for x in cur.execute("select name from texts")]))
input_str = "\n".join(names)

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


def process(content, channel_id):
    match content[:2]:
        # fuzzy search
        case "f{":
            inp = content[2:-1]
            results = filter_card_name(inp)
            msg = None
            match len(results):
                case 0:
                    msg = f"[{inp}]"
                case 1:
                    msg = f"> [{results[0]}]"
                case n:
                    content = "\n".join(['> ' + x for x in results[:7]])
                    if n > 7:
                        content += "\n..."
                    msg = content
            if msg is not None:
                bot.sendMessage(channel_id, msg)
        case _:
            pass


if __name__ == "__main__":
    bot.gateway.run(auto_reconnect=True)
