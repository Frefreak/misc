import os
import json
import random
import string
import argparse
import time
from shlex import quote
from typing import Optional, Dict

from fabric import Connection


# run_type = client or server
def mk_trojan_config(
    run_type: str, password: str, domain: str, fallback_addr: str, fallback_port: int
) -> Dict:
    config = {}
    match run_type:
        case "client":
            config = {
                "run_type": "client",
                "local_addr": "127.0.0.1",
                "local_port": 1080,
                "remote_addr": domain,
                "remote_port": 443,
                "password": [password],
                "ssl": {
                    "sni": domain,
                },
                "mux": {
                    "enabled": True,
                },
            }
        case "server":
            config = {
                "run_type": "server",
                "local_addr": "0.0.0.0",
                "local_port": 443,
                "remote_addr": "127.0.0.1",
                "remote_port": 80,
                "password": [password],
                "ssl": {
                    "cert": f"/etc/letsencrypt/live/{domain}/fullchain.pem",
                    "key": f"/etc/letsencrypt/live/{domain}/privkey.pem",
                    "fallback_port": fallback_addr,
                    "fallback_port": fallback_port,
                },
            }
        case _:
            config = {}

    return config


def prepare_cert(conn, domain, secret_id, secret_key):
    conn.sudo("pip install certbot certbot-dns-tencentcloud")
    conn.sudo("certbot plugins")
    env_str = (
        f"TENCENTCLOUD_SECRET_ID={secret_id} TENCENTCLOUD_SECRET_KEY={secret_key} "
    )
    conn.sudo(env_str + f"certbot certonly -a dns-tencentcloud -d {domain}")


def install(args):
    # print(args)
    domain = get_env("DOMAIN")

    if domain is None:
        return

    conn = Connection(args.host)
    conn.run("uname -a")

    if args.acquire_cert:
        secret_id = get_env("TENCENTCLOUD_SECRET_ID")
        secret_key = get_env("TENCENTCLOUD_SECRET_KEY")
        if secret_id is None or secret_key is None:
            exit(1)
        prepare_cert(conn, domain, secret_id, secret_key)

    download_url = "https://github.com/p4gefau1t/trojan-go/releases/download/v0.10.6/trojan-go-linux-amd64.zip"

    # download
    print(f"downloading with trojan url: {download_url}")
    ts = time.strftime("%Y%m%d_%H%M%S")
    conn.run(f"mkdir {ts}")
    conn.run(f"cd {ts} && curl -L {download_url} -o trojan.zip && unzip trojan.zip")

    # make config
    password = mk_random_password(20)
    print("making trojan config")
    config = mk_trojan_config(
        "server", password, domain, args.fallback_port, args.fallback_port
    )
    config_str = json.dumps(config, indent=2)
    conn.run(f"cd {ts} && echo {quote(config_str)} > config.json")

    # copy files
    conn.sudo("mkdir -p /etc/trojan-go")
    conn.sudo(f"mv {ts}/config.json /etc/trojan-go/config.json")
    conn.run(
        f"cd {ts} && sed -i 's|/usr/bin/trojan-go|/usr/local/bin/trojan-go|' example/trojan-go.service"
    )
    # TODO: make proper permission
    conn.run(f"cd {ts} && sed -i '/^User=/d' example/trojan-go.service")
    conn.sudo(f"cp {ts}/trojan-go /usr/local/bin")
    conn.sudo(f"cp {ts}/example/trojan-go.service /etc/systemd/system")

    # start nginx
    # TODO: make this more robust
    print("starting nginx")
    conn.run("command -v nginx || sudo apt install -y nginx")
    conn.sudo("systemctl start nginx")

    print("waiting to start nginx")
    time.sleep(3)
    # start services
    conn.sudo("systemctl enable trojan-go")
    conn.run("curl localhost:80 && sudo systemctl start trojan-go")

    # generate a local trojan config
    config = mk_trojan_config("client", password, domain, "", 0)
    config_str = json.dumps(config, indent=2)
    with open("config.json", "w") as f:
        f.write(config_str)


def bbr(args):
    conn = Connection(args.host)
    conn.run("uname -a")
    conn.sudo("bash -c 'echo net.core.default_qdisc=fq >> /etc/sysctl.conf'")
    conn.sudo("bash -c 'echo net.ipv4.tcp_congestion_control=bbr >> /etc/sysctl.conf'")
    conn.sudo("sysctl -p")
    conn.run("sysctl net.ipv4.tcp_congestion_control")


# 50% symbol, 50% alphanumuric
def mk_random_password(size: int) -> str:
    s = ["" for _ in range(size)]
    symbol_set = """!@#$%^&*-_+=;:"""
    for i in range(size):
        if random.random() < 0.5:
            s[i] = random.choice(symbol_set)
        elif random.random() < 0.7:
            s[i] = random.choice(string.digits)
        else:
            s[i] = random.choice(string.ascii_letters)
    return "".join(s)


parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

# Create parser for the "install" subcommand
install_parser = subparsers.add_parser(
    "install",
    help="by default, use certbot-dns-tencentcloud plugin to obtain cert from Let's Encrypt",
)
install_parser.add_argument("host", help="target host to operate")
install_parser.add_argument(
    "--acquire-cert", action="store_true", help="try to obtain cert"
)
install_parser.add_argument("-o", "--output", help="output client config json")
install_parser.add_argument(
    "--fallback-addr", required=True, type=str, help="fallback addr for server config"
)
install_parser.add_argument(
    "--fallback-port",
    type=int,
    default=443,
    help="fallback port for server config",
)
install_parser.set_defaults(func=install)

# Create parser for the "bbr" subcommand
install_parser = subparsers.add_parser(
    "bbr",
    help="enable bbr",
)
install_parser.add_argument("host", help="target host to operate")
install_parser.set_defaults(func=bbr)


def get_env(key) -> Optional[str]:
    val = os.getenv(key, None)
    if val is None or len(val) == 0:
        print(f"{key}")
        return None
    return val


def main():
    args = parser.parse_args()
    if not hasattr(args, "func"):
        print("invalid command")
        return
    # args.func(args)


if __name__ == "__main__":
    main()
