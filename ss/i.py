import os
import json
import random
import string
import argparse
import time
from base64 import b64encode
from shlex import quote
from typing import Optional, Dict

import requests
from fabric import Connection


# run_type = client or server
def mk_trojan_config(run_type, password, domain) -> Dict:
    config = {
        "run_type": run_type,
        "local_addr": "0.0.0.0" if run_type == "server" else "127.0.0.1",
        "local_port": 443 if run_type == "server" else 1080,
        "remote_addr": "127.0.0.1" if run_type == "server" else domain,
        "remote_port": 80 if run_type == "server" else 443,
        "password": [password],
        "log_level": 1,
        "ssl": {
            "cert": f"/etc/letsencrypt/live/{domain}/fullchain.pem"
            if run_type == "server"
            else "",
            "key": f"/etc/letsencrypt/live/{domain}/privkey.pem"
            if run_type == "server"
            else "",
            "key_password": "",
            "cipher": "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384",
            "cipher_tls13": "TLS_AES_128_GCM_SHA256:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_256_GCM_SHA384",
            "prefer_server_cipher": True,
            "alpn": ["http/1.1"],
            "alpn_port_override": {"h2": 81},
            "reuse_session": True,
            "session_ticket": False,
            "session_timeout": 600,
            "plain_http_response": "",
            "curves": "",
            "dhparam": "",
        },
        "tcp": {
            "prefer_ipv4": False,
            "no_delay": True,
            "keep_alive": True,
            "reuse_port": False,
            "fast_open": False,
            "fast_open_qlen": 20,
        },
        "mysql": {
            "enabled": False,
        },
    }
    if run_type == "client":
        config['ssl']['verify'] = True
        config['ssl']['verify_hostname'] = True
        config['ssl']['alpn'] = ['h2', 'http/1.1']

    return config


def mk_systemd_config(folder, user):
    s = f"""
[Unit]
Description=trojan
After=network.target

[Service]
Type=simple
WorkingDirectory=/home/{user}/{folder}/trojan
ExecStart=/home/{user}/{folder}/trojan/trojan -c config.json

[Install]
WantedBy=multi-user.target
    """
    return s


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

    if not args.skip_cert:
        secret_id = get_env("TENCENTCLOUD_SECRET_ID")
        secret_key = get_env("TENCENTCLOUD_SECRET_KEY")
        if secret_id is None or secret_key is None:
            exit(1)
        prepare_cert(conn, domain, secret_id, secret_key)

    if args.trojan_version:
        if not args.trojan_version.startswith("v"):
            print("invalid trojan version string")
            return
        download_url = f"https://github.com/trojan-gfw/trojan/releases/download/{args.trojan_version}/trojan-{args.trojan_version[1:]}-linux-amd64.tar.xz"
    elif args.trojan_url:
        download_url = args.trojan_url
    else:
        download_url = get_latest_trojan()

    if download_url is None:
        print("failed to get trojan url")
        return

    print(f"downloading with trojan url: {download_url}")
    ts = time.strftime("%Y%m%d_%H%M%S")
    conn.run(f"mkdir {ts}")
    conn.run(f"cd {ts} && curl -L {download_url} -o trojan.tar.xz && tar xvf trojan.tar.xz")

    password = mk_random_password(20)

    print("making trojan config")
    config = mk_trojan_config("server", password, domain)
    config_str = json.dumps(config, indent=2)
    conn.run(f"cd {ts}/trojan && echo {quote(config_str)} > config.json")

    print("systemd config")
    config = mk_systemd_config(ts, conn.user)
    b64_config = b64encode(config.encode()).decode()

    conn.sudo(f"bash -c 'echo {b64_config} | base64 -d > /etc/systemd/system/trojan.service'")
    conn.sudo("systemctl enable trojan")
    conn.sudo("systemctl start trojan")

    # generate a local trojan config
    config = mk_trojan_config("client", password, domain)
    config_str = json.dumps(config, indent=2)
    with open("config.json", 'w') as f:
        f.write(config_str)

def bbr(args):
    conn = Connection(args.host)
    conn.run("uname -a")
    conn.sudo("bash -c 'echo net.core.default_qdisc=fq >> /etc/sysctl.conf'")
    conn.sudo("bash -c 'echo net.ipv4.tcp_congestion_control=bbr >> /etc/sysctl.conf'")
    conn.sudo('sysctl -p')
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


def get_latest_trojan() -> Optional[str]:
    r = requests.get(
        "https://github.com/trojan-gfw/trojan/releases/latest", allow_redirects=False
    )
    if r.status_code != 302:
        print("error getting latest trojan")
        return None
    location = r.headers["Location"]
    v = os.path.basename(location)
    if not v.startswith("v"):
        print(f"failed to parse tag string {v}")
        return None
    return f"https://github.com/trojan-gfw/trojan/releases/download/{v}/trojan-{v[1:]}-linux-amd64.tar.xz"


parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

# Create parser for the "install" subcommand
install_parser = subparsers.add_parser(
    "install",
    help="by default, use certbot-dns-tencentcloud plugin to obtain cert from Let's Encrypt",
)
install_parser.add_argument("host", help="target host to operate")
install_parser.add_argument(
    "--trojan-version", help="use this version instead of the latest to download trojan"
)
install_parser.add_argument(
    "--trojan-url", help="use this url instead of the latest to download trojan"
)
install_parser.add_argument(
    "--skip-cert", action="store_true", help="do not try to obtain cert"
)
install_parser.add_argument(
    "-o", "--output", help="output client config json"
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
    args.func(args)


if __name__ == "__main__":
    main()
