#!/usr/bin/env bash

if [ $UID -ne 0 ]; then
	echo need to run as root
	exit 1
fi

if [ -z $1 ]; then
	echo domain required
	exit 1
fi

domain=$1

pip3 install certbot certbot-dns-tencentcloud
certbot plugins

certbot certonly -a dns-tencentcloud -d $domain
