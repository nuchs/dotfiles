#! /usr/bin/env python2
from subprocess import check_output

mailDir = "/home/nuchs/.mailaccounts/"

def get_pass(account):
    return check_output(["gpg", "-dq", mailDir + account + "pass.gpg"]).strip("\n")

def get_user(account):
    with open(mailDir + "my" + account, "r") as file:
        return file.read().strip("\n")
