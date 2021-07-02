#! /usr/bin/env bash

# Restart the FAW instance
ps aux | grep '[i]n-docker\|[q]ueue_client\|[v]ue' | awk '{print $2}' | xargs kill

